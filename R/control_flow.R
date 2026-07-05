# ---------------------------------------------------------------------------
# Control-flow unwrapping
#
# T-SQL ETL scripts routinely wrap statements in procedural control flow:
#
#   IF OBJECT_ID('tempdb..#t') IS NOT NULL DROP TABLE #t
#   IF @load = 1 BEGIN SELECT ... INTO #t FROM ...; END
#   WHILE @i < 10 BEGIN INSERT INTO ... SELECT ...; SET @i += 1; END
#
# The statement classifier marks these "unknown", which would silently drop
# any SELECT lineage inside them. unwrap_control_flow() recovers that
# lineage by extracting the governed statements (BEGIN...END bodies, or the
# single statement following the condition) and splicing them back into the
# statement stream. Conditional execution itself is NOT modelled — the
# inner statements are treated as if they always run, and a note is logged
# so the caller can surface that caveat.
# ---------------------------------------------------------------------------

# Tokenise the word tokens of a SQL string, recording each word's start
# position (1-based character index) and its parenthesis depth. Comments
# (line and nested block), string literals, bracket-quoted and double-quoted
# identifiers are skipped entirely, so keywords inside them are invisible.
sql_word_tokens <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  words  <- character()
  starts <- integer()
  depths <- integer()
  state  <- "normal"
  cdepth <- 0L   # block-comment nesting
  pdepth <- 0L   # parenthesis depth
  i <- 1L

  while (i <= n) {
    ch <- chars[i]
    ch2 <- if (i < n) chars[i + 1L] else ""

    if (state == "normal") {
      if (ch == "-" && ch2 == "-") {
        state <- "line"; i <- i + 2L
      } else if (ch == "/" && ch2 == "*") {
        state <- "block"; cdepth <- 1L; i <- i + 2L
      } else if (ch == "'") {
        state <- "string"; i <- i + 1L
      } else if (ch == "[") {
        state <- "bracket"; i <- i + 1L
      } else if (ch == '"') {
        state <- "dquote"; i <- i + 1L
      } else if (ch == "(") {
        pdepth <- pdepth + 1L; i <- i + 1L
      } else if (ch == ")") {
        pdepth <- pdepth - 1L; i <- i + 1L
      } else if (grepl("[A-Za-z_@#]", ch)) {
        j <- i
        while (j <= n && grepl("[A-Za-z0-9_@#$]", chars[j])) j <- j + 1L
        words  <- c(words, toupper(paste(chars[i:(j - 1L)], collapse = "")))
        starts <- c(starts, i)
        depths <- c(depths, pdepth)
        i <- j
      } else {
        i <- i + 1L
      }
    } else if (state == "line") {
      if (ch == "\n") state <- "normal"
      i <- i + 1L
    } else if (state == "block") {
      if (ch == "/" && ch2 == "*") {
        cdepth <- cdepth + 1L; i <- i + 2L
      } else if (ch == "*" && ch2 == "/") {
        cdepth <- cdepth - 1L; i <- i + 2L
        if (cdepth == 0L) state <- "normal"
      } else {
        i <- i + 1L
      }
    } else if (state == "string") {
      if (ch == "'" && ch2 == "'") i <- i + 2L
      else { if (ch == "'") state <- "normal"; i <- i + 1L }
    } else if (state == "bracket") {
      if (ch == "]" && ch2 == "]") i <- i + 2L
      else { if (ch == "]") state <- "normal"; i <- i + 1L }
    } else {  # dquote
      if (ch == '"' && ch2 == '"') i <- i + 2L
      else { if (ch == '"') state <- "normal"; i <- i + 1L }
    }
  }

  tibble::tibble(word = words, start = starts, depth = depths)
}

# Extract the executable body from a control-flow statement.
#
# Two shapes are handled:
#   (a) BEGIN ... END blocks (including several, e.g. IF ... BEGIN x END
#       ELSE BEGIN y END): every top-level block body is extracted and the
#       bodies are joined with ';'. BEGIN TRAN[SACTION] is not a block;
#       BEGIN TRY / BEGIN CATCH are. A CASE ... END inside the block is
#       tracked so its END does not close the block early.
#   (b) single-statement IF/WHILE (no BEGIN): the text from the first
#       depth-0 statement-starting keyword after the condition.
#
# Returns the body text, or NULL when no executable body is found.
extract_control_flow_body <- function(text) {
  toks <- sql_word_tokens(text)
  nt <- nrow(toks)
  if (nt == 0L) return(NULL)

  bodies <- character()
  stack <- character(0)     # open "begin" / "case" markers
  body_start <- NA_integer_
  k <- 1L

  while (k <= nt) {
    w <- toks$word[k]
    nxt <- if (k < nt) toks$word[k + 1L] else ""

    if (w == "CASE") {
      stack <- c(stack, "case")

    } else if (w == "BEGIN") {
      if (nxt %in% c("TRAN", "TRANSACTION")) {
        k <- k + 2L
        next
      }
      has_marker <- nxt %in% c("TRY", "CATCH")
      stack <- c(stack, "begin")
      if (sum(stack == "begin") == 1L) {
        # Body starts right after BEGIN (or after the TRY/CATCH marker).
        marker_k <- if (has_marker) k + 1L else k
        body_start <- toks$start[marker_k] + nchar(toks$word[marker_k])
      }
      if (has_marker) k <- k + 1L

    } else if (w == "END") {
      if (length(stack) > 0L) {
        top <- stack[length(stack)]
        stack <- stack[-length(stack)]
        if (identical(top, "begin") && sum(stack == "begin") == 0L &&
            !is.na(body_start)) {
          bodies <- c(bodies, substr(text, body_start, toks$start[k] - 1L))
          body_start <- NA_integer_
        }
      }
      if (nxt %in% c("TRY", "CATCH")) k <- k + 1L
    }
    k <- k + 1L
  }

  bodies <- trimws(bodies)
  bodies <- bodies[nzchar(bodies)]
  if (length(bodies) > 0L) {
    return(paste(bodies, collapse = ";\n"))
  }

  # No block: single-statement IF/WHILE form. Find the first depth-0
  # statement-starting keyword after the leading IF/WHILE/ELSE token —
  # a SELECT inside IF EXISTS (...) sits at depth 1 and is skipped.
  starters <- c("SELECT", "INSERT", "UPDATE", "DELETE", "DROP", "CREATE",
                "EXEC", "EXECUTE", "PRINT", "SET", "DECLARE", "WITH",
                "MERGE", "TRUNCATE")
  cand <- which(toks$depth == 0L & toks$word %in% starters &
                  seq_len(nt) > 1L)
  if (length(cand) > 0L) {
    return(substr(text, toks$start[cand[1L]], nchar(text)))
  }
  NULL
}

# Replace control-flow wrapper statements ("unknown" kind starting with
# IF / WHILE / ELSE / BEGIN) with the statements they govern, re-splitting
# and re-classifying the extracted bodies (recursively, up to 3 levels).
# Statements whose body yields nothing usable are left untouched.
#
# Returns list(statements = tibble with renumbered seq, notes = character
# vector of unwrap messages for the skip log).
unwrap_control_flow <- function(stmts, depth = 0L) {
  notes <- character()
  if (nrow(stmts) == 0L || depth >= 3L) {
    return(list(statements = stmts, notes = notes))
  }

  rows <- list()
  for (i in seq_len(nrow(stmts))) {
    row <- stmts[i, ]
    kw <- leading_keywords(row$text)

    if (identical(row$kind, "unknown") &&
        grepl("^(IF|WHILE|ELSE|BEGIN)\\b", kw)) {
      body <- extract_control_flow_body(row$text)
      if (!is.null(body) && nzchar(trimws(body))) {
        sub <- classify_statements(split_statements(body))
        subres <- unwrap_control_flow(sub, depth + 1L)
        sub <- subres$statements
        if (nrow(sub) > 0L && any(sub$kind != "unknown")) {
          notes <- c(notes, subres$notes, sprintf(
            paste0("statement %d: IF/WHILE/BEGIN wrapper unwrapped; ",
                   "conditional execution is not modelled"),
            row$seq
          ))
          for (j in seq_len(nrow(sub))) {
            rows[[length(rows) + 1L]] <- sub[j, ]
          }
          next
        }
      }
    }
    rows[[length(rows) + 1L]] <- row
  }

  out <- dplyr::bind_rows(rows)
  out$seq <- seq_len(nrow(out))
  list(statements = out, notes = notes)
}
