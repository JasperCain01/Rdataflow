# ---------------------------------------------------------------------------
# Statement splitter
#
# Implements a single-pass character state machine that splits a T-SQL script
# into individual statements at semicolons or lone GO batch terminators. The
# machine tracks quoting and comment context so that terminators embedded in
# string literals, block comments, line comments, or bracketed identifiers are
# NOT treated as boundaries.
# ---------------------------------------------------------------------------

#' Split a T-SQL script into individual statements
#'
#' Applies a character-level state machine that recognises `;` and lone `GO`
#' batch terminators only when they appear outside string literals, block
#' comments (including nested `/* /* */ */` comments), line comments, and
#' bracket-quoted identifiers. `GO` is only treated as a terminator when it
#' is the first token on its line (matching SQL Server's rule that `GO` must
#' appear alone on a line); an optional repeat count (`GO 5`) is consumed
#' with it. Original text (including comments) is preserved in each chunk.
#'
#' @param sql A length-1 character vector containing the full SQL script.
#'
#' @return A tibble with columns:
#'   - `seq` (integer): 1-based statement index in script order.
#'   - `text` (character): raw statement text, trimmed of leading/trailing
#'     whitespace, with the terminator stripped.
#'   - `terminator` (character): `";"`, `"GO"`, or `""` (final statement
#'     with no trailing terminator).
#'
#'   Empty chunks (e.g. between `;;`) are silently dropped.
#' @export
split_statements <- function(sql) {
  stopifnot(is.character(sql), length(sql) == 1)

  # Normalize encoding to UTF-8 before any byte-level work. Files saved by SSMS
  # as UTF-8 BOM or other Windows encodings cause gsub() to abort with
  # "input string 1 is invalid in this locale". iconv() with sub = "" silently
  # drops unmappable bytes rather than aborting.
  sql <- iconv(sql, to = "UTF-8", sub = "")

  # Expand any Windows CRLF to LF so line-comment detection is uniform.
  sql <- gsub("\r\n", "\n", sql, fixed = TRUE)

  chars <- strsplit(sql, "")[[1]]
  n <- length(chars)

  # Output accumulators: a list of completed statement records.
  chunks <- list()

  # State machine variables.
  state    <- "normal"    # current parse state
  buf      <- character() # characters accumulating for the current statement
  seq_idx  <- 1L          # next statement index
  comment_depth <- 0L     # nesting depth inside block comments (T-SQL nests)

  i <- 1L  # current character position (1-based)

  # Helper: flush buffer as a completed statement, reset buf.
  flush_chunk <- function(term) {
    text <- trimws(paste(buf, collapse = ""))
    if (nzchar(text)) {
      chunks[[seq_idx]] <<- list(seq = seq_idx, text = text, terminator = term)
      seq_idx <<- seq_idx + 1L
    }
    buf <<- character()
  }

  while (i <= n) {
    ch <- chars[i]
    ch2 <- if (i < n) chars[i + 1L] else ""

    if (state == "normal") {

      # --- Enter line comment
      if (ch == "-" && ch2 == "-") {
        buf <- c(buf, ch, ch2)
        i <- i + 2L
        state <- "line_comment"

      # --- Enter block comment
      } else if (ch == "/" && ch2 == "*") {
        buf <- c(buf, ch, ch2)
        i <- i + 2L
        state <- "block_comment"
        comment_depth <- 1L

      # --- Enter N-prefix or bare string literal
      } else if ((ch == "N" || ch == "n") && ch2 == "'") {
        buf <- c(buf, ch, ch2)
        i <- i + 2L
        state <- "string"

      } else if (ch == "'") {
        buf <- c(buf, ch)
        i <- i + 1L
        state <- "string"

      # --- Enter bracket-quoted identifier
      } else if (ch == "[") {
        buf <- c(buf, ch)
        i <- i + 1L
        state <- "bracket_ident"

      # --- Enter double-quoted identifier
      } else if (ch == '"') {
        buf <- c(buf, ch)
        i <- i + 1L
        state <- "quoted_ident"

      # --- Semicolon terminator
      } else if (ch == ";") {
        flush_chunk(";")
        i <- i + 1L

      # --- Potential GO batch terminator
      # SQL Server requires GO to be the first token on its line: only
      # whitespace may precede it back to the previous newline (or start of
      # input). After GO, only whitespace, an optional repeat count (GO 5),
      # and then end-of-line / end-of-input / ';' may follow. Anything else
      # (e.g. GOTO, "SELECT 1 AS go") is ordinary text.
      } else if ((ch == "G" || ch == "g") && (ch2 == "O" || ch2 == "o")) {
        # Scan backwards: only spaces/tabs allowed between line start and G.
        j <- i - 1L
        while (j >= 1L && chars[j] %in% c(" ", "\t")) j <- j - 1L
        prev_ok <- j < 1L || chars[j] %in% c("\n", "\r")

        # Scan forwards past optional whitespace and repeat count.
        k <- i + 2L
        while (k <= n && chars[k] %in% c(" ", "\t")) k <- k + 1L
        while (k <= n && chars[k] >= "0" && chars[k] <= "9") k <- k + 1L
        while (k <= n && chars[k] %in% c(" ", "\t")) k <- k + 1L
        next_ok <- k > n || chars[k] %in% c("\n", "\r", ";")

        if (prev_ok && next_ok) {
          # A lone GO line: consume it (and any repeat count), then flush.
          flush_chunk("GO")
          i <- k
        } else {
          buf <- c(buf, ch)
          i <- i + 1L
        }

      } else {
        buf <- c(buf, ch)
        i <- i + 1L
      }

    } else if (state == "line_comment") {
      # Line comments end at newline; the newline belongs to the next state.
      buf <- c(buf, ch)
      i <- i + 1L
      if (ch == "\n") state <- "normal"

    } else if (state == "block_comment") {
      # T-SQL block comments nest: /* outer /* inner */ still comment */.
      # Track depth so we only return to normal at the matching close.
      if (ch == "/" && ch2 == "*") {
        buf <- c(buf, ch, ch2)
        i <- i + 2L
        comment_depth <- comment_depth + 1L
      } else if (ch == "*" && ch2 == "/") {
        buf <- c(buf, ch, ch2)
        i <- i + 2L
        comment_depth <- comment_depth - 1L
        if (comment_depth == 0L) state <- "normal"
      } else {
        buf <- c(buf, ch)
        i <- i + 1L
      }

    } else if (state == "string") {
      # String literals end at a lone ' not followed by another ' (doubled
      # quotes are an escape — stay in string state).
      buf <- c(buf, ch)
      i <- i + 1L
      if (ch == "'" && ch2 != "'") {
        state <- "normal"
      } else if (ch == "'" && ch2 == "'") {
        # Consume the second quote of the escape pair before looping.
        buf <- c(buf, ch2)
        i <- i + 1L
      }

    } else if (state == "bracket_ident") {
      # Bracket identifiers end at ] not followed by another ] (doubled ]]
      # is an escape — stay in bracket_ident state).
      buf <- c(buf, ch)
      i <- i + 1L
      if (ch == "]" && ch2 != "]") {
        state <- "normal"
      } else if (ch == "]" && ch2 == "]") {
        buf <- c(buf, ch2)
        i <- i + 1L
      }

    } else if (state == "quoted_ident") {
      # Double-quoted identifiers end at a lone " not followed by another ".
      buf <- c(buf, ch)
      i <- i + 1L
      if (ch == '"' && ch2 != '"') {
        state <- "normal"
      } else if (ch == '"' && ch2 == '"') {
        buf <- c(buf, ch2)
        i <- i + 1L
      }
    }
  }

  # Flush any trailing statement (no terminator).
  flush_chunk("")

  if (length(chunks) == 0) {
    return(tibble::tibble(
      seq = integer(),
      text = character(),
      terminator = character()
    ))
  }

  tibble::tibble(
    seq         = vapply(chunks, `[[`, integer(1),  "seq"),
    text        = vapply(chunks, `[[`, character(1), "text"),
    terminator  = vapply(chunks, `[[`, character(1), "terminator")
  )
}
