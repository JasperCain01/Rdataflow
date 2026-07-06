# ---------------------------------------------------------------------------
# Statement classifier
#
# Takes the raw tibble from split_statements() and adds a `kind` column by
# matching leading keywords against an ordered rule table. The key discriminator
# for ambiguous forms (INSERT…SELECT vs INSERT…VALUES, CTAS vs CREATE TABLE) is
# whether a SELECT keyword exists at paren-depth 0 in the comment-stripped text.
# ---------------------------------------------------------------------------

# Strip line comments (--…\n) and block comments (/* … */, including nested
# comments, which T-SQL allows) from a SQL string. String literals and
# bracket-quoted identifiers are preserved verbatim so that comment markers
# inside them ('--', '/*') are not treated as comments. Each removed comment
# is replaced by a single space so token boundaries survive.
strip_comments <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  out <- character(n)   # preallocate; unused slots stay ""
  state <- "normal"
  depth <- 0L           # block-comment nesting depth
  i <- 1L
  o <- 0L               # output write position

  put <- function(x) {
    o <<- o + 1L
    out[o] <<- x
  }

  while (i <= n) {
    ch <- chars[i]
    ch2 <- if (i < n) chars[i + 1L] else ""

    if (state == "normal") {
      if (ch == "-" && ch2 == "-") {
        state <- "line_comment"
        i <- i + 2L
      } else if (ch == "/" && ch2 == "*") {
        state <- "block_comment"
        depth <- 1L
        i <- i + 2L
      } else if (ch == "'") {
        put(ch); i <- i + 1L
        state <- "string"
      } else if (ch == "[") {
        put(ch); i <- i + 1L
        state <- "bracket"
      } else if (ch == '"') {
        put(ch); i <- i + 1L
        state <- "dquote"
      } else {
        put(ch); i <- i + 1L
      }

    } else if (state == "line_comment") {
      if (ch == "\n") {
        put(" "); put("\n")
        state <- "normal"
      }
      i <- i + 1L

    } else if (state == "block_comment") {
      if (ch == "/" && ch2 == "*") {
        depth <- depth + 1L
        i <- i + 2L
      } else if (ch == "*" && ch2 == "/") {
        depth <- depth - 1L
        i <- i + 2L
        if (depth == 0L) {
          put(" ")
          state <- "normal"
        }
      } else {
        i <- i + 1L
      }

    } else if (state == "string") {
      put(ch); i <- i + 1L
      if (ch == "'") {
        if (ch2 == "'") {          # doubled '' escape — still in string
          put(ch2); i <- i + 1L
        } else {
          state <- "normal"
        }
      }

    } else if (state == "bracket") {
      put(ch); i <- i + 1L
      if (ch == "]") {
        if (ch2 == "]") {          # doubled ]] escape
          put(ch2); i <- i + 1L
        } else {
          state <- "normal"
        }
      }

    } else if (state == "dquote") {
      put(ch); i <- i + 1L
      if (ch == '"') {
        if (ch2 == '"') {
          put(ch2); i <- i + 1L
        } else {
          state <- "normal"
        }
      }
    }
  }

  paste(out[seq_len(o)], collapse = "")
}

# Return TRUE if `keyword` appears as a whole word at paren-depth 0 in the
# comment-stripped text, outside string literals and quoted identifiers.
# This discriminates INSERT…SELECT from INSERT…VALUES, CTAS from plain
# CREATE TABLE, and SELECT…INTO from a plain SELECT (a subquery's SELECT or
# a string containing 'into' will not match).
has_depth0_keyword <- function(text, keyword) {
  text <- strip_comments(text)
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  kw_chars <- strsplit(toupper(keyword), "", fixed = TRUE)[[1]]
  kw_len <- length(kw_chars)
  is_word <- function(c) grepl("[A-Za-z0-9_@#]", c)

  depth <- 0L
  state <- "normal"
  i <- 1L

  while (i <= n) {
    ch <- chars[i]
    ch2 <- if (i < n) chars[i + 1L] else ""

    if (state == "normal") {
      if (ch == "'") {
        state <- "string"; i <- i + 1L
      } else if (ch == "[") {
        state <- "bracket"; i <- i + 1L
      } else if (ch == '"') {
        state <- "dquote"; i <- i + 1L
      } else if (ch == "(") {
        depth <- depth + 1L; i <- i + 1L
      } else if (ch == ")") {
        depth <- depth - 1L; i <- i + 1L
      } else if (depth == 0L && toupper(ch) == kw_chars[1]) {
        # Candidate keyword start: check the full word with boundaries.
        end <- i + kw_len - 1L
        before_ok <- i == 1L || !is_word(chars[i - 1L])
        word_ok <- end <= n &&
          identical(toupper(paste(chars[i:end], collapse = "")),
                    paste(kw_chars, collapse = ""))
        after_ok <- end >= n || !is_word(chars[end + 1L])
        if (before_ok && word_ok && after_ok) return(TRUE)
        i <- i + 1L
      } else {
        i <- i + 1L
      }
    } else if (state == "string") {
      if (ch == "'" && ch2 == "'") { i <- i + 2L }
      else { if (ch == "'") state <- "normal"; i <- i + 1L }
    } else if (state == "bracket") {
      if (ch == "]" && ch2 == "]") { i <- i + 2L }
      else { if (ch == "]") state <- "normal"; i <- i + 1L }
    } else if (state == "dquote") {
      if (ch == '"' && ch2 == '"') { i <- i + 2L }
      else { if (ch == '"') state <- "normal"; i <- i + 1L }
    }
  }
  FALSE
}

# Backwards-compatible wrapper used throughout the classifier.
has_depth0_select <- function(text) has_depth0_keyword(text, "SELECT")

# Extract the leading keyword(s) from a comment-stripped SQL string, upper-
# cased and collapsed to a single space, for rule matching.
leading_keywords <- function(text) {
  text <- strip_comments(text)
  # Pull the first few meaningful tokens (letters only — skip punctuation).
  tokens <- regmatches(text, gregexpr("[A-Za-z_@#][A-Za-z0-9_@#]*", text))[[1]]
  if (length(tokens) == 0L) return("")
  # Upper-case the first six tokens; enough to cover all rule-table patterns
  # (e.g. CREATE UNIQUE NONCLUSTERED COLUMNSTORE INDEX name).
  paste(toupper(tokens[seq_len(min(6L, length(tokens)))]), collapse = " ")
}

# Classify a single statement text string into one of the design rule-table
# kinds. Returns a length-1 character vector.
classify_one <- function(text) {
  kw <- leading_keywords(text)

  # Rule table in priority order (matches design §2). CREATE TABLE is tested
  # before CREATE INDEX so that a table whose name contains the word INDEX
  # (e.g. dbo.index_stats) is not misclassified; the INDEX rule itself uses
  # a word boundary for the same reason.
  if (grepl("^DECLARE @", kw))                            return("declare")
  if (grepl("^SET @", kw))                                return("set_var")
  if (grepl("^DROP TABLE", kw))                           return("drop")
  if (grepl("^BEGIN TRAN\\b|^BEGIN TRANSACTION\\b|^COMMIT\\b|^ROLLBACK\\b",
            kw))                                          return("transaction")
  if (grepl("^MERGE\\b", kw))                              return("merge")
  # Every UPDATE is routed through the select path; a plain `UPDATE t SET
  # ... FROM ... JOIN ...` has no SELECT keyword at all, so (unlike
  # CREATE TABLE/INSERT) there's no depth-0-SELECT form to discriminate.
  if (grepl("^UPDATE\\b", kw))                             return("update")

  if (grepl("^CREATE TABLE", kw)) {
    return(if (has_depth0_select(text)) "select_into" else "create_table")
  }

  if (grepl("^CREATE\\b.*\\bINDEX\\b", kw))               return("create_index")

  if (grepl("^WITH\\b", kw)) {
    if (!has_depth0_select(text)) return("unknown")
    # WITH ... SELECT ... INTO is a SELECT INTO; plain WITH ... SELECT is select.
    if (has_depth0_keyword(text, "INTO")) return("select_into")
    return("select")
  }

  if (grepl("^INSERT INTO|^INSERT ", kw)) {
    return(if (has_depth0_select(text)) "insert_select" else "insert_values")
  }

  if (grepl("^SELECT\\b", kw)) {
    # SELECT … INTO … is a select_into; plain SELECT is a select. The INTO
    # test is depth-0 and string-aware so a subquery's INTO-like tokens or a
    # string literal containing "into" cannot trigger it.
    if (has_depth0_keyword(text, "INTO")) return("select_into")
    return("select")
  }

  "unknown"
}

#' Classify raw split statements by SQL kind
#'
#' Adds a `kind` column to the tibble returned by [split_statements()].
#' Classification follows the ordered rule table in the package design
#' (DECLARE > SET > DROP > MERGE > UPDATE > CREATE TABLE > INDEX > WITH >
#' INSERT > SELECT), using a paren-depth 0, string-aware SELECT test to
#' discriminate CTAS/INSERT-SELECT from plain CREATE TABLE / INSERT-VALUES.
#'
#' @param statements_raw A tibble with at least `seq` and `text` columns,
#'   as returned by [split_statements()].
#'
#' @return The input tibble with an additional `kind` character column. Values:
#'   `"declare"`, `"set_var"`, `"drop"`, `"transaction"`, `"merge"`,
#'   `"update"`, `"create_index"`, `"create_table"`, `"select_into"`,
#'   `"select"`, `"insert_select"`, `"insert_values"`, `"unknown"`.
#' @export
classify_statements <- function(statements_raw) {
  kinds <- vapply(statements_raw$text, classify_one, character(1),
                  USE.NAMES = FALSE)
  statements_raw$kind <- kinds
  statements_raw
}
