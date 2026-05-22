# ---------------------------------------------------------------------------
# Statement classifier
#
# Takes the raw tibble from split_statements() and adds a `kind` column by
# matching leading keywords against an ordered rule table. The key discriminator
# for ambiguous forms (INSERT…SELECT vs INSERT…VALUES, CTAS vs CREATE TABLE) is
# whether a SELECT keyword exists at paren-depth 0 in the comment-stripped text.
# ---------------------------------------------------------------------------

# Strip line comments (--…\n) and block comments (/* … */) from a SQL string.
# Returns only the comment-free text so keyword matching is unambiguous.
strip_comments <- function(text) {
  # Remove block comments first (greedy-minimal, DOTALL via perl).
  text <- gsub("/\\*.*?\\*/", " ", text, perl = TRUE)
  # Remove line comments.
  text <- gsub("--[^\n]*", " ", text)
  text
}

# Return TRUE if the comment-stripped text contains a SELECT keyword at
# paren-depth 0. This discriminates INSERT…SELECT from INSERT…VALUES and
# CREATE TABLE…AS SELECT (CTAS) from a plain CREATE TABLE.
has_depth0_select <- function(text) {
  text <- strip_comments(text)
  chars <- strsplit(text, "")[[1]]
  depth <- 0L
  i <- 1L
  n <- length(chars)

  while (i <= n) {
    ch <- chars[i]
    if (ch == "(") {
      depth <- depth + 1L
      i <- i + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
      i <- i + 1L
    } else if (depth == 0L && (ch == "S" || ch == "s")) {
      # Check for SELECT (case-insensitive) as a whole word.
      candidate <- paste(chars[i:min(i + 5L, n)], collapse = "")
      if (grepl("^SELECT\\b", candidate, ignore.case = TRUE)) {
        return(TRUE)
      }
      i <- i + 1L
    } else {
      i <- i + 1L
    }
  }
  FALSE
}

# Extract the leading keyword(s) from a comment-stripped SQL string, upper-
# cased and collapsed to a single space, for rule matching.
leading_keywords <- function(text) {
  text <- strip_comments(text)
  # Pull the first few meaningful tokens (letters only — skip punctuation).
  tokens <- regmatches(text, gregexpr("[A-Za-z_@#][A-Za-z0-9_@#]*", text))[[1]]
  if (length(tokens) == 0L) return("")
  # Upper-case the first four tokens; enough to cover all rule-table patterns.
  paste(toupper(tokens[seq_len(min(4L, length(tokens)))]), collapse = " ")
}

# Classify a single statement text string into one of the design rule-table
# kinds. Returns a length-1 character vector.
classify_one <- function(text) {
  kw <- leading_keywords(text)
  has_sel <- NULL  # compute lazily — only when needed

  # Rule table in priority order (matches design §2).
  if (grepl("^DECLARE @", kw))                            return("declare")
  if (grepl("^SET @", kw))                                return("set_var")
  if (grepl("^DROP TABLE", kw))                           return("drop")
  if (grepl("^CREATE .* INDEX|^CREATE INDEX", kw))        return("create_index")

  if (grepl("^CREATE TABLE", kw)) {
    has_sel <- has_depth0_select(text)
    return(if (has_sel) "select_into" else "create_table")
  }

  if (grepl("^WITH\\b", kw)) {
    has_sel <- has_depth0_select(text)
    if (!has_sel) return("unknown")
    # WITH ... SELECT ... INTO is a SELECT INTO; plain WITH ... SELECT is select.
    clean <- strip_comments(text)
    if (grepl("\\bINTO\\b", clean, ignore.case = TRUE)) return("select_into")
    return("select")
  }

  if (grepl("^INSERT INTO|^INSERT ", kw)) {
    has_sel <- has_depth0_select(text)
    return(if (has_sel) "insert_select" else "insert_values")
  }

  if (grepl("^SELECT\\b", kw)) {
    # SELECT … INTO … is a select_into; plain SELECT is a select.
    clean <- strip_comments(text)
    if (grepl("\\bINTO\\b", clean, ignore.case = TRUE)) return("select_into")
    return("select")
  }

  "unknown"
}

#' Classify raw split statements by SQL kind
#'
#' Adds a `kind` column to the tibble returned by [split_statements()].
#' Classification follows the ordered rule table in the package design
#' (DECLARE > SET > DROP > INDEX > CREATE TABLE > WITH > INSERT > SELECT),
#' using a paren-depth 0 SELECT test to discriminate CTAS/INSERT-SELECT
#' from plain CREATE TABLE / INSERT-VALUES.
#'
#' @param statements_raw A tibble with at least `seq` and `text` columns,
#'   as returned by [split_statements()].
#'
#' @return The input tibble with an additional `kind` character column. Values:
#'   `"declare"`, `"set_var"`, `"drop"`, `"create_index"`, `"create_table"`,
#'   `"select_into"`, `"select"`, `"insert_select"`, `"insert_values"`,
#'   `"unknown"`.
#' @export
classify_statements <- function(statements_raw) {
  kinds <- vapply(statements_raw$text, classify_one, character(1),
                  USE.NAMES = FALSE)
  statements_raw$kind <- kinds
  statements_raw
}
