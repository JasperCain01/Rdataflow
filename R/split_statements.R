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
#' comments, line comments, and bracket-quoted identifiers. Original text
#' (including comments) is preserved in each chunk.
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
      # Lone GO: must be preceded by newline/start and followed by
      # newline/end/whitespace (not part of a longer identifier like GOTO).
      } else if ((ch == "G" || ch == "g") && (ch2 == "O" || ch2 == "o")) {
        # Check that the character after GO is end-of-input, whitespace,
        # newline, or semicolon — and that the character before G was start,
        # newline, or whitespace.
        ch3 <- if (i + 1L < n) chars[i + 2L] else ""
        prev_ch <- if (i > 1L) chars[i - 1L] else "\n"

        prev_ok <- prev_ch %in% c("\n", "\r", " ", "\t") || i == 1L
        next_ok <- ch3 == "" || ch3 %in% c("\n", "\r", " ", "\t", ";")

        if (prev_ok && next_ok) {
          # This is a lone GO: consume it (don't add to buf) and flush.
          flush_chunk("GO")
          i <- i + 2L
          # Also skip any trailing whitespace/newline on the GO line.
          while (i <= n && chars[i] %in% c(" ", "\t")) i <- i + 1L
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
      # Block comments end at */ (both chars consumed, transition to normal).
      buf <- c(buf, ch)
      i <- i + 1L
      if (ch == "*" && ch2 == "/") {
        buf <- c(buf, ch2)
        i <- i + 1L
        state <- "normal"
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
