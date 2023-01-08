#' Get last non-empty row from selection
#'
#' Ignores spaces and tabs
#'
#' @param text Text from `getSourceEditorContext()$selection`
#' @param end_row Last row index in selection
#' @param start_row First row index in selection, for offset
#'
#' @return Integer of 1 + Index of last non-empty row
get_ending_position <- function(text, end_row, start_row) {
  end_i <- end_row - start_row

  if (end_i == 0)
    return (end_row + 1L)

  contents <- strsplit(text, "\n")[[1]]
  blank_lines <- grepl("^[ \t]*$", contents, perl = TRUE)

  if (!blank_lines[end_i])
    return(end_i + 1L + start_row)

  while (blank_lines[end_i]) {
    end_i <- end_i - 1L
  }

  end_i + start_row
}
