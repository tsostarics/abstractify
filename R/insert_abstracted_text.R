#' Insert and reformat function definition
#'
#' @param fx_text Text to insert, a function definition
#' @param start_row Starting row of selection
#' @param new_end_row Ending row for insertion, from `get_ending_position`
#' @param doc_id Active document id returned from `getSourceEditorContext()`
insert_abstracted_text <- function(fx_text,
                                   start_row,
                                   new_end_row,
                                   doc_id) {
  stopifnot(length(fx_text) == 1L)
  rstudioapi::insertText(
    location =
      rstudioapi::document_range(c(start_row, 1),
                                 c(new_end_row, 1)),
    text = fx_text,
    id = doc_id
  )

  # Select the inserted text and run reformatCode (Ctrl+Shift+A) to indent
  rstudioapi::setSelectionRanges(c(start_row, 1, new_end_row, 2), id = doc_id)
  rstudioapi::executeCommand("reformatCode")

  # Highlight 'new_function' so user can start typing actual function name
  rstudioapi::setCursorPosition(c(start_row, 1, start_row, 13), id = doc_id)
  rstudioapi::executeCommand("activateSource") # Ensures focus on source pane

  # note: if abstractify() is run in the console, focus will not change from
  #       the console to the source pane again, but using the addin works fine
}
