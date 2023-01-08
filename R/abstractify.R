#' Abstract highlighted text to new function call
#'
#' Sometimes you write a bunch of lines of code and decide that it should
#' actually be a function so that you can reuse it with different arguments.
#' You would need to identify which symbols in the code need to be abstracted
#' over and added to the list of arguments. It can be tedious to hunt down
#' everything, so this function will do it for you by identifying all the
#' symbols in some highlighted text and finding which ones are not accounted
#' for by function calls or builtin operators in the global environment.
#'
#' This function will add text to the currently active source document,
#' reformat it, then highlight and shift focus to the new function name.
#'
#' @export
abstractify <- function() {
  # Extract information from the highlighted text
  highlighted_text <- rstudioapi::getSourceEditorContext()

  doc_id <- highlighted_text$id # Used to write results back to the right doc
  selection <- highlighted_text$selection[[1]] # Highlighted text

  # Get the extents of the highlighted text
  start_row <- selection$range$start[1]
  end_row <- selection$range$end[1]

  # Calculate which row to add a final } to based on where the text stops,
  # ignoring potentially many additional blank highlighted lines
  new_end_row <- get_ending_position(selection$text, end_row, start_row)

  # Parse highlighted code and produce a new function definition that is
  # abstracted away from the global symbols
  abstracted_text <- get_abstracted_text(selection$text)

  ## RStudio API calls to insert and reformat text
  # Insert the text at the appropriate location

  insert_abstracted_text(abstracted_text, start_row, new_end_row, doc_id)
}

