#' Abstract a function based on global environment
#'
#' Given highlighted text from `getSourceEditorContext()$selection`, determine
#' which symbols are in the global environment and should be abstracted over
#' as a function call argument. This will ignore any symbols corresponding to
#' function calls, operators, unquoted vars in tidyverse verbs, and builtin
#' symbols.
#'
#' @param text Text from `getSourceEditorContext()$selection`
#'
#' @return A function definition of the following form:
#'
#' new_function <- function(..identified symbols..) {
#'  ...highlighted text...
#' }
get_abstracted_text <- function(text) {
  symbols_to_abstract <- .parse_symbols(text)
  # Trim leading/trailing whitespace
  text <- gsub("\\s+$|^\\s+", "", text, perl = TRUE)

  # Based on which symbols need to be abstracted over, collapse them into
  # a string as function arguments. Here it will be in the order of appearance.
  # One could imagine not enforcing the unique call above, counting the
  # number of appearances a symbol makes, then reordering them in descending
  # order to approximate having the "important" things first.
  fx_args <- paste(symbols_to_abstract, collapse = ",\n")


  # Slot in the function arguments and written code into a new definition
  fx_text <- paste0("new_function <- function(",
                    fx_args,
                    ") {\n",
                    text,
                    "\n}\n")
  fx_text
}


#' Get highlighted text only
#'
#' Helper for testing
#'
#' @return Highlighted text as a string
.get_highlighted_text <- function(){
  rstudioapi::getSourceEditorContext()$selection[[1]]$text
}

