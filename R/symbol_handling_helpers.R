#' Parse the highlighted text and get all the global variables
#'
#' Wrapper for the symbol extraction helpers
#'
#' @param text Text from `getSourceEditorContext()$selection`
#'
#' @return A character vector of which symbols should be abstracted over for
#' a new function definition
.parse_symbols <- function(text) {
  selected_exprs <- parse(text = text)

  found_symbols <- .get_global_symbols(selected_exprs)
  candidate_symbols <- .get_abstraction_candidates(found_symbols)
  symbols_to_abstract <- .remove_assigned_candidates(selected_exprs,
                                                     candidate_symbols)
  symbols_to_abstract
}

#' Check type of identified symbols
#'
#'  Figure out which symbols we need to extract over.
#'  Some things not to abstract over:
#'    - closure = function call
#'  - special = things like ::
#'    - error   = typically from tidyverse verbs where a symbol is to be
#'  evaluated in the context of a piped in dataframe, eg cyl in
#'  mutate(mtcars, cyl = 3)
#'  - builtin  = things like c or `+`
#'
#' @param global_symbols Output of `.get_global_symbols`
#'
#' @return Character vector of length <= global_symbols (usually smaller)
.get_abstraction_candidates <- function(global_symbols) {
  disallowed_types <- c('closure' = FALSE,
                        'special' = FALSE,
                        'error' = FALSE,
                        'builtin' = FALSE)


  is_candidate <-
    vapply(global_symbols,
           \(s) {
             sym_type <- tryCatch(typeof(eval(as.symbol(s))),
                                  error = \(e) "error")
             is.na(disallowed_types[sym_type])
           },
           TRUE)

  global_symbols[is_candidate]
}

#' Filter out assignments
#'
#' Removes any candidate symbols on the LHS of an assignment expression
#'
#' @param selected_exprs Output of `parse(text = text)`
#' @param abstraction_candidates Output of `.get_abstraction_candidates`
#'
#' @return Character vector of length <= abstraction candidates (smaller if
#' there were any assignment operations)
.remove_assigned_candidates <- function(selected_exprs, abstraction_candidates) {
  should_ignore <- .get_ignored_syms(selected_exprs)
  non_assigned_syms <- TRUE

  if (!is.null(should_ignore))
    non_assigned_syms <- vapply(abstraction_candidates,
                                \(s) is.na(should_ignore[s]),
                                TRUE)

  # If non_assigned_syms is length 1 and TRUE, it will recycle for the
  # whole length of abstraction_candidates (eg (1:30)[TRUE] == 1:30)
  abstraction_candidates[non_assigned_syms]
}

#' Find which symbols exist in global environment
#'
#' Used to find the unique set of symbols from a provided set of expressions
#' in the global environment
#'
#' @param selected_exprs Output of `parse(text = text)`
#'
#' @return Character vector of symbol names
.get_global_symbols <- function(selected_exprs){
  lapply(selected_exprs,
         \(x) {
           globals::findGlobals(x)
         }) |>
    c(recursive = TRUE) |>
    unique()
}

#' Determine which symbols to ignore
#'
#' Symbols should be ignored if they're on the LHS of an assigment
#'
#' @param selected_exprs Output of `parse(text = text)`
#'
#' @return Logical named vector of which symbols should be ignored. Note that
#' this is later used by indexing via the symbol name, and if it doesn't exist
#' then NA is returned which is picked up by a call to `is.na`
.get_ignored_syms <- function(selected_exprs) {
  is_assignment <- .is_assignment_expr(selected_exprs)

  if (!any(is_assignment))
    return(NULL)

  assigned_names <- .get_assigned_symnames(selected_exprs[is_assignment])

  should_ignore <- rep(TRUE, length(assigned_names))
  names(should_ignore) <- assigned_names

  should_ignore
}

#' Check if expression is an assignment
#'
#' Checks if any expressions are of the form `varname <- ...`, in which
#' case we don't need to care about varname.
#'
#' @param selected_exprs Output of `parse(text = text)`
#'
#' @return Logical vector of which expressions are assignments
.is_assignment_expr <- function(selected_exprs) {
  vapply(selected_exprs,
         \(x)
         ifelse(length(x) == 1, # Edge case where a single symbol is highlighted
                FALSE,
                identical(eval(x[[1]]), `<-`)),
         TRUE
  )
}

#' Get LHS of assignment
#'
#' @param selected_exprs Output of `parse(text = text)`
#'
#' @return Character vector of length 1 containing the LHS of an assignment
.get_assigned_symnames <- function(selected_exprs) {
  vapply(selected_exprs,
         \(x) {
          as.character(x[[2]])
         },
         "char"
  )
           # as.character(selected_exprs[[1]][[2]])
}
