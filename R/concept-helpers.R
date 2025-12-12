#' Creates a new concept
#'
#' @param key a character key for that concept
#'
#' @param label a label used for that concept
#'
#' @param acceptor a concept acceptor. An acceptor is an object that decides if
#'   a column of the input dataset could be used as a potential mapping.
#'
#' @param standardizer a standardizer is a function that takes an input mapping
#'   and transforms it to a standardized internal representation. E.g. mapping
#'   1/2 to Male/Female for sex.
#'
#' @param validator a validator is a function that is another layer of security
#'   validating the result of the standardizer. Only if the validator validates
#'   the input a mapping is successfully created.
#'
#' @param prototype the initial datatype for that concept (pre mapping). It
#'   should be the same type as the result of the standardizer.
#'
#' @param implausibility A function used to determine if concept values are
#'   implausible.
#'
#' @noRd
concept <- function(
  key,
  label,
  acceptor,
  standardizer,
  validator,
  prototype,
  is_implausible = NULL,
  implausible_deps = NULL
) {
  list(
    key = key,
    label = label,
    acceptor = acceptor,
    standardizer = standardizer,
    validator = validator,
    prototype = prototype,
    is_implausible = is_implausible,
    implausible_deps = implausible_deps
  )
}

concept_keys <- function(concepts) {
  vapply(concepts, \(x) x$key, character(1))
}

concept_labels <- function(concept_keys) {
  stopifnot(
    all(concept_keys %in% names(concepts))
  )

  vapply(concepts[concept_keys], `[[`, character(1), "label", USE.NAMES = FALSE)
}

concept_acceptor <- function(fun, error_msg) {
  structure(
    list(
      fun = fun,
      error_msg = error_msg
    ),
    class = "concept_acceptor"
  )
}

concepts_list <- function(...) {
  conc_list <- list(...)
  conc_keys <- vapply(conc_list, `[[`, character(1), "key")
  set_names(conc_list, conc_keys)
}

ALL_VALUE <- "All"
