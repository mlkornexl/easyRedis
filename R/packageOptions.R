#' Package Options
#'
#' Get and set package options.
#'
#' @param ... either a list of character strings (names of options to get) or
#'     a named list of options to set
#'
#' @return For \code{...} being an unnamed list of character strings, the
#'     function returns a list of options. If the resulting list is of length
#'     one, the list will be flattene, i.e. only the first item will be
#'     returned. If not parameter will be supplied, the complete list of
#'     options will be returned.
#'
#'     For \code{...} being a names list of options to be set, the function
#'     invisibly returns the original values (if set) of the set options.
#'
#' @export
redis_options <- function(...) {
  opts <- list(...)

  if (length(opts) == 0) {
    return(as.list(.options))

  } else if (is.null(names(opts))) {
    opts <- opts[vapply(opts, is.character, FUN.VALUE = logical(1))]
    opts <- sapply(opts, get0, envir = .options,
                   simplify = FALSE, USE.NAMES = TRUE)
    if (length(opts) == 1) opts <- opts[[1]]

    return(opts)

  } else {
    opts <- opts[names(opts) != '']
    opts_old <- as.list(.options)
    opts_old <- opts_old[intersect(names(opts), names(opts_old))]

    for (i in names(opts)) {
      assign(i, opts[[i]], envir = .options)
    }

    invisible(opts_old)
  }

}

.options <- new.env(parent = emptyenv())
