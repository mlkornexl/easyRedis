#' Load Config-Files to Redis Cache
#'
#' The function loads config data, stored as JSON-strings in config files to
#' the Redis cache.
#'
#' @param file character vector of file names
#' @param ... additional paramter passed to \code{\link{redis_connect}} in
#'     case of no open Redis connection
#' @param append logical, if \code{TRUE} config settings will be appended to
#'     existing settings (if any). Duplicate settings will be overwritten.
#'
#' @section Data Format:
#' All config files must contain data in JSON format. Basic elements of the
#' data structure at top level are
#' \describe{
#'     \item{\code{key}}{\emph{mandatory}, unique key to store and access data
#'         in the Redis cache}
#'     \item{\code{value}}{\emph{mandatory}, data object containing a list of
#'         key-value pairs. This can be any list of named data. There is one
#'         \emph{mandatory} element: \code{id}. This is a unique identifier of
#'         the data. It will be matched to \code{key} and no key must be
#'         assigned to one or more \code{id}s (e.g. from multiple
#'         config-files)}
#'     \item{\code{auto_load}}{\emph{optional}, an auto load obejct, i.e. a
#'         list of key-value pairs, see Auto Load section for details.}
#' }
#'
#' For loading a minimal config file, see Examples.
#'
#' @section Auto Load:
#' The \code{auto_load} section of the configuration file contains a named
#' list of code to create configuration settings dynamically. Code can be
#' any valid expression e.g. \preformatted{"date": "Sys.Date()"}
#' or, using \code{::} to locate a function within a package,
#' \preformatted{"date": "lubridate::today()"}
#' to set config value \code{date} to the current date (using
#' \code{\link[base]{Sys.Date}()}).
#'
#' Code can also be a file name with an optional expression (separated by a
#' colon) to be evaluated within the context of this file. If no expression
#' is given, variable \code{value} as defined within the file, will be returned.
#'
#' Relative paths for file names are given with respect to the location of
#' the config file.
#'
#' @return a list of same length as `file` containing configuration settings;
#'     if `file` is of length 1, only the first list element will be returned
#'
#'
#' @examples
#' \dontrun{
#' ## show template for minimal configuration file
#' file <- system.file('templates/minimal.cfg', package = 'easyRedis')
#' cat(file)
#'
#' ## *** SET UP AN OPEN REDIS-CONNECTION FIRST! ***
#' redis_loadConfig(file)
#' }
#'
#' @references see \url{https://en.wikipedia.org/wiki/JSON} for details on
#'     JSON file format
#'
#' @export
#'
redis_loadConfig <- function(file, ..., append = FALSE) {
  if (identical(class(try(rredis::redisInfo(), silent = TRUE)),
                'try-error')) {
    redis_connect(...)
    on.exit(rredis::redisClose())
  }

  config <- vector('list', length(file))

  for (i in seq_along(file)) {

    autoLoadEnv <- parent.frame()

    config[[i]] <- .readConfigFile(file[i], envir = autoLoadEnv)
    config_old <- redis_getConfig(attr(config[[i]], 'key'))

    if (!is.null(config_old)) {
      if (!identical(attr(config[[i]], 'id'), attr(config_old, 'id'))) {
        stop(sprintf('Existing key \'%s\' with different ID!',
                     attr(config[[i]], 'key')))
      }

      if (append) {
        config_old[names(config[[i]])] <- config[[i]]
        config[[i]] <- config_old
      }
    }

    value <- jsonlite::toJSON(config[[i]], auto_unbox = TRUE, na = NULL)
    key <- attr(config[[i]], 'key')
    status <- rredis::redisSet(key, value)

    config[[i]] <- redis_getConfig(key)
    attr(config[[i]], 'status') <- status
  }

  if (length(file) == 1) config <- config[[1]]

  invisible(config)

}





.readConfigFile <- function(file, envir) {
  config <- jsonlite::fromJSON(file)

  key <- config$key
  id <- config$value$id
  autoLoad <- config$auto_load

  config <- config$value

  if (is.null(key))
    stop('Missing key in config file \'', file, '\'.')

  if (is.null(config))
    stop('Missing value section in config file \'', file, '\'.')

  if (is.null(id))
    stop('Missing id in config file \'', file, '\'.')

  if (length(autoLoad) > 0) {
    autoLoad <- sapply(autoLoad, .autoLoadConfig, envir = envir,
                       path = dirname(file),
                       simplify = FALSE, USE.NAMES = TRUE)
    config[names(autoLoad)] <- autoLoad
  }

  attr(config, 'id') <- id
  attr(config, 'key') <- key

  return(config)
}



.autoLoadConfig <- function(autoLoad, envir, path) {
  # if autoLoad is a file, an no expression is provided, add "value"
  autoLoad <- stringr::str_replace(autoLoad, '(?i)(?<=\\.R)$', ':value')

  autoLoadEnv <- new.env(parent = envir)

  if (stringr::str_detect(autoLoad, '(?i).+?\\.R:[^:]')) {
    # autoLoad is of type <R-file>:<expression>
    file <- stringr::str_extract(autoLoad, '(?i).+?\\.R(?=:)')
    expr <- stringr::str_extract(autoLoad, '(?i)(?<=\\.R:).+$')

    if (!(stringr::str_detect(file, '^.:(/|\\\\)') ||
          stringr::str_detect(file, '^[/\\\\]') ||
          stringr::str_detect(file, '^~'))) {
      file <- file.path(path, file)
    }

    source(file, local = autoLoadEnv)
  } else {
    expr <- autoLoad
  }

  eval(parse(text = expr), envir = autoLoadEnv)
}


















