#' Load Config-Files to Redis Cache
#'
#' The function loads config data, stored as JSON-strings in config files to
#' the Redis cache.
#'
#' @param file character vector of file names (a single file name for
#'     \code{redis_readConfigFile})
#' @param append logical, if \code{TRUE} config settings will be appended to
#'     existing settings (if any). Duplicate settings will be overwritten.
#'
#' @return a list of configuration settings
#'
#' @examples
#' \dontrun{
#' ## show template for minimal configuration file
#' file <- system.file('templates/minimal.cfg', package = 'easyRedis')
#' cat(stringr::str_c(readLines(file), collapse = '\n'))
#'
#' ## *** SET UP AN OPEN REDIS-CONNECTION FIRST! ***
#' config <- redis_loadConfig(file)
#'
#' key <- attr(config, 'key')
#'
#' redis_getConfig(key)
#'
#' ## *** CLEAR REDIS KEY! ***
#' rredis::redisDelete(key)
#' }
#'
#' @references see \url{https://en.wikipedia.org/wiki/JSON} for details on
#'     JSON file format
#'
#' @export
#'
redis_loadConfig <- function(file, append = FALSE) {
  if (identical(class(try(rredis::redisInfo(), silent = TRUE)),
                'try-error')) {
    redis_connect()
    on.exit(rredis::redisClose())
  }

  config <- purrr::map(file, redis_readConfigFile) %>%
    purrr::map(function(cfg, append) {
      key <- attr(cfg, 'key')

      if (!append && rredis::redisExists(key)) rredis::redisDelete(key)

      redis_setConfig(key, .list = cfg)
    }, append = FALSE)

  if (length(file) == 1) config <- config[[1]]

  invisible(config)

}



#' @rdname redis_loadConfig
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
redis_readConfigFile <- function(file) {
  if (!file.exists(file)) {
    stop('File "', file, '" does not exist.')
  }

  config <- jsonlite::fromJSON(file)

  if (is.null(config$key))
    stop('Missing key in config file \'', file, '\'.')

  if (is.null(config$value))
    stop('Missing value section in config file \'', file, '\'.')

  if (is.null(config$value$id))
    config$value$id <- uuid::UUIDgenerate()

  env <- list2env(config$value, parent = .GlobalEnv)

  wd_old <- setwd(dirname(normalizePath(file)))
  on.exit(setwd(wd_old))

  config$auto_load <- purrr::map(config$auto_load, .auto_load, parent = env)

  config$value <- c(config$value[setdiff(names(config$value),
                                         names(config$auto_load))],
                    config$auto_load)

  attributes(config$value) <- list(id = config$value$id,
                                   key = config$key)

  return(config$value)
}


#' @rdname redis_loadConfig
#' @param expr character string, expression to evaluate, see Auto-Load section
#'     for details
#' @param parent environment, parent environment for the environment in which
#'     \code{expr} will be evaluated
#'
#' @section Auto-Load:
#' The \code{auto_load} section of the config settings contains a named list
#' of R expressions, e.g.
#' \preformatted{"auto_load": {
#'     "date": "Sys.Date()"
#' }}
#' will set the config value \code{date} to the current date (using
#' \code{\link[base]{Sys.Date}()}). Use \code{"::"} to preface the function
#' with the package name (just like in plain R). A single colon \code{:} can
#' be used to preface the expression with a file name (i.e. anything
#' that ends with \code{".R"} or \code{".r"}), that will be sourced
#' before the expression will be evaluated.
#'
#' If file name is provided without any expression, variable \code{value} as
#' defined by the script will be returned.
#'
#' All expressions (and the sourcing of the R-script, in case a file name is
#' provided) will be evaluated in an separate environment with parent
#' environment \code{parent}.
#'
#' Function \code{redis_readConfigFile}, that calls \code{.auto_load} will set
#' \code{parent} to a new environment containing all non-auto-load config
#' settings. The parent environment of this is set to
#' \code{\link[base]{.GlobalEnv}}. Thus all config settings and any object
#' within the global search path will be available for execution of
#' \code{expr}.
#'
#' Function \code{redis_readConfigFile} further sets the working directory
#' temporarily to the config file's base directory, i.e. all relative paths
#' in a file name will be with respect to this directory.
#'
.auto_load <- function(expr, parent) {

  e <- new.env(parent = parent)

  file <- stringr::str_extract(expr, '(?i).+?\\.R(?=(:|$))')

  if (is.na(file)) {
    expr <- stringr::str_extract(expr, '(?i)(?<=\\.R:).+')
    if (is.na(expr)) expr <- 'value'

    source(file, local = e)
  }

  eval(parse(text = expr), envir = e)

}


















