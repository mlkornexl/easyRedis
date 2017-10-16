#' easyRedis: Helper Package for Redis Caches
#'
#' The package provides various function for working with Redis data bases
#'
#' @section Functions:
#' \describe{
#'    \item{\code{\link{redis_options}}}{get or set options for \pkg{easyRedis}
#'        package}
#'    \item{\code{\link{redis_connect}}, \code{\link{redis_close}}}{connect to
#'        Redis data base or close the connection}
#'    \item{\code{\link{redis_loadConfig}}}{loads configuration files to
#'        Redis data base}
#'    \item{\code{\link{redis_getConfig}}}{get configuration settings from
#'        Redis data base}
#'    \item{\code{\link{redis_logger}}}{define a logger to publish messages to
#'        a Redis data base}
#'
#' }
#'
#' @docType package
#' @name easyRedis
#' @aliases easyRedis-package
#'
#' @importFrom magrittr %>%
#'
NULL


.message <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  file <- system.file(file.path('templates', 'TraceMessage.json'),
                      package = pkgname, lib.loc = libname)
  assign('trace', jsonlite::fromJSON(file), envir = .message)

  file <- system.file(file.path('templates', 'DebugMessage.json'),
                      package = pkgname, lib.loc = libname)
  assign('debug', jsonlite::fromJSON(file), envir = .message)

  file <- system.file(file.path('templates', 'ErrorMessage.json'),
                      package = pkgname, lib.loc = libname)
  assign('error', jsonlite::fromJSON(file), envir = .message)

  file <- system.file(file.path('templates', 'FatalMessage.json'),
                      package = pkgname, lib.loc = libname)
  assign('fatal', jsonlite::fromJSON(file), envir = .message)

  redis_options(ApplicationId = '00000000-0000-0000-0000-000000000000',
                ApplicationInstanceId = '00000000-0000-0000-0000-000000000000')

}
