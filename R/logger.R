#' Logging to Redis Database
#'
#' The functions define logging environment for writing log messages to
#' the Redis data base using \pkg{futile.logger}'s logging facilities
#'
#'
#' @name redis_logger
NULL

#' @describeIn redis_logger defines logging message's layout, see Layout
#'     section for details
#'
#' @param ApplicationId string, unique ID of the logging application
#' @param ApplicationInstanceId string, unique ID of the single instance of
#'     the logging application
#'
#' @section Layout:
#'     The logging message is a JSON string with two elements at the base level
#'     \describe{
#'         \item{\code{Metadata}}{contains \code{ApplicationId} and
#'             \code{ApplicationInstanceId}}
#'         \item{\code{LoggingDocument}}{contains a list of message specific
#'             elements}
#'     }
#'
#'     Both meta data \code{ApplicationId} and \code{ApplicationInstanceId}
#'     must be supplied. If missing the respective \link[=redis_options]{redis
#'     options} will be used.
#'
#'     The \code{level} argument in the returned function will control the
#'     layout of the \code{LoggingDocument} list. Additional arguments
#'     \code{...} either set list elements of \code{LoggingDocument} directly
#'     (if present) or will be coerced to a "details" JSON-string.
#'
#' @return \code{layout.redis} returns a function with arguments \code{level},
#'     \code{msg}, and additional arguments \code{...}, see Layout section and
#'     \code{\link[futile.logger]{flog.layout}} for details.
#'
#' @seealso \code{\link[futile.logger]{flog.layout}} for managing layouts in
#'     \pkg{futile.logger} package
#'
#' @export
#'
layout.redis <- function(ApplicationId, ApplicationInstanceId) {

  function(level, msg, ...) {
    args <- list(...)

    if (level == futile.logger::TRACE) {
      msg <- get('trace', envir = .message)
      iMessage <- 'Message'
      iDetail <- 'Detail'
    } else if (level == futile.logger::DEBUG) {
      msg <- get('debug', envir = .message)
      iMessage <- 'Message'
      iDetail <- 'Detail'
    } else if (level == futile.logger::ERROR) {
      msg <- get('error', envir = .message)
      iMessage <- 'ErrorMessage'
      iDetail <- 'ErrorDetail'
    } else if (level == futile.logger::FATAL) {
      iMessage <- 'ErrorMessage'
      iDetail <- 'ErrorDetail'
    } else {
      stop(sprintf('No layout for debugging level %d.', level))
    }

    if (!missing(msg)) args[[iMessage]] <- c(msg, args[[iMessage]])

    details <- args[setdiff(names(args), names(msg$LoggingDocument))]

    args <- args[intersect(names(args), names(msg$LoggingDocument))] %>%
      vapply(jsonlite::toJSON, auto_unbox = TRUE, POSIXt = 'ISO8601',
             FUN.VALUE = character(1))

    if (!is.null(args[[iDetail]])) details <- c(args[[iDetail]], details)
    if (length(details) == 1) details <- details[[1]]
    if (length(details) >= 1) {
      msg$LoggingDocument[[iDetail]] <-
        jsonlite::toJSON(details, auto_unbox = TRUE, POSIXt = 'ISO8601')
    }

    jsonlite::toJSON(msg, auto_unbox = TRUE, POSIXt = 'ISO9601')
  }

}



#' @describeIn redis_logger defines a logging appender to publish logging
#'     messages to a Redis data base channel \code{channel}
#'
#' @param channel Redis channel to publish logging messages
#' @param logToConsole logical, if \code{TRUE} logging message is written to
#'     console too
#'
#' @return \code{appender.redis} returns a function with one argument
#'     \code{line} that publishes the logging message to the Redis channel
#'     \code{channel} and optionally writes it to the console
#'     (\code{\link[base]{stdout}()}).
#'
#' @seealso \code{\link[futile.logger]{flog.appender}} for managing logging
#'     appenders in \pkg{futile.logger} package
#'
#' @export
#'
appender.redis <- function(channel, logToConsole = FALSE) {
  function(line) {
    if (logToConsole) futile.logger::appender.console()(line)

    if (identical(class(try(rredis::redisInfo(), silent = TRUE)),
                  'try-error')) {
      redis_connect()
      on.exit(rredis::redisClose())
    }

    response <- try(rredis::redisPublish(channel, charToRaw(line)),
                    silent = TRUE)
    if (identical(class(response), 'try-error')) {
      stop(sprintf('Error when publishing to log-channel %s:\n%s',
                   channel, response))
    }
  }
}



#' @describeIn redis_logger defines a new logger \code{redis} and sets
#'     layout and appender to the return values of \code{layout.redis()} and
#'     \code{appender.redis()}, respectively.
#'
#' @seealso \code{\link[futile.logger]{flog.logger}} for managing loggers in
#'     \pkg{futile.logger} package
#'
#' @export
#'
redis_logger <- function(ApplicationId, ApplicationInstanceId,
                         channel = redis_options('logChannel'),
                         logToConsole = FALSE) {

  if (missing(ApplicationId))
    ApplicationId <- redis_options('ApplicationId')

  if (missing(ApplicationInstanceId))
    ApplicationInstanceId <- redis_options('ApplicationInstanceId')

  if (is.null(channel))
    stop('Logging channel must be specified.')

  futile.logger::flog.logger(name = 'redis',
                             appender = appender.redis(logToConsole),
                             layout = layout.redis(ApplicationId,
                                                   ApplicationInstanceId))
}
