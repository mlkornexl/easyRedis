#' Connect to Redis Cache
#'
#' The function is a wrapper for \code{\link[rredis]{redisConnect}} function
#' that allows to pass host name, port, and authorization as a connection
#' string rather than separate variables.
#'
#' @param conn a connection string for Redis cache, see details
#' @param host the Redis server host name, see
#'     \code{\link[rredis]{redisConnect}} for details. This will overwrite any
#'     settings of the connection string \code{conn}.
#' @param port the Redis port number, see
#'     \code{\link[rredis]{redisConnect}} for details. This will overwrite any
#'     settings of the connection string \code{conn}.
#' @param password the Redis authentification password, see
#'     \code{\link[rredis]{redisConnect}} for details. This will overwrite any
#'     settings of the connection string \code{conn}.
#' @param ... other parameter passed to \code{\link[rredis]{redisConnect}} or
#'     \code{\link[rredis]{redisClose}}
#'
#'
#' @details The connection string \code{...} contains hostname, port, and
#'     authorization (password) for the redis cache. The connection string is
#'     of the form
#'     \code{host=\var{<hostname>};port=\var{<port>};password=\var{<auth>}}.
#'
#'     The port can be included in the hostname with a colon as separator:
#'     \code{host=\var{<hostname>}:\var{<port>};}.
#'
#'     As password may contain any string, it \emph{must} be the last part of
#'     the connection string.
#'
#' @seealso \code{\link[rredis]{redisConnect}} for basis function to connect
#'     to a Redis Server
#'
#' @export
#'
redis_connect <- function(conn = redis_options('connection'),
                          host, port, password, ...) {
  if (is.null(conn)) {
    stop('Connection string for Redis cache must be set!')
  }

  conn <- list(
    host = stringr::str_extract(conn, '(?<=host=).*?(?=;)'),
    port = as.integer(stringr::str_extract(conn, '(?<=port=)\\d*?(?=;)')),
    password = stringr::str_extract(conn, '(?<=password=).*$')
  )

  if (is.na(conn$port) & !is.na(conn$host)) {
    conn$port <- stringr::str_extract(conn, '(?<=:)[[:digit:]]+$')
  }

  conn <- conn[!vapply(conn, is.na, FUN.VALUE = logical(1))]

  invisible(do.call(rredis::redisConnect, conn))
}

#' @rdname redis_connect
#' @export
#'
redis_close <- function(...) rredis::redisClose(...)
