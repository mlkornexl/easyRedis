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
#' @return logical value, \code{TRUE} if connection has been opened,
#'     \code{FALSE} if a connection has already been opened (note that this has
#'     not necessarily need to be the one specified by \code{conn})
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

  if (.redis_isOpen()) invisible(FALSE)

  if (missing(host))
    host <- stringr::str_extract(conn, '(?i)(?<=host=).*?(?=;)')
  if (missing(port))
    port <- as.integer(stringr::str_extract(conn, '(?i)(?<=port=)\\d*?(?=;)'))
  if (missing(password))
    password <- stringr::str_extract(conn, '(?i)(?<=password=).*$')
  if (is.na(conn$port) & !is.na(conn$host)) {
    conn$port <- stringr::str_extract(conn$host, '(?<=:)[[:digit:]]+$')
  }

  conn <- list(host = host, port = port, password = password)
  purrr::invoke(rredis::redisConnect, conn[!purrr::map_lgl(conn, is.na)])

  invisible(TRUE)
}

#' @rdname redis_connect
#' @export
#'
redis_close <- function(...) rredis::redisClose(...)

.redis_isOpen <- function() {
  identical(class(try(rredis::redisInfo(), silent = TRUE)), 'try-error')
}
