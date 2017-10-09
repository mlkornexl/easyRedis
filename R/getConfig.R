#' Get Configuration Data from Redis Cache
#'
#' @param key character string, key to identify config data in Redis cache
#' @param ... additional paramter passed to \code{\link{redis_connect}} in
#'     case of no open Redis connection
#'
#' @return an object deparsed from JSON string that is stored in Redis cache
#'     with key \code{key}.
#'
#'     The return value has two attributes
#'     \describe{
#'         \item{\code{id}}{the unique identifier of the config data}
#'         \item{\code{key}}{the Redis cache key}
#'     }
#'
#'     If \code{key} is not found, \code{NULL} is returned.
#'
#' @export
#'
redis_getConfig <- function(key, ...) {
  if (identical(class(try(rredis::redisInfo(), silent = TRUE)),
                'try-error')) {
    redis_connect(...)
    on.exit(rredis::redisClose())
  }

  if (!rredis::redisExists(key)) return(NULL)

  value <- rredis::redisGet(key) %>%
    jsonlite::fromJSON()

  attr(value, 'id') <- value$id
  attr(value, 'key') <- key

  return(value)
}
