#' Get Configuration Data from Redis Cache
#'
#' @param key character string, key to identify config data in Redis cache
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
redis_getConfig <- function(key) {
  if (!redis_connect()) on.exit(rredis::redisClose())

  if (!rredis::redisExists(key)) return(NULL)

  config <- rredis::redisGet(key) %>%
    jsonlite::fromJSON()

  attr(config, 'id') <- config$id
  attr(config, 'key') <- key

  return(config)
}
