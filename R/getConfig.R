#' Get Configuration Data from Redis Cache
#'
#' @param key character string, key to identify config data in Redis cache
#' @param auto_load character string, file path of config file to be loaded
#'     automatically, if cached config is \code{NULL}.
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
redis_getConfig <- function(key, auto_load) {
  if (redis_connect()) on.exit(rredis::redisClose())

  if (!rredis::redisExists(key)) {
    if (missing(auto_load)) {
      return(NULL)
    } else {
      return(redis_loadConfig(auto_load))
    }
  }

  config <- rredis::redisGet(key) %>%
    jsonlite::fromJSON()

  attr(config, 'id') <- config$id
  attr(config, 'key') <- key

  return(config)
}
