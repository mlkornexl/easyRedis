#' Set Config Settings
#'
#' The function sets config entries with a given key in the Redis cache.
#'
#' @param key character, key to identify config settings
#' @param ... named arguments for config settings
#' @param .list list of config settings, see details
#'
#' @details The config settings may either be passed using \code{...} argument
#'     or \code{.list}. If \code{.list} is provided, any other parameter in
#'     \code{...} will be omitted.
#'
#'     If \code{key} does not exist, a new key will be generated. The ID will,
#'     if not provided within the list of config settings, generated using
#'     \code{\link[uuid]{UUIDgenerate}()}.
#'
#'     If \code{key} exists and config settings contain an ID, an error will be
#'     raised if the key's ID and the config settings list ID do not match.
#'
#' @return a list of configuration settings
#'
#' @export
redis_setConfig <- function(key, ..., .list) {
  if (missing(.list)) .list <- list(...)

  if (is.null(names(.list))) {
    stop('Config settings must be a *named* list.')

  } else if (any(names(.list) == '')) {
    warning('Config settings contain unnamed entries.\n',
            'These will be omitted.')
    .list <- .list[names(.list) != '']
  }

  if (redis_connect()) on.exit(rredis::redisClose())

  if (length(.list) == 0)  invisible(redis_getConfig(key))

  .list$id <- .checkId(key, .list$id)
  if (is.null(.list$id)) stop('ID mismatch for key "', key, '".')

  config <- redis_getConfig(key)

  config <- c(config[setdiff(names(config), names(.list))],
              .list) %>%
    jsonlite::toJSON(auto_unbox = TRUE, na = NULL)

  status <- rredis::redisSet(key, config)
  config <- redis_getConfig(key)
  attr(config, 'status') <- status

  invisible(config)
}

.checkId <- function(key, id) {
  if (redis_connect()) on.exit(rredis::redisClose())

  if (!rredis::redisExists(key)) {
    if (is.null(id)) id <- uuid::UUIDgenerate()

  } else {
    config <- redis_getConfig(key)

    if (!is.null(attr(config, 'id'))) {
      if (is.null(id)) id <- uuid::UUIDgenerate()

    } else {
      if (is.null(id)) id <- attr(config, 'id')
      if (id != attr(config, 'id')) id <- NULL
    }
  }

  return(id)
}
