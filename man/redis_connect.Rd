% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect.R
\name{redis_connect}
\alias{redis_connect}
\alias{redis_close}
\title{Connect to Redis Cache}
\usage{
redis_connect(conn = redis_options("connection"), host, port, password, ...)

redis_close(...)
}
\arguments{
\item{conn}{a connection string for Redis cache, see details}

\item{host}{the Redis server host name, see
\code{\link[rredis]{redisConnect}} for details. This will overwrite any
settings of the connection string \code{conn}.}

\item{port}{the Redis port number, see
\code{\link[rredis]{redisConnect}} for details. This will overwrite any
settings of the connection string \code{conn}.}

\item{password}{the Redis authentification password, see
\code{\link[rredis]{redisConnect}} for details. This will overwrite any
settings of the connection string \code{conn}.}

\item{...}{other parameter passed to \code{\link[rredis]{redisConnect}} or
\code{\link[rredis]{redisClose}}}
}
\value{
logical value, \code{TRUE} if connection has been opened,
    \code{FALSE} if a connection has already been opened (note that this has
    not necessarily need to be the one specified by \code{conn})
}
\description{
The function is a wrapper for \code{\link[rredis]{redisConnect}} function
that allows to pass host name, port, and authorization as a connection
string rather than separate variables.
}
\details{
The connection string \code{...} contains hostname, port, and
    authorization (password) for the redis cache. The connection string is
    of the form
    \code{host=\var{<hostname>};port=\var{<port>};password=\var{<auth>}}.

    The port can be included in the hostname with a colon as separator:
    \code{host=\var{<hostname>}:\var{<port>};}.

    As password may contain any string, it \emph{must} be the last part of
    the connection string.
}
\seealso{
\code{\link[rredis]{redisConnect}} for basis function to connect
    to a Redis Server
}
