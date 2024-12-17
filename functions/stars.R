#' bnd_attrs generic
#'
#' @param x An object
#' @param ... Arguments passed to or from other methods
#' @return a `stars` object
bind_attrs <- function(x, ...) {
  UseMethod("bind_attrs", x)
}

#' Bind one or more `stars` objects by attribute.
#'
#' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
#'
#' @param x a stars object
#' @param ... one or more `stars` objects to bind to `x` as additional attributes.  Any NULL elements are
#'   silently removed first. Ignored if `x` is a list.
#' @return `stars` objects
bind_attrs.default <- function(x, ...){
  stop("first argument must be either list or a `stars` object")
}

#' Bind one or more `stars` objects by attribute.
#'
#' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
#'
#' @param x a stars object
#' @param ... one or more `stars` objects to bind to `x` as additional attributes.  Any NULL elements are
#'   silently removed first.
#' @return `stars` objects
bind_attrs.stars <- function(x, ...){
  x = list(x, ...)
  x = x[!sapply(x, is.null)]
  if (length(x) == 0) stop("input has zero length")
  do.call(c, append(x, list(along = NA_integer_)))
}

#' Bind a list of \code{stars} objects by attribute.
#'
#' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
#'
#' @param x list of `stars` objects
#' @param ... ignored
#' @return `stars` objects
bind_attrs.list <- function(x, ...){
  if (length(x) == 0) stop("input has zero length")
  do.call(c, append(x, list(along = NA_integer_)))
}
