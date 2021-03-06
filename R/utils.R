#' Attach a 'v' class attribute to object
#'
#' @param \ldots Objects to concatenate with c().
#'
#' @export
#' @keywords internal
v <- function(...) {
  x <- c(...)
  structure(x, class=c("v", class(x)))
}

#' @importFrom jsonlite toJSON
json <- function(data) {
  names <- names(data)
  is_array <- sapply(data, FUN=is.array)
  json <- character(length(data))
  for (kk in seq_along(data)) {
    value <- data[kk]
    auto_unbox <- !inherits(data[[kk]], "v")
    value <- toJSON(data[kk], auto_unbox=auto_unbox)
    value <- gsub("[}]$", "", gsub("^[{]", "", value))
    json[[kk]] <- value;
  }
  json <- paste(json, collapse=", ")
  json <- c("{", json, "}")
  json <- paste(json, collapse=" ")
  json
}
