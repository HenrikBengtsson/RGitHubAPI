#' List labels
#'
#' Retrievs all labels.
#'
#' @param owner Character string.
#' @param repo Character string.
#' @return Returns a character vector.
#'
#' @export
#' @importFrom httr GET
#' @importFrom httr content
listLabels <- function(owner, repo) {
  url <- gitUrl("/repos/:owner/:repo/labels", owner, repo)
  res <- content(GET(url, auth), as="parsed")
  if (length(res) > 0L) names(res) <- sapply(res, FUN=`[[`, "name")
  res
}

#' Checks existance of labels
#'
#' Checks whether a set of labels exists or not.
#'
#' @param labels Character vector.
#' @param owner Character string.
#' @param repo Character string.
#' @return Returns a named logical vector.
#'
#' @export
hasLabels <- function(labels, owner, repo) {
  res <- listLabels(owner, repo)
  res <- is.element(labels, names(res))
  names(res) <- labels
  res
}

#' Creates a new label
#'
#' Creates a new label
#'
#' @param label Character string.
#' @param color Character string.
#' @param owner Character string.
#' @param repo Character string.
#' @return Returns a named logical vector.
#'
#' @export
#' @importFrom httr POST
createLabel <- function(label, color, owner, repo) {
  color <- tolower(color)
  color <- gsub("^#([0-9a-f]{6})$", "\\1", color)
  stopifnot(grepl("^[0-9a-f]{6}$", color))

  url <- gitUrl("/repos/:owner/:repo/labels", owner, repo)
  data <- list(name=label, color=color)
  res <- POST(url, auth, body=json(data), encode="json")
  res
}
