#' List labels
#'
#' Retrievs all labels.
#'
#' @param repos A GitHubRepository object.
#' @return Returns a character vector.
#'
#' @export
#' @importFrom httr GET
#' @importFrom httr content
listLabels <- function(repos) {
  url <- gitUrl(repos, "/repos/:owner/:repo/labels")
  res <- content(GET(url, repos$auth), as="parsed")
  if (length(res) > 0L) names(res) <- sapply(res, FUN=`[[`, "name")
  res
}

#' Checks existance of labels
#'
#' Checks whether a set of labels exists or not.
#'
#' @param repos A GitHubRepository object.
#' @param labels Character vector.
#' @return Returns a named logical vector.
#'
#' @export
hasLabels <- function(repos, labels) {
  res <- listLabels(repos)
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
#' @param repos A GitHubRepository object.
#' @return Returns a named logical vector.
#'
#' @export
#' @importFrom httr POST
createLabel <- function(repos, label, color) {
  color <- tolower(color)
  color <- gsub("^#([0-9a-f]{6})$", "\\1", color)
  stopifnot(grepl("^[0-9a-f]{6}$", color))

  url <- gitUrl(repos, "/repos/:owner/:repo/labels")
  data <- list(name=label, color=color)
  res <- POST(url, repos$auth, body=json(data), encode="json")
  res
}
