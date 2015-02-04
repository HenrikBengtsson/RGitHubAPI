#' Creates a reference to a GitHub repository
#'
#' Creates a reference to a GitHub repository
#'
#' @param owner Character string.
#' @param repo Character string.
#' @param auth (optional) Character string.
#' @return Returns a named list.
#'
#' @export
repos <- function(owner, repo, auth=NULL) {
  structure(list(owner=owner, repos=repo, auth=auth), class="GitHubRepository")
}

gitUrl <- function(repos, url) {
  url <- sprintf("https://api.github.com%s", url)
  url <- gsub(":owner", repos$owner, url, fixed=TRUE)
  url <- gsub(":repo", repos$repo, url, fixed=TRUE)
  url
} # gitUrl()
