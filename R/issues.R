#' List issues
#'
#' Retrievs all issues.
#'
#' @param repos A GitHubRepository object.
#' @return Returns a character vector.
#'
#' @export
#' @importFrom httr GET
#' @importFrom httr content
listIssues <- function(repos) {
  url <- gitUrl(repos, "/repos/:owner/:repo/issues")
  res <- content(GET(url, repos$auth), as="parsed")
  if (length(res) > 0L) names(res) <- sapply(res, FUN=`[[`, "title")
  res
}

#' Checks existance of issues
#'
#' Checks whether a set of issues exists or not by
#' scanning issue titles.
#'
#' @param repos A GitHubRepository object.
#' @param titles Character vector.
#' @param agrep A numeric in [0,1] controlling amount of discrepance allowed.
#' @return Returns a named logical vector.
#'
#' @export
hasIssues <- function(repos, titles, agrep=0) {
  res <- listIssues(repos)
  if (length(res) == 0) {
    res <- rep(FALSE, times=length(titles))
  } else if (agrep == 0) {
    res <- is.element(titles, names(res))
  } else {
    res <- sapply(titles, FUN=function(title) {
      agrepl(title, res, max.distance=agrep, ignore.case=TRUE, fixed=TRUE)
    })
  }
  names(res) <- titles
  res
}


#' Creates a new issue
#'
#' Creates a new issue
#'
#' @param repos A GitHubRepository object.
#' @param title Character string.
#' @param body Character string.
#' @param assignee (optional) Character string.
#' @param milestone (optional) Character vector.
#' @param labels (optional) Character vector.
#' @param agrep A numeric in [0,1] used to test for already existing
#' issues in order to avoid duplicates.
#' @return Returns a named logical vector.
#'
#' @export
#' @importFrom httr POST
#' @importFrom R.oo attachLocally
#' @importFrom R.methodsS3 throw
createIssue <- function(repos, title, body, assignee=NULL, milestone=NULL, labels=NULL, agrep=0.1) {
  if (is.list(title)) attachLocally(title)
  if (agrep >= 0 && hasIssues(title, agrep=agrep, repos=repos)) {
    throw("An issue with a very similar title already exists: ", title)
  }

  url <- gitUrl(repos, "/repos/:owner/:repo/issues")
  data <- list(title=title, body=body, assignee=assignee, milestone=milestone, labels=labels)
  data <- data[sapply(data, FUN=length) > 0L]
#  cat(json(data))
  res <- POST(url, repos$auth, body=json(data), encode="json")
  res
}
