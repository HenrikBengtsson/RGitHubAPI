#' List issues
#'
#' Retrievs all issues.
#'
#' @param owner Character string.
#' @param repo Character string.
#' @return Returns a character vector.
#'
#' @export
#' @importFrom httr GET
#' @importFrom httr content
listIssues <- function(owner, repo) {
  url <- gitUrl("/repos/:owner/:repo/issues", owner, repo)
  res <- content(GET(url, auth), as="parsed")
  if (length(res) > 0L) names(res) <- sapply(res, FUN=`[[`, "title")
  res
}

#' Checks existance of issues
#'
#' Checks whether a set of issues exists or not by
#' scanning issue titles.
#'
#' @param titles Character vector.
#' @param agrep A numeric in [0,1] controlling amount of discrepance allowed.
#' @param owner Character string.
#' @param repo Character string.
#' @return Returns a named logical vector.
#'
#' @export
hasIssues <- function(titles, agrep=0, owner, repo) {
  res <- listIssues(owner, repo)
  if (length(res) == 0) {
    res <- rep(FALSE, length=length(titles))
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
#' @param title Character string.
#' @param body Character string.
#' @param assignee (optional) Character string.
#' @param milestone (optional) Character vector.
#' @param labels (optional) Character vector.
#' @param agrep A numeric in [0,1] used to test for already existing
#' issues in order to avoid duplicates.
#' @param owner Character string.
#' @param repo Character string.
#' @return Returns a named logical vector.
#'
#' @export
#' @importFrom httr POST
#' @importFrom R.oo attachLocally
#' @importFrom R.methodsS3 throw
createIssue <- function(title, body, assignee=NULL, milestone=NULL, labels=NULL, agrep=0.1, owner, repo) {
  if (is.list(title)) attachLocally(title)
  if (agrep >= 0 && hasIssues(title, agrep=agrep, owner=owner, repo=repo)) {
    throw("An issue with a very similar title already exists: ", title)
  }

  url <- gitUrl("/repos/:owner/:repo/issues", owner, repo)
  data <- list(title=title, body=body, assignee=assignee, milestone=milestone, labels=labels)
  data <- data[sapply(data, FUN=length) > 0L]
  cat(json(data))
  res <- POST(url, auth, body=json(data), encode="json")
  res
}
