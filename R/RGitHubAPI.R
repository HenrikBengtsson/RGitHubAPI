library("httr")
library("jsonlite")

v <- function(...) {
  x <- c(...)
  structure(x, class=c("v", class(x)))
}

gitUrl <- function(url, owner, repo) {
  url <- sprintf("https://api.github.com%s", url)
  url <- gsub(":owner", owner, url, fixed=TRUE)
  url <- gsub(":repo", repo, url, fixed=TRUE)
  url
} # gitUrl()

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Labels
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
listLabels <- function(owner, repo) {
  url <- gitUrl("/repos/:owner/:repo/labels", owner, repo)
  res <- content(GET(url, auth), as="parsed")
  if (length(res) > 0L) names(res) <- sapply(res, FUN=`[[`, "name")
  res
}

hasLabels <- function(labels, owner, repo) {
  res <- listLabels(owner, repo)
  res <- is.element(labels, names(res))
  names(res) <- labels
  res
}

createLabel <- function(label, color, owner, repo) {
  color <- tolower(color)
  color <- gsub("^#([0-9a-f]{6})$", "\\1", color)
  stopifnot(grepl("^[0-9a-f]{6}$", color))

  url <- gitUrl("/repos/:owner/:repo/labels", owner, repo)
  data <- list(name=label, color=color)
  res <- POST(url, auth, body=json(data), encode="json")
  res
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Issues
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
listIssues <- function(owner, repo) {
  url <- gitUrl("/repos/:owner/:repo/issues", owner, repo)
  res <- content(GET(url, auth), as="parsed")
  if (length(res) > 0L) names(res) <- sapply(res, FUN=`[[`, "title")
  res
}

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
