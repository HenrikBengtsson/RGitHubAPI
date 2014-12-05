library("R.utils")
source("R/RGitHubAPI.R")

## downloadFile(url="http://curl.haxx.se/ca/cacert.pem")
## httpGET(url, cainfo="cacert.pem")

# GitHub token (https://github.com/settings/applications)
token <- Sys.getenv("GITHUB_TOKEN")
auth <- authenticate(token, "x-oauth-basic", type="basic")

owner <- "HenrikBengtsson"
repo <- "34ef56ab1243"

cran_pkgs <- c(
# R.* packages
"R.methodsS3",
"R.oo",
"R.utils",
"R.cache",
"R.devices",
"R.huge",
"R.rsp",
"R.filesets",
"R.matlab",
# aroma.* packages
"aroma.core",
"aroma.affymetrix",
"aroma.apd",
"aroma.cn",
# Miscellaneous packages
"matrixStats",
"PSCBS"
)

bioc_pkgs <- c(
"aroma.light",
"affxparser",
"illuminaio"
)

## List all labels
labels <- listLabels(owner, repo)
print(labels)

## Check if labels exist
print(hasLabels("CRAN Policy", owner, repo))

## Create new labels
#for (pkg in cran_pkgs) {
#  res <- createLabel("CRAN Policy", color="#eb6420", owner=owner, repo=pkg)
#  print(res)
#}


issue <- list(
  title="An test issue",
  body="Bla bla bla...",
  labels=v("wontfix")
)
res <- try(createIssue(issue, owner=owner, repo=repo))
print(res)


## Create new issue
issue <- list(
  title="Update package title to use Title Case",
  body=
"From Writing R Extensions:
> The mandatory 'Title' field should give a short description of the package. Some package listings may truncate the title to 65 characters. **It should use title case (that is, use capitals for the principal words)**, not use any markup, not have any continuation lines, and not end in a period."
#  labels=v("CRAN Policy")
)

pkgs <- bioc_pkgs
for (pkg in pkgs) {
##  res <- try(createIssue(issue, owner=owner, repo=pkg))
  print(res)
}
