## covr: skip=all

.onAttach <- function(libname, pkgname) {
  pi <- utils::packageDescription(pkgname)
  pkgStartupMessage(pkgname, " v", pi$Version, " (", pi$Date,
           ") successfully loaded. See ?", pkgname, " for help.")
}


############################################################################
# HISTORY:
# 2015-02-03
# o Created.
############################################################################
