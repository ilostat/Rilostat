.onAttach <- function(libname, pkgname) {
  try(.check_version_once(pkgname, quiet = TRUE), silent = TRUE)
}