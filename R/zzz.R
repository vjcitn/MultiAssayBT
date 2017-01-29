
.onLoad <- function(libname, pkgname) {
 apiKey <- Sys.getenv("GOOGLE_API_KEY", unset=NA)
  if (!is.na(apiKey) && nchar(apiKey) > 0) {
    authenticate(apiKey=apiKey)
  }
  invisible()
}

