require.or.install <- function(package.name) {
  if (!require(package.name, character.only=TRUE)) {
    install.packages(package.name, dep=TRUE)
    if (!require(package.name, character.only=TRUE)) {
      stop(paste("Could not install package ", package.name))
    }
  }
}

download.if.not.present <- function(url, file.name) {
  if (!file.exists(file.name)) {
    download.file(url, file.name)
  }
}

unzip.if.not.present <- function(zip.file.name, file.name) {
  if (!file.exists(file.name)) {
    unzip(zip.file.name, overwrite=TRUE, exdir=file.name)
  }
}
