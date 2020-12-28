
download_bperdata <- function(destination = NA) {

  if (is.na(destination)) {
    dest <- getwd()
  } else if (dir.exists(destination)) {
    dest <- paste0(getwd(), "/", destination)
  } else {
    dir.create(destination)
    dest <- paste0(getwd(), "/", destination)
  }

  print("Downloading Apartments data set...")
  download.file(url = "https://github.com/bwilden/bperdata/blob/master/data/apartments.rda?raw=true",
                destfile = paste0(dest, "/apartments.rda"))

  load(paste0(dest, "/apartments.rda"))
}
