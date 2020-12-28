
#' Download Required Ethnorace Distribution Data
#'
#' @param destination A file name in the current working directory where you
#'   would like to download the data files. The function will create the folder
#'   if no folder of that name currently exists. Defaults to downloading the
#'   files into the current working directory.
#'
#' @param download Option to download data sets from
#'   \url{https://github.com/bwilden/bperdata}. Only needs to be done once.
#'
#' @param load_data Should data sets be loaded into the Global Environment?
#'   Defaults to TRUE.
#'
#' @export
load_bperdata <- function(destination = NA,
                          download = FALSE,
                          load_data = TRUE) {
  file_list <- c(
    "apartments",
    "birth_years",
    "blocks",
    "firstnames",
    "genders",
    "parties",
    "surnames",
    "zips"
  )

  if (is.na(destination)) {
    dest <- getwd()
  } else if (dir.exists(destination)) {
    dest <- paste0(getwd(), "/", destination)
  } else {
    dir.create(destination)
    dest <- paste0(getwd(), "/", destination)
  }

  if (length(list.files(destination, pattern = ".rda")) == 0 &
      download == FALSE) {
    stop(
      "The directory is empty! Set 'download = TRUE' to download files into destination folder first"
    )
  }

  if (length(list.files(destination, pattern = ".rda")) < length(file_list) &
      download == FALSE) {
    warning("The directory does not contain the full set of bper's required data files")
  }

  for (datafile in file_list) {

    if (download == TRUE) {
      print(paste0("Downloading ", toupper(datafile), " data set..."))
      download.file(
        url = paste0(
          "https://github.com/bwilden/bperdata/blob/master/data/",
          datafile,
          ".rda?raw=true"
        ),
        destfile = paste0(dest, "/", datafile, ".rda")
      )
    }

    if (load_data == TRUE) {
      load(file = paste0(dest, "/", datafile, ".rda"), .GlobalEnv)
    }
  }
}
