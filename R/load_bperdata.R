
#' Download Ethnorace Distribution Data
#'
#' This function downloads, and loads into the Global Environment, the set of
#' data files necessary for \code{\link{predict_ethnorace}}. Combined file size
#' is 59 MB. For more information see \url{https://github.com/bwilden/bperdata}.
#'
#' @section Files: \code{apartments.rda}, \code{birth_years.rda},
#'   \code{blocks.rda}, \code{counties}, \code{firstnames.rda},
#'   \code{genders.rda}, \code{nationwide} \code{parties.rda},
#'   \code{places.rda}, \code{states}, \code{surnames.rda}, \code{zips.rda}
#'
#' @param destination A folder in the current working directory into which you
#'   would like to download the data files. The function will create the folder
#'   if no folder of that name currently exists. If left blank defaults to
#'   downloading the files into the current working directory.
#'
#' @param download Option to download data sets from
#'   \url{https://github.com/bwilden/bperdata/tree/master/data}. Only needs to
#'   be done once if \code{save_files = TRUE}.
#'
#' @param load_data Option to load data sets into the Global Environment? If you
#'   want to download the files only, set \code{load_data = FALSE}.
#'
#' @param save_files Option to delete files in directory after loading into
#'   Global Environment.
#'
#' @param include_blocks Option to avoid downloading/loading the Census Blocks
#'   data set if using other geolocations instead. The blocks data is by the
#'   largest file to download.
#'
#' @examples
#' # First time:
#' \dontrun{
#' load_bperdata(destination = "bperdata", download = TRUE)
#' }
#'
#' # After files are downloaded:
#' \dontrun{
#' load_bperdata(destination = "bperdata", download = FALSE)
#' }
#'
#' @seealso \code{\link{predict_ethnorace}}
#'
#' @export
load_bperdata <- function(destination = NA,
                          download = FALSE,
                          load_data = TRUE,
                          save_files = TRUE,
                          include_blocks = FALSE) {

  file_list <- c(
    "apartments",
    "birth_years",
    "counties",
    "firstnames",
    "genders",
    "nationwide",
    "parties",
    "places",
    "states",
    "surnames",
    "zips"
  )

  if (include_blocks == TRUE) {
    file_list <- c(file_list, "blocks")
  }

  if (is.na(destination)) {
    dest <- here::here()
  } else if (dir.exists(destination)) {
    dest <- here::here(destination)
  } else {
    dir.create(destination)
    dest <- here::here(destination)
  }

  if (length(list.files(dest, pattern = ".rda")) == 0 &
    download == FALSE) {
    stop(
      "The directory is empty! Set 'download = TRUE' to download files into destination folder first"
    )
  }

  if (length(list.files(dest, pattern = ".rda")) < length(file_list) &
    download == FALSE) {
    warning("The directory does not contain the full set of bper's required data files")
  }

  for (datafile in file_list) {
    if (download == TRUE) {
      print(paste0("Downloading ", toupper(datafile), " data set..."))
      utils::download.file(
        url = paste0(
          "https://github.com/bwilden/bperdata/blob/master/data/",
          datafile,
          ".rda?raw=true"
        ),
        destfile = here::here(dest, paste0(datafile, ".rda")),
        mode = "wb"
      )
    }

    if (load_data == TRUE) {
      load(file = here::here(dest, paste0(datafile, ".rda")), .GlobalEnv)
    }

    if (save_files == FALSE) {
      file.remove(here::here(dest, paste0(datafile, ".rda")))
    }
  }
}
