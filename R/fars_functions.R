#' Read in CSV file.
#'
#' Read the CSV file containing American public yearly data
#' regarding fatal injuries suffered in motor vehicle traffic crashes, then
#' convert the default dataframe format to tibble.
#'
#' @param filename name of CSV file
#'
#' @return Tibble of read-in dataset
#'
#'
#' @examples
#' \dontrun{fars_read("accident_2013.csv")}
#'
#' @details
#' \itemize{
#'  \item{}{If the \code{filename} does not exist in the current dictionary, R
#' will be stopped and pose a warning massage}
#'  \item{}{User is recommended to initially install package \code{\link{readr}}
#' and \code{\link{dplyr}}}
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#------------------------------------
#'@title make_filename
#'@description rename a specific CSV file and print the the name after renaming
#'@references https://www.coursera.org/learn/r-packages/peer/25XGp/documenting-code
#'Make name for read-in dataset of specific year
#'
#'Make name for the CSV file containing Americam public
#'yearly data regarding fatal injuries suffered in motor
#'vehicle traffic crashes.
#'
#' @param year integer or string specifying
#' year of the dataset ready to be read in.
#'
#' @return string or a vector of strings specifying the CSV file name.
#'
#' @examples
#' \dontrun{make_filename <- function(2013)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#--------------------------------
#'@title fars_read_years
#'@description rename the file's name with the entered year and then read the file and store the data inside dat variable
#'It through warning when input is invalid
#'@references https://www.coursera.org/learn/r-packages/peer/25XGp/documenting-code
#'@param year input from user 
#'@export
#'@return nothing
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#-----------------------------------------
#'@title fars_summarize_years
#'@description Creates a summary table with the counts of car fatalities
#'@references https://www.coursera.org/learn/r-packages/peer/25XGp/documenting-code
#'@param year input from user 
#'@export
#'@return data table with file's data 
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#---------------------------------------
#'Plot the spot of crashes
#'
#'Plot the spot of fatal injuries suffered in motor vehicle
#'traffic crashes on the map of the state specified by state
#'number.
#'
#' 
#' @param state.num string or integer specifying the state number
#' corresponing to an existing state in US
#'
#' @param year integer specifying year of the dataset ready to be
#' read in
#'
#' @return Plot of crashes on the map by latitude and longitude
#'
#'@examples
#' \dontrun{year <- 2013
#' state.num <- "31"
#' fars_map_state(state.num,year)}
#'
#'@details
#' \itemize{
#'  \item{}{Valid years are exclusively 2013, 2014 and 2015, an error
#'  message will be post and program will be stopped if invalid years
#'  are inputted}
#'  \item{}{User is required to install prerequisted package \code{\link{dplyr}}
#'  and \code{\link{maps}}, an error message will be post and program will be stopped}
#'  }
#'
#'@export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}