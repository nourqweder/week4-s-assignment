#'@title fars_read
#'@description Reas CSV file and display the data
#'@references https://www.coursera.org/learn/r-packages/peer/25XGp/documenting-code
#'@param filename the file name which looking for to display its data
#'@example fars_read(".\data\accident_2013.csv")
#'@export
#'@return data table with file's data 

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
#'@param year input from user 
#'@example make_filename(2019) 
#'@export
#'@return renamed file by adding year to the end
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
#'@title fars_map_state
#'@description Reas CSV file and display the data
#'@references https://www.coursera.org/learn/r-packages/peer/25XGp/documenting-code
#'@param state.num
#'@param year
#'@param state.num  integer number congruous to the state in US 
#'@export
#'@return It plots a map a car information according to a givein US stateic
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