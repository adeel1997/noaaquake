#'@title Data cleaning and manipulation from raw NOAA data
#'
#'@details This function takes NOAA earthquake data and creates a \code{Date} column
#'     of type date by combining \code{Year}, \code{Mo} and \code{Dy}. It then
#'     makes sure that \code{Longitude} and \code{Latitude} are of type numeric
#'
#' @param data a data frame of NOAA earthquake.
#'
#' @importFrom magrittr %>%
#'
#' @return A \code{tibble} with a new column \code{Date} but removing
#'     \code{Year}, \code{Mo} & \code{Dy} variables of the input data frame.
#' @examples
#' data(NOAA_data)
#' eq_clean_data(NOAA_data)
#'
#' @export
eq_clean_data <- function(data){
    ## Converting date into date column
    data <- data %>% dplyr::mutate(date = as.Date(paste0(Year,"-",Mo,"-",Dy),format=
                                               "%Y-%m-%d"))
    ## Converting Lat Long into numeric
    data$Latitude <- as.numeric(data$Latitude)
    data$Longitude <- as.numeric(data$Longitude)
    ## Getting the location name and splitting with colon
    location_name <- strsplit(data$Location.Name,split = ":")
    ## Country is first in the list and second is region
    Country <- sapply(location_name,"[",1)
    ## Converting All caps to Title case
    Country <- stringr::str_to_title(Country)
    data$Country <- Country
    ## Removing the country and Selecting the region
    region <- sapply(location_name,"[",2)
    ## Removing extra white space
    region <- stringr::str_trim(region)
    ## Converting All caps to title case
    region <- stringr::str_to_title(region)
    ## Converting into a vector
    region <- as.vector(unlist(region))
    data$Location_Name <- region
    return(data)

}

