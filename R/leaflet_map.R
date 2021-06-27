#' @title Leaflet map tool
#'
#' @param data Adding the dataframe
#' @param annot_col Annotate column that will come as a popup
#'
#' @importFrom magrittr %>%
#' @return It returns a leaflet map
#'
#' @examples
#' \dontrun{
#' data_clean <- NOAA_data %>%
#' eq_clean_data() %>%
#' dplyr::filter(Country == "India"|Country == "Pakistan" & lubridate::year(date) >= 2000)
#' eq_map(data = data_clean, annot_col = "Mag")
#' }
#' @export
eq_map <- function(data, annot_col) {
    # Annot_col in a character string
    leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(data = data, radius = ~ Mag*1.5,opacity = 0.5,weight = 5,
                                  lng = ~ Longitude, lat = ~ Latitude,
                                  popup = ~ data[[annot_col]])
}


#' @title Adding customized label in the Leaflet map
#'
#' @param data Adding the data dataframe
#'
#' @return This returns a HTML tags for the customized label
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' data_clean <- NOAA_data %>%
#' eq_clean_data()%>%
#'    dplyr::filter(Country == "India"|Country == "Pakistan" & lubridate::year(date) >= 2000)%>%
#'    dplyr::mutate(popup_text = eq_create_label(.))
#' eq_map(data = data_clean, annot_col = "popup_text")
#' }
#' @export
eq_create_label <- function(data) {
    labelled_data <- data %>%
        dplyr::mutate(popup_text = paste("<b>Location:</b>", Location_Name, "<br />",
                                         "<b>Magnitude:</b>", Mag, "<br />",
                                         "<b>Total Deaths:</b>", Deaths, "<br />"),
                      popup_text = ifelse(is.na(Location_Name) | is.na(Mag) | is.na(Deaths),
                                          paste("<b>No Data Available</b>"), popup_text))
    labelled_data$popup_text
}




