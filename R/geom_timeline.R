
#' @title Geom function for creating the timeline from Earthquake data from NOAA
#'
#' @param mapping set aesthetics
#' @param data The data of which we want to make the timeline
#' @param stat The statistics that you want to use
#' @param position Position of the string
#' @param na.rm If False, null value are removed
#' @param show.legend Should layer have a legend or not
#' @param inherit.aes Basically inherit the aesthetics or not
#' @param xmin Providing the minimum date for the data to be plotted
#' @param xmax Providing maximum date for the data to be plotted
#' @param ... Other arguments that can be passed
#'
#' @importFrom magrittr %>%
#' @return This return the geom timeline
#'
#' @examples
#' \dontrun{
#' data(NOAA_data)
#' NOAA_data %>% eq_clean_data() %>%
#' dplyr::filter(Country %in% c("India", "Pakistan")) %>%
#'    ggplot(aes(x = date, y = Country, color = Total.Deaths, size = Mag,
#'               magnitude = Mag, label =Location_Name)) +
#'    labs(color="#deaths",size="Magnitude",x="DATE")+
#'    geom_timeline(alpha=0.2,xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31'))+
#'   labs(x="DATE")+theme_minimal()
#' }
#' @export

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          xmin = as.Date('0001-01-01'), xmax = as.Date('2020-01-01'), ...) {
    ggplot2::layer(
        geom = GeomTimeline, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, xmin = xmin, xmax = xmax, ...)
    )
}



#' @title ggproto timeline object of class ggproto
#'
#' @description This is the setup required for creating a new geom class. This GeomTimeLine
#'     inherits from a top level class called Geom.It creates a geom that draws the
#'     timeline for a specified date interval and put points on it for each earthquake.
#'
#' @export

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = c("x"),
                                 non_missing_aes = c("size", "shape", "colour"),
                                 default_aes = ggplot2::aes(
                                     y = 0.25, shape = 19, colour = "black", size = 1.25, fill = 'black',
                                     alpha = NA, stroke = 0.5
                                 ),
                                 extra_params = c("na.rm", "xmin", "xmax"),

                                 setup_data = function(data, params) {
                                     data <- data %>%
                                         dplyr::filter(dplyr::between(x, params$xmin, params$xmax))
                                 },

                                 draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                                     coords <- coord$transform(data, panel_params)

                                     grid::pointsGrob(
                                         coords$x, coords$y,
                                         pch = coords$shape,
                                         gp = grid::gpar(
                                             col = ggplot2::alpha(coords$colour, coords$alpha),
                                             fill = ggplot2::alpha(coords$fill, coords$alpha),
                                             # Stroke is added around the outside of the point
                                             fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2,
                                             lwd = coords$stroke * ggplot2::.stroke / 2
                                         )
                                     )
                                 },

                                 draw_key = ggplot2::draw_key_point
)

