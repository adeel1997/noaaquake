
#' @title ggproto GeomTimeline Label object of class ggproto
#'
#' @description This is the setup required for creating a new geom class. This GeomTimeLabel
#'     inherits from a top level class called Geom.It creates a geom that draws the
#'     label of the eartquae for a specified date interval and put points on it for each earthquake.
#'
#' @export

GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                      required_aes = c("x", "label"),

                                      default_aes = ggplot2::aes(
                                          y = 0.2, colour = "transparent", size = 0.3, offset = 0.08, angle =25,
                                          alpha = NA, family = "", fontface = 1, lineheight = 0.5
                                      ),

                                      draw_panel = function(data, panel_params, coord, parse = FALSE,
                                                            na.rm = FALSE, check_overlap = FALSE) {
                                          lab <- data$label
                                          data <- coord$transform(data, panel_params)

                                          text <- grid::textGrob(
                                              lab,
                                              data$x, data$y + data$offset, default.units = "native",
                                              just = 0,
                                              rot = data$angle,
                                              gp = grid::gpar(
                                                  col = ggplot2::alpha(data$colour, data$alpha),
                                                  fontsize = (data$size * ggplot2::.pt)/2,
                                                  fontfamily = data$family,
                                                  fontface = data$fontface,
                                                  lineheight = data$lineheight
                                              )
                                          )

                                          lines <- grid::segmentsGrob(x0 = data$x,
                                                                      x1 = data$x,
                                                                      y0 = data$y,
                                                                      y1 = data$y + data$offset,
                                                                      default.units = "native",
                                                                      gp = grid::gpar(lwd = data$lineheight))

                                          grid::gTree(children = grid::gList(
                                              text,
                                              lines
                                          ))
                                      },

                                      draw_key = ggplot2::draw_key_text
)




#' Adding label to Geom Timeline
#'
#' @param mapping set aesthetics
#' @param data provide the dataframe of the labels
#' @param stat Any statistics
#' @param position Position of the label
#' @param ... Other arguments that can be passed
#' @param na.rm TRUE if you want to remove the missing and False if not
#' @param show.legend TRUE to show legend
#' @param inherit.aes Basically inherit the aesthetics or not
#'
#' @return This returns the geom timeline with label
#'
#' @examples
#' \dontrun{
#' labels <- NOAA_data %>% eq_clean_data() %>%
#' dplyr::filter(Country %in% c("India", "Pakistan"))%>%
#'    filter(date> ymd('2000-01-01') & date <ymd('2015-12-31'))%>%
#'   top_n(5,Mag)
#'
#' NOAA_data %>% eq_clean_data() %>%
#'    dplyr::filter(Country %in% c("Pakistan", "India")) %>%
#'    ggplot(aes(x = date, y = Country, color = Total.Deaths, size = Mag,
#'               magnitude = Mag, label =Location_Name)) +
#'    geom_timeline(alpha=0.4,xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31'))+
#'    geom_timeline_label(data = labels,
#'                        mapping = aes(x = date, y = Country, label = Location_Name))+
#'    labs(color="#deaths",size="Richter scale value",x="DATE")+
#'    theme_minimal()
#'    }
#' @export

geom_timeline_label <- function(mapping = NULL, data = NULL,
                                stat = "identity",
                                position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE)
{
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomTimelineLabel,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}

