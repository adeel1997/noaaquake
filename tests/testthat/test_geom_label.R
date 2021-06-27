library(noaaquake)

context("Testing that the geom label function
        run and return the correct objects")

test_that("geom_label runs correctly", {
     labels <- NOAA_data %>% eq_clean_data() %>%
     dplyr::filter(Country %in% c("India", "Pakistan"))%>%
         dplyr::filter(date > lubridate::ymd('2000-01-01') & date < lubridate::ymd('2015-12-31'))%>%
         dplyr::top_n(5,Mag)

    plot =  NOAA_data %>% eq_clean_data() %>%
        dplyr::filter(Country %in% c("Pakistan", "India")) %>%
        ggplot2::ggplot(ggplot2::aes(x = date, y = Country, color = Total.Deaths, size = Mag,
                   magnitude = Mag, label =Location_Name)) +
        geom_timeline(alpha=0.4,xmin = lubridate::ymd('2000-01-01'), xmax = lubridate::ymd('2015-12-31'))+
        geom_timeline_label(data = labels,
                            mapping = ggplot2::aes(x = date, y = Country, label = Location_Name))+
        ggplot2::labs(color="#deaths",size="Richter scale value",x="DATE")+
        ggplot2::theme_minimal()


    ## Testing whether the resulting plot is ggplot or not
    expect_s3_class(labels,"data.frame")
    expect_equal(class(plot)[1], "gg")
    expect_equal(class(plot)[2], "ggplot")
})
