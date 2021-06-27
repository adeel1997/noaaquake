library(noaaquake)

context("Testing that the geom timeline function
        run and return the correct objects")

test_that("geom_timeline runs correctly", {

    plot = NOAA_data %>% eq_clean_data() %>%
         dplyr::filter(Country %in% c("India", "Pakistan")) %>%
            ggplot2::ggplot(ggplot2::aes(x = date, y = Country, color = Total.Deaths, size = Mag,
                       magnitude = Mag, label =Location_Name)) +
            ggplot2::labs(color="#deaths",size="Magnitude",x="DATE")+
            geom_timeline(alpha=0.2,xmin = lubridate::ymd('2000-01-01'), xmax = lubridate::ymd('2015-12-31'))+
            ggplot2::labs(x="DATE")+ggplot2::theme_minimal()

    ## Testing whether the resulting plot is ggplot or not
    expect_equal(class(plot)[1], "gg")
    expect_equal(class(plot)[2], "ggplot")
})
