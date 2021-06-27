library(noaaquake)

context("Testing the output map is a leaflet map or not")

test_that("eq_map runs without error", {
    data_clean <- NOAA_data %>%
        eq_clean_data()%>%
        dplyr::filter(Country == "India"|Country == "Pakistan" & lubridate::year(date) >= 2000)%>%
        tidyr::drop_na(Latitude,Longitude)
    map = eq_map(data = data_clean, annot_col = "Mag")

    expect_equal(class(map)[1], "leaflet")
    expect_equal(class(map)[2], "htmlwidget")
})

