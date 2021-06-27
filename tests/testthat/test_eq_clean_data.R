library(noaaquake)
context("Testing that the clean_data function runs correctly and classes matches ")

test_that("Data gets cleaned correctly", {
    df <- NOAA_data %>% eq_clean_data()
    expect_s3_class(df, "data.frame")        # the result will be a dataframe
    expect_s3_class(df$date, "Date")         # the date column will be of the class date
    expect_type(df$Latitude, "double")  # that each of LATITUDE and LONGITUDE are of double
    expect_type(df$Longitude, "double")
})
