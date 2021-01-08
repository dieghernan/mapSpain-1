library(tinytest)

expect_silent(esp_get_munic_siane())

expect_message(esp_get_munic_siane(verbose = TRUE))
expect_message(esp_get_munic_siane(verbose = TRUE, update_cache = TRUE))
expect_message(esp_get_munic_siane(region = "Canarias", verbose = TRUE))

expect_error(esp_get_munic_siane(year = "2019-15-23"))
expect_error(esp_get_munic_siane(year = "2019-15"))
expect_error(esp_get_munic_siane(region = "XX"))
expect_error(esp_get_munic_siane(epsg = "5689"))
expect_error(esp_get_munic_siane(resolution = 5.6))
expect_error(esp_get_munic_siane(year = "2040"))
expect_error(esp_get_munic_siane(munic = "XX"))

expect_silent(esp_get_munic_siane(moveCAN = FALSE))
expect_silent(esp_get_munic_siane(moveCAN = c(0, 10)))
expect_silent(esp_get_munic_siane(year = "2019-10-23"))
expect_silent(esp_get_munic_siane(munic = "Nieva"))
expect_silent(esp_get_munic_siane(region = "Alava"))


a <- mapSpain::esp_codelist
n <- a$nuts1.name

s <- esp_get_munic_siane(region = n)
expect_equal(length(unique(s$cpro)), 52)
