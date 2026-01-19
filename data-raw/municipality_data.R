
# Save municipality names
municipality_names <- read.table("data-raw/kommunenummer.csv", sep = ";", header = TRUE)
usethis::use_data(municipality_names, overwrite = TRUE)

# Save county names
county_names <- read.table("data-raw/fylkesnummer.csv", sep = ";", header = TRUE)
usethis::use_data(county_names, overwrite = TRUE)
