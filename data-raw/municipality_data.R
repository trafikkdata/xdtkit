
# Save municipality names
municipality_names <- read.table(
  "data-raw/kommunenummer.csv",
  sep = ";",
  header = TRUE,
  encoding = "UTF-8",
  fileEncoding = "UTF-8"
)
usethis::use_data(municipality_names, overwrite = TRUE, compress = "xz")

# Save county names
county_names <- read.table(
  "data-raw/fylkesnummer.csv",
  sep = ";",
  header = TRUE,
  encoding = "UTF-8",
  fileEncoding = "UTF-8"
)
usethis::use_data(county_names, overwrite = TRUE, compress = "xz")
