# raw data for Sturgeon fishing regulations

RegsSturgeon <- read.csv(file = "data-raw/regs_sturgeon.csv", header = TRUE)
RegsStriper <- read.csv(file = "data-raw/regs_striper.csv", header = TRUE)

usethis::use_data(
  RegsSturgeon,
  RegsStriper,
  internal = TRUE,
  overwrite = TRUE
)
