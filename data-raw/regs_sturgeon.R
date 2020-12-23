# raw data for Sturgeon fishing regulations

RegsSturgeon <- read.csv(file = "data-raw/regs_sturgeon.csv", header = TRUE)

devtools::use_data(RegsSturgeon, overwrite = TRUE)
