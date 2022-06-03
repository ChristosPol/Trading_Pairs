# Preamble ---------------------------------------------------------------------
rm(list = ls())
.rs.restartR()

# Source functions
path_source <- "Code/Source"
files.sources = list.files(path_source, full.names = T)
sapply(files.sources, source)

# Choose which unix time to use for pulling data
# Choose from ["start_of_time", "manually", "latest_available"]
unix_time <- "latest_available"

# Choose any pair to pull
pair <- "BTCUSD"
# pair <- "SHIBEUR"
# Path to save results
data_path <- "Data"

# Create pair directory
dir.create(paste(data_path, pair, sep ="/"), showWarnings = FALSE)

# Fix path
pair_data_results <- paste(data_path, pair, sep ="/")

# Select initial id based on unix_time arg
initial_id <- select_period(unix_time,  diff_time = 90)

# Pull historical trades since initial id from epoch time
hist_trades_pair(sleep = 1, hist_id = initial_id, pair = pair)
#ATOMUSD --- 
#BTCUSD ----
#ADAUSD----
#DOTUSD---
#EOSUSD----
#ETHUSD----
#LINKUSD----
# MANAUSD----
#MATICUSD----
#NANOUSD
#UNIUSD

pairs <- c("ATOMUSD", "BTCUSD", "ADAUSD", "DOTUSD", "EOSUSD", "ETHUSD", "LINKUSD",
  "MANAUSD", "MATICUSD", "NANOUSD", "UNIUSD")


pairs <- c("ATOMUSD", "LINKUSD",
           "MATICUSD")
