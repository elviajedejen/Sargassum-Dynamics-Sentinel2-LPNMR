##Metereological Data
#NOAA NDBC Buoy 42085 - stdmet data
#Study Period: 2016 - 2024


#Load packages
library(dplyr)
library(purrr)
library(readr)

# ----------------
# 1. Set path
# ----------------
path <- "D:/Owner/Doctorado/Thesis/Chapter 1.  Perez et al (2026)/Data/NOAA_Buoy_42085/Standard Meteorological Data (stdmet)/"

# Check that the folder exists
file.exists(path)

# List all .txt files
files <- list.files(path, pattern = "\\.txt$", full.names = TRUE)

# Check files found
print(files)
print(length(files))
basename(files)

# ----------------
# 2. Function to read one NDBC file
# ----------------
read_ndbc_file <- function(file) {
  
 # Read first two header lines manually
  header_names <- readLines(file, n = 1)
  header_units <- readLines(file, n = 2)[2]
  
 # Remove leading # from first header line
  header_names <- gsub("^#", "", header_names)

# Split column names
  col_names <- strsplit(header_names, "\\s+")[[1]]
  col_names <- col_names[col_names != ""]
  
# Read data starting after the two header lines
  df <- read.table(
    file,
    skip = 2,
    header = FALSE,
    fill = TRUE,
    stringsAsFactors = FALSE
  )
  
# Assign column names
  names(df) <- col_names
  
 return(df)
}

# ----------------
# 3. Read all files and combine
# ----------------
data_list <- lapply(files, read_ndbc_file)
buoy_data <- bind_rows(data_list)

# Inspect structure
str(buoy_data)
head(buoy_data)