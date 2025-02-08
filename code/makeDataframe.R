library(readxl)
library(dplyr)

columns_to_convert_date <- c("YEAR", "MONTH", "DAY")
columns_to_convert_time <- c("HOUR", "MIN", "SEC")
oldDf <- read_excel("polygon mirzai/zagros90-24.xls", sheet="zagros90-24")


# Convert specified columns to character
changingDf <- oldDf %>%
  mutate_at(vars(columns_to_convert_date), as.character) 

changingDf <- changingDf %>%
  mutate(
    MONTH = sprintf("%02d", as.numeric(MONTH)),
    DAY = sprintf("%02d", as.numeric(DAY)),
    HOUR = sprintf("%02d", as.numeric(HOUR)),
    MIN = sprintf("%02d", as.numeric(MIN)),
    SEC = sprintf("%05.2f", as.numeric(SEC)),
    Long = sprintf("%.4f", as.numeric(Long)) , 
    Lat = sprintf("%.4f", as.numeric(Lat)),
    MW = sprintf("%.1f", as.numeric(MW))# Using %05.2f for leading zeros and two decimal places
  ) %>%
#   mutate(SEC = sprintf("%.2f", SEC)) %>%
  mutate_at(vars(columns_to_convert_time), as.character) 

# Create a new column "DATE" by concatenating "YEAR", "MONTH", and "DATE"
changingDf <- changingDf %>%
  mutate(date = factor(paste(get("YEAR"), get("MONTH"), get("DAY"), sep = "-"))) %>%
  mutate(time = factor(paste(get("HOUR"), get("MIN"), get("SEC"), sep = ":"))) %>%
  mutate(long = factor(paste(get("Long")))) %>%
  mutate(lat = factor(paste(get("Lat")))) %>%
  mutate(mag = factor(paste(get("MW")))) %>%
  select(date, time, long, lat, mag)

changingDf <- as.data.frame(changingDf)
changingDf
# df <- data.frame(Doubles=double(),
#                  Ints=integer(),
#                  Factors=factor(),
#                  Logicals=logical(),
#                  Characters=character(),
#                  stringsAsFactors=FALSE)

write.table(changingDf, "changingDf.txt", sep = "\t", row.names = FALSE)
