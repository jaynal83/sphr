# Locating a directory where data and code files are stored

setwd("C:/Users/jayna/Documents/GitHub/Materials")

# Loading necessary libraries for data processing and analysis
library(readr)
library(dplyr)

# Importing a csv file into R working environment
dfSession4a <- read_csv(
  file = "Example_data_session_4.csv"
)

# Checking variable properties and first few data points
glimpse(dfSession4a)

# Defining appropriate measurement scale for sex variable
dfSession4a <- dfSession4a %>%
  mutate(
    sex = factor(
      x = sex,
      levels = c(1,2),
      labels = c("Male", "Female")
    )
  )

# Defining appropriate measurement scale for pain variable
dfSession4a <- dfSession4a %>%
  mutate(
    pain = factor(
      x = pain,
      levels = c(1,2, 3),
      labels = c("No Pain", "Mild Pain", "Severe Pain"),
      ordered = TRUE
    )
  )

# Defining appropriate measurement scale for painYn variable
dfSession4a <- dfSession4a %>%
  mutate(
    painYn = as.logical(x=painYn)
  )

# Exporting processed data into a csv file
write_csv(
  x = dfSession4a,
  file = "df_session4_processed_data.csv"
)
