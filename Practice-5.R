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

# Defining appropriate measurement scale for "sex" 
# "pain" and "marital"
dfSession4a <- dfSession4a %>%
  mutate(
    sexNominal = factor(
      x = sex,
      levels = c(1,2),
      labels = c("Male", "Female")
    ),
    painOrdinal = factor(
      x = pain,
      levels = c(1,2, 3),
      labels = c("No Pain", "Mild Pain", "Severe Pain"),
      ordered = TRUE
    ),
    maritalNominal = factor(
      x = marital,
      levels = c(1,2,3,4),
      labels =  c("Married", "Divorced", "Widow(er)", "Never Married")
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

# Checking if we can perform mathematical operation on nominal and
# ordinal variables. To do so we will use mean() function

# mean() function on original "sex" variable before convert into nominal scale
mean(dfSession4a$sex)

# mean() function or converted sex variable "sexNominal"
mean(dfSession4a$sexNominal)

# mean() function on a logical variable, this is equivalent to
# calculating proportion of "yes" of a binary variable
mean(dfSession4a$painYn)

# Creating a table of descriptive statistics

library(table1)

table1(~sex+sexNominal+age+pain+painOrdinal+painYn+maritalNominal, 
       data = dfSession4a)

# Create separate column for each clusters
table1(~sex+sexNominal+age+pain+painOrdinal+painYn+maritalNominal
       | clusterID, 
       data = dfSession4a)

# Adding a meaningful label for a variable
label(dfSession4a$sexNominal) <- "Sex of the participants"

# Separate male and female within each clusters
table1(~sex+sexNominal+age+pain+painOrdinal+painYn+maritalNominal
       | clusterID + sexNominal, 
       data = dfSession4a)

# the line above will produce same output as of the line below
table1(~sex+sexNominal+age+pain+painOrdinal+painYn+maritalNominal
       | clusterID * sexNominal, 
       data = dfSession4a)

# If we don't need overall column, then overall=FALSE as below
table1(~sex+sexNominal+age+pain+painOrdinal+painYn+maritalNominal
       | clusterID * sexNominal, 
       data = dfSession4a,
       overall = FALSE)
