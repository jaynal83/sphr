# Locating a directory where data and code files are stored 
#(comment the following line and put your computer's location)

#setwd("C:/Users/jayna/Documents/GitHub/Materials")

# Loading necessary libraries for data processing and analysis
library(readr)
library(dplyr)

# Importing a csv file into R working environment
dfSession4a <- read_csv(
  file = "Data-1.csv"
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

# We want to report Mean and SD as Mean (SD) for ratio scale variables
table1(~sexNominal+age+painOrdinal+painYn+maritalNominal
       | clusterID, 
       render.continuous = c(.="Mean (SD)"),
       data = dfSession4a,
       overall = FALSE)

# We want to report Mean and SD as Median for ratio scale variables
table1(~sexNominal+age+painOrdinal+painYn+maritalNominal
       | clusterID, 
       render.continuous = c(.="Median"),
       data = dfSession4a,
       overall = FALSE)


# Instead of Mean (SD), we want to display Median and IQR 
table1(~sexNominal+age+painOrdinal+painYn+maritalNominal
       | clusterID, 
       render.continuous = c(.="Median (Q1-Q3)"),
       data = dfSession4a,
       overall = FALSE)

table1(~sexNominal+age+painOrdinal+painYn+maritalNominal
       | clusterID, 
       render.continuous = c(.="Median (IQR)"),
       data = dfSession4a,
       overall = FALSE)


# We want to calculate proportion of person who has pain 

dfProp <- dfSession4a %>%
  summarise(
    propPain = mean(painYn)
  )

# We want to calculate proportion of male who has pain 

dfProp <- dfSession4a %>%
  filter(
    sexNominal=="Male"
  ) %>%
  summarise(
    propPain = mean(painYn)
  )

# We want to calculate proportion of Female who has pain 
# Write down the code in line 133

dfProp <- dfSession4a %>%
  filter(
    sexNominal=="Female"
  ) %>%
  summarise(
    propPain = mean(painYn)
  )

# We want to calculate proportion for male and female 
# both at the same time

dfProp <- dfSession4a %>%
  group_by(
    sexNominal
  ) %>%
  summarise(
    propPain = mean(painYn)
  )

# We want to know whether the proportion of male or female
# are similar across clusters

dfProp <- dfSession4a %>%
  group_by(
    clusterID, sexNominal
  ) %>%
  summarise(
    propPain = mean(painYn)
  )
# What is the best way to present this findings?

# Now we want to compute confidence interval of the proportion 
# for both male and female 

dfProp <- lm(painYn~sexNominal, data = dfSession4a)

library(sjPlot)
tab_model(dfProp)

# Sampling Distribution
# Let's consider the data we have is the entire population
# We want to know the sampling distribution of average age, minimum age, maximum age
# median age, standard deviation of age
#
# We also want to know the sampling distribution of proportion of pain

library(purrr)
library(ggplot2)

# Population mean age = 27.67
# Population sd of age =9.73  

# Sampling distribution of mean age
rep(x = 50, times = 500) %>% 
  map_dfr(
    function(x){
      dfSession4a %>%
        sample_n(
          size = x
        ) %>%
        summarise(
          mean_age = mean(age)
        ) 
    }
  ) %>%
  ggplot()+
  geom_histogram(
    aes(
      x=mean_age
    )
  )

# Sampling distribution of median age
rep(x = 10, times = 1000) %>% 
  map_dfr(
    function(x){
      dfSession4a %>%
        sample_n(
          size = x
        ) %>%
        summarise(
          median_age = median(age)
        ) 
    }
  ) %>%
  ggplot()+
  geom_histogram(
    aes(
      x=median_age
    )
  )

# Sampling distribution of maximum age
rep(x = 10, times = 10000) %>% 
  map_dfr(
    function(x){
      dfSession4a %>%
        sample_n(
          size = x
        ) %>%
        summarise(
          max_age = max(age)
        ) 
    }
  ) %>%
  ggplot()+
  geom_histogram(
    aes(
      x=max_age
    )
  )

# Sampling distribution of Minimum age
rep(x = 10, times = 1000) %>% 
  map_dfr(
    function(x){
      dfSession4a %>%
        sample_n(
          size = x
        ) %>%
        summarise(
          min_age = min(age)
        ) 
    }
  ) %>%
  ggplot()+
  geom_histogram(
    aes(
      x=min_age
    )
  )

# Sampling distribution of Standard deviation of age
rep(x = 10, times = 10000) %>% 
  map_dfr(
    function(x){
      dfSession4a %>%
        sample_n(
          size = x
        ) %>%
        summarise(
          sd_age = sd(age)
        ) 
    }
  ) %>%
  ggplot()+
  geom_histogram(
    aes(
      x=sd_age
    )
  )

# Sampling distribution of proportion (painYn)
rep(x = 100, times = 10000) %>% 
  map_dfr(
    function(x){
      dfSession4a %>%
        sample_n(
          size = x
        ) %>%
        summarise(
          porpPain = mean(painYn)
        ) 
    }
  ) %>%
  ggplot()+
  geom_histogram(
    aes(
      x=porpPain
    )
  )

# Sampling distribution of Mean age by sex
rep(x = 10, times = 1000) %>% 
  map_dfr(
    function(x){
      dfSession4a %>%
        group_by(
          sexNominal
        ) %>%
        sample_n(
          size = x
        ) %>%
        summarise(
          mean_age = mean(age)
        ) 
    }
  ) %>%
  ggplot()+
  geom_histogram(
    aes(
      x=mean_age
    )
  ) +
  facet_wrap(
    ~sexNominal, ncol=2
  )


# Regression Modeling 

dfProp <- dfSession4a %>%
  group_by(
    sexNominal
  ) %>%
  summarise(
    propPain = mean(painYn)
  )


# 1. What is the difference between proportion of pain (risk difference) between male and female

mdl_RD <- glm(
  painYn ~ sexNominal, 
  family = "gaussian",
  data = dfSession4a
)


# to see model output summary
summary(mdl_RD)

# to get confidence interval
confint(mdl_RD)

# To adjust by an external variable e.g. age
mdl_RD_adj <- glm(
  painYn~sexNominal+age,
  family = "gaussian",
  data = dfSession4a
)

summary(mdl_RD_adj)
confint(mdl_RD_adj)

# To get formatted tabular output of a regression model
library(sjPlot)

tab_model(mdl_RD)

# 2. What is the ratio between proportion of pain (risk ratio) between male and female

mdl_RR <- glm(
  painYn~sexNominal,
  family = "poisson",
  data = dfSession4a
)

summary(mdl_RR)
exp(coef(mdl_RR))
exp(confint(mdl_RR))

# Odds ratio
mdl_OR <- glm(
  painYn~sexNominal,
  family = "binomial",
  data = dfSession4a
)

summary(mdl_OR)
exp(coef(mdl_OR))
