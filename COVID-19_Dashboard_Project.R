# Coding in R Language
# Final Exam
# Summer 2024
# The Very Best Team

# Load necessary packages.
library(tidyverse)

# Import data.
COVID <- read.csv("~/TheVeryBest_CodingInR/WHO-COVID-19-global-table-data.csv")

# Explore data.
summary(COVID)
str(COVID)
names(COVID)

# Create a subset of COVID dataset to include necessary variables such as Name, Who.Region, Cases...cumulative.total.per.100000.population,
# and Deaths...cumulative.total.per.100000.population and rename the variables.
COVID_clean <- COVID %>% 
  select(Name, WHO.Region, 
         Cases.Cumulative.Per.100k = Cases...cumulative.total.per.100000.population, 
         Deaths.Cumulative.Per.100k = Deaths...cumulative.total.per.100000.population)
