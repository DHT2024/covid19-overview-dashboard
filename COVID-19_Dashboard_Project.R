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
