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

# The global stats for total cumulative cases per 100k population and total cumulative deaths per 100k population
COVID_global <- COVID_clean %>%
  arrange(desc(Cases.Cumulative.Per.100k)) %>% 
  head(1)

# High Spread Countries

# Select three countries from the top 10 countries with the highest number of total cases per 100K population.
# Arrange the Cases.Cumulative.Per.100k column from descending and isolate the top 10 countries. 
COVID_high_spread <- COVID_clean %>%
  arrange(desc(Cases.Cumulative.Per.100k)) %>% 
  head(11) # Filter for the top 11 entries because Global is also included in the top Cases.Cumulative.Per.100k.

# The countries selected to represent the countries with the highest number of total cases per 100k population are 
# Cyprus, Austria and Republic of Korea.
COVID_high_spread <- COVID_high_spread %>%
  filter(Name %in% c("Cyprus", "Austria", "Republic of Korea"))

# Plot the countries with one of the highest number of total cases per 100k population. 
ggplot(COVID_high_spread, aes(x = Name, y = Cases.Cumulative.Per.100k)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "High Spread Countries",
       x = "Country",
       y = "Cumulative Cases Per 100K Population") +
  ylim(0, 80000) +
  theme_minimal()
