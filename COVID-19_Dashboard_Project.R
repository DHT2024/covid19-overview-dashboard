# Coding in R Language
# Final Exam
# Summer 2024
# The Very Best Team

# Load necessary packages.
library(tidyverse)
library(httr)
library(jsonlite)

# Import data.
COVID <- read.csv("~/TheVeryBest_CodingInR/WHO-COVID-19-global-table-data.csv")

# Explore data.
summary(COVID)
str(COVID)
names(COVID)

# The global stats for total cumulative cases and per 100k population and total cumulative deaths and per 100k population
COVID_global <- COVID %>%
  arrange(desc(Cases...cumulative.total)) %>% 
  head(1)

# Create a subset of COVID dataset to include necessary variables such as Name, Who.Region, Cases...cumulative.total.per.100000.population,
# and Deaths...cumulative.total.per.100000.population and rename the variables.
COVID_clean <- COVID %>% 
  select(Name, WHO.Region, 
         Cases.Cumulative.Per.100k = Cases...cumulative.total.per.100000.population, 
         Deaths.Cumulative.Per.100k = Deaths...cumulative.total.per.100000.population)

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

#Low Spread Countries 

# Select three other countries where number of total cases per 100K population is less than 10,000.
# Filter Cases.Cumulative.Per.100k with less than 10000 cases and arrange in descending order.
COVID_low_spread <- COVID_clean %>%
  filter(Cases.Cumulative.Per.100k < 10000) %>%
  arrange(desc(Cases.Cumulative.Per.100k))

# The countries selected to represent the countries with <10,000 cases per 100k population are 
# Cuba, Guyana, Iran.
COVID_low_spread <- COVID_low_spread %>%
  filter(Name %in% c("Cuba", "Guyana", "Iran (Islamic Republic of)"))

# Plot the countries with <10k total cases per 100k population.
ggplot(COVID_low_spread, aes(x = Name, y = Cases.Cumulative.Per.100k)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Low Spread Countries",
       x = "Country",
       y = "Cumulative Cases Per 100K Population") +
  ylim(0, 80000) +
  theme_minimal()

# High Mortality Countries 

# Select three countries from the top 10 countries with the highest number of total deaths per 100K population.
# Arrange the Deaths.Cumulative.Per.100k column from descending and isolate the top 10 countries. 
COVID_high_mortality <- COVID_clean %>%
  arrange(desc(Deaths.Cumulative.Per.100k)) %>% 
  head(11) # Filter for the top 11 entries because Global is also included in the top Deaths.Cumulative.Per.100k

# The countries selected to represent the countries with the highest number of total deaths per 100k population are 
# Peru, Hungary, and Bosnia and Herzegovina. 
COVID_high_mortality <- COVID_high_mortality %>%
  filter(Name %in% c("Peru", "Hungary", "Bosnia and Herzegovina"))

# Plot the countries with one of the highest number of total death per 100k population. 
ggplot(COVID_high_mortality, aes(x = Name, y = Deaths.Cumulative.Per.100k)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "High Mortality Countries",
       x = "Country",
       y = "Cumulative Deaths Per 100K Population") +
  ylim(0, 800) +
  theme_minimal()

# Low Mortality Countries 

# Select three other countries where number of total deaths per 100K population is less than 250. 
# Filter Deaths.Cumulative.Per.100k with less than 250 deaths and arrange in descending order.
COVID_low_mortality <- COVID_clean %>%
  filter(Deaths.Cumulative.Per.100k < 250) %>%
  arrange(desc(Deaths.Cumulative.Per.100k))

# The countries selected to represent the countries with <10,000 deaths per 100k population are 
# Guam, Bahamas, Germany
COVID_low_mortality <- COVID_low_mortality %>% 
  filter(Name %in% c("Guam", "Bahamas", "Germany"))

# Plot the countries with <10k total death per 100k population.
ggplot(COVID_low_mortality, aes(x = Name, y = Deaths.Cumulative.Per.100k)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Low Mortality Countries",
       x = "Country",
       y = "Cumulative Deaths Per 100K Population") +
  ylim(0, 800) +
  theme_minimal()

# Extract demographic and socioeconomic indicator from World Bank API
# Build query
base <- "https://api.worldbank.org/v2"
endpoint <- "/country/AUT;KOR;CYP;GUY;CUB;IRN;PER;HUN;BIH;BHS;DEU;GUM"
query <- "/indicator/SP.POP.TOTL;EN.POP.DNST;NY.GDP.MKTP.CD;NY.GNP.MKTP.CD;SP.DYN.LE00.IN;SI.POV.NAHC;SI.POV.GINI;SE.ADT.LITR.ZS;NY.ADJ.NNTY.PC.CD;SP.DYN.CBRT.IN;SP.DYN.CDRT.IN;SP.POP.GROW;SP.URB.TOTL.IN.ZS;SP.RUR.TOTL.ZS"
output_format <- "?format=json&per_page=5000&date=2020:2022&source=2"

# Combine all parts of the query to produce a call
call <- paste0(base, endpoint, query, output_format)
call

# Get info from call and check status
get_data <- GET(call_final)
get_data$status_code

# Extract content from the request
get_data_text <- content(get_data, "text")

# Convert JSON file into an R object
get_data_json <- fromJSON(get_data_text, flatten = T)
