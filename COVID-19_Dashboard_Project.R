# Coding in R Language
# Final Exam
# Summer 2024
# The Very Best Team

# Load necessary packages.
library(tidyverse)
library(httr)
library(jsonlite)

# Import data.
COVID <- read.csv("WHO-COVID-19-global-table-data.csv")

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
get_data <- GET(call)
get_data$status_code

# Extract content from the request
get_data_text <- content(get_data, "text")

# Convert JSON file into an R object
get_data_json <- fromJSON(get_data_text, flatten = T)

# Converted information to dataframe
socioecon_df <- get_data_json[2][[1]]
socioecon_df

# Renamed column names and select relevant columns
socioecon_df <- socioecon_df %>% rename(
  country_iso = countryiso3code,
  year = date,
  socioecon_id = indicator.id,
  socioecon_indicator = indicator.value,
  country = country.value
) %>% select(country, country_iso, year, socioecon_id, socioecon_indicator, value)

# Import covid data with dates
covid_dates <- read.csv("WHO-COVID-19-global-data.csv")

# Filtered covid data with dates by countries of interest.
covid_dates_countries <- covid_dates %>%
  filter(Country %in% c("Austria", "Bahamas", "Bosnia and Herzegovina", "Cuba", "Cyprus", "Germany", "Guam", "Guyana", "Hungary", "Iran (Islamic Republic of)", "Republic of Korea", "Peru"))

# Converted dates in covid data with dates to date format.
covid_dates_countries$Date_reported <- as.POSIXct(covid_dates_countries$Date_reported)
# Filtered observations between start of 2020 to end of 2022.
covid_dates_countries_dates <- covid_dates_countries %>%
  filter(between(Date_reported,as.Date("2020-01-01"),as.Date("2022-12-31")))

# Change names in API data frame to match covid data with dates.
socioecon_df$country[which(socioecon_df$country == "Bahamas, The")] <- "Bahamas"
socioecon_df$country[which(socioecon_df$country == "Iran, Islamic Rep.")] <- "Iran (Islamic Republic of)"
socioecon_df$country[which(socioecon_df$country == "Korea, Rep.")] <- "Republic of Korea"

# Filtered API data by year and population data, then alphabetizing.
country_population_2020 <- socioecon_df %>%
  filter(socioecon_id %in% c("SP.POP.TOTL")) %>%
  filter(year %in% c("2020")) %>%
  arrange(country)

country_population_2021 <- socioecon_df %>%
  filter(socioecon_id %in% c("SP.POP.TOTL")) %>%
  filter(year %in% c("2021")) %>% 
  arrange(country)

country_population_2022 <- socioecon_df %>%
  filter(socioecon_id %in% c("SP.POP.TOTL")) %>%
  filter(year %in% c("2022")) %>%
  arrange(country)

# Prevalence in 2020 = Confirmed Cases (2020) / Population (2020)
covid_dates_2020 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2020-01-01"),as.Date("2020-12-31"))) %>%
  group_by(Country) %>%
  summarize(cases_2020 = sum(New_cases, na.rm = T))

covid_prevalence_2020 <- covid_dates_2020
prevalence_2020 <- covid_dates_2020$cases_2020 / country_population_2020$value
covid_prevalence_2020$population_2020 <- country_population_2020$value
covid_prevalence_2020$prevalence_2020 <- prevalence_2020
covid_prevalence_2020$prevalence_2020_percent <- covid_prevalence_2020$prevalence_2020*100
View(covid_prevalence_2020)

# Prevalence in 2021 = Confirmed Cases (2021) / Population (2021)
covid_dates_2021 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2021-01-01"),as.Date("2021-12-31"))) %>%
  group_by(Country) %>%
  summarize(cases_2021 = sum(New_cases, na.rm = T))

covid_prevalence_2021 <- covid_dates_2021
prevalence_2021 <- covid_dates_2021$cases_2021 / country_population_2021$value
covid_prevalence_2021$population_2021 <- country_population_2021$value
covid_prevalence_2021$prevalence_2021 <- prevalence_2021
covid_prevalence_2021$prevalence_2021_percent <- covid_prevalence_2021$prevalence_2021*100
View(covid_prevalence_2021)

# Prevalence in 2022 = Confirmed Cases (2022) / Population (2022)
covid_dates_2022 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2022-01-01"),as.Date("2022-12-31"))) %>%
  group_by(Country) %>%
  summarize(cases_2022 = sum(New_cases, na.rm = T))

covid_prevalence_2022 <- covid_dates_2022
prevalence_2022 <- covid_dates_2022$cases_2022 / country_population_2022$value
covid_prevalence_2022$population_2022 <- country_population_2022$value
covid_prevalence_2022$prevalence_2022 <- prevalence_2022
covid_prevalence_2022$prevalence_2022_percent <- covid_prevalence_2022$prevalence_2022*100
View(covid_prevalence_2022)

# Average Prevalence over 2020-2022 = Avg()
average_prevalence <- as.data.frame(covid_prevalence_2020$Country)
average_prevalence$prevalence_2020 <- covid_prevalence_2020$prevalence_2020
average_prevalence$prevalence_2021 <- covid_prevalence_2021$prevalence_2021
average_prevalence$prevalence_2022 <- covid_prevalence_2022$prevalence_2022

average_prevalence <- average_prevalence %>%
  rename("Country" = "covid_prevalence_2020$Country") %>%
  mutate(avg_prevalence = (prevalence_2020 + prevalence_2021 + prevalence_2022)/3)
average_prevalence$avg_prevalence_percent <- average_prevalence$avg_prevalence*100

#### Case Fatality Rate ####
# CFR in 2020 = Confirmed Deaths (2020) / Confirmed Cases (2020)
covid_deaths_2020 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2020-01-01"),as.Date("2020-12-31"))) %>%
  group_by(Country) %>%
  summarize(cases_2020 = sum(New_cases, na.rm = T),
            deaths_2020 = sum(New_deaths, na.rm = T))

covid_cfr_2020 <- covid_deaths_2020
covid_cfr_2020$cfr_2020 <- covid_cfr_2020$deaths_2020 / covid_cfr_2020$cases_2020
covid_cfr_2020$cfr_2020_percent <- covid_cfr_2020$cfr_2020*100
View(covid_cfr_2020)

# CFR in 2021 = Confirmed Deaths (2021) / Confirmed Cases (2021)
covid_deaths_2021 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2021-01-01"),as.Date("2021-12-31"))) %>%
  group_by(Country) %>%
  summarize(cases_2021 = sum(New_cases, na.rm = T),
            deaths_2021 = sum(New_deaths, na.rm = T))

covid_cfr_2021 <- covid_deaths_2021
covid_cfr_2021$cfr_2021 <- covid_cfr_2021$deaths_2021 / covid_cfr_2021$cases_2021
covid_cfr_2021$cfr_2021_percent <- covid_cfr_2021$cfr_2021*100
View(covid_cfr_2021)

# CFR in 2022 = Confirmed Deaths (2022) / Confirmed Cases (2022)
covid_deaths_2022 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2022-01-01"),as.Date("2022-12-31"))) %>%
  group_by(Country) %>%
  summarize(cases_2022 = sum(New_cases, na.rm = T),
            deaths_2022 = sum(New_deaths, na.rm = T))

covid_cfr_2022 <- covid_deaths_2022
covid_cfr_2022$cfr_2022 <- covid_cfr_2022$deaths_2022 / covid_cfr_2022$cases_2022
covid_cfr_2022$cfr_2022_percent <- covid_cfr_2022$cfr_2022*100
View(covid_cfr_2022)

# Average CFR over 2020-2022 = Avg()
average_cfr <- as.data.frame(covid_cfr_2020$Country)
average_cfr$cfr_2020 <- covid_cfr_2020$cfr_2020
average_cfr$cfr_2021 <- covid_cfr_2021$cfr_2021
average_cfr$cfr_2022 <- covid_cfr_2022$cfr_2022

average_cfr <- average_cfr %>%
  rename("Country" = "covid_cfr_2020$Country") %>%
  mutate(avg_cfr = (cfr_2020 + cfr_2021 + cfr_2022)/3)
average_cfr$avg_cfr_percent <- average_cfr$avg_cfr*100

#### Mortality Rate ####
# MR in 2020 = Confirmed Deaths (2020) / Population (2020)
covid_mortality_2020 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2020-01-01"),as.Date("2020-12-31"))) %>%
  group_by(Country) %>%
  summarize(deaths_2020 = sum(New_deaths, na.rm = T))

covid_mr_2020 <- covid_mortality_2020
mr_2020 <- covid_mr_2020$deaths_2020 / country_population_2020$value
covid_mr_2020$population_2020 <- country_population_2020$value
covid_mr_2020$mr_2020 <- mr_2020
covid_mr_2020$mr_2020_percent <- covid_mr_2020$mr_2020*100
View(covid_mr_2020)

# MR in 2021 = Confirmed Deaths (2021) / Population (2021)
covid_mortality_2021 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2021-01-01"),as.Date("2021-12-31"))) %>%
  group_by(Country) %>%
  summarize(deaths_2021 = sum(New_deaths, na.rm = T))

covid_mr_2021 <- covid_mortality_2021
mr_2021 <- covid_mr_2021$deaths_2021 / country_population_2021$value
covid_mr_2021$population_2021 <- country_population_2021$value
covid_mr_2021$mr_2021 <- mr_2021
covid_mr_2021$mr_2021_percent <- covid_mr_2021$mr_2021*100
View(covid_mr_2021)

# MR in 2022 = Confirmed Deaths (2022) / Population (2022)
covid_mortality_2022 <- covid_dates_countries_dates %>%
  filter(between(Date_reported,as.Date("2022-01-01"),as.Date("2022-12-31"))) %>%
  group_by(Country) %>%
  summarize(deaths_2022 = sum(New_deaths, na.rm = T))

covid_mr_2022 <- covid_mortality_2022
mr_2022 <- covid_mr_2022$deaths_2022 / country_population_2022$value
covid_mr_2022$population_2022 <- country_population_2022$value
covid_mr_2022$mr_2022 <- mr_2022
covid_mr_2022$mr_2022_percent <- covid_mr_2022$mr_2022*100
View(covid_mr_2022)

# Average MR over 2020-2022 = Avg()
average_mr <- as.data.frame(covid_mr_2020$Country)
average_mr$mr_2020 <- covid_mr_2020$mr_2020
average_mr$mr_2021 <- covid_mr_2021$mr_2021
average_mr$mr_2022 <- covid_mr_2022$mr_2022

average_mr <- average_mr %>%
  rename("Country" = "covid_mr_2020$Country") %>%
  mutate(avg_mr = (mr_2020 + mr_2021 + mr_2022)/3)
average_mr$avg_mr_percent <- average_mr$avg_mr*100

#### Statistical Analysis ####

socioecon_df_v2 <- socioecon_df %>%
  group_by(country, socioecon_id) %>%
  summarize(avg_values = mean(value, na.rm = T))

# Total Population for each country
# Adding thresholds and converting grade to factor
socioecon_df_pop <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "SP.POP.TOTL"),]
socioecon_df_pop <- socioecon_df_pop %>%
  mutate(pop_grade = cut(avg_values, breaks = c(0, 1e6, 10e6, 25e6, 50e6, Inf), 
                         labels = c("Very Small", "Small", "Medium", "Large", "Very Large")))
socioecon_df_pop$pop_grade <- factor(socioecon_df_pop$pop_grade, 
                                     levels = c("Very Small", "Small", "Medium", "Large", "Very Large"))

# Population Density for each country
# Adding thresholds and converting grade to factor
# socioecon_df_pop <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "EN.POP.DNST"),]
# socioecon_df_pop <- socioecon_df_pop %>%
  # mutate(pop_grade = cut(avg_values, breaks = c(0, 1e6, 10e6, 25e6, 50e6, Inf), 
                         # labels = c("Very Small", "Small", "Medium", "Large", "Very Large")))
# socioecon_df_pop$pop_grade <- factor(socioecon_df_pop$pop_grade, 
                                    # levels = c("Very Small", "Small", "Medium", "Large", "Very Large"))
