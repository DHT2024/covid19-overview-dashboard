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

#' Extract demographic/socioeconomic indicator from World Bank API to create
#' a dataframe with data for all countries to aid with setting thresholds.
#' Build query for 2 API calls, as calling API for all countries and all
#' desired indicators is too large of a call. Two dataframes required  
#' each with ~1/2 of desired indicators for all countries
endpoint_all <- "/country"
query_all_v2 <- "/indicator/SP.DYN.CBRT.IN;SP.DYN.CDRT.IN;SP.POP.GROW;SP.URB.TOTL.IN.ZS;SP.RUR.TOTL.ZS"
# Combine all parts of the query to produce a call
call_all <- paste0(base, endpoint_all, query, output_format)
call_all

call_all_v2 <- paste0(base, endpoint_all, query_all_v2, output_format)
call_all_v2
# Get info from call and check status
get_data_all <- GET(call_all)
get_data_all$status_code

get_data_all_v2 <- GET(call_all_v2)
get_data_all_v2$status_code
# Extract content from the request
get_data_text_all <- content(get_data_all, "text")

get_data_text_all_v2 <- content(get_data_all_v2, "text")
# Convert JSON file into an R object
get_data_json_all <- fromJSON(get_data_text_all, flatten = T)

get_data_json_all_v2 <- fromJSON(get_data_text_all_v2, flatten = T)
# Converted information to dataframe
socioecon_df_all <- get_data_json_all[2][[1]]
socioecon_df_all

socioecon_df_all_v2 <- get_data_json_all_v2[2][[1]]
socioecon_df_all_v2
# Renamed column names and select relevant columns
socioecon_df_all <- socioecon_df_all %>% rename(
  country_iso = countryiso3code,
  year = date,
  socioecon_id = indicator.id,
  socioecon_indicator = indicator.value,
  country = country.value
) %>% select(country, country_iso, year, socioecon_id, socioecon_indicator, value)

socioecon_df_all_v2 <- socioecon_df_all_v2 %>% rename(
  country_iso = countryiso3code,
  year = date,
  socioecon_id = indicator.id,
  socioecon_indicator = indicator.value,
  country = country.value
) %>% select(country, country_iso, year, socioecon_id, socioecon_indicator, value)

# Total Population for each country
socioecon_df_pop <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "SP.POP.TOTL"),]

# Checking for distribution of global population data
pop_indices <- which(socioecon_df_all$socioecon_id == "SP.POP.TOTL")
quantile(socioecon_df_all$value[pop_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_pop <- socioecon_df_pop %>%
  mutate(pop_grade = cut(avg_values, breaks = c(0, 724273, 5685807, 19603733, 125998302, Inf), 
                         labels = c("Very Small", "Small", "Medium", "Large", "Very Large")))
socioecon_df_pop$pop_grade <- factor(socioecon_df_pop$pop_grade, 
                                     levels = c("Very Small", "Small", "Medium", "Large", "Very Large"))

# Population Density for each country
socioecon_df_pop_den <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "EN.POP.DNST"),]

# Checking for distribution of global population density data
pop_den_indices <- which(socioecon_df_all$socioecon_id == "EN.POP.DNST")
quantile(socioecon_df_all$value[pop_den_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_pop_den <- socioecon_df_pop_den %>%
  mutate(pop_den_grade = cut(avg_values, breaks = c(0, 30.53911, 60.19171, 107.56739, 241.18333, Inf), 
                         labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_pop_den$pop_den_grade <- factor(socioecon_df_pop_den$pop_den_grade, 
                              levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# GDP for each country
socioecon_df_gdp <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "NY.GDP.MKTP.CD"),]

# Checking for distribution of global GDP data
gdp_indices <- which(socioecon_df_all$socioecon_id == "NY.GDP.MKTP.CD")
quantile(socioecon_df_all$value[gdp_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_gdp <- socioecon_df_gdp %>%
  mutate(gdp_grade = cut(avg_values, breaks = c(0, 6.887147e+09, 2.611333e+10, 1.648734e+11, 1.414560e+12, Inf), 
                             labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_gdp$gdp_grade <- factor(socioecon_df_gdp$gdp_grade, 
                                             levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# GNI for each country
socioecon_df_gni <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "NY.GNP.MKTP.CD"),]

# Checking for distribution of global GNI data
gni_indices <- which(socioecon_df_all$socioecon_id == "NY.GNP.MKTP.CD")
quantile(socioecon_df_all$value[gni_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_gni <- socioecon_df_gni %>%
  mutate(gni_grade = cut(avg_values, breaks = c(0, 8.209395e+09, 2.896157e+10, 1.939661e+11, 1.429618e+12, Inf), 
                         labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_gni$gni_grade <- factor(socioecon_df_gni$gni_grade, 
                                     levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Life expectancy for each country
socioecon_df_le <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "SP.DYN.LE00.IN"),]

# Checking for distribution of global life expectancy data
le_indices <- which(socioecon_df_all$socioecon_id == "SP.DYN.LE00.IN")
quantile(socioecon_df_all$value[le_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_le <- socioecon_df_le %>%
  mutate(le_grade = cut(avg_values, breaks = c(0, 64.48500, 70.98600, 74.25366, 78.94400, Inf), 
                         labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_le$le_grade <- factor(socioecon_df_le$le_grade, 
                                     levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Birth rate (per 1000) for each country
socioecon_df_br <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "SP.DYN.CBRT.IN"),]

# Checking for distribution of global birth rate data
br_indices <- which(socioecon_df_all_v2$socioecon_id == "SP.DYN.CBRT.IN")
quantile(socioecon_df_all_v2$value[br_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_br <- socioecon_df_br %>%
  mutate(br_grade = cut(avg_values, breaks = c(0, 10.000, 13.099, 18.791, 28.706, Inf), 
                        labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_br$br_grade <- factor(socioecon_df_br$br_grade, 
                                   levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Death rate (per 1000) for each country
socioecon_df_dr <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "SP.DYN.CDRT.IN"),]

# Checking for distribution of global death rate data
dr_indices <- which(socioecon_df_all_v2$socioecon_id == "SP.DYN.CDRT.IN")
quantile(socioecon_df_all_v2$value[dr_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_dr <- socioecon_df_dr %>%
  mutate(dr_grade = cut(avg_values, breaks = c(0, 6.253, 7.448, 8.661, 10.200, Inf), 
                        labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_dr$dr_grade <- factor(socioecon_df_dr$dr_grade, 
                                   levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Population growth % for each country
socioecon_df_pgp <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "SP.POP.GROW"),]

# Checking for distribution of global population growth % data
pgp_indices <- which(socioecon_df_all_v2$socioecon_id == "SP.POP.GROW")
quantile(socioecon_df_all_v2$value[pgp_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_pgp <- socioecon_df_pgp %>%
  mutate(pgp_grade = cut(avg_values, breaks = c(-999, 0.08169316, 0.69471769, 1.23729900, 2.17409124, Inf), 
                        labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_pgp$pgp_grade <- factor(socioecon_df_pgp$pgp_grade, 
                                   levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Urban population % for each country
socioecon_df_upp <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "SP.URB.TOTL.IN.ZS"),]

# Checking for distribution of global urban population % data
upp_indices <- which(socioecon_df_all_v2$socioecon_id == "SP.URB.TOTL.IN.ZS")
quantile(socioecon_df_all_v2$value[upp_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_upp <- socioecon_df_upp %>%
  mutate(upp_grade = cut(avg_values, breaks = c(0, 38.54600, 55.11800, 67.84700, 81.85058, Inf), 
                         labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_upp$upp_grade <- factor(socioecon_df_upp$upp_grade, 
                                     levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Rural population % for each country
socioecon_df_rpp <- socioecon_df_v2[which(socioecon_df_v2$socioecon_id == "SP.RUR.TOTL.ZS"),]

# Checking for distribution of global rural population % data
rpp_indices <- which(socioecon_df_all_v2$socioecon_id == "SP.RUR.TOTL.ZS")
quantile(socioecon_df_all_v2$value[rpp_indices], prob = c(0.2, 0.4, 0.6, 0.8), type = 1, na.rm=T)

# Adding thresholds and converting grade to factor
socioecon_df_rpp <- socioecon_df_rpp %>%
  mutate(rpp_grade = cut(avg_values, breaks = c(0, 18.14942, 32.15300, 44.88200, 61.45400, Inf), 
                         labels = c("Very Low", "Low", "Medium", "High", "Very High")))
socioecon_df_rpp$rpp_grade <- factor(socioecon_df_rpp$rpp_grade, 
                                     levels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Create final data frame with average CFR and socioeconomic grades by country
names(average_cfr)[1] <- "country"
final_analysis_df <- average_cfr
final_analysis_df <- average_cfr[,c(1,5)]
final_analysis_df$pop_grade <- socioecon_df_pop$pop_grade
final_analysis_df$pop_den_grade <- socioecon_df_pop_den$pop_den_grade
final_analysis_df$gdp_grade <- socioecon_df_gdp$gdp_grade
final_analysis_df$gni_grade <- socioecon_df_gni$gni_grade
final_analysis_df$le_grade <- socioecon_df_le$le_grade
final_analysis_df$br_grade <- socioecon_df_br$br_grade
final_analysis_df$dr_grade <- socioecon_df_dr$dr_grade
final_analysis_df$pgp_grade <- socioecon_df_pgp$pgp_grade
final_analysis_df$upp_grade <- socioecon_df_upp$upp_grade
final_analysis_df$rpp_grade <- socioecon_df_rpp$rpp_grade

# Attempting linear regression to find strongest predictor
model <- lm(avg_cfr ~ pop_grade + pop_den_grade + gdp_grade + 
              le_grade + br_grade + dr_grade + pgp_grade + 
              upp_grade + rpp_grade, data=final_analysis_df)
summary(model)
anova(model)
#' Low sample size, leading to high multicollinearity, low variation 
#' within groups and no residual degrees of freedom. Other analysis 
#' required to find strongest predictor.

#' Try linear regression using average CFR as the response and 
#' socioeconomic indicators as continuous values, not grades. Create a new
#' data frame for this.
final_analysis_df_cont <- average_cfr
final_analysis_df_cont <- average_cfr[,c(1,5)]
final_analysis_df_cont$pop_values <- socioecon_df_pop$avg_values
final_analysis_df_cont$pop_den_values <- socioecon_df_pop_den$avg_values
final_analysis_df_cont$gdp_values <- socioecon_df_gdp$avg_values
final_analysis_df_cont$gni_values <- socioecon_df_gni$avg_values
final_analysis_df_cont$le_values <- socioecon_df_le$avg_values
final_analysis_df_cont$br_values <- socioecon_df_br$avg_values
final_analysis_df_cont$dr_values <- socioecon_df_dr$avg_values
final_analysis_df_cont$pgp_values <- socioecon_df_pgp$avg_values
final_analysis_df_cont$upp_values <- socioecon_df_upp$avg_values
final_analysis_df_cont$rpp_values <- socioecon_df_rpp$avg_values

model2 <- lm(avg_cfr ~ pop_values + pop_den_values + gdp_values +
               gni_values + le_values + br_values + dr_values +
               pgp_values + upp_values + rpp_values, data = final_analysis_df_cont)
plot(final_analysis_df_cont$avg_cfr,fitted(model2))
#' Unable to plot due to NAs in GNI values. Remove GNI indicator from model 
#' to correct this, as opposed to removing countries with NA GNI values.
model3 <- lm(avg_cfr ~ pop_values + pop_den_values + gdp_values +
               le_values + br_values + dr_values +
               pgp_values + upp_values + rpp_values, data = final_analysis_df_cont)
#' Plotting the actual values of average CFR vs the fitted values from 
#' the model
plot(final_analysis_df_cont$avg_cfr,fitted(model3))
# Plotting the above with ggplot and line of perfect fit (y=x)
ggplot(final_analysis_df_cont, aes(x=avg_cfr, y=fitted(model3))) +
  geom_point() + xlab("Average CFR") + ylab("Fitted Average CFR Values") +
  geom_abline(slope=1)
summary(model3)
anova(model3)
#' PGP has the largest coefficient magnitude, though significance unable 
#' to be calculated. Another analysis required.

#' Run correlation analysis using continuous data, after removing country 
#' name variable.
final_analysis_df_cont <- final_analysis_df_cont[,-c(1)]

install.packages("corrplot")
library(corrplot)
correlation_object <- cor(final_analysis_df_cont)
corrplot(correlation_object, method = "number", tl.cex=0.5, 
         number.cex = 0.5, type = "upper")
#' Life expectancy has the largest magnitude correlation with average CFR,
#' with population density as a close second.

#' Run ANOVA on the strongest predictors to determine significance.
anova_pop_den <- aov(avg_cfr ~ pop_grade, data = final_analysis_df)
summary(anova_pop_den)

anova_le <- aov(avg_cfr ~ le_grade, data = final_analysis_df)
summary(anova_le)
# Life expectancy has the lower p-value, though not significant.

#' Graph the relationship between average CFR and life expectancy, 
#' strongest predictor.
install.packages("ggpubr")
install.packages("ggplot2")
library(ggpubr)
library(ggplot2)
boxplot_le <- ggboxplot(final_analysis_df, x = "le_grade", y = "avg_cfr", 
                     color = "le_grade", palette = c("#00AFBB", "#E7B800", "#FC4E07", "lightgreen", "violet"),
                     ylab = "Average CFR", xlab = "Life Expectancy Group", show.legend = FALSE)
boxplot_le <- boxplot_le + theme(legend.position = "none")
boxplot_le

#' Graph the relationship between average CFR and population density, 
#' second strongest predictor.
boxplot_pop_den <- ggboxplot(final_analysis_df, x = "pop_den_grade", y = "avg_cfr", 
                        color = "pop_den_grade", palette = c("#00AFBB", "#E7B800", "#FC4E07", "lightgreen", "violet"),
                        ylab = "Average CFR", xlab = "Population Density Group", show.legend = FALSE)
boxplot_pop_den <- boxplot_pop_den + theme(legend.position = "none")
boxplot_pop_den