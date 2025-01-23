# COVID-19 Overview Dashboard Project Plan


1) covid19_summary - Vian 

a) EDA - total global cases and total global deaths (https://data.who.int/dashboards/covid19/data)

b) Create a subset of COVID dataset to include necessary variables such as Name, Who.Region, Cases...cumulative.total.per.100000.population, and Deaths...cumulative.total.per.100000.population and rename the variables

c) Select three countries from the top 10 countries with the highest number of total cases per 100K population (high spread countries): Cyprus, Austria, and Republic of Korea

d) Select three other countries where number of total cases per 100K population is less than 10,000 (low spread countries): Cuba, Guyana, and Iran

e) Select three countries from the top 10 countries with the highest number of total deaths per 100K population (high mortality countries): Peru, Hungary, and Bosnia and Herzegovina

f) Select three other countries where number of total deaths per 100K population is less than 250 (low mortality countries): Guam, Bahamas, and Germany


2) socioeconomic_indicators - Bilal, Trinley

a) Obtain primary and additional socioeconomic indicators from API
  - Specifically, look for the three groups of indicators: Population, Economic, Social
  - Population indicators: Total population, population density, urban population (as a percentage of total population), rural population (as a percentage of total population), crude birth/death rate per 1000 people, annual population growth (%)
  - Economic indicators: GDP, GNI, net income per capita, poverty headcount ratio
  - Social indicators: Life expectancy, GINI index, adult literacy rate (% of people ages 15 and above)

b) Filtered data by countries of interest within the 2020-2022 timeframe


3) peak_pandemic - Tenzin

a) Calculate prevalence: confirmed cases / total population

b) Case fatality rate (CFR): confirmed deaths / confirmed cases

c) Mortality rate (MR): confirmed deaths / total population
  - Prevalence, CFR, and MR all calculated indivudally for years 2020-2022, as well as an average value calculated over the 3-year period


4) statistical_analysis - Bilal, Tenzin

a) Thresholds for socioeconomic indicators
 - Extract socioeconomic indicator data from API for all countries to aid with setting thresholds
 - Determine quantile distributions of global data for each indicator to set thresholds, convert the grades to factors
   
b) Create dataframe for final analysis with average CFR and all socioeconomic indicator grades for all countries of interest

c) Run linear regression to identify strongest predictor
 - Intially test with categorical data; can use continuous data if this fails
   
d) Run other analyses if necessary (e.g. correlation analysis)

e) Use ANOVA to compare strongest predictors and determine significance


5) dashboard_creation - Vian, Trinley
   
a) COVID-19 summary

b) Country Indicators: 
  - Create three separate sections for each group of indicators by using value boxes to indicate which group the displayed data is a part of
  - Create stacked bar plots for each country displaying the measurements for each of the indicators for each year with plotly
  - The plots should be interactive such that the user can obtain more information by hovering above each bar
  - For each group of indicators, create a tabset display to include all stacked bar plots for the specific appropriate indicators
  - State where the data is from
    
c) COVID-19 Analysis: Key Metrics
  - Create a value box at the top that talks about what this page displays, the general trend seen in the graphs, and how to interact with the plots
  - Create interactive line graphs displaying the calculated COVID-19 prevalence, case fatality, and mortality rates throughout 2020-2022 with plotly
  - Create a tabset to display the three graphs such that they each take up a single tab
  - Create an interactive table that displays the average prevalence, case fatality, and mortality rates for all 12 countries. 
  - Create a tabset to display these tables such that each table takes up a single tab
  - State where the data is from
  
d) Statistical Analysis/Correlation
 - Create plots to show the relationship between strongest indicators and CFR
 - Include linear regression model/table, correlation plot, ANOVA table, and box plots
