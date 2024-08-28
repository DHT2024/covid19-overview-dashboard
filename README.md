# TheVeryBest
The Very Best Team - COVID-19 Overview Dashboard Project Plan

1) covid19_summary
Choose countries (high spread, low spread, high mortality, low mortality); around 3 from each group, list has 12 total - Vian 
https://data.who.int/dashboards/covid19/data

a) Sorta EDA moment - total global cases and total global deaths (for the dashboard) 

b) Do we need to rename the names (ie. CÔøΩte d'Ivoire)?

c) Create a subset of COVID dataset to include necessary variables such as Name, Who.Region, Cases...cumulative.total.per.100000.population, and Deaths...cumulative.total.per.100000.population and rename the variables

d) Select three countries from the top 10 countries with the highest number of total cases per 100K population (high spread countries).

e) Select three other countries where number of total cases per 100K population is less than 10,000 (low spread countries).

f) Select three countries from the top 10 countries with the highest number of total deaths per 100K population (high mortality countries)

g) Select three other countries where number of total deaths per 100K population is less than 250 (low mortality countries)

Maybe display it as graphs of the 3 choices and their respective cumulative case/death score?

2) socioeconomic_indicators - Bilal, Trinley

a) Obtain primary and additional socioeconomic indicators from API
  - Specifically, look for the three groups of indicators: Population, Economic, Social
  - Population indicators: Total population, population density, urban population (as a percentage of total population), rural population (as a percentage of total population), crude birth/death rate per 1000 people, annual population growth (%)
  - Economic indicators: GDP, GNI, net income per capita, poverty headcount ratio
  - Social indicators: Life expectancy, GINI index, adult literacy rate (% of people ages 15 and above)



3) peak_pandemic - Tenzin

a) Calculate prevalence, case fatality rate, and mortality rate

4) statistical_analysis - Bilal, Tenzin

a) Thresholds for socioeconomic indicators

b) Single out one indicator that is the best indicators 

c) run some quick and dirty analysis on all your indicators to figure out which one looks the strongest

d) ANOVA

e) Regression and p value

f) Correlation  

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
