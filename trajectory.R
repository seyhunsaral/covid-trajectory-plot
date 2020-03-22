library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(scales)

# Cases data
# source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
# https://github.com/CSSEGISandData/COVID-19
cases  <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


# Population by Country
# source: datahub.io
# taking 2016 data
# https://datahub.io/JohnSnowLabs/population-figures-by-country
pops  <- read_csv("https://datahub.io/JohnSnowLabs/population-figures-by-country/r/population-figures-by-country-csv.csv")  %>% select(country = Country, population = Year_2016)  %>%
mutate(country = ifelse(country == "United States", "US", country)) #renaming us


cases  <- cases  %>%
  # Summing the province level data 
  select(-c("Province/State","Lat","Long"))  %>%  
  group_by(`Country/Region`)  %>%
  summarise_all(sum)  %>%
  rename(country = `Country/Region`)  %>%
  pivot_longer(-country, names_to = "date", values_to = "cases")  %>%
  mutate(date = mdy(date))  %>%
  filter(cases >= 100)

cases100  <- cases  %>% group_by(country)  %>% summarize( first_100th = min(date))

max_cases  <- cases  %>% group_by(country)  %>% summarize( max_case  = max(cases))  %>% filter(max_case >= 160)


df_final  <- cases  %>%
  filter(country %in% max_cases$country)  %>% 
  left_join(cases100, by = c("country"))  %>%
  mutate(days_since = date - first_100th)  %>%
  left_join(pops, by = c("country"))  %>%
  mutate(case_per_hundth = 1000000 *  cases / population)  %>%
  filter(population > 1000000)  %>%
  mutate(country_dm = country)

df_final  %>%
  ggplot() +
  geom_line(data = df_final[,-1], aes(y=case_per_hundth, x = days_since, fill = country_dm),color = "gray") +
  geom_line(aes(y=case_per_hundth, x = days_since), color = "red", size = 1.2) +
  guides(color=FALSE) +
  facet_wrap(~country) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
              labels = trans_format("log10", math_format(10^.x))) +
  scale_x_continuous(breaks = seq(0,60,10)) +
  xlab("100. vakanin gorulmesinden itibaren gecen gun sayisi")  +
  ylab("Vaka sayisi") +
  theme_bw()




    
