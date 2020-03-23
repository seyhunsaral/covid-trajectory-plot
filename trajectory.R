library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
#library(scales)


# Cases data
# source: 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE
# https://github.com/CSSEGISandData/COVID-19
df  <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")



df  <- df  %>%
  # Summing up province data
  select(-c("Province/State","Lat","Long"))  %>%  
  group_by(`Country/Region`)  %>%
  summarise_all(sum)  %>%
  rename(country = `Country/Region`)  %>%
  pivot_longer(-country, names_to = "date", values_to = "cases")  %>%
  mutate(date = mdy(date))  

cases100th  <- df  %>% filter(cases >= 100)  %>% group_by(country)  %>% summarize( first_100th = min(date))

countries_high <- df  %>% group_by(country)  %>% summarize( max_case  = max(cases))  %>% filter(max_case >= 300) 



df_plot1  <- df  %>%
  filter(cases >=100)  %>%
  filter(country %in% countries_high$country)  %>% 
  filter(!country %in% c("Cruise Ship"))  %>% # removing "cruise ship"
  left_join(cases100th
          , by = c("country"))  %>%
  mutate(days_since = date - first_100th)  %>%
  mutate(country_dm = country) 


df_plot1  %>%
  ggplot(aes(y=cases, x = days_since, group = country_dm)) +
  geom_line(data = df_plot1[,-1], color = "gray") +
  geom_line(color = "#BB0000", size = 1.2) +
  guides(color=FALSE) +
  facet_wrap(~country) +
  scale_y_log10(breaks= c(100,1000,10000,50000)) +
  scale_x_continuous(breaks = seq(0,60,20)) +
  xlab("Number of days since the 100th case")  +
  ylab("Number of cases") +
  theme_bw()

  ggsave("plot1.png", height = 8, width = 16) 



### Population normalized

# Data: Population by Country
# source: datahub.io
# taking 2016 data
# https://datahub.io/JohnSnowLabs/population-figures-by-country
pops  <- read_csv("https://datahub.io/JohnSnowLabs/population-figures-by-country/r/population-figures-by-country-csv.csv")  %>% select(country = Country, population = Year_2016)  %>%
mutate(country = ifelse(country == "United States", "US", country)) #renaming us



df_plot2  <- df_plot1  %>%
  left_join(pops, by = c("country"))  %>%
  mutate(cases_per_million = 1000000 *  cases / population)  %>%
  filter(population > 50000)  

df_plot2  %>% 
  mutate(country_dm = country)   %>% 
  ggplot(aes(y=cases_per_million, x = days_since, group = country_dm)) +
  geom_line(data = df_plot2[,-1], color = "gray") +
  geom_line(color = "#BB0000", size = 1.2)  +
  geom_point(data = filter(df_plot2, date == max(df_plot2$date)), color = "#BB0000", size  = 2) +
  guides(color=FALSE) +
  facet_wrap(~country) +
  scale_y_log10(breaks= c(0.1, 1,10,100,1000,10000,50000), labels = c("0.1", "1", "10", "100", "1000","10.000", "50.0000")) +
  scale_x_continuous(breaks = seq(0,60,20)) +
  xlab("Number of days since the 100th case")  +
  ylab("Number of cases per one million inhabitants") +
  theme(
    panel.background = element_rect(fill = "#fff1e6",
                                    colour = "#fff1e6",
                                    size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill = "#fff1e6"),
    panel.grid.major.y = ggplot2::element_line(
      color = "#e6d9cf",
    ),
    panel.grid.major.x = ggplot2::element_line(
      color = "#e6d9cf",
    ),
    panel.grid.minor = ggplot2::element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(colour = '#BB0000', size = 10,hjust = 0)
    ) +
  labs(title="COVID-19 Cases per one million inhabitants", subtitle="created by @seyhunsaral. \nInspired by @jburnmurdoch's plot https://www.ft.com/coronavirus-latest.\nEducation purposes only. Not associated with FT or any other institution.")

  ggsave("plot2.png", height = 10, width = 12) 

