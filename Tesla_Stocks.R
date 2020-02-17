library(dplyr)
library(lubridate)
library(tidytext)
library(tidyr)
library(ggplot2)

tesla_stock <- read.csv("TSLA.csv", stringsAsFactors = F)

# max open and close through the years and months
#min open and close through the years and month
#Highest percentage jump

#Highest close open difference
tesla_stock %>% mutate(open_close = Close - Open) %>% filter(open_close == max(open_close))

#separate date into year, month, day
tsl_spearate <- tesla_stock %>% separate(Date, c("year", "month", "day"), sep = "-") 


#Average year close
tsl_spearate %>% group_by(year) %>% summarize(yearly_avg = mean(Close)) %>%
                 ggplot(aes(year, yearly_avg, color = year, group = 1)) + geom_line()

#Average monthly close
tsl_spearate %>% group_by(year, month) %>% summarize(monthly_avg = mean(Close)) %>% 
  ggplot(aes(month ,monthly_avg, colour = year, group = year)) +geom_line() +geom_point()
  

# diff of close each day
tsl_spearate %>% mutate(lag_close = lag(Close), daily_diff = Close - lag_close, 
                 percentage = abs(Close-lag_close)/((Close+lag_close)/2)*100)
                 
  
  
  
  
  
  
#highest close open difference per year 
tsl_spearate %>% group_by(year) %>% summarize(max(Close -Open))
  


tsl <- tesla_stock %>% separate(Date, c("year", "month", "day"), sep = "-") %>% 
  mutate(close_open = Close- Open, percentage = (abs(Close - Open)/((Close + Open)/2))*100)

tsl %>% group_by(year) %>% filter(percentage == max(percentage))
  