library(dplyr)
library(lubridate)
library(tidytext)
library(tidyr)

tesla_stock <- read.csv("TSLA.csv", stringsAsFactors = F)

# max open and close through the years and months
#min open and close through the years and month
#Highest percentage jump

#Highest close open difference
tesla_stock %>% mutate(open_close = Close - Open) %>% filter(open_close == max(open_close))

#separate date into year, month, day
tsl_spearate <- tesla_stock %>% separate(Date, c("year", "month", "day"), sep = "-")

#highest close open difference per year 
tsl_sperate %>% group_by(year) %>% summarize(max(Close -Open))
  