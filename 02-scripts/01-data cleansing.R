# import libraries
#install.packages("tidyverse") # https://www.tidyverse.org 
library(tidyverse)
library(lubridate)
library(readr)

# read the data
# PLEASE NOTE, THAT MY PERSONAL CSV FILE IS NOT INCLUDED IN THE REPO; PLEASE
# USE YOUR OWN!
li_cons <- read_csv("./01-data/Connections.csv", skip = 2)

# make some basic counts
li_cons %>% 
  dim()

# get column names
li_cons %>% 
  colnames()

# remove columns First and Last name as well as Email address, 
# since they will not be analyzed
li_cons <- li_cons %>% 
  select("Company", "Position", "Connected On")

# make some basic counts
li_cons %>% 
  dim()

# fix the date (change type from <chr> to <date>)
# Anyone who knows how to do this in a Tidyverse manner?
li_cons["connectedOn"] <- lapply(li_cons["Connected On"], function (x) {parse_date(x, "%d %b %Y")})

li_cons <- li_cons %>% 
  select("Company", "Position", "connectedOn")

li_cons %>% 
  sample_n(3)

# generate columns for year, month, and day of the week

li_cons <- li_cons %>% 
  mutate(
    connectedOnYear = year(connectedOn),
    connectedOnMonth = formatC(month(connectedOn), width = 2, flag = "0"),
    connectedOnYearMonth = paste(connectedOnYear, connectedOnMonth, sep = "-"),
    connectedOnDay = formatC(day(connectedOn), width = 2, flag = "0"),
    connectedOnDOW = wday(connectedOn, label = TRUE, abbr = TRUE),
    connectedOnQuarter = quarter(connectedOn),
    connectedOnQuarterStr = paste(connectedOnYear, " - Q", connectedOnQuarter, sep = "")
  ) 

li_cons %>% 
  sample_n(3)

# write cleansed data to disk
write_csv(li_cons, "./01-data/cleansed_Connections.csv")
