library(lubridate)  # date conversion
library(sqldf)      # because i know what I want to do in SQL
library(ggplot2)    # All plotting

#pull in this handy data from a good person
setwd ('..')
lake<- read.csv('./dataisbeautiful-2018-12/data/lake_mendota.csv', stringsAsFactors = F)

# convert dates to dates
lake$date_closed <- ymd(lake$date_closed)
lake$date_opened <- ymd(lake$date_opened)

# make a dataframe with one day of the year for every year that is in the lake dataset
first_date<- min(lake$date_closed)
last_date<- max(lake$date_opened)  

all_dates<- data.frame(seq(first_date, last_date, by = "day"))
names(all_dates) <- "date"

# inner join all dates and the lake-open dates
lake_expanded<- sqldf("SELECT date
                      ,year
                      ,date_closed
                      ,date_opened
                      ,days
                      FROM all_dates
                      JOIN lake
                        ON all_dates.date >= lake.date_closed
                        AND all_dates.date <= lake.date_opened")
lake_expanded$day <- format(lake_expanded$date, "%m-%d")

qplot(lake_expanded$day)

