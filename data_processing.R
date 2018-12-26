library(lubridate)  # date conversion
library(sqldf)      # because i know what I want to do in SQL
library(ggplot2)    # All plotting
library(dplyr)      # data munging
library(plotly)     # for tooltips on the graph

colors<- c("#472A7AFF", "#440154FF", "#481769FF", "#65CB5EFF", "#23898EFF", "#3D4E8AFF", "#433D84FF",
           "#297B8EFF", "#46C06FFF", "#1F978BFF", "#2EB37CFF", "#2E6D8EFF", "#B0DD2FFF", "#21A585FF",
           "#89D548FF", "#D8E219FF", "#355E8DFF", "#FDE725FF")

#pull in this handy data from a good person
setwd ('..')
lake<- read.csv('./dataisbeautiful-2018-12/data/lake_mendota.csv', stringsAsFactors = F)
#save the file for me
write.csv(lake, "lake_mendota_data.csv", row.names = F)

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
# common year for plotting
lake_expanded <- lake_expanded %>%
  mutate(year2= case_when(
    month(date) >= 10 ~ "2015",
    TRUE ~ "2016")) %>%
  mutate(x_axis_date = as.Date(paste0(year2, "-", format(lake_expanded$date, "%m-%d")), "%Y-%m-%d"), 
         decade = cut(year, breaks= 18, ordered_result = T))
#to make the axis wrap, assign Oct 1- Dec 31 as 2015, and the rest to 2016

p<-ggplot(lake_expanded, aes(x=x_axis_date, fill = decade, 
                             text = paste0(format(date,"%b-%d"),
                                          '<br>Years:', substr(decade, 2,5), '-', substr(decade, 7,11)))) +
  geom_bar(stat = "count", position = "stack", width = 1) +
  # scale_fill_continuous(low="blue", high="red")+ 
  scale_x_date(date_labels = "%b") + 
  geom_hline(yintercept=163) +
  labs(y="", x = "Calendar Date", 
       title = "Number of days Lake Mendota was open for ice fishing",
       subtitle = "over the last 163 winters, by calendar day") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 20, face = "bold", 
                                  margin = margin(10,0,10,0)), 
        axis.text = element_text(colour = "grey12"), 
        axis.ticks = element_line(colour = "grey12"), 
        plot.background = element_rect(colour = "grey82"), 
        panel.background = element_rect(fill = "grey82", colour = NA),
        legend.title=element_blank(),
        legend.position = "none")
p
