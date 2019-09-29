## load libraries
library(tidyverse)
library(rvest)
library(lubridate)
library(RColorBrewer)

## retrieve the mta revenue data
mta_revenue <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/mta_revenue.csv")

## check the structure of the mta revenue table
str(mta_revenue)

## remove unwanted columns and rename some columns
mta_revenue <- mta_revenue %>% select(-c("Fifty2525", "Date Prefix", 
                                         "Revenue Type Note", "Adjustments Outcome",
                                         "No-Adjustments Value", "Date",
                                         "Framework Selection Calculation")) %>%
  rename(Business_Line_Group = 'Business Line (group)',
         Business_Line = 'Business Line',
         Revenue_Description = Description1,
         Revenue_Type = 'Revenue and Expense Type',
         Revenue_Cost = Value)

## change all NA values to 0
mta_revenue[is.na(mta_revenue)] = 0

## check the mta revenue table
head(mta_revenue)

## create a graph for subway revenue totals each year
## exclude year 2019 since the year is not yet completed
mta_revenue %>% filter(Year <= 2018 & Revenue_Description == "Subway") %>%
  ggplot(aes(Year, Revenue_Cost)) + 
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(breaks = seq(2007, 2018, 1)) + 
  coord_cartesian(ylim = c(1500, 3500)) +
  xlab("Year") + ylab("Total Revenue (millions)") +
  ggtitle("MTA Subway Fare Revenue") + 
  theme_bw()

## retrieve subway ridership data
subway_riders_url <- "http://web.mta.info/nyct/facts/ridership/"

## get the html code
subway_riders_html <- read_html(subway_riders_url)

## get the html nodes
subway_riders_table <- subway_riders_html %>% html_nodes("table")

## check the html nodes
subway_riders_table

## find the subway table
subway_riders_table[[2]]

## turn the table to a data frame
subway_riders <- subway_riders_table[[2]] %>% html_table

## check the structure of the subway ridership data
str(subway_riders)

## change the year column to numbers
subway_riders[,1] <- subway_riders[,1] %>% as.numeric()

## remove the commas from the remaining columns and convert them to numbers
subway_riders[,2] <- subway_riders[,2] %>% str_replace_all(",","") %>% as.numeric()
subway_riders[,3] <- subway_riders[,3] %>% str_replace_all(",","") %>% as.numeric()
subway_riders[,4] <- subway_riders[,4] %>% str_replace_all(",","") %>% as.numeric()
subway_riders[,5] <- subway_riders[,5] %>% str_replace_all(",","") %>% as.numeric()
subway_riders[,6] <- subway_riders[,6] %>% str_replace_all(",","") %>% as.numeric()

## convert the data to a tibble
subway_riders <- subway_riders %>% as_tibble()

## rename the columns
subway_riders <- subway_riders %>% 
  rename(Weekdays = `Average Weekday`,
         Saturdays = `Average Saturday`,
         Sundays = `Average Sunday`,
         Weekends = `Average Weekend`)

## spread the subway riders table by creating a ridership category
subway_riders <- subway_riders %>% 
  gather(Ridership_Category, Average_Riders, 'Weekdays':'Annual Total')

## arrange the ridership category in descending order
subway_riders <- subway_riders %>% 
  mutate(Ridership_Category = reorder(Ridership_Category, -Average_Riders))

## check the table
head(subway_riders)

## create a graph for subway ridership
subway_riders %>% 
  filter(Ridership_Category == "Weekdays" | 
           Ridership_Category == "Weekends") %>%
  ggplot(aes(Year, Average_Riders)) + 
  geom_bar(stat = "identity", fill = "black") + 
  scale_x_continuous(breaks = seq(2013, 2018, 1)) + 
  coord_cartesian(ylim = c(3500000, 6000000)) +
  xlab("Year") + ylab("Average Number of Riders") +
  ggtitle("Subway Ridership") +
  facet_wrap(~Ridership_Category) + theme_bw()

## set up a link to the uber data
uber_url <- "https://github.com/jessicapadilla/mta_citi_uber/blob/master/all_uber_data.csv?raw=true"

## read the csv from the link
uber_ridership <- read_csv(uber_url)

## check the structure of the data
str(uber_ridership)

## filter the data to just uber data
uber <- uber_ridership %>% filter(`Base Name` == "UBER")

## remove unwanted columns and rename some columns
uber <- uber %>% select(-c("Base License Number", "DBA")) %>%
  rename(Company = "Base Name",
         Month_Name = "Month Name",
         Total_Trips = "Total Dispatched Trips",
         Total_Shared_Trips = "Total Dispatched Shared Trips",
         Unique_Vehicles = "Unique Dispatched Vehicles")

## check the table
head(uber)
