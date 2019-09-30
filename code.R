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
uber <- uber %>% select(c("Base Name", "Year", "Month", "Month Name", "Total Dispatched Trips")) %>%
  rename(Company = "Base Name",
         Month_Name = "Month Name",
         Total_Trips_or_Total_Passengers = "Total Dispatched Trips")

## check the table
head(uber)

## get the Citi Bikes Q3 2013 data
citi_q32013 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_launch_to_sep2013.csv")

## select the columns that are needed from the data
citi_q32013 <- citi_q32013 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q4 2013 data
citi_q42013 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2013_to_dec2013.csv")

## select the columns that are needed from the data
citi_q42013 <- citi_q42013 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q1 2014 data
citi_q12014 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2014_to_mar2014.csv")

## select the columns that are needed from the data
citi_q12014 <- citi_q12014 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q2 2014 data
citi_q22014 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2014_to_jun2014.csv")

## select the columns that are needed from the data
citi_q22014 <- citi_q22014 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q3 2014 data
citi_q32014 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2014_to_sep2014.csv")

## select the columns that are needed from the data
citi_q32014 <- citi_q32014 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q4 2014 data
citi_q42014 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2014_to_dec2014.csv")

## select the columns that are needed from the data
citi_q42014 <- citi_q42014 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q1 2015 data
citi_q12015 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2015_to_mar2015.csv")

## select the columns that are needed from the data
citi_q12015 <- citi_q12015 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q2 2015 data
citi_q22015 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2015_to_jun2015.csv")

## select the columns that are needed from the data
citi_q22015 <- citi_q22015 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q3 2015 data
citi_q32015 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2015_to_sep2015.csv")

## select the columns that are needed from the data
citi_q32015 <- citi_q32015 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q4 2015 data
citi_q42015 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2015_to_dec2015.csv")

## select the columns that are needed from the data
citi_q42015 <- citi_q42015 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q1 2016 data
citi_q12016 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2016_to_mar2016.csv")

## select the columns that are needed from the data
citi_q12016 <- citi_q12016 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q2 2016 data
citi_q22016 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2016_to_jun2016.csv")

## select the columns that are needed from the data
citi_q22016 <- citi_q22016 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q3 2016 data
citi_q32016 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2016_to_sep2016.csv")

## select the columns that are needed from the data
citi_q32016 <- citi_q32016 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q4 2016 data
citi_q42016 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2016_to_dec2016.csv")

## select the columns that are needed from the data
citi_q42016 <- citi_q42016 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q1 2017 data
citi_q12017 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2017_to_mar2017.csv")

## select the columns that are needed from the data
citi_q12017 <- citi_q12017 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q2 2017 data
citi_q22017 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2017_to_jun2017.csv")

## select the columns that are needed from the data
citi_q22017 <- citi_q22017 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q3 2017 data
citi_q32017 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2017_to_sep2017.csv")

## select the columns that are needed from the data
citi_q32017 <- citi_q32017 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q4 2017 data
citi_q42017 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2017_to_dec2017.csv")

## select the columns that are needed from the data
citi_q42017 <- citi_q42017 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q1 2018 data
citi_q12018 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2018_to_mar2018.csv")

## select the columns that are needed from the data
citi_q12018 <- citi_q12018 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q2 2018 data
citi_q22018 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2018_to_jun2018.csv")

## select the columns that are needed from the data
citi_q22018 <- citi_q22018 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q3 2018 data
citi_q32018 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2018_to_sep2018.csv")

## select the columns that are needed from the data
citi_q32018 <- citi_q32018 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## get the Citi Bikes Q4 2018 data
citi_q42018 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2018_to_dec2018.csv")

## select the columns that are needed from the data
citi_q42018 <- citi_q42018 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`)

## combine all the citi data
citi <- rbind(citi_q32013, citi_q42013, 
              citi_q12014, citi_q22014, citi_q32014, citi_q42014,
              citi_q12015, citi_q22015, citi_q32015, citi_q42015,
              citi_q12016, citi_q22016, citi_q32016, citi_q42016,
              citi_q12017, citi_q22017, citi_q32017, citi_q42017,
              citi_q12018, citi_q22018, citi_q32018, citi_q42018)

## check the citi data
head(citi)

## change the date column to a date format
citi$Date <- mdy(citi$Date)

## rename the trips column and add a Month column
citi <- citi %>% rename(Total_Trips_or_Total_Passengers = `Trips over the past 24-hours (midnight to 11:59pm)`) %>%
  mutate(Month = Date) 

## edit the Month column to include the Month number only
citi$Month <- citi$Month %>% str_replace("^(\\d{4})-(\\d{2})-(\\d{2})$", "\\2") %>% as.numeric()

## add a Month name column
citi <- citi %>% mutate(Month_Name = Month)

## edit the Month name column to include the Month name only
citi <- citi %>% mutate(Month_Name = case_when(
  Month_Name == 1 ~ "January",
  Month_Name == 2 ~ "February",
  Month_Name == 3 ~ "March",
  Month_Name == 4 ~ "April",
  Month_Name == 5 ~ "May",
  Month_Name == 6 ~ "June",
  Month_Name == 7 ~ "July",
  Month_Name == 8 ~ "August",
  Month_Name == 9 ~ "September",
  Month_Name == 10 ~ "October",
  Month_Name == 11 ~ "November",
  Month_Name == 12 ~ "December"
))

## check the citi data
head(citi)

## add a Year column
citi <- citi %>% mutate(Year = Date)

## edit the Year column to include the Year only
citi$Year <- citi$Year %>% str_replace("^(\\d{4})-(\\d{2})-(\\d{2})$", "\\1") %>% as.numeric()

## remove the Date column
citi <- citi %>% select(-Date)

## add a Company column
citi$Company <- "CITI"

## create a table for uber and citi riders
citi_uber <- rbind(citi, uber)

## check the table
head(citi_uber)

## reorder the columns
citi_uber <- citi_uber[c("Company", "Year", "Month", "Month_Name", "Total_Trips_or_Total_Passengers")]

## check the table again
head(citi_uber)

## create a facet wrap for citi and uber drivers
citi_uber %>%
  ggplot(aes(Year, Total_Trips_or_Total_Passengers, col = Company)) + 
  geom_jitter()
