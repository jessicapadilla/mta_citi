## load libraries
library(tidyverse)
library(rvest)
library(lubridate)
library(gridExtra)

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
  rename(Average_Weekdays = `Average Weekday`,
         Average_Saturdays = `Average Saturday`,
         Average_Sundays = `Average Sunday`,
         Average_Weekends = `Average Weekend`,
         Annual_Total = `Annual Total`)

## check the table
head(subway_riders)

## gather the subway riders data
subway_riders <- subway_riders %>% 
  gather(Category, Riders, Average_Weekdays:Annual_Total)

## create a graph showing the average number of riders on weekdays
weekdays <- subway_riders %>% filter(Category == "Average_Weekdays") %>%
  ggplot(aes(Year, Riders)) + 
  geom_point(color = "red", show.legend = FALSE) + 
  geom_line(color = "red", show.legend = FALSE) + 
  coord_cartesian(ylim = c(5400000, 5700000)) +
  xlab("Year") + ylab("Average Number of Riders") + ggtitle("Weekdays") +
  theme_bw()

## create a graph showing the average number of riders on weekends
weekends <- subway_riders %>% filter(Category == "Average_Weekends") %>%
  ggplot(aes(Year, Riders, col = )) + 
  geom_point(color = "purple", show.legend = FALSE) + 
  geom_line(color = "purple", show.legend = FALSE) + 
  coord_cartesian(ylim = c(5400000, 6000000)) +
  xlab("Year") + ylab("Average Number of Riders") + ggtitle("Weekends") +
  theme_bw()
                         
## put the graphs side by side
grid.arrange(weekdays, weekends, ncol = 2)

## create a table for annual subway totals
annual_subway <- subway_riders %>% filter(Category == "Annual_Total") %>%
  spread(Category, Riders)

## add a company column to the table
annual_subway$Company <- "MTA"

## check the table
head(annual_subway)

## set up a link to the uber data
uber_url <- "https://github.com/jessicapadilla/mta_citi_uber/blob/master/all_uber_data.csv?raw=true"

## read the csv from the link
uber_riders <- read_csv(uber_url)

## check the structure of the data
str(uber_riders)

## filter the data to just uber data
uber_riders <- uber_riders %>% filter(`Base Name` == "UBER")

## remove unwanted columns and rename some columns
uber_riders <- uber_riders %>% 
  select(c("Base Name", "Year", "Month", "Month Name", "Total Dispatched Trips")) %>%
  rename(Company = "Base Name",
         Month_Name = "Month Name",
         Total_Trips_or_Passengers = "Total Dispatched Trips")

## check the table
head(uber_riders)

## create a table for annual uber totals
annual_uber <- uber_riders %>% group_by(Year) %>%
  summarize(Annual_Total = sum(Total_Trips_or_Passengers))

## add a company column to the table
annual_uber$Company <- "Uber"

## check the table
head(annual_uber)

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
citi_riders <- rbind(citi_q32013, citi_q42013, 
              citi_q12014, citi_q22014, citi_q32014, citi_q42014,
              citi_q12015, citi_q22015, citi_q32015, citi_q42015,
              citi_q12016, citi_q22016, citi_q32016, citi_q42016,
              citi_q12017, citi_q22017, citi_q32017, citi_q42017,
              citi_q12018, citi_q22018, citi_q32018, citi_q42018)

## check the citi data
head(citi_riders)

## change the date column to a date format
citi_riders$Date <- mdy(citi_riders$Date)

## rename the trips column and add a Month column
citi_riders <- citi_riders %>% rename(Total_Trips_or_Passengers = `Trips over the past 24-hours (midnight to 11:59pm)`) %>%
  mutate(Month = Date) 

## edit the Month column to include the Month number only
citi_riders$Month <- citi_riders$Month %>% str_replace("^(\\d{4})-(\\d{2})-(\\d{2})$", "\\2") %>% as.numeric()

## add a Month name column
citi_riders <- citi_riders %>% mutate(Month_Name = Month)

## edit the Month name column to include the Month name only
citi_riders <- citi_riders %>% mutate(Month_Name = case_when(
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

## add a Year column
citi_riders <- citi_riders %>% mutate(Year = Date)

## edit the Year column to include the Year only
citi_riders$Year <- citi_riders$Year %>% 
  str_replace("^(\\d{4})-(\\d{2})-(\\d{2})$", "\\1") %>% as.numeric()

## add a Company column
citi_riders$Company <- "Citi Bike"

## check the citi bike table
head(citi_riders)

## create a graph for citi riders
citi_riders %>% group_by(Company, Year, Month) %>%
  summarize(Total_Trips = sum(Total_Trips_or_Passengers)) %>%
  ggplot(aes(Year, Total_Trips)) +
  geom_area()

## create a table for annual citi totals
annual_citi <- citi_riders %>% group_by(Year) %>%
  summarize(Annual_Total = sum(Total_Trips_or_Passengers))

## add a company column to the table
annual_citi$Company <- "Citi Bike"

## check the table
head(annual_citi)

## merge all the annual total tables
annual_ridership <- rbind(annual_subway, annual_uber, annual_citi)

## create a graph showing annual ridership between companies
annual_ridership %>% filter(Company == "Citi Bike") %>%
  ggplot(aes(Year, Annual_Total, col = Company)) +
  geom_point() + geom_line() + 
  scale_x_continuous(breaks = seq(2013, 2018, 1))

annual_ridership %>% filter(Company == "Uber" & Year < 2019) %>%
  ggplot(aes(Year, Annual_Total, col = Company)) +
  geom_point() + geom_line() +
  scale_x_continuous(breaks = seq(2013, 2018, 1))

annual_ridership %>% filter(Company == "MTA" & Year < 2019) %>%
  ggplot(aes(Year, Annual_Total, col = Company)) +
  geom_point() + geom_line()

annual_ridership %>% ggplot(aes(Year, Company, col = Annual_Total)) + 
  geom_point(size = 2)

citi_riders %>% ggplot(aes(Year, Total_Trips_or_Passengers)) + geom_jitter()

uber_riders %>% ggplot(aes(Year, Total_Trips_or_Passengers, col = Month_Name)) + 
  geom_jitter()
