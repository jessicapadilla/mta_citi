## load libraries
library(tidyverse)
library(rvest)
library(gridExtra)
library(lubridate)

## retrieve the mta revenue data
mta_revenue <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/mta_revenue.csv")

## check the structure of the mta revenue table
str(mta_revenue)

## remove unwanted columns and rename some columns
mta_revenue <- mta_revenue %>% select(-c("Fifty2525", "Date Prefix", 
                                         "Revenue Type Note", "Adjustments Outcome",
                                         "No-Adjustments Value", "Date",
                                         "Framework Selection Calculation")) %>%
  rename(business_line_group = 'Business Line (group)',
         business_line = 'Business Line',
         revenue_description = Description1,
         revenue_type = 'Revenue and Expense Type',
         year = Year,
         revenue_value = Value)

## change all NA values to 0
mta_revenue[is.na(mta_revenue)] = 0

## check the mta revenue table
head(mta_revenue)

## create a graph for subway fare revenue totals each year
## exclude year 2019 since the year is not yet completed
mta_revenue %>% filter(year <= 2018 & revenue_description == "Subway") %>%
  ggplot(aes(year, revenue_value)) + 
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(breaks = seq(2007, 2018, 1)) + 
  xlab("Year") + ylab("Total Revenue (millions)") +
  ggtitle("MTA Subway Fare Revenue") + 
  theme_bw()

## set the link for subway ridership totals
subway_totals_url <- "http://web.mta.info/nyct/facts/ridership/"

## get the html code
subway_totals_html <- read_html(subway_totals_url)

## get the tables from the html code
subway_totals_table <- subway_totals_html %>% html_nodes("table")

## find the table
subway_totals_table

## get the subway totals table
subway_totals_table[[2]]

## turn the table to a data frame
subway_totals <- subway_totals_table[[2]] %>% html_table

## check the structure of the subway ridership data
str(subway_totals)

## change the year column to numbers
subway_totals[,1] <- subway_totals[,1] %>% as.numeric()

## create a function to remove commas from a column and turn the column to numbers
numbers <- function(x){
  x <- str_replace_all(x, ",", "")
  x <- as.numeric(x)
}

## remove the commas from the remaining columns and convert them to numbers
subway_totals[,2:6] <- lapply(subway_totals[,(2:6)], numbers)

## convert the data to a tibble
subway_totals <- subway_totals %>% as_tibble()

## rename the columns
subway_totals <- subway_totals %>% 
  rename(year = Year,
         average_weekdays = `Average Weekday`,
         average_saturdays = `Average Saturday`,
         average_sundays = `Average Sunday`,
         average_weekends = `Average Weekend`,
         annual_total = `Annual Total`)

## check the table
head(subway_totals)

## create a graph showing the annual number of subway riders
subway_totals %>% 
  ggplot(aes(year, annual_total)) + 
  geom_point(color = "purple", show.legend = FALSE) + 
  geom_line(color = "purple", show.legend = FALSE) + 
  coord_cartesian(ylim = c(1650000000, 1800000000)) +
  xlab("Year") + ylab("Total Number of Riders") + 
  ggtitle("Annual Subway Ridership") + theme_bw()

## retrieve train totals data
subway_trains <- read_csv("https://github.com/jessicapadilla/mta-mismanagement/raw/master/subway_service_delivered.csv")

## check the structure of the table
str(subway_trains)

## remove unwanted columns and rename some columns
subway_trains <- subway_trains %>% select(-division) %>%
  rename(scheduled_trains = "num_sched_trains", actual_trains = "num_actual_trains",
         ratio_scheduled_to_actual_trains = "service delivered")

## add a year column with the values from the month column
subway_trains <- subway_trains %>% mutate(year = month)

## edit the year column by removing the month and change the year to numbers
subway_trains$year <- subway_trains$year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway totals table
head(subway_trains)

## create a graph showing the total number of trains each year
## exclude year 2019 since the year is not yet completed
subway_trains %>% filter(year <= 2018) %>% 
  mutate(year = factor(year)) %>% 
  ggplot(aes(scheduled_trains, actual_trains, col = line)) + 
  geom_point(size = 2) + facet_grid(. ~ year) +
  scale_color_discrete(name = "Subway Line") + xlim(0, 4000) + ylim(0, 4000) +
  xlab("Scheduled Number of Trains") + ylab("Actual Number of Trains") +
  ggtitle("Total Number of Subways") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## retrieve subway incidents data
subway_incidents <- read_csv("https://github.com/jessicapadilla/mta-mismanagement/raw/master/subway_major_incidents.csv")

## check the structure of the subway incidents table
str(subway_incidents)

## remove the division column and rename the category column
subway_incidents <- subway_incidents %>% select(-division) %>% 
  rename(type_of_issue = category)

## add a year column using the values from the month column
subway_incidents <- subway_incidents %>% mutate(year = month)

## edit the year column by removing the month and change the year to numbers
subway_incidents$year <- subway_incidents$year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the different types of issues
unique(subway_incidents$type_of_issue)

## make the persons on trackbed/police/medical text shorter
subway_incidents$type_of_issue <- subway_incidents$type_of_issue %>%
  str_replace("Persons on Trackbed/Police/Medical", "Passenger")

## reorder the types of issue
subway_incidents$type_of_issue <- factor(subway_incidents$type_of_issue, 
                                         levels = c("Signals", "Stations and Structure", 
                                                    "Subway Car", "Track", 
                                                    "Passenger", "Other"))

## check the subway incidents table
head(subway_incidents)

## create a graph showing the number of subway-related incidents per year
## exclude 2019 since the year is not yet completed
subway_incidents %>% 
  filter(year <= 2018 & type_of_issue %in% c("Signals", "Stations and Structure", "Subway Car", "Track")) %>%
  group_by(year, type_of_issue) %>%
  summarize(total_count = sum(count)) %>%
  ggplot(aes(year, total_count)) + 
  geom_bar(stat = "identity", fill = "black") + 
  coord_cartesian(ylim = c(0, 300)) +
  facet_wrap(.~type_of_issue, ncol = 2) +
  xlab("Year") + ylab("Total Number of Incidents") +
  ggtitle("Subway Incidents") + theme_bw()

## retrieve subway platform times data
subway_platform_times <- read_csv("https://github.com/jessicapadilla/mta-mismanagement/raw/master/subway_platform_time.csv")

## check the structure of the subway platform table
str(subway_platform_times)

## remove the division column and rename some columns
subway_platform_times <- subway_platform_times %>% select(-"division") %>% 
  rename(total_passengers = "num_passengers", 
         additional_platform_time = "additional platform time")

## add a year column by adding values from the month column
subway_platform_times <- subway_platform_times %>% mutate(year = month)

## edit the year column by removing the month and change the year to numbers
subway_platform_times$year <- subway_platform_times$year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway platform table
head(subway_platform_times)

## create a graph for additional platform times
## exclude 2019 since the year is not yet completed
add_platform <- subway_platform_times %>% filter(year <= 2018) %>% group_by(year) %>%
  summarize(average_additional_platform = mean(additional_platform_time)) %>%
  ggplot(aes(year, average_additional_platform)) +
  geom_point(color = "purple") + geom_line(color = "purple") +
  coord_cartesian(ylim = c(0, 2.0)) +
  xlab("Year") + ylab("Minutes") + ggtitle("Average Additional Platform Time") +
  theme_bw()

## retrieve subway train times data
subway_train_times <- read_csv("https://github.com/jessicapadilla/mta-mismanagement/raw/master/subway_train_time.csv")

## check the structure of the subway train times table
str(subway_train_times)

## remvoe the divison column and rename some columns
subway_train_times <- subway_train_times %>% select(-"division") %>% 
  rename(total_passengers = "num_passengers", 
         additional_train_time = "additional train time")

## add a year column using the values from the month column
subway_train_times <- subway_train_times %>% mutate(year = month)

## edit the year column by removing the month and change the year to numbers
subway_train_times$year <- subway_train_times$year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway train times table
head(subway_train_times)

## create a graph for additional train times
## exclude 2019 since the year is not yet completed
add_train <- subway_train_times %>% filter(year <= 2018) %>% group_by(year) %>%
  summarize(average_additional_train = mean(additional_train_time)) %>%
  ggplot(aes(year, average_additional_train)) +
  geom_point(color = "purple") + geom_line(color = "purple") +
  coord_cartesian(ylim = c(0, 2.0)) +
  xlab("Year") + ylab("Minutes") + ggtitle("Average Additional Train Time") +
  theme_bw()

## put the additional time graphs side by side
grid.arrange(add_platform, add_train, ncol = 1)

## get the Citi Bikes Q3 2013 data
citi_q32013 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_launch_to_sep2013.csv")

## select the columns that are needed from the data
citi_q32013 <- citi_q32013 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members`)

## get the Citi Bikes Q4 2013 data
citi_q42013 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2013_to_dec2013.csv")

## select the columns that are needed from the data
citi_q42013 <- citi_q42013 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members`)

## get the Citi Bikes Q1 2014 data
citi_q12014 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2014_to_mar2014.csv")

## select the columns that are needed from the data
citi_q12014 <- citi_q12014 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members`)

## get the Citi Bikes Q2 2014 data
citi_q22014 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2014_to_jun2014.csv")

## select the columns that are needed from the data
citi_q22014 <- citi_q22014 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Memberships Sold`) %>%
  rename(`Total Annual Members` = `Total Annual Memberships Sold`)

## get the Citi Bikes Q3 2014 data
citi_q32014 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2014_to_sep2014.csv")

## select the columns that are needed from the data
citi_q32014 <- citi_q32014 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Memberships Sold`) %>%
  rename(`Total Annual Members` = `Total Annual Memberships Sold`)

## get the Citi Bikes Q4 2014 data
citi_q42014 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2014_to_dec2014.csv")

## select the columns that are needed from the data
citi_q42014 <- citi_q42014 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Memberships Sold`) %>%
  rename(`Total Annual Members` = `Total Annual Memberships Sold`)

## get the Citi Bikes Q1 2015 data
citi_q12015 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2015_to_mar2015.csv")

## select the columns that are needed from the data
citi_q12015 <- citi_q12015 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>%
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q2 2015 data
citi_q22015 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2015_to_jun2015.csv")

## select the columns that are needed from the data
citi_q22015 <- citi_q22015 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Memberships Sold`) %>% 
  rename(`Total Annual Members` = `Total Annual Memberships Sold`)

## get the Citi Bikes Q3 2015 data
citi_q32015 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2015_to_sep2015.csv")

## select the columns that are needed from the data
citi_q32015 <- citi_q32015 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Memberships Sold`) %>% 
  rename(`Total Annual Members` = `Total Annual Memberships Sold`)

## get the Citi Bikes Q4 2015 data
citi_q42015 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2015_to_dec2015.csv")

## select the columns that are needed from the data
citi_q42015 <- citi_q42015 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q1 2016 data
citi_q12016 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2016_to_mar2016.csv")

## select the columns that are needed from the data
citi_q12016 <- citi_q12016 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q2 2016 data
citi_q22016 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2016_to_jun2016.csv")

## select the columns that are needed from the data
citi_q22016 <- citi_q22016 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q3 2016 data
citi_q32016 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2016_to_sep2016.csv")

## select the columns that are needed from the data
citi_q32016 <- citi_q32016 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q4 2016 data
citi_q42016 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2016_to_dec2016.csv")

## select the columns that are needed from the data
citi_q42016 <- citi_q42016 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q1 2017 data
citi_q12017 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2017_to_mar2017.csv")

## select the columns that are needed from the data
citi_q12017 <- citi_q12017 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q2 2017 data
citi_q22017 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2017_to_jun2017.csv")

## select the columns that are needed from the data
citi_q22017 <- citi_q22017 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q3 2017 data
citi_q32017 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2017_to_sep2017.csv")

## select the columns that are needed from the data
citi_q32017 <- citi_q32017 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q4 2017 data
citi_q42017 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2017_to_dec2017.csv")

## select the columns that are needed from the data
citi_q42017 <- citi_q42017 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q1 2018 data
citi_q12018 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jan2018_to_mar2018.csv")

## select the columns that are needed from the data
citi_q12018 <- citi_q12018 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q2 2018 data
citi_q22018 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_apr2018_to_jun2018.csv")

## select the columns that are needed from the data
citi_q22018 <- citi_q22018 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q3 2018 data
citi_q32018 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_jul2018_to_sep2018.csv")

## select the columns that are needed from the data
citi_q32018 <- citi_q32018 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## get the Citi Bikes Q4 2018 data
citi_q42018 <- read_csv("https://github.com/jessicapadilla/mta_citi_uber/raw/master/citi_oct2018_to_dec2018.csv")

## select the columns that are needed from the data
citi_q42018 <- citi_q42018 %>% select(Date, `Trips over the past 24-hours (midnight to 11:59pm)`, `Total Annual Members (All Time)`) %>% 
  rename(`Total Annual Members` = `Total Annual Members (All Time)`)

## combine all the citi data
citi_riders <- rbind(citi_q32013, citi_q42013, 
                     citi_q12014, citi_q22014, citi_q32014, citi_q42014,
                     citi_q12015, citi_q22015, citi_q32015, citi_q42015,
                     citi_q12016, citi_q22016, citi_q32016, citi_q42016,
                     citi_q12017, citi_q22017, citi_q32017, citi_q42017,
                     citi_q12018, citi_q22018, citi_q32018, citi_q42018)

## check the citi data
head(citi_riders)

## rename the columns
citi_riders <- citi_riders %>% 
  rename(date = Date, total_citi_trips = `Trips over the past 24-hours (midnight to 11:59pm)`, 
         citi_annual_to_date = `Total Annual Members`)

## convert the date column to dates
citi_riders$date <- mdy(citi_riders$date)

## add a year column using the values from the date column
citi_riders <- citi_riders %>% mutate(year = date)

## edit the year column by removing the rest of the date and change the year to numbers
citi_riders$year <- citi_riders$year %>% 
  str_replace("^(\\d{4})-(\\d{2})-(\\d{2})$", "\\1") %>% as.numeric()

## add a month column using the values from the date column
citi_riders <- citi_riders %>% mutate(month = date)

## edit the year column by removing the rest of the date and change the year to numbers
citi_riders$month <- citi_riders$month %>% 
  str_replace("^(\\d{4})-(\\d{2})-(\\d{2})$", "\\2") %>% as.numeric()

## create a graph showing the number of citi bike trips
citi_riders %>% ggplot(aes(year, total_citi_trips)) + 
  geom_jitter(color = "purple", alpha = 0.3, size = 3, width = 0.3) + 
  scale_x_continuous(breaks = seq(2013, 2018, 1)) + 
  xlab("Year") + ylab("Number of Daily Trips") +
  ggtitle("Citi Bike Trips") +
  theme_bw()

## store the number of annual citi bike members at the end of each year
## keep only the relevant columns
annual_citi <- citi_riders %>% 
  filter(date == "2013-12-31" | date == "2014-12-31" | date == "2015-12-31" | date == "2016-12-31" | date == "2017-12-31" | date == "2018-12-31") %>%
  select(year, citi_annual_to_date)

## merge the annual citi data to the annual subway data
## rename the annual subway column
## only keep the columns needed for correlation
annual_riders <- left_join(subway_totals, annual_citi, by = "year") %>%
  rename(annual_subway = annual_total)

## create a graph showing the correlation between subway ridership and annual citi bike memberships
## filter the data to 2015 and after since subway ridership started to decline in 2015
annual_riders %>% filter(year >= 2015) %>%
  ggplot(aes(citi_annual_to_date, average_weekends)) +
  geom_point() + geom_line() + geom_abline() + theme_bw()

cor(annual_riders$citi_annual_to_date, annual_riders$average_weekends)



## retrieve monthly subway ridership data
## devices that measure subway ridership do not reset the count to 0 every day
## devices just keep counting the number of riders throughout the year
## to get how many riders there are per month, subtract the count at the beginning of the month from the count at the end of the month
## ridership is measured every 4 hours
## filter data to include 20:00 and after to get the total riders at the end of the day
## to filter by time, 20:00 was converted to 72000 seconds (60 seconds x 60 minutes x 20 hours)
## create a function to read data from a link and to modify data accordingly
subway <- function(x){
  file <- read_csv(x)
  revised <- file %>% 
    filter(TIME >= 72000) %>%                    ## filter the data for 20:00 and after
    select(-c(EXITS, DESC)) %>%                  ## remove unnecessary columns
    rename(c_a = `C/A`,                          ## rename columns
           unit = UNIT, 
           scp = SCP, 
           station = STATION, 
           line = LINENAME,
           division = DIVISION, 
           date_measured = DATE, 
           time = TIME,
           entries = ENTRIES)
  revised$date_measured <- mdy(revised$date_measured)              ## convert the date column to dates
  revised$entries <- as.numeric(revised$entries) ## convert the entries column to numbers 
  return(revised)
}

jan2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150103.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150110.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150117.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150124.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150131.txt"
)

jan2015 <- lapply(jan2015, subway) %>% bind_rows()

feb2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150207.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150214.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150221.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150228.txt"
)

feb2015 <- lapply(feb2015, subway) %>% bind_rows()

mar2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150307.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150314.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150321.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150328.txt"
)

mar2015 <- lapply(mar2015, subway) %>% bind_rows()

apr2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150404.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150411.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150418.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150425.txt"
)

apr2015 <- lapply(apr2015, subway) %>% bind_rows()

may2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150502.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150509.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150516.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150523.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150530.txt"
)

may2015 <- lapply(may2015, subway) %>% bind_rows()

jun2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150606.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150613.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150620.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150627.txt"
)

jun2015 <- lapply(jun2015, subway) %>% bind_rows()

jul2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150704.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150711.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150718.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150725.txt"
)

jul2015 <- lapply(jul2015, subway) %>% bind_rows()

aug2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150801.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150808.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150815.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150822.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150829.txt"
)

aug2015 <- lapply(aug2015, subway) %>% bind_rows()

sep2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150905.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150912.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150919.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150926.txt"
)

sep2015 <- lapply(sep2015, subway) %>% bind_rows()

oct2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151003.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151010.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151017.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151024.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151031.txt"
)

oct2015 <- lapply(oct2015, subway) %>% bind_rows()

nov2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151107.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151114.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151121.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151128.txt"
)

nov2015 <- lapply(nov2015, subway) %>% bind_rows()

dec2015 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151205.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151212.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151219.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_151226.txt"
)

dec2015 <- lapply(dec2015, subway) %>% bind_rows()

jan2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160102.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160109.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160116.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160123.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160130.txt"
)

jan2016 <- lapply(jan2016, subway) %>% bind_rows()

feb2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160206.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160213.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160220.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160227.txt"
)

feb2016 <- lapply(feb2016, subway) %>% bind_rows()

mar2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160305.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160312.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160319.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160326.txt"
)

mar2016 <- lapply(mar2016, subway) %>% bind_rows()

apr2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160402.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160409.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160416.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160423.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160430.txt"
)

apr2016 <- lapply(apr2016, subway) %>% bind_rows()

may2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160507.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160514.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160521.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160528.txt"
)

may2016 <- lapply(may2016, subway) %>% bind_rows()

jun2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160604.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160611.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160618.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160625.txt"
)

jun2016 <- lapply(jun2016, subway) %>% bind_rows()

jul2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160702.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160709.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160716.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160723.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160730.txt"
)

jul2016 <- lapply(jul2016, subway) %>% bind_rows()

aug2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160806.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160813.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160820.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160827.txt"
)

aug2016 <- lapply(aug2016, subway) %>% bind_rows()

sep2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160903.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160910.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160917.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_160924.txt"
)

sep2016 <- lapply(sep2016, subway) %>% bind_rows()

oct2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161001.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161008.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161015.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161022.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161029.txt"
)

oct2016 <- lapply(oct2016, subway) %>% bind_rows()

nov2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161105.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161112.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161119.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161126.txt"
)

nov2016 <- lapply(nov2016, subway) %>% bind_rows()

dec2016 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161203.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161210.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161217.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161224.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_161231.txt"
)  

dec2016 <- lapply(dec2016, subway) %>% bind_rows()  

jan2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170107.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170114.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170121.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170128.txt"
)

jan2017 <- lapply(jan2017, subway) %>% bind_rows()

feb2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170204.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170211.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170218.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170225.txt"
)

feb2017 <- lapply(feb2017, subway) %>% bind_rows()

mar2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170304.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170311.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170318.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170325.txt"
)

mar2017 <- lapply(mar2017, subway) %>% bind_rows()

apr2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170401.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170408.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170415.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170422.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170429.txt"
)

apr2017 <- lapply(apr2017, subway) %>% bind_rows()

may2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170506.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170513.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170520.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170527.txt"
)

may2017 <- lapply(may2017, subway) %>% bind_rows()

jun2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170603.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170610.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170617.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170624.txt"
)

jun2017 <- lapply(jun2017, subway) %>% bind_rows()

jul2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170701.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170708.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170715.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170722.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170729.txt"
)

jul2017 <- lapply(jul2017, subway) %>% bind_rows()

aug2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170805.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170812.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170819.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170826.txt"
)

aug2017 <- lapply(aug2017, subway) %>% bind_rows()

sep2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170902.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170909.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170916.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170923.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_170930.txt"
)

sep2017 <- lapply(sep2017, subway) %>% bind_rows()

oct2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171007.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171014.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171021.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171028.txt"
)

oct2017 <- lapply(oct2017, subway) %>% bind_rows()

nov2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171104.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171111.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171118.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171125.txt"
)

nov2017 <- lapply(nov2017, subway) %>% bind_rows()

dec2017 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171202.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171209.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171216.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171223.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_171230.txt"
)

dec2017 <- lapply(dec2017, subway) %>% bind_rows()

jan2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180106.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180113.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180120.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180127.txt"
)

jan2018 <- lapply(jan2018, subway) %>% bind_rows()

feb2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180203.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180210.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180217.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180224.txt"
)

feb2018 <- lapply(feb2018, subway) %>% bind_rows()

mar2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180303.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180310.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180317.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180324.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180331.txt"
)

mar2018 <- lapply(mar2018, subway) %>% bind_rows()

apr2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180407.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180414.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180421.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180428.txt"
)

apr2018 <- lapply(apr2018, subway) %>% bind_rows()

may2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180505.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180512.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180519.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180526.txt"
)

may2018 <- lapply(may2018, subway) %>% bind_rows()

jun2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180602.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180609.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180616.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180623.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180630.txt"
)

jun2018 <- lapply(jun2018, subway) %>% bind_rows()

jul2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180707.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180714.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180721.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180728.txt"
)

jul2018 <- lapply(jul2018, subway) %>% bind_rows()

aug2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180804.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180811.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180818.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180825.txt"
)

aug2018 <- lapply(aug2018, subway) %>% bind_rows()

sep2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180901.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180908.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180915.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180922.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_180929.txt"
)

sep2018 <- lapply(sep2018, subway) %>% bind_rows()

oct2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181006.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181013.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181020.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181027.txt"
)

oct2018 <- lapply(oct2018, subway) %>% bind_rows()

nov2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181103.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181110.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181117.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181124.txt"
)

nov2018 <- lapply(nov2018, subway) %>% bind_rows()

dec2018 <- c(
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181201.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181208.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181215.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181222.txt",
  "http://web.mta.info/developers/data/nyct/turnstile/turnstile_181229.txt"
)

dec2018 <- lapply(dec2018, subway) %>% bind_rows()

subway_riders <- rbind(
  jan2015, feb2015, mar2015, apr2015,
  may2015, jun2015, jul2015, aug2015,
  sep2015, oct2015, nov2015, dec2015,
  jan2016, feb2016, mar2016, apr2016,
  may2016, jun2016, jul2016, aug2016,
  sep2016, oct2016, nov2016, dec2016,
  jan2017, feb2017, mar2017, apr2017,
  may2017, jun2017, jul2017, aug2017,
  sep2017, oct2017, nov2017, dec2017,
  jan2018, feb2018, mar2018, apr2018,
  may2018, jun2018, jul2018, aug2018,
  sep2018, oct2018, nov2018, dec2018
)

subway_riders <- subway_riders[order(subway_riders$scp, 
                                     subway_riders$station, 
                                     subway_riders$unit, 
                                     subway_riders$c_a, 
                                     subway_riders$date_measured, 
                                     -subway_riders$time),]

subway_riders <- subway_riders %>% 
  distinct(scp, station, unit, c_a, date_measured, .keep_all = TRUE)

subway_riders <- subway_riders %>% group_by(scp, station, line) %>%
  arrange(scp, station, line) %>%
  mutate(total_riders = entries - lag(entries, default = first(entries))) %>%
  filter(total_riders > 0 & total_riders < 100000)

subway_riders2 <- subway_riders %>% group_by(date_measured) %>% 
  summarize(totals = sum(total_riders))

citi_riders <- citi_riders %>% rename(date_measured = Date)
citi_riders$date_measured <- mdy(citi_riders$date_measured)
citi_riders <- citi_riders %>% filter(total_citi <= 1000000)

test <- left_join(subway_riders2, citi_riders, by = "date_measured")

test %>% ggplot(aes(totals, total_citi)) + geom_point()
test %>% ggplot(aes(date_measured, totals)) + geom_point()

test %>% ggplot(aes(date_measured, totals))+ geom_point()

test <- test %>% mutate(year = date_measured)

test$year <-test$year %>% str_replace("(\\d{4})-(\\d{2})-(\\d{2})", "\\1")
test$year <- as.numeric(test$year)

test <- test %>% mutate(month = date_measured)

test$month <- test$month %>% str_replace("(\\d{4})-(\\d{2})-(\\d{2})", "\\2")
test$month <- as.numeric(test$month)

test2 <- test %>% group_by(year, month) %>%
  summarize(monthtotal = sum(totals))

test2 %>% ggplot(aes(month, monthtotal)) + geom_point()




## change the date column to a date format
citi_riders$Date <- mdy(citi_riders$Date)

## rename the trips column and add a Month column
citi_riders <- citi_riders %>% rename(total_citibike_trips = `Trips over the past 24-hours (midnight to 11:59pm)`)

citi_riders <- citi_riders %>% rename(date_measured = Date)


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
  ggplot(aes(Year, Riders)) + 
  geom_point(color = "purple", show.legend = FALSE) + 
  geom_line(color = "purple", show.legend = FALSE) + 
  coord_cartesian(ylim = c(5400000, 6000000)) +
  xlab("Year") + ylab("Average Number of Riders") + ggtitle("Weekends") +
  theme_bw()

## put the graphs side by side
grid.arrange(weekdays, weekends, ncol = 2)

min(start_mar2015$Date)
max(end_mar2015$Date)
