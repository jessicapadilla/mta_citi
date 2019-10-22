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
  rename(Business_Line_Group = 'Business Line (group)',
         Business_Line = 'Business Line',
         Revenue_Description = Description1,
         Revenue_Type = 'Revenue and Expense Type',
         Revenue_Cost = Value)

## change all NA values to 0
mta_revenue[is.na(mta_revenue)] = 0

## check the mta revenue table
head(mta_revenue)

## create a graph for subway fare revenue totals each year
## exclude year 2019 since the year is not yet completed
mta_revenue %>% filter(Year <= 2018 & Revenue_Description == "Subway") %>%
  ggplot(aes(Year, Revenue_Cost)) + 
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(breaks = seq(2007, 2018, 1)) + 
  xlab("Year") + ylab("Total Revenue (millions)") +
  ggtitle("MTA Subway Fare Revenue") + 
  theme_bw()

## retrieve annual subway ridership data
subway_riders_url <- "http://web.mta.info/nyct/facts/ridership/"

## get the html code
subway_riders_html <- read_html(subway_riders_url)

## get the tables from the html code
subway_riders_table <- subway_riders_html %>% html_nodes("table")

## find the table
subway_riders_table

## get the subway table
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

## create a graph showing the annual number of subway riders
subway_riders %>% 
  ggplot(aes(Year, Annual_Total)) + 
  geom_point(color = "purple", show.legend = FALSE) + 
  geom_line(color = "purple", show.legend = FALSE) + 
  coord_cartesian(ylim = c(1650000000, 1800000000)) +
  xlab("Year") + ylab("Total Number of Riders") + ggtitle("Annual Subway Ridership") +
  theme_bw()

## retrieve subway totals data
subway_totals <- read_csv("https://github.com/jessicapadilla/mta-mismanagement/raw/master/subway_service_delivered.csv")

## check the structure of the table
str(subway_totals)

## remove unwanted columns and rename some columns
subway_totals <- subway_totals %>% select(-division) %>%
  rename(Month = "month", Line = "line",
         Scheduled_Trains = "num_sched_trains", Actual_Trains = "num_actual_trains",
         Ratio_Scheduled_to_Actual_Trains = "service delivered")

## add a year column with the values from the month column
subway_totals <- subway_totals %>% mutate(Year = Month)

## edit the year column by removing the month and change the year to numbers
subway_totals$Year <- subway_totals$Year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway totals table
head(subway_totals)

## create a graph showing the total number of subways each year
## exclude year 2019 since the year is not yet completed
subway_totals %>% filter(Year < 2019) %>% 
  mutate(Year = factor(Year)) %>% 
  ggplot(aes(Scheduled_Trains, Actual_Trains, col = Line)) + 
  geom_point(size = 2) + facet_grid(. ~ Year) +
  scale_color_discrete(name = "Subway Line") + xlim(0, 4000) + ylim(0, 4000) +
  xlab("Scheduled Number of Trains") + ylab("Actual Number of Trains") +
  ggtitle("Total Number of Subways from 2015-2018") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## retrieve subway incidents data
subway_incidents <- read_csv("https://github.com/jessicapadilla/mta-mismanagement/raw/master/subway_major_incidents.csv")

## check the structure of the subway incidents table
str(subway_incidents)

## remove the division column and rename columns
subway_incidents <- subway_incidents %>% select(-division) %>% 
  rename(Month = month, Line = line, Type_of_Issue = category, Count = count)

## add a year column using the values from the month column
subway_incidents <- subway_incidents %>% mutate(Year = Month)

## edit the year column by removing the month and change the year to numbers
subway_incidents$Year <- subway_incidents$Year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the different types of issues
unique(subway_incidents$Type_of_Issue)

## make the persons on trackbed/police/medical text shorter
subway_incidents$Type_of_Issue <- subway_incidents$Type_of_Issue %>%
  str_replace("Persons on Trackbed/Police/Medical", "Passenger")

## reorder the types of issue
subway_incidents$Type_of_Issue <- factor(subway_incidents$Type_of_Issue, 
                                         levels = c("Signals", "Stations and Structure", 
                                                    "Subway Car", "Track", 
                                                    "Passenger", "Other"))

## check the subway incidents table
head(subway_incidents)

## create a graph showing the number of subway-related incidents per year
## exclude 2019 since the year is not yet completed
subway_incidents %>% filter(Year < 2019 & Type_of_Issue %in% c("Signals", "Stations and Structure", "Subway Car", "Track")) %>%
  group_by(Year, Type_of_Issue) %>%
  summarize(Total_Count = sum(Count)) %>%
  ggplot(aes(Year, Total_Count)) + 
  geom_bar(stat = "identity", fill = "black") + 
  coord_cartesian(ylim = c(0, 300)) +
  facet_wrap(.~Type_of_Issue, ncol = 2) +
  xlab("Year") + ylab("Total Number of Incidents") +
  ggtitle("Subway Incidents from 2015-2018") + theme_bw()

## retrieve subway platform times data
subway_platform_times <- read_csv("https://github.com/jessicapadilla/mta-mismanagement/raw/master/subway_platform_time.csv")

## check the structure of the subway platform table
str(subway_platform_times)

## edit and rename some columns
subway_platform_times <- subway_platform_times %>% select(-"division") %>% 
  rename(Month = "month", Line = "line", Period = "period",
         Total_Passengers = "num_passengers", Additional_Platform_Time = "additional platform time")

## edit the period column
subway_platform_times$Period <- subway_platform_times$Period %>%
  str_replace("offpeak", "Off-Peak") %>% str_replace("peak", "Peak")

## add a year column by adding values from the month column
subway_platform_times <- subway_platform_times %>% mutate(Year = Month)

## edit the year column by removing the month and change the year to numbers
subway_platform_times$Year <- subway_platform_times$Year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway platform table
head(subway_platform_times)

## create a graph for additional platform times
## exclude 2019 since the year is not yet completed
add_platform <- subway_platform_times %>% filter(Year < 2019) %>% group_by(Year) %>%
  summarize(Average_Additional_Platform = mean(Additional_Platform_Time)) %>%
  ggplot(aes(Year, Average_Additional_Platform)) +
  geom_point(color = "purple") + geom_line(color = "purple") +
  coord_cartesian(ylim = c(0, 2.0)) +
  xlab("Year") + ylab("Minutes") + ggtitle("Average Additional Platform Time") +
  theme_bw()

## retrieve subway train times data
subway_train_times <- read_csv("https://github.com/jessicapadilla/mta-mismanagement/raw/master/subway_train_time.csv")

## check the structure of the subway train times table
str(subway_train_times)

## edit the columns
subway_train_times <- subway_train_times %>% select(-"division") %>% 
  rename(Month = "month", Line = "line", Period = "period",
         Total_Passengers = "num_passengers", 
         Additional_Train_Time = "additional train time")

## edit the period column
subway_train_times$Period <- subway_train_times$Period %>%
  str_replace("offpeak", "Off-Peak") %>% str_replace("peak", "Peak")

## add a year column using the values from the month column
subway_train_times <- subway_train_times %>% mutate(Year = Month)

## edit the year column by removing the month and change the year to numbers
subway_train_times$Year <- subway_train_times$Year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway train times table
head(subway_train_times)

## create a graph for additional train times
## exclude 2019 since the year is not yet completed
add_train <- subway_train_times %>% filter(Year < 2019) %>% group_by(Year) %>%
  summarize(Average_Additional_Train = mean(Additional_Train_Time)) %>%
  ggplot(aes(Year, Average_Additional_Train)) +
  geom_point(color = "purple") + geom_line(color = "purple") +
  coord_cartesian(ylim = c(0, 2.0)) +
  xlab("Year") + ylab("Minutes") + ggtitle("Average Additional Train Time") +
  theme_bw()

## put the additional time graphs side by side
grid.arrange(add_platform, add_train, ncol = 2)

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
    filter(!DESC == "RECOVR AUD") %>%            ## remove rows with RECOVR AUD, which means there was an error with the device during that time period
    filter(TIME >= 72000) %>%                    ## filter the data for 20:00 and after
    select(-c(EXITS, DESC)) %>%                  ## remove unnecessary columns
    rename(C_A = `C/A`,                          ## rename columns
           Unit = UNIT, 
           Scp = SCP, 
           Station = STATION, 
           Line = LINENAME,
           Division = DIVISION, 
           Date = DATE, 
           Time = TIME,
           Entries = ENTRIES) 
  revised$Date <- mdy(revised$Date)              ## convert the date column to dates
  revised$Entries <- as.numeric(revised$Entries) ## convert the entries column to numbers 
  return(revised)
}

## get subway data for beginning of jan 2015
start_jan2015 <- "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150103.txt" %>% subway()

## get subway data for end of jan 2015
end_jan2015 <- "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150207.txt" %>% subway()

## combine all the jan 2015 data
## filter it to include data from the beginning and end of january
jan2015 <- rbind(start_jan2015, end_jan2015) %>% 
  filter(Date == "2015-01-01" | Date == "2015-01-31")

## arrange the data to include counts from the same device (scp)
## create a column to calculate the difference between counts at the beginning and end of the month
## remove the time column
jan2015 <- jan2015 %>% arrange(Scp, Station, Line) %>% spread(Date, Entries) %>% 
  rename(Start_Passengers = "2015-01-01", End_Passengers = "2015-01-31") %>%
  mutate(Total_Passengers = End_Passengers - Start_Passengers) %>%
  select(-Time)

## check the table
head(jan2015)

## get subway data for beginning of feb 2015
start_feb2015 <- "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150207.txt" %>% subway()

## get subway data for end of feb 2015
end_feb2015 <- "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150307.txt" %>% subway()

## combine all the feb 2015 data
## filter it to include data from the beginning and end of february
feb2015 <- rbind(start_feb2015, end_feb2015) %>% 
  filter(Date == "2015-02-01" | Date == "2015-02-28")

## arrange the data to include counts from the same device (scp)
## create a column to calculate the difference between counts at the beginning and end of the month
## remove the time column
feb2015 <- feb2015 %>% arrange(Scp, Station, Line) %>% spread(Date, Entries) %>% 
  rename(Start_Passengers = "2015-02-01", End_Passengers = "2015-02-28") %>%
  mutate(Total_Passengers = End_Passengers - Start_Passengers) %>%
  select(-Time)

## check the table
head(feb2015)

## get subway data for beginning of mar 2015
start_mar2015 <- "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150307.txt" %>% subway()

## get subway data for end of mar 2015
end_mar2015 <- "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150404.txt" %>% subway()

## combine all the mar 2015 data
## filter it to include data from the beginning and end of march
## remove the time column
mar2015 <- rbind(start_mar2015, end_mar2015) %>% 
  filter(Date == "2015-03-01" | Date == "2015-03-31") %>% select(-Time)

## arrange the data to include counts from the same device (scp)
## create a column to calculate the difference between counts at the beginning and end of the month
mar2015 <- mar2015 %>% arrange(Scp, Station, Line) %>% spread(Date, Entries) %>% 
  rename(Start_Passengers = "2015-03-01", End_Passengers = "2015-03-31") %>%
  mutate(Total_Passengers = End_Passengers - Start_Passengers)

## check the table
head(mar2015)

## get subway data for beginning of apr 2015
start_apr2015 <- "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150404.txt" %>% subway()

## get subway data for end of apr 2015
end_apr2015 <- "http://web.mta.info/developers/data/nyct/turnstile/turnstile_150502.txt" %>% subway()

## combine all the apr 2015 data
## filter it to include data from the beginning and end of april
apr2015 <- rbind(start_apr2015, end_apr2015) %>% 
  filter(Date == "2015-04-01" | Date == "2015-04-30")

## arrange the data to include counts from the same device (scp)
## create a column to calculate the difference between counts at the beginning and end of the month
## remove the time column
apr2015 <- apr2015 %>% arrange(Scp, Station, Line) %>% spread(Date, Entries) %>% 
  rename(Start_Passengers = "2015-04-01", End_Passengers = "2015-04-30") %>%
  mutate(Total_Passengers = End_Passengers - Start_Passengers) %>% select(-Time)

## check the table
head(apr2015)







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



annual_ridership %>% ggplot(aes(Year, Company, col = Annual_Total)) + 
  geom_point(size = 2)

citi_riders %>% ggplot(aes(Year, Total_Trips_or_Passengers)) + geom_jitter()

uber_riders %>% ggplot(aes(Year, Total_Trips_or_Passengers, col = Month_Name)) + 
  geom_jitter()

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
