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
  mutate(total_riders = entries - lag(entries, default = first(entries)))

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

min(start_mar2015$Date)
max(end_mar2015$Date)
