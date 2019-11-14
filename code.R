## load libraries
library(tidyverse)
library(rvest)
library(gridExtra)
library(RColorBrewer)
library(ggpubr)

## retrieve the mta revenue data
mta_revenue <- read_csv("https://github.com/jessicapadilla/problem_with_mta/raw/master/mta_revenue.csv")

## check the structure of the mta revenue data
str(mta_revenue)

## select needed columns and rename them
mta_revenue <- mta_revenue %>% 
  select(c("Business Line (group)", "Business Line",
           "Description1", "Revenue and Expense Type",
           "Year", "Value")) %>%
  rename(business_line_group = "Business Line (group)",
         business_line = "Business Line",
         revenue_description = "Description1",
         revenue_type = "Revenue and Expense Type",
         mta_year = "Year",
         revenue_value = "Value")

## change all NA values in the revenue value column to 0
mta_revenue$revenue_value[is.na(mta_revenue$revenue_value)] = 0

## check the mta revenue data
head(mta_revenue)

## create a graph for subway fare revenue totals each year
## exclude year 2019 since the year is not yet completed
mta_revenue %>% filter(mta_year <= 2018 & revenue_description == "Subway") %>%
  ggplot(aes(mta_year, revenue_value)) + 
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(breaks = seq(2007, 2018, 1)) + 
  xlab("Year") + ylab("Total Revenue (millions)") +
  ggtitle("MTA Subway Fare Revenue") + 
  theme_bw()

## set the link for subway ridership data
subway_ridership_url <- "http://web.mta.info/nyct/facts/ridership/"

## get the html code from the webpage
subway_ridership_html <- read_html(subway_ridership_url)

## get the html nodes
subway_ridership_nodes <- subway_ridership_html %>% html_nodes("table")

## find the subway table
subway_ridership_nodes

## turn the table to a data frame
subway_ridership <- subway_ridership_nodes[[2]] %>% html_table

## check the structure of the subway ridership data
str(subway_ridership)

## select the needed columns
## rename the year column
## then convert the data to a tibble
subway_ridership <- subway_ridership %>% 
  select(c("Year", "Average Weekday", "Average Weekend")) %>% 
  rename(mta_year = "Year") %>% as_tibble()

## gather the columns
subway_ridership <- subway_ridership %>% 
  gather(category, totals, "Average Weekday":"Average Weekend")

## remove commas from the totals column and convert it to numbers
subway_ridership$totals <- subway_ridership$totals %>%
  str_replace_all(",", "") %>% as.numeric()

## create a graph for average weekday subway ridership
weekday <- subway_ridership %>% 
  filter(category == "Average Weekday") %>%
  ggplot(aes(mta_year, totals)) + 
  geom_point(color = "purple") + geom_line(color = "purple") +
  scale_x_continuous(breaks = seq(2013, 2018, 1)) + 
  coord_cartesian(ylim = c(5300000, 6000000)) +
  xlab("Year") + ylab("Number of Subway Riders") +
  ggtitle("Average Weekday Ridership") +
  theme_bw()

## create a graph for average weekend subway ridership
weekend <- subway_ridership %>% 
  filter(category == "Average Weekend") %>%
  ggplot(aes(mta_year, totals)) + 
  geom_point(color = "red") + geom_line(color = "red") +
  scale_x_continuous(breaks = seq(2013, 2018, 1)) + 
  coord_cartesian(ylim = c(5300000, 6000000)) +
  xlab("Year") + ylab("Number of Subway Riders") +
  ggtitle("Average Weekend Ridership") +
  theme_bw()

## put the ridership graphs side by side
grid.arrange(weekday, weekend, ncol = 2)

## retrieve subway incidents data
subway_incidents <- read_csv("https://github.com/jessicapadilla/problem_with_mta/raw/master/subway_major_incidents.csv")

## check the structure of the subway incidents table
str(subway_incidents)

## remove the division column and rename the category column
subway_incidents <- subway_incidents %>% select(-division) %>% 
  rename(type_of_issue = category)

## add a year column using the values from the month column
subway_incidents <- subway_incidents %>% mutate(mta_year = month)

## edit the year column by removing the month
## change the year to numbers
subway_incidents$mta_year <- subway_incidents$mta_year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the different types of issues
unique(subway_incidents$type_of_issue)

## reorder the types of issue
subway_incidents$type_of_issue <- factor(subway_incidents$type_of_issue,
                                 levels = c("Signals", 
                                            "Stations and Structure", 
                                            "Subway Car", "Track", 
                                            "Persons on Trackbed/Police/Medical", "Other"))

## check the subway incidents table
head(subway_incidents)

## create a graph showing the number of subway incidents per year
## exclude 2019 since the year is not yet completed
subway_incidents %>% 
  filter(mta_year <= 2018 & type_of_issue %in% 
           c("Signals", "Stations and Structure", "Subway Car", "Track")) %>%
  group_by(mta_year, type_of_issue) %>%
  summarize(total_count = sum(count)) %>%
  ggplot(aes(mta_year, total_count)) + 
  geom_bar(stat = "identity", fill = "black") +
  coord_cartesian(ylim = c(0, 300)) +
  facet_wrap(.~type_of_issue, ncol = 2) +
  xlab("Year") + ylab("Total Number of Incidents") +
  ggtitle("Subway Incidents") + theme_bw()

## retrieve subway platform times data
subway_platform_times <- read_csv("https://github.com/jessicapadilla/problem_with_mta/raw/master/subway_platform_time.csv")

## check the structure of the subway platform table
str(subway_platform_times)

## remove the division and number of passengers columns
## rename the platform time column
subway_platform_times <- subway_platform_times %>% 
  select(-c("division", "num_passengers")) %>% 
  rename(additional_platform_time = "additional platform time")

## add a year column by using the values from the month column
subway_platform_times <- subway_platform_times %>% mutate(mta_year = month)

## edit the year column by removing the month and change the year to numbers
subway_platform_times$mta_year <- subway_platform_times$mta_year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway platform table
head(subway_platform_times)

## create a graph for additional platform times
## exclude 2019 since the year is not yet completed
add_platform <- subway_platform_times %>% 
  filter(mta_year <= 2018) %>% group_by(mta_year) %>%
  summarize(average_additional_platform = mean(additional_platform_time)) %>%
  ggplot(aes(mta_year, average_additional_platform)) +
  geom_point(color = "purple") + geom_line(color = "purple") +
  coord_cartesian(ylim = c(0.75, 1.75)) +
  xlab("Year") + ylab("Minutes") + 
  ggtitle("Average Additional Platform Time") +
  theme_bw()

## retrieve subway train times data
subway_train_times <- read_csv("https://github.com/jessicapadilla/problem_with_mta/raw/master/subway_train_time.csv")

## check the structure of the subway train times table
str(subway_train_times)

## remove the division and number of passengers columns
## rename the train time column
subway_train_times <- subway_train_times %>% 
  select(-c("division", "num_passengers")) %>% 
  rename(additional_train_time = "additional train time")

## add a year column by using the values from the month column
subway_train_times <- subway_train_times %>% mutate(mta_year = month)

## edit the year column by removing the month and change the year to numbers
subway_train_times$mta_year <- subway_train_times$mta_year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway train times table
head(subway_train_times)

## create a graph for additional train times
## exclude 2019 since the year is not yet completed
add_train <- subway_train_times %>% 
  filter(mta_year <= 2018) %>% group_by(mta_year) %>%
  summarize(average_additional_train = mean(additional_train_time)) %>%
  ggplot(aes(mta_year, average_additional_train)) +
  geom_point(color = "red") + geom_line(color = "red") +
  coord_cartesian(ylim = c(0.75, 1.75)) +
  xlab("Year") + ylab("Minutes") + 
  ggtitle("Average Additional Train Time") +
  theme_bw()

## put the additional time graphs side by side
grid.arrange(add_platform, add_train, ncol = 1)

## retrieve subway delivered data
subway_delivered <- read_csv("https://github.com/jessicapadilla/problem_with_mta/raw/master/subway_service_delivered.csv")

## check the structure of the subway delivered table
str(subway_delivered)

## remove the division column and rename some columns
subway_delivered <- subway_delivered %>% select(-division) %>%
  rename(scheduled_trains = num_sched_trains, 
         actual_trains = num_actual_trains)

## add a year column by using the values from the month column
subway_delivered <- subway_delivered %>% mutate(mta_year = month)

## edit the year column by removing the month and change the year to numbers
subway_delivered$mta_year <- subway_delivered$mta_year %>% 
  str_replace("^(\\d{4})-(\\d{2})$", "\\1") %>% as.numeric()

## check the subway delivered times table
head(subway_delivered)

## create a graph showing the number of subways each year
## exclude year 2019 since the year is not yet completed
subway_delivered %>% filter(mta_year <= 2018) %>% 
  mutate(mta_year = factor(mta_year)) %>% 
  ggplot(aes(scheduled_trains, actual_trains, col = line)) + 
  geom_point() + facet_grid(. ~ mta_year) +
  scale_color_discrete(name = "Subway Line") + xlim(0, 3500) + ylim(0, 3500) +
  xlab("Scheduled Number of Trains") + ylab("Actual Number of Trains") +
  ggtitle("Total Number of Subways") + theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## retrieve mta budget data
mta_budget <- read_csv("https://github.com/jessicapadilla/problem_with_mta/raw/master/mta_budget.csv")

## check the structure of the mta budget table
str(mta_budget)

## remove unwanted columns and rename some columns
mta_budget <- mta_budget %>% 
  select(c("Business Line", "Description1", 
           "Revenue and Expense Type",
           "Year", "Value")) %>%
  rename(business_line = "Business Line", 
         expense_description = "Description1",
         expense_type = "Revenue and Expense Type",
         mta_year = "Year",
         expense_cost = "Value")

## check how many expense types there are
unique(mta_budget$expense_type)

## rename the expense types
mta_budget$expense_type <- mta_budget$expense_type %>%
  str_replace("Labor Expense", "Labor") %>%
  str_replace("Non-labor Expense", "Non-Labor") %>%
  str_replace("Debt Service", "Debt") %>%
  str_replace("Other Expense Adjustments", "Other")

## check how many expense descriptions there are for each expense type
unique(mta_budget$expense_description[mta_budget$expense_type == "Labor"])
unique(mta_budget$expense_description[mta_budget$expense_type == "Non-Labor"])
unique(mta_budget$expense_description[mta_budget$expense_type == "Debt"])
unique(mta_budget$expense_description[mta_budget$expense_type == "Other"])

## rename some of the expense descriptions
mta_budget <- mta_budget %>% mutate(expense_description = case_when(
  expense_type == "Labor" & expense_description == "Payroll" ~ "Payroll",
  expense_type == "Labor" & expense_description == "Overtime" ~ "Overtime",
  expense_type == "Labor" & expense_description == "Health and Welfare" ~ "Health and Welfare",
  expense_type == "Labor" & expense_description %in% c("OPEB Current Payment", "Pensions", "Pensions Offset", "Other Fringe Benefits") ~ "Pensions and Other Benefits",
  expense_type == "Labor" & expense_description == "Reimbursable Overhead" ~ "Reimbursable Overhead",
  expense_type == "Non-Labor" & expense_description %in% c("Electric Power", "Fuel") ~ "Electricity and Fuel",
  expense_type == "Non-Labor" & expense_description %in% c("Insurance", "Claims", "Other Business Expenses") ~ "Other Expenses",
  expense_type == "Non-Labor" & expense_description %in% c("Paratransit Service Contracts", "Maintenance and Other Operating Contracts", "Professional Service Contracts") ~ "Contracts",
  expense_type == "Non-Labor" & expense_description == "Materials and Supplies" ~ "Materials and Supplies",
  expense_type == "Debt Services" & expense_description %in% c("Total MTA Bus Debt Service", "Total CRR Debt Service", "Total NYCT Debt Service", "Total SIRTOA Debt Service", "Total MTA HQ Debt Service for 2 Broadway Certificates of Participation", "Total B&T Debt Service", "BAB Subsidy") ~ "Debt Service"))

## check the mta budget table
head(mta_budget)

## create a graph for mta budget totals each year
## exclude any data after 2019 and any expenses with a value of 0 or below
mta_budget %>% group_by(mta_year, expense_type) %>% 
  filter(mta_year <= 2019 & expense_cost > 0) %>%
  summarize(total_cost = sum(expense_cost)) %>%
  ggplot(aes(mta_year, total_cost)) + 
  geom_bar(stat = "identity", fill = "black") +
  scale_x_continuous(breaks = seq(2007, 2019, 1)) +
  xlab("Year") + ylab("Total Budget (millions)") +
  ggtitle("MTA Budget") + 
  theme_bw()

## create a line graph for breakdown of mta budget
## exclude any data after 2019
mta_budget %>% group_by(mta_year, expense_type) %>% 
  filter(mta_year <= 2019) %>%
  summarize(total_cost = sum(expense_cost)) %>%
  ggplot(aes(mta_year, total_cost, col = expense_type)) + 
  geom_point() + geom_line() +
  scale_color_manual(values = c("#6600FF","#FF0099", "#0000FF", "#FF00FF"), 
                     name = "Expense Type") +
  scale_x_continuous(breaks = seq(2007, 2019, 1)) + 
  xlab("Year") + ylab("Total Expense Cost (millions)") + 
  ggtitle("Breakdown of MTA Budget") +
  theme_bw()

## set up the amount of colors needed for a bar plot
nb.cols <- 13

## assign the number of colors to a palette
mycolors <- colorRampPalette(brewer.pal(8, "BuPu"))(nb.cols)

## create a graph for labor expenses
## exclude any data after 2019
labor <- mta_budget %>% 
  filter(expense_type == "Labor" & mta_year <= 2019) %>% 
  mutate(mta_year = factor(mta_year)) %>%
  group_by(mta_year, expense_description) %>%
  summarize(total_expense_cost = sum(expense_cost)) %>% 
  filter(total_expense_cost > 0) %>%
  mutate(expense_description = reorder(expense_description, -total_expense_cost)) %>%
  ggplot(aes(expense_description, total_expense_cost, fill = mta_year)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  
  ylim(0, 6000) +
  scale_fill_manual(values = mycolors, name = "Year") +
  ylab("Cost of Expense (millions)") +
  ggtitle("MTA Labor Expenses") + theme_bw() +
  theme(axis.title.x = element_blank())

## create a graph for non-labor expenses
## exclude any data after 2019
non_labor <- mta_budget %>% 
  filter(expense_type == "Non-Labor" & mta_year <= 2019) %>% 
  mutate(mta_year = factor(mta_year)) %>%
  group_by(mta_year, expense_description) %>%
  summarize(total_expense_cost = sum(expense_cost)) %>% 
  filter(total_expense_cost > 0) %>%
  mutate(expense_description = reorder(expense_description, -total_expense_cost)) %>%
  ggplot(aes(expense_description, total_expense_cost, fill = mta_year)) + 
  geom_bar(position = "dodge", stat = "identity", colour = "black") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +  
  ylim(0, 6000) + 
  scale_fill_manual(values = mycolors, name = "Year") +
  ylab("Cost of Expense (millions)") +
  ggtitle("MTA Non-Labor Expenses") + theme_bw() +
  theme(axis.title.x = element_blank())

## arrange the graphs side by side
ggarrange(labor, non_labor, ncol = 2, nrow = 1, 
          common.legend = TRUE, legend = "bottom")
