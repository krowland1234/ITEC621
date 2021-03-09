Atif Sahin, Josh, Kevin Gold, Abass, Kevin Rowland
ITEC 621
Final Project Spring 2021

#install and load all libraries needed for project
install.packages("readr")
library(readr)

install.packages("datetime")
library(datetime)

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

#read Covid Dataset, Jets ETF (labeled JetsETF), and Entertainment ETF (labeled FunETF)  
Covid <- read_csv("Covid.csv")
JetsETF <- read_csv("Jets.csv")
FunETF <- read_csv("Fun.csv")


#View Covid Dataset
str(Covid)
View(Covid)
#The Covid dataset has 8 variables. 
# 1st variable: SNo, the key variable of the dataset. A unique reporting ID number for each report. 
# 2nd variable: observation date, is the date each data was reported. It's in a character format and will be recoded into a datetime format from the datetime package. 
# 3rd variable: Province/State, is the state or province of a specific country the report comes from. We probably won't use Province/State in this report much, as our report will focus more on the country level. 
# 4th variable: Country/Region. 
# 5th variable: Last Update - The date the data was last adjusted. We won't use this. 
# 6th variable: Confirmed cases - Our main dataset
# 7th variable: Deaths - The number of deaths
# 8th variable: Recovered- The number of recovered patients 

# change observationdate to date and check 
Covid$ObservationDate <- as.Date(Covid$ObservationDate, "%m/%d/%Y")
class(Covid$ObservationDate)

#groupby Covid$

Covid_bydate <- Covid %>% 
  group_by(ObservationDate) %>%
  summarise(total_case = sum(Confirmed))

#visualize rise in Covid-19 cases worldwide with simple line plot
ggplot(data = Covid_bydate, aes(x = ObservationDate, y = total_case)) +
  geom_line(color = "red", linetype = "dashed") +
  scale_y_continuous(name = "Confirmed Cases", labels = scales::comma) + 
  scale_x_date(name = "Date") + 
  ggtitle("Total Confirmed Covid-19 Cases Worldwide")


#plot new cases everyday ((n cases) - (n-1 cases))
Covid_bydate$dailychange <- (Covid_bydate$total_case - lag(Covid_bydate$total_case)) / lag(Covid_bydate$total_case)


#visualize Covid-19 daily rate change 
ggplot(data = Covid_bydate, aes(x = ObservationDate, y = dailychange)) +
  geom_line(color = "red", linetype = "solid") +
  scale_y_continuous(name = "Confirmed Cases % Change", labels = scales::comma) + 
  scale_x_date(name = "Date") + 
  ggtitle("Covid-19 Cases % Change")


#take ETF data and change date column to date  
FunETF$Date <- as.Date(FunETF$Date, "%m/%d/%Y")
JetsETF$Date <- as.Date(JetsETF$Date, "%m/%d/%Y")

#replace ETF name spaces with underscores 
names(FunETF) <- gsub(" ", "_", names(FunETF))
names(JetsETF) <- gsub(" ", "_", names(JetsETF))

#plot daily change in covid cases & daily change in ETF prices, as a %
ggplot(NULL, aes(x, y)) +
  geom_line(data = Covid_bydate, aes(x = ObservationDate, y = dailychange, colour = "Covid"), size = 1) +
  geom_line(data = FunETF, aes(x = Date, y = Daily_Return, colour = "FunETF"), size = 1) + 
  geom_line(data = JetsETF, aes(x = Date, y = Daily_Return, colour = "JetsETF"), size = 1) + 
  scale_color_manual("", 
                     breaks = c("Covid", "FunETF", "JetsETF"),
                     values = c("red", "green4", "blue")) +
  ggtitle("% Change Covid Cases vs. ETF Prices") + xlab("Date") + ylab("% Change") + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"))





