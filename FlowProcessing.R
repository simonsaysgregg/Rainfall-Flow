## CREATION OF FLOW FILE CALDONIA WETLAND ANALYSIS
## DATA PERIOD 10/01/18 - 11/31/20
## data location in PROJECT DIRECTORY
## dataset preparation
## 1st in processing order

## Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
## Statistical analysis
require("statsr")        # Lots of stats stuff
require("stats")        # Lots of stats stuff
## Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("TTR")
#require("xlsx")        # creates errors # Reads and writes to xlsx file
require("purrr")
require("tidyr")
require("fBasics")
require("pls")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package


## Read file FLOWDATA_TOTAL.CSV
Flow <- read.csv("C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/FLOWDATA_TOTAL.csv")
## Rename columns
colnames(Flow) <- c("month", "year", "mgd", "cfs", "type")

##View(Flow)

## Combine month year columns
y <- Flow$year
m <- Flow$month

Flow$Date <- as.yearmon(paste(Flow$year, Flow$month), "%Y %m")
##View(Flow)

## Select desired columns
WorkingFlow <- Flow %>%
  select(Date,
         type,
         cfs) 
##View(WorkingFlow)

WorkingFlow2 <- WorkingFlow %>%
  left_join( rain.sum3, by = "Date")
View(WorkingFlow2)
## Rename columns
colnames(WorkingFlow2) <- c("Date", "type", "cfs", "total")

## Plot
ggplot(WorkingFlow, aes(x = Date, y = cfs, color = type))+
  geom_line()+
  ggtitle("Wetland Flow")+
  labs(y = "Flow (cfs)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

ggplot(data = WorkingFlow2, aes(x = Date))+
  ggtitle("Wetland Flow")+
  geom_line(aes(y = cfs, colour = type), size = 1.1)+
  geom_point(aes(y = total/4), size = 1.1)+
  # scale_color_manual(values = c("#e41a1c", 
  #                               "#377eb8",
  #                               "#4daf4a"))+
  scale_y_continuous(sec.axis = sec_axis(~.*4, name = "Rainfall Total (in)"))+
  labs(y = "Flow (cfs)", x = "Date")+
  # theme_classic()+
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        plot.title = element_text(size = 20, 
                                  hjust = 0.5),
        axis.title = element_text(size = 18),
        axis.line = element_blank(),
        axis.text = element_text(size = 14))


## Subset values for datasets
minflow <- WorkingFlow %>%
  subset(type == "MIN") %>%
  select(Date,
         cfs)

maxflow <- WorkingFlow %>%
  subset(type == "MAX") %>%
  select(Date,
         cfs)

avgflow <- WorkingFlow %>%
  subset(type == "AVG") %>%
  select(Date,
         cfs)

## Write file FLow DS
write.csv(minflow, "C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/minflow.csv")
write.csv(maxflow, "C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/maxflow.csv")
write.csv(avgflow, "C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/avgflow.csv")



## Flow Time Series
## View(avgflow)

## Create Time Series
## Use in matching observations
## 'by' 1-min interval
ts <- seq(ymd_hm("2018-10-01 00:00"), ymd_hm("2020-12-01 10:00"), by = 60) 
## Make data frame
ts.df <- data.frame(timestamp=ts)
##View(ts.df)

ts.df.1 <- ts.df %>%
  mutate(Date = date(timestamp),
         hour = hour(timestamp),
         minute = minute(timestamp),
         time = paste(hour, minute, sep = ":"))

## Combine month year columns
y <- year(ts.df.1$timestamp)
m <- month(ts.df.1$timestamp)

ts.df.1$Datematch <- as.yearmon(paste(y, m), "%Y %m")
##View(ts.df.1)

## Flow file

timeseries <- ts.df.1 %>%
  select(Date,
         time,
         Datematch) 

colnames(timeseries) <- c("Date1", "time", "Date")

timeseries1 <- timeseries %>%
  left_join(avgflow, by = "Date")
View(timeseries1)

flowfile <- timeseries1 %>%
  select(Date1,
         time,
         cfs)
colnames(flowfile) <- c("Date", "time", "cfs")
View(flowfile)

## Write file FLow DS
write.csv(flowfile, "C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/AvgFlow_timeseries.csv")


## rAINFALL CONTRIBUTION TO ANNUAL FLOW
## 48 mONTH dATA

monthly <- read.csv("C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/CRONOSresults_KRWI48.csv", skip = 14)
## Rename columns
colnames(monthly) <- c("date", "omit", "rainfall")

## Select
monthly1 <- monthly %>%
  select(date,
         rainfall)
View(monthly1)

monthly1$date <- my(monthly1$date)

monthly2 <- monthly1 %>%
  mutate(year = year(date),
         month = month(date))
View(monthly2)

## Select
monthly3 <- monthly2 %>%
  select(year,
         month,
         rainfall)
View(monthly3)

## Combine month year columns
y1 <- monthly3$year
m1 <- monthly3$month

monthly3$Date <- as.yearmon(paste(monthly3$year, monthly3$month), "%Y %m")
View(monthly3)

## Select
monthly4 <- monthly3 %>%
  select(Date,
         rainfall)
View(monthly4)

## calculation
area <- 8.11 + 8.045 + 0.57 + 0.477 + 0.4646 + 0.4422 + 0.4960 + 0.4957 + 0.4867 + 0.6663 + 0.6557 + 0.4644 + 0.4721 + 0.5027 + 0.4626 + 0.5043 + 0.4601 + 0.6740 + 0.5488 + 0.4603 + 0.4707 + 0.4503 + 0.4413 + 0.4408 + 0.4468 + 0.5834 + 0.5973 + 0.4796 + 0.4726 + 0.4639 + 0.4644 + 0.4790 + 0.4390 + 0.5789
acft_to_gal <- (43560 * 7.481)/(10^6)
est_acc <- monthly4 %>%
  mutate(acc = ((area * rainfall/12))*acft_to_gal)
View(est_acc)

## read avg flow
aveflow <- read.csv("C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/avgflow.csv")
View(aveflow)

## Conversion of ave flow to MGD
cfs_to_MGD <- 0.646317
flowavg <- aveflow %>%
  select(Date,
         cfs) %>%
  mutate(MGD = cfs * cfs_to_MGD)
View(flowavg)

flowavg$Date <- my(flowavg$Date)

flowavg <- flowavg %>%
  mutate(year = year(Date),
         month = month(Date))
View(flowavg)


## Combine month year columns
y2 <- flowavg$year
m2 <- flowavg$month

flowavg$Date <- as.yearmon(paste(y2, m2), "%Y %m")
View(flowavg)


## MGD to Monthly flow MG
days <- c(31,30,31,31,28,31,30,31,30,31,31,30,31,30,31,31,29,31,30,31,30,31,31,30,31,30)
flowavg$days <- days
monthlyflow <- flowavg %>%
  mutate(MG = days * MGD) %>%
  select(Date,
         MG)
View(monthlyflow)

flow_cont <- est_acc %>%
  left_join(monthlyflow, by = "Date") %>%
  subset(!is.na(MG)) %>%
  mutate(percent = (acc/MG)*100 )
View(flow_cont)


## write
write.csv(flow_cont, "C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/Rainfall_Contribution.csv")
