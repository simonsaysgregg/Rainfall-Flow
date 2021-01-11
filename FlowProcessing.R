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
