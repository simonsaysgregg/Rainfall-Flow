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

## Create Time Series
## Use in matching observations
## 'by' 60-min interval
ts <- seq(ymd_hm("2018-10-01 00:00"), ymd_hm("2020-12-01 10:00"), by = 60) 
## Make data frame
ts.df <- data.frame(timestamp=ts)
##View(ts.df)

## JOIN RAINFALL DATA FROM ROANOKE RAPIDS
## Read file CRONOSresults_KRWI_ROANOKE RAPIDS
rain.DS <- read.csv("C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/CRONOSresults_KRWI_ROANOKE RAPIDS.csv", skip = 14)
## Rename columns
colnames(rain.DS) <- c("timestamp", "rain")
## Reformat dates
rain.DS$timestamp <- mdy_hm(rain.DS$timestamp, tz = "GMT")
##View(rain)
## Join rainfall data
DS <- left_join(ts.df, rain.DS, by = "timestamp")
## Convert values to numeric
DS$rain <- as.numeric(DS$rain)
##View(DS)

## CLEAN DATASET
DS.sub <- subset(DS, rain != "missing")
DS.sub <- subset(DS.sub, rain != "Failed QC")
##View(DS.sub)

## Plot
ggplot(DS.sub, aes(timestamp, rain))+
  geom_point()

DS.sub2 <- subset(DS.sub, rain > 0)
##View(DS.sub2)

## Plot
ggplot(DS.sub2, aes(timestamp, rain))+
  geom_point()

## Monthly summarize
DS.sum <- DS.sub2 %>%
  mutate(month = month(timestamp),
         year = year(timestamp))

## Monthly sum 2018
MonAccu18 <- DS.sum %>%
  subset(year == 2018) %>%
  group_by(month) %>%
  summarize(sum = sum(rain, na.rm = TRUE))

## Monthly sum 2019
MonAccu19 <- DS.sum %>%
  subset(year == 2019) %>%
  group_by(month) %>%
  summarize(sum = sum(rain, na.rm = TRUE))

## Monthly sum 2020
MonAccu20 <- DS.sum %>%
  subset(year == 2020) %>%
  group_by(month) %>%
  summarize(sum = sum(rain, na.rm = TRUE))

##View(MonAccu18)
##View(MonAccu19)
##View(MonAccu20)
  
## Summary Table
V.month <- c(2018,2018,2018,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020)
##View(V.month)

rain.sum <- bind_rows(MonAccu18, MonAccu19, MonAccu20)
##View(rain.sum)
rain.sum2 <- bind_cols(V.month, rain.sum)
##View(rain.sum2)

## Rename columns
colnames(rain.sum2) <- c("year", "month", "total (in)")
## View(rain.sum2)

## Write file Rainfall Summary
write.csv(rain.sum2, "C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/Rainfall_Summary.csv")
## Write file Rainfall DS
write.csv(DS.sub2, "C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/Rainfall.csv")