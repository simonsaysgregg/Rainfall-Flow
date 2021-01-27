## ANALYSIS OF WQ PARMETERS AT CALDONIA BIMS
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
WQ.raw <- read.csv("C:/KBE/Projects/Caledonia Wetlands/Data/R/CaledoniaWTLDS/Caledonia Parameters_Raw BIMS.csv")

## Combine month year columns
y <- WQ.raw$Year
m <- WQ.raw$Month

WQ.raw$Date <- as.yearmon(paste(y, m), "%Y %m")
View(WQ.raw)

## SELECT COLUMNS FOR USE
WQ.working <- WQ.raw %>%
  select(Date,
         Param.Class,
         Parameter,
         UoM,
         Value,
         Cell.Type)



## SUBSET FC PARAMETERS FOR PLOTTING
FC <- WQ.working %>%
  subset(Param.Class == "Bacteriological")
View(FC)

## MELT
FC.short <- FC %>%
  select(Date,
         Value,
         Cell.Type)
View(FC.short)

## Plot
ggplot(FC.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_point()+
  ggtitle("Fecal Coliform")+
  labs(y = "#/100mL", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET GI PARAMETERS FOR PLOTTING
GI <- WQ.working %>%
  subset(Param.Class == "General Inorganic")
View(GI)

## MELT
GI.short <- GI %>%
  select(Date,
         Value,
         Cell.Type)
View(GI.short)

## Plot
ggplot(GI.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  ggtitle("General Inorganic - Cholrine Residual")+
  labs(y = "ug/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET OG PARAMETERS FOR PLOTTING
OG <- WQ.working %>%
  subset(Parameter == "00556 - Oil & Grease")
View(OG)

## MELT
OG.short <- OG %>%
  select(Date,
         Value,
         Cell.Type)
View(OG.short)

## Plot
ggplot(OG.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_point()+
  ggtitle("General Organic - Oil & Grease")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET TN PARAMETERS FOR PLOTTING
TN <- WQ.working %>%
  subset(Parameter == "CO600 - Nitrogen, Total - Concentration")
View(TN)

## MELT
TN.short <- TN %>%
  select(Date,
         Value,
         Cell.Type)
View(TN.short)

## Plot
ggplot(TN.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_point()+
  ggtitle("Total Nitrogen")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


## SUBSET NH3 PARAMETERS FOR PLOTTING
NH3 <- WQ.working %>%
  subset(Parameter == "CO610 - Nitrogen, Ammonia Total (as N) - Concentration")
View(NH3)

## MELT
NH3.short <- NH3 %>%
  select(Date,
         Value,
         Cell.Type)
View(NH3.short)

## Plot
ggplot(NH3.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  ggtitle("Ammonia Nitrogen")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET TP PARAMETERS FOR PLOTTING
TP <- WQ.working %>%
  subset(Parameter == "CO665 - Phosphorus, Total (as P) - Concentration")
View(TP)

## MELT
TP.short <- TP %>%
  select(Date,
         Value,
         Cell.Type)
View(TP.short)

## Plot
ggplot(TP.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  ggtitle("Total Phosphorous")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


## SUBSET BOD PARAMETERS FOR PLOTTING
BOD <- WQ.working %>%
  subset(Parameter == "CO310 - BOD, 5-Day (20 Deg. C) - Concentration")
View(BOD)

## MELT
BOD.short <- BOD %>%
  select(Date,
         Value,
         Cell.Type)
View(BOD.short)

## Plot
ggplot(BOD.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  ggtitle("Biochemical Oxygen Demand")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET TSS PARAMETERS FOR PLOTTING
TSS <- WQ.working %>%
  subset(Parameter == "CO530 - Solids, Total Suspended - Concentration")
View(TSS)

## MELT
TSS.short <- TSS %>%
  select(Date,
         Value,
         Cell.Type)
View(TSS.short)

## Plot
ggplot(TSS.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  ggtitle("Total Suspended Solid")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


