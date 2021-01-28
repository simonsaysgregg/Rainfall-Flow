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
#View(WQ.raw)

## SELECT COLUMNS FOR USE
WQ.working <- WQ.raw %>%
  select(Date,
         Param.Class,
         Parameter,
         UoM,
         Value,
         Cell.Type)

rain.acc <- rain.sum2 %>%
  select(Date,
         `total (in)`)
#View(rain.acc)
colnames(rain.acc) <- c("Date", "Total")

WQ.working <- WQ.working %>%
  left_join(rain.acc, by = "Date")
#View(WQ.working)

## SUBSET FC PARAMETERS FOR PLOTTING
FC <- WQ.working %>%
  subset(Param.Class == "Bacteriological")
#View(FC)

## MELT
FC.short <- FC %>%
  select(Date,
         Value,
         Cell.Type,
         Total)
#View(FC.short)

## Plot
ggplot(FC.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_point()+
  geom_line(aes(y = Total * 250, color = "Rainfall Total (in)"))+
  scale_y_continuous(sec.axis = sec_axis(~. / 250, name = "Rainfall Total (in)"))+
  geom_hline(aes(yintercept = 400, linetype = "Daily Maximum"))+
  geom_hline(aes(yintercept = 200, linetype = "Monthly Average"))+
  ggtitle("Fecal Coliform")+
  labs(y = "#/100mL", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET GI PARAMETERS FOR PLOTTING
GI <- WQ.working %>%
  subset(Param.Class == "General Inorganic")
#View(GI)

## MELT
GI.short <- GI %>%
  select(Date,
         Value,
         Cell.Type,
         Total)
#View(GI.short)

## Plot
ggplot(GI.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  geom_line(aes(y = Total * 5, color = "Rainfall Total (in)"))+
  scale_y_continuous(sec.axis = sec_axis(~. / 5, name = "Rainfall Total (in)"))+
  geom_hline(aes(yintercept = 28, linetype = "Daily Maximum"))+
  ggtitle("General Inorganic - Cholrine Residual")+
  labs(y = "ug/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET OG PARAMETERS FOR PLOTTING
OG <- WQ.working %>%
  subset(Parameter == "00556 - Oil & Grease")
#View(OG)

## MELT
OG.short <- OG %>%
  select(Date,
         Value,
         Cell.Type,
         Total)
#View(OG.short)

## Plot
ggplot(OG.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_point()+
  geom_line(aes(y = Total * 5, color = "Rainfall Total (in)"))+
  scale_y_continuous(sec.axis = sec_axis(~. / 5, name = "Rainfall Total (in)"))+
  geom_hline(aes(yintercept = 60, linetype = "Daily Maximum"))+
  geom_hline(aes(yintercept = 30, linetype = "Monthly Average"))+
  ggtitle("General Organic - Oil & Grease")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET TN PARAMETERS FOR PLOTTING
TN <- WQ.working %>%
  subset(Parameter == "CO600 - Nitrogen, Total - Concentration")
#View(TN)

## MELT
TN.short <- TN %>%
  select(Date,
         Value,
         Cell.Type,
         Total)
#View(TN.short)

## Plot
ggplot(TN.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_point()+
  geom_line(aes(y = Total * 5, color = "Rainfall Total (in)"))+
  scale_y_continuous(sec.axis = sec_axis(~. / 5, name = "Rainfall Total (in)"))+
  #geom_hline(aes(yintercept = 60, linetype = "Daily Maximum"))+
  #geom_hline(aes(yintercept = 30, linetype = "Monthly Average"))+
  ggtitle("Total Nitrogen")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


## SUBSET NH3 PARAMETERS FOR PLOTTING
NH3 <- WQ.working %>%
  subset(Parameter == "CO610 - Nitrogen, Ammonia Total (as N) - Concentration")
#View(NH3)

## MELT
NH3.short <- NH3 %>%
  select(Date,
         Value,
         Cell.Type,
         Total)
#View(NH3.short)

## Plot
ggplot(NH3.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  geom_line(aes(y = Total * 2, color = "Rainfall Total (in)"))+
  scale_y_continuous(sec.axis = sec_axis(~. / 2, name = "Rainfall Total (in)"))+
  #geom_hline(aes(yintercept = 60, linetype = "Daily Maximum"))+
  #geom_hline(aes(yintercept = 30, linetype = "Monthly Average"))+
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
#View(BOD)

## MELT
BOD.short <- BOD %>%
  select(Date,
         Value,
         Cell.Type,
         Total)
#View(BOD.short)

## Plot
ggplot(BOD.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  geom_line(aes(y = Total * 5, color = "Rainfall Total (in)"))+
  scale_y_continuous(sec.axis = sec_axis(~. / 5, name = "Rainfall Total (in)"))+
  geom_hline(aes(yintercept = 45, linetype = "Daily Maximum"))+
  geom_hline(aes(yintercept = 30, linetype = "Monthly Average"))+
  ggtitle("Biochemical Oxygen Demand")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

## SUBSET TSS PARAMETERS FOR PLOTTING
TSS <- WQ.working %>%
  subset(Parameter == "CO530 - Solids, Total Suspended - Concentration")
#View(TSS)

## MELT
TSS.short <- TSS %>%
  select(Date,
         Value,
         Cell.Type,
         Total)
#View(TSS.short)

## Plot
ggplot(TSS.short, aes(x = Date, y = Value, color = Cell.Type))+
  geom_line()+
  geom_line(aes(y = Total * 12, color = "Rainfall Total (in)"))+
  scale_y_continuous(sec.axis = sec_axis(~. / 12, name = "Rainfall Total (in)"))+
  geom_hline(aes(yintercept = 135, linetype = "Daily Maximum"))+
  geom_hline(aes(yintercept = 90, linetype = "Monthly Average"))+
  ggtitle("Total Suspended Solid")+
  labs(y = "mg/L", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


