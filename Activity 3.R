# James Maisano
# ENVST 325
# Activity 3

# In class activity ----
# installing packages
install.packages(c("ggplot2", "dplyr"))
# adding packages to library
library(ggplot2)
library(dplyr)
# Install additonal package
install.packages("lubridate")
library(lubridate)
# reading in data
datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

# checking the column names
colnames(datCO2)

# change the 4 column name
colnames(datCO2)[4] <- "CO2"
# check names again
colnames(datCO2)

# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)
# make a vector of all levels
name.Ent <- levels(datCO2$Entity)

# example plot of CO2 and year
plot(datCO2$Year, datCO2$CO2)

# new data frame for US
US <- datCO2[datCO2$Entity == "United States",]
# new data frame for Mexico
ME <- datCO2[datCO2$Entity == "Mexico",]

# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (tons CO2)", #y axis label
     xlab = "Year") #x axis label

# make a plot of US CO2 - changing the graph
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )

# ggplot graphing
ggplot(US,aes(x=Year, y=CO2))+
  geom_point()+
  labs(x="Year", y="US Fossil Fuel Emissisons (tons of CO2)")+
  theme_classic()
# filter for North America
NorthA <- datCO2 %>%
  filter(Entity == "United States" | 
           Entity =="Mexico" |
         Entity ==  "Canada")
# North america graph
ggplot(NorthA, 
       aes(x=Year, y=CO2, color=Entity))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("red", "royalblue", "darkgoldenrod3"))

# In Class Prompts ----
# Prompt 1
# reading in climate change csv
datCC <- read.csv("/cloud/project/activity03/climate-change.csv")

# reading the column names
colnames(datCC)

# renaming column 4
colnames(datCC)[4] <- "TempAnom"
# changing date column in the data frame
datCC$date <- ymd(datCC$Day)

# Creating data frame for the northern hemisphere
NorthHemi <- datCC[datCC$Entity == "Northern Hemisphere",]
# Creating data frame for the southern hemisphere
SouthHemi <- datCC[datCC$Entity == "Southern Hemisphere",]

# plot of north and south hemisphere air temp anom in base R
plot(NorthHemi$date,
     NorthHemi$TempAnom,
     type = "b",
     pch = 19,
     ylab = "Temperature Anomolies in C",
     xlab = "Date",
     col = "red")
lines(SouthHemi$date,
       SouthHemi$TempAnom,
       type = "b",
       pch = 19,
       col = "darkblue")

# same graph in ggplot 2
NorthHemi$Hemisphere <- "Northern"
SouthHemi$Hemisphere <- "Southern"
# Filter for the data
Hemi <- datCC %>%
  filter(Entity == "Northern Hemisphere" | 
           Entity == "Southern Hemisphere")
# ggplot graph
ggplot(Hemi,
       aes(x = date, 
           y = TempAnom, 
           color = Entity)) +
  geom_point() +
  geom_line() +
  labs(x = "Date",
       y = "Temperature Anomalies (°C)") +
  scale_color_manual(values = c("red", "darkblue")) 
          
# Prompt 2
# filter for the three countries
NorthA <- datCO2 %>%
  filter(Entity == "United States" |
           Entity == "Mexico" |
           Entity == "Canada")
# All time emissions
TotalEmissions <- NorthA %>%
  group_by(Entity) %>%
  summarise(Total_CO2 = sum(CO2, na.rm = TRUE))
# making the bar chart
ggplot(TotalEmissions,
       aes(x = Entity, 
           y = Total_CO2, 
           fill = Entity)) +
  geom_col() +
  scale_fill_manual(values = c("red", 
                               "darkgoldenrod3", 
                               "royalblue")) +
  labs(x = "Country",
       y = "Total All-Time CO2 Emissions (tons)")

# Homework Prompts ----
# Prompt 1
# Filtering the selected countries
Emissions3 <- datCO2 %>%
  filter(Entity == "Iceland" |
           Entity == "Japan" |
           Entity == "Norway")
# Plotting the countries using ggplot
ggplot(Emissions3,
       aes(x = Year,
           y = CO2,
           color = Entity)) +
  geom_line(size = 1) +
  labs(title = "CO2 Emissions Over Time",
       x = "Year",
       y = "Annual CO2 Emissions (tons)") +
  scale_color_manual(values = c("#02529C",
                                "#ffb7c5",
                                "#BA0C2F")) +
  theme_classic()

# Prompt 2
# Filter for world data from CO2
WorldCO2 <- datCO2 %>%
  filter(Entity == "World")
# Filter for world data from CC
WorldTemp <- datCC %>%
  filter(Entity == "World")

# plot for CO2 data
ggplot(WorldCO2,
       aes(x = Year, y = CO2)) +
  geom_line(color = "darkgrey", linewidth = 1) +
  labs(title = "Global CO₂ Emissions Over Time",
       x = "Year",
       y = "CO₂ Emissions (tons)") +
  theme_classic()

# plot for temp data
ggplot(WorldTemp,
       aes(x = date, y = TempAnom)) +
  geom_line(color = "orange", linewidth = 1) +
  labs(title = "Global Air Temperature Anomalies",
       x = "Year",
       y = "Temperature Anomaly (°C)") +
  theme_classic()

# Prompt 3
