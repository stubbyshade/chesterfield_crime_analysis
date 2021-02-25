# Introduction to Data Science Coursework Script

# ------------- LIBRARIES -----------------

library(dplyr)
library(plyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(tmap)
library(rgdal)
library(tseries)
library(forecast)

# -------------- DATA COLLECTION -----------------

# We need to load the CSV files into dataframes and merge them into 1 dataframe 
# for ease of use

# Put the CSV files containing the police data for "street" into separate lists 
# depending on the year, and one with all of them
# Note: this requires the user to have files named "street_Ncsv" where N is year
# Note 2: street_totalcsv contains all the months for 2018 and 2019 because it 
# is to be used in forecasting
derbyshiretotal <- list.files("street_totalcsv", pattern = "*.csv", full.names =  TRUE)

derbyshire2020 <- list.files("street_2020csv", pattern = "*.csv", full.names = TRUE) 
derbyshire2019 <- list.files("street_2019csv", pattern = "*.csv", full.names = TRUE)
derbyshire2018 <- list.files("street_2018csv", pattern = "*.csv", full.names = TRUE) 

# Then create the dataframes from these CSV files
df_derbyshire <- ldply(derbyshiretotal, read.csv)

df_derbyshire2020 <- ldply(derbyshire2020 , read.csv)
df_derbyshire2019 <- ldply(derbyshire2019, read.csv)
df_derbyshire2018 <- ldply(derbyshire2018, read.csv)

# -------------- DATA CLEANING ---------------

# crime.id is not useful to us and "context" contains nothing but NA so get rid 
# of these
df_derbyshire <- df_derbyshire[,2:11]

df_derbyshire2020 <- df_derbyshire2020[,2:11]
df_derbyshire2019 <- df_derbyshire2019[,2:11]
df_derbyshire2018 <- df_derbyshire2018[,2:11]

# Every input for "Reported.by" and "Falls.within" is the same, and therefore 
# no use. Neither is last.outcome.category
df_derbyshire <- df_derbyshire[ -c(2,3,10) ]

df_derbyshire2020 <- df_derbyshire2020[ -c(2,3,10) ]
df_derbyshire2019 <- df_derbyshire2019[ -c(2,3,10) ]
df_derbyshire2018 <- df_derbyshire2018[ -c(2,3,10) ]

# -------------- TRANFORMING THE DATA ---------------

# First, count the frequencies of each crime in Chesterfield using "dplyr::count"
# a count function also belongs to the library plyr, so we use dplyr::

# Here we use the function grepl() to filter out the crimes in Chesterfield

chescrimes2020 <- df_derbyshire2020 %>% 
  filter(grepl('Chesterfield', LSOA.name)) %>%
  dplyr::count(Crime.type)

colnames(chescrimes2020) <- c("Crime", "Frequency")

# and chesterfield crimes in 2019
chescrimes2019 <- df_derbyshire2019 %>% 
  filter(grepl("Chesterfield", LSOA.name)) %>%
  dplyr::count(Crime.type)

colnames(chescrimes2019) <- c("Crime", "Frequency")

# and finally 2018
chescrimes2018 <- df_derbyshire2018 %>% 
  filter(grepl("Chesterfield", LSOA.name)) %>%
  dplyr::count(Crime.type)

colnames(chescrimes2018) <- c("Crime", "Frequency")

# Now, combine the datasets and rename the frequency columns that apply to the 
# years accordingly

chesCrimeFreq <- merge(chescrimes2018, chescrimes2019, by = "Crime") %>%
  left_join(., chescrimes2020, by = "Crime")

colnames(chesCrimeFreq) <- c("Crime","x2018", "x2019", "x2020")

  # -------------- EXPLORING AND COMMUNICATING THE DATA --------------------

# Plot the barchart in descending order using reorder()
chesCrimeFreq %>%
  gather("Year", "Frequency",-Crime) %>%
  ggplot(aes(x = reorder(Crime, -Frequency), y= Frequency, fill = Year)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_light()

# It's hard to read the bar graph because there are a lot of variables, we can 
# remove the bottom 6 (i.e. the least significant crimes) to allow for a more 
# clearer visualisation
chesCrimeFreq <- chesCrimeFreq[-c(2, 5, 6, 8, 10, 12), ]

chesCrimeFreq %>%
  gather("Year", "Frequency",-Crime) %>%
  ggplot(aes(x = reorder(Crime, -Frequency), y= Frequency, fill = Year)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=Frequency), vjust=1.6, color="white",
            position = position_dodge(0.9), size=2.5)+
  scale_fill_discrete(name = "Year", labels = c("2018", "2019", "2020")) +
  labs(title="Frequency of Crime Reported in Chesterfield from 2018 - 2020",
       x ="Type of Crime", 
       y = "Reported Occurances",
       caption = "Data source: UK Police Dataset") +
  theme_light()

# We could create a chart that shows the increase or decrease in % of each crime each year
# First, create a new column with the percentage change as the variable
percentage2019 <- chesCrimeFreq %>% 
  mutate(Change_from_2019 = x2020 - x2019, 
         Percent_Change_2019 = Change_from_2019/x2019*100)

percentage2018 <- chesCrimeFreq %>% 
  mutate(Change_from_2018 = x2019 - x2018, 
         Percent_Change_2018 = Change_from_2018/x2018*100)

# Then plot
percentage2018 %>%
  ggplot(aes(x = reorder(Crime, Percent_Change_2018), y = Percent_Change_2018)) +
  geom_bar(stat = "identity") +
  geom_col(fill="dodgerblue4") +
  labs(x = "",
       y = "Percentage Change",
       title = "Percentage Change of Recorded Crime in Chesterfield from 2018
       to 2019",
       caption = "Data source: UK Police Dataset") +
  coord_flip() +
  theme_light()

# Then plot
percentage2019 %>%
  ggplot(aes(x = reorder(Crime, Percent_Change_2019), y = Percent_Change_2019)) +
  geom_bar(stat = "identity") +
  geom_col(fill="dodgerblue4") +
  labs(x = "",
       y = "Percentage Change",
       title = "Percentage Change of Recorded Crime in Chesterfield from 2019 to 2020",
       caption = "Data source: UK Police Dataset") +
  coord_flip() +
  theme_light()

# -------------- TIME SERIES ANALYSIS AND PREDICTION ---------------

# First count how many crimes occur on each day
chestrend <- df_derbyshire %>%
  filter(grepl("Chesterfield", LSOA.name)) %>%
  group_by(Month) %>%
  dplyr::count(Crime.type)

colnames(chestrend) <- c("Month", "Crime", "Frequency")

# Remove lowest occurring crimes to tidy up the plot, same as before
lowfrqcrimes <- c("Bicycle theft", "Drugs", "Other crime", "Possession of weapons", 
              "Robbery", "Theft from the person")

chestrend <- filter(chestrend, !grepl(paste(lowfrqcrimes, collapse="|"), Crime))

ggplot(chestrend, aes(x=Month, y=Frequency, color = Crime, group = Crime)) + 
  geom_line(size= 1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Month",
       y = "Reported Occurances",
       title = "Frequency of  Crime Reported in Chesterfield from 2018-2020 by Month",
       colour = "Type of Crime",
       caption = "Data source: UK Police Dataset")

# Change to date format so we can convert to time series format
chestrend$Month <- as.Date(paste0(chestrend$Month, "-01"))

# Now, we can make some predictions. Let's focus on one of the most prevalent crimes;
violencetrend <- filter(chestrend, grepl("Violence", Crime))

# Plot a graph with a Loess regression curve to check for a trend
ggplot(violencetrend, aes(x=Month, y=Frequency, group=1)) + 
  geom_line(size= 1, colour = "dodgerblue4") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Month",
       y = "Reported Occurances",
       title = "Frequency of  Violence and Sexual Assaults Reported in 
       Chesterfield from 2018-2020 by Month",
       caption = "Data source: UK Police Dataset") +
  stat_smooth(
    size = 0.8,
    color = "firebrick",
    se = FALSE,
    method = "loess"
  )

# We need to convert our violencetrend dataframe to time series format
ts_violentcrimes <- ts(violencetrend$Frequency, frequency=12, start=c(2018, 1))

# Check components of our time series
ts_components <- decompose(ts_violentcrimes)
plot(ts_components)

# We have an upward trend and a seasonality
ndiffs(ts_violentcrimes) # shows us how many first differences we need

sts_violentcrimes <- diff(ts_violentcrimes, differences = 1)
plot(sts_violentcrimes)


# Create our model
fitARIMA <- arima(ts_violentcrimes, 
                  order=c(0,1,0),
                  seasonal = list(order = c(1,0,0), 
                  period = 12))

# Create our forecasted model
forecastedmodel <- forecast(fitARIMA, h=12)
autoplot(forecastedmodel)

# Check our model's accuracy
qqnorm(forecastedmodel$residuals)
acf(forecastedmodel$residuals)
pacf(forecastedmodel$residuals)

# Plot the model
autoplot(forecastedmodel) +
  labs(x = "Time",
       y = "Reported Occurances",
       title= "2021 Forecasts of Violence and Sexual Assaults in Chesterfield")

# ------------ MAPS -------------

# I want to look at maps of crimes in Chesterfield. First we need to download 
# Chesterfield's LSOA boundary data then use readOGR to read it in. 

# Create variables for this year and previous years

chesboundary2020 <- readOGR(dsn="./BoundaryData", layer="england_lsoa_2011")
chesboundary2019 <- readOGR(dsn="./BoundaryData", layer="england_lsoa_2011")
chesboundary2018 <- readOGR(dsn="./BoundaryData", layer="england_lsoa_2011")

# Look at where violence and sexual occur in Chesterfield in 2020 and group 
# them together in a new variable called violence_freqN where N is the year

chescrimesLSOA2020<-df_derbyshire2020 %>%
  filter(grepl('Violence', Crime.type)) %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  dplyr::summarise(violence_freq2020=n())

chesboundary2020@data<-left_join(chesboundary2020@data, chescrimesLSOA2020,
                                 by=c('code'='LSOA.code'))

# then 2019 
chescrimesLSOA2019<-df_derbyshire2019 %>%
  filter(grepl('Violence', Crime.type)) %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  dplyr::summarise(violence_freq2019=n())

chesboundary2019@data<-left_join(chesboundary2019@data, chescrimesLSOA2019,
                             by=c('code'='LSOA.code'))


# and 2018
chescrimesLSOA2018<-df_derbyshire2018 %>%
  filter(grepl('Violence', Crime.type)) %>%
  select(LSOA.code, LSOA.name, Crime.type) %>%
  group_by(LSOA.code) %>%
  dplyr::summarise(violence_freq2018=n())

chesboundary2018@data<-left_join(chesboundary2018@data, chescrimesLSOA2018,
                                 by=c('code'='LSOA.code'))

# Change map to colour_blind style to increase accessibility
tmap_style("col_blind")

# Remove the missing values
chesboundary2020[is.na(chesboundary2020@data$violence_freq2020)]<-0

# Plot the boundaries with crime frequency information
tm_shape(chesboundary2020) +
  tm_fill("violence_freq2020", alpha=0.7, style="kmeans", border.col = "black", 
          palette = "seq",
          title = "Frequency of Reported Violence and Sexual Assault") +
  tm_borders(alpha=0.7) +
  tm_layout("Violence and Sexual Assault in Chesterfield - 2020") +
  tm_legend(position = c("right", "bottom"),
            legend.title.size = 0.5)

tm_shape(chesboundary2019) +
  tm_fill("violence_freq2019", alpha=0.7, style="kmeans", border.col = "black", 
          palette = "seq",
          title = "Frequency of Reported Violence and Sexual Assault") +
  tm_borders(alpha=0.7) +
  tm_layout("Violence and Sexual Assault in Chesterfield - 2019") +
  tm_legend(position = c("right", "bottom"),
            legend.title.size = 0.5)

tm_shape(chesboundary2018) +
  tm_fill("violence_freq2018", alpha=0.7, style="kmeans", border.col = "black", 
          palette = "seq",
          title = "Frequency of Reported Violence and Sexual Assault") +
  tm_borders(alpha=0.7) +
  tm_layout("Violence and Sexual Assault in Chesterfield - 2018") +
  tm_legend(position = c("right", "bottom"),
            legend.title.size = 0.5)


