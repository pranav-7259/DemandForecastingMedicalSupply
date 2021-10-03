#Loading libraries

library("tidyverse")
library("stats")
library("lubridate")
library("forecast")

#Reading data from CSV file

#-------------------------------------------------------------------------
#This data determines the number of cases of Aedes per month 
#for the Tarrant County of Texas
#-------------------------------------------------------------------------

tarrant_data <- readLines("C:\\Users\\Pranav Shekhar\\OneDrive\\Desktop\\Tarrant.csv")
tarrant_data_csv <- c(32,44,47,53,32,27,34,56,45,59,34,36,34,44,45,45,24,44,13,14,34,31,38,27,22,12,38,24,16,31,95,35,27,33,33,62,55)


#Generating a Time Series Plot for the data from year 2016 to 2018
tarrant_ts <- ts(tarrant_data_csv, start = decimal_date(ymd("2016-01-01")),
                 frequency = 12)

plot.ts(tarrant_ts)

#Creating ARIMA model for time series forecasting
tarrant_fit <- auto.arima(tarrant_ts)
forecast(tarrant_fit,12)

plot(forecast(tarrant_fit,12))

#---------------------------------------------------------------------
#This data determines the number of cases of Aedes per month 
#for the Collin County of Texas
#---------------------------------------------------------------------

tarrant_data <- readLines("C:\\Users\\Pranav Shekhar\\OneDrive\\Desktop\\Collin.csv")

collin_data_csv <- c(28,35,57,44,60,34,35,34,44,45,45,24,44,13,14,34,31,38,
                     27,22,12,38,24,26,31,94,35,28,33,34,61,32,40,48,51,43)

#Generating a Time Series Plot for the data from year 2016 to 2018
collin_ts <- ts(collin_data_csv, start = decimal_date(ymd("2016-01-01")),
                frequency = 12)

plot.ts(collin_ts)

#Creating ARIMA model for time series forecasting
collin_fit <- auto.arima(collin_ts)
forecast(collin_fit,12)

plot(forecast(collin_fit,12))

#------------------------------------------------------------------------
#Storing the forecast results in a dataframe
#----------------------------------------------------------------------

collin_fit_df <- print(forecast(collin_fit,12))
tarrant_fit_df <- print(forecast(tarrant_fit,12))


#--------------------------------------------------------------------
#Printing results to divert resources to appropriate county
#-------------------------------------------------------------------


fit_diff <- c(collin_fit_df$`Point Forecast`-tarrant_fit_df$`Point Forecast`)
results_df <- collin_fit_df
results_df$County_Resource <- fit_diff
results_df <- results_df['County_Resource']
county_name <- ifelse(fit_diff <= 0, "Tarrant", "Collin")
results_df$County_Name <- county_name


#---------------------------------------------------------
#Printing Results
#--------------------------------------------------------

resource_allocation <- results_df['County_Name']
print(resource_allocation)
