# set working directory to folder with all csv data source files
setwd("/cloud/project/Fitbit Data")

dailyActivity <- read_csv("dailyActivity_merged.csv")
dailySleep <- read_csv("sleepDay_merged.csv")
weightLog <- read_csv("weightLogInfo_merged.csv")
hr <- read_csv("heartrate_seconds_merged.csv")
hourlyIntensities <- read_csv("hourlyIntensities_merged.csv")