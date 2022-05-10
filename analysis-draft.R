#adding weekday columns
dailyActivity$weekDay <- strptime(dailyActivity$logDate,format="%m/%d/%Y") %>%
  as.Date(dailyActivity$logDate) %>%
  weekdays(., abbreviate = FALSE)

dailyActivity$weekDayNum <- strptime(dailyActivity$logDate,format="%m/%d/%Y") %>%
  as.Date(dailyActivity$logDate) %>%
  wday(., week_start=1)

hourlyIntensities$weekDay <- strptime(hourlyIntensities$logDate,format="%m/%d/%Y") %>%
  as.Date(hourlyIntensities$logDate) %>%
  weekdays(., abbreviate = FALSE)

hourlyIntensities$weekDayNum <- strptime(hourlyIntensities$logDate,format="%m/%d/%Y") %>%
  as.Date(hourlyIntensities$logDate) %>%
  wday(., week_start=1)


#Grouping daily activity for daily averages of users
dailyActivity_UserSummary <- dailyActivity %>%
  group_by(Id) %>%
  summarise(n = n(), steps = round(mean(TotalSteps)), calories = round(mean(Calories)),
            VeryActiveMinutes = round(mean(VeryActiveMinutes)), 
            FairlyActiveMinutes = round(mean(FairlyActiveMinutes)), 
            LightlyActiveMinutes = round(mean(LightlyActiveMinutes)), 
            SedentaryMinutes = round(mean(SedentaryMinutes)))

#grouping sleep log by user
dailySleep_UserSummary <- dailySleep %>%
  group_by(Id) %>%
  summarise(n=n(), averageTimeAsleep = mean(TotalMinutesAsleep), averageTimeInBed = mean(TotalTimeInBed))

#grouping weight log by user
weightLog_UserSummary <- weightLog %>%
  group_by(Id) %>%
  summarise(n=n(), weight_lbs = mean(WeightPounds), max_weight = max(WeightPounds), min_weight = min(WeightPounds), averageBMI = mean(BMI))

n_distinct(dailyActivity_UserSummary$Id)
n_distinct(dailySleep_UserSummary$Id)
n_distinct(weightLog_UserSummary$Id)


#Assigning categories to users based on number of days logged
dailyActivity_UserSummary$Usage <- ifelse(dailyActivity_UserSummary$n<=10,"Low", 
                ifelse(dailyActivity_UserSummary$n>10 & dailyActivity_UserSummary$n<21, "Medium",
                       "Regular"))

dailySleep_UserSummary$Usage <- ifelse(dailySleep_UserSummary$n<=10,"Low", 
                ifelse(dailySleep_UserSummary$n>10 & dailySleep_UserSummary$n<21, "Medium", "Regular"))

weightLog_UserSummary$Usage <- ifelse(weightLog_UserSummary$n<=10,"Low", 
                ifelse(weightLog_UserSummary$n>10 & weightLog_UserSummary$n<21, "Medium", "Regular"))


#Grouping users based on number of days logged
dailyActivityUsage <- dailyActivity_UserSummary %>%
  group_by(Usage) %>%
  summarise(userCount=n(), average_usage= round(mean(n),0))

dailySleepUsage <- dailySleep_UserSummary %>%
  group_by(Usage) %>%
  summarise(userCount=n(), average_usage= round(mean(n),0))

weightLogUsage <- weightLog_UserSummary %>%
  group_by(Usage) %>%
  summarise(userCount=n(), average_usage= round(mean(n),0))

#Sorting categories based on no. of users in each category
dailyActivityUsage <- dailyActivityUsage[order(-dailyActivityUsage$average_usage),]
dailySleepUsage <- dailySleepUsage[order(-dailySleepUsage$average_usage),]
weightLogUsage <- weightLogUsage[order(-weightLogUsage$average_usage),]

#Adding percentage column for no. of users in each category
dailyActivityUsage$perc <-scales::percent(
                          dailyActivityUsage$userCount/sum(dailyActivityUsage$userCount))
dailySleepUsage$perc <- scales::percent(dailySleepUsage$userCount/sum(dailySleepUsage$userCount))
weightLogUsage$perc <- scales::percent(weightLogUsage$userCount/sum(weightLogUsage$userCount))


#Visual representation of usage
gg_activity <- ggplot(dailyActivityUsage, aes(x = "", y = userCount, fill = Usage)) +
     geom_col(color = "white") +
     geom_text(aes(label = perc),
               position = position_stack(vjust = 0.5)) +
     coord_polar(theta = "y") +
     scale_fill_manual(values = c("#ff6666", "#fff81f", "#9efa61")) +
     labs(title="Activity Logging") +
     theme_void()+
     theme(legend.position="none", plot.title = element_text(hjust = 0.5))

gg_usageSleep <- ggplot(dailySleepUsage, aes(x = "", y = userCount, fill = Usage)) +
     geom_col(color = "white") +
     geom_text(aes(label = perc),
               position = position_stack(vjust = 0.5)) +
     coord_polar(theta = "y") +
     scale_fill_manual(values = c("#ff6666", "#fff81f", "#9efa61")) +
     labs(title="Sleep Logging") +
     theme_void()+
     theme(legend.position="bottom", plot.title = element_text(hjust = 0.5))

gg_usageWeight <- ggplot(weightLogUsage, aes(x = "", y = userCount, fill = Usage)) +
     geom_col(color = "white") +
     geom_text(aes(label = perc),
               position = position_stack(vjust = 0.5)) +
     coord_polar(theta = "y") +
     scale_fill_manual(values = c("#ff6666", "#fff81f", "#9efa61")) +
     labs(title="Weight Logging") +
     theme_void()+
     theme(legend.position="none", plot.title = element_text(hjust = 0.5))


grid.arrange(gg_activity, gg_usageSleep,gg_usageWeight, ncol=3)


dailyActivity %>%
  select(TotalSteps, TotalDistance, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes, Calories) %>%
  summary()


# Removing rows of data with no activity
dailyActivity <- dailyActivity %>%
  filter(SedentaryMinutes != 1440 & TotalSteps !=0)

#calculating percentage of users who average less than 10,000 steps a day
round(nrow(dailyActivity_UserSummary[dailyActivity_UserSummary$steps < 10000,]) / nrow(dailyActivity_UserSummary) * 100)


gg_stepsWeekday <- dailyActivity %>%
  group_by(weekDay) %>%
  summarise(steps = mean(TotalSteps), weekDayNum = mean(weekDayNum)) %>%
  ggplot(., aes(x=reorder(weekDay, weekDayNum), y=steps)) + 
  geom_bar(stat = "identity", fill="steelblue")  + 
  geom_hline(yintercept = 10000) +
  labs(x="", y = "Average Steps") +
  geom_text(aes(label = round(steps)), vjust = 2, colour = "white")

print(gg_stepsWeekday)