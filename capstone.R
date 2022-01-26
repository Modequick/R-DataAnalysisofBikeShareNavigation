library("tidyverse")
library("janitor")
library(scales)
library(lubridate)


df1 <- `202005.divvy.tripdata`
df2 <- `202006.divvy.tripdata`
df3 <- `202007.divvy.tripdata`
df4 <- `202008.divvy.tripdata`
df5 <- `202009.divvy.tripdata`
df6 <- `202010.divvy.tripdata`
df7 <- `202011.divvy.tripdata`
df8 <- `202012.divvy.tripdata`
df9 <- `202101.divvy.tripdata`
df10 <- `202102.divvy.tripdata`
df11 <- `202103.divvy.tripdata`
df12 <- `202104.divvy.tripdata`

#preprocessing
df <-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
#removing outliers
df <- na.omit(df)
#removing duplicates
df <- distinct(df,ride_id, .keep_all = TRUE)

dim(df)

#adding new features
df$started_at <- ymd_hms (df$started_at)
df$ended_at <- ymd_hms(df$ended_at)
#adding rent_time
df <- mutate(df, rent_time = df$ended_at-df$started_at)
#adding days at given date
df$day <- weekdays(as.Date(df$started_at))

#adding month and year
df$Month_Yr <- format(as.Date(df$started_at), "%Y-%m")
#adding hour
df  <- df %>% 
  mutate(start_hour=strftime(df$started_at, format = "%H",tz = "UTC"))

#grouping by dawn morning afternoon and night
x <- as.integer(df$start_hour)
df$categories <- cut(x, breaks = c(-1,5,11,17,25),
                  labels = c("Dawn","Morning","Afternoon","Night"))



#removing outliers 
  q1 <- quantile(df$rent_time,0.25)
  q3 <- quantile(df$rent_time,0.75)
  IQR <- q3-q1
  lowerbound <- q1-1.5*IQR
  upperbound <- q3+1.5*IQR

df <- subset(df, df$rent_time> lowerbound & df$rent_time< upperbound)

#analyzing

#how many are there for member and casual?
df %>% 
  group_by(member_casual) %>% 
  summarize(count=length(ride_id),
            percentage_of_total=(length(ride_id)/nrow(df))*100)

#how is the characteristic by rent time between member and casual?
df %>%
  group_by(member_casual) %>%
    summarize(mean(rent_time/60))

#how is the characteristic of member casual by days?
days_data <-df %>% 
  group_by(day,member_casual) %>%
    summarize(count=length(ride_id),
              percentage = (length(ride_id)/nrow(df))*100)

ggplot(data = days_data, aes(x= day,fill = member_casual))+  geom_bar(aes(y = percentage),
           position = "dodge", stat = "identity") 

#how is the chracteristic by time phase?
characteristicbytime <-df %>%
  group_by(categories,member_casual) %>%
    summarize(count=length(ride_id))

ggplot(data= characteristicbytime,aes(x=categories,fill=member_casual)) + geom_bar(aes(y=count),
          position="dodge" ,stat="identity")


#how is the characteristic by time?
dftime <- df %>% group_by(start_hour,member_casual) %>% summarize(counter=length(ride_id))
ggplot(data= dftime,aes(x=start_hour,fill=member_casual)) + geom_bar(aes(y=counter ,col="#69b3a2"),
            position="dodge" ,stat="identity") + labs(title="Member Casual by Hour")


#calculating by start location
dfcasual <- filter(df,member_casual=="casual")
locmarketing <- dfcasual%>%
  group_by(start_station_name, member_casual) %>% 
  summarize(count=length(ride_id)) %>%
  arrange(desc(count))

head(locmarketing)
ggplot(data= head(locmarketing),aes(x=start_station_name,fill=member_casual)) + geom_bar(aes(y=count ,col="#69b3a2"),
      position="dodge" ,stat="identity") + labs(title="top 5 location marketing")



