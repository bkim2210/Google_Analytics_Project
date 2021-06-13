
aprilset<-read.csv("202004-divvy-tripdata.csv")
mayset<-read.csv("202005-divvy-tripdata.csv")
juneset<-read.csv("202006-divvy-tripdata.csv")
julyset<-read.csv("202007-divvy-tripdata.csv")
augustset<-read.csv("202008-divvy-tripdata.csv")
sepset<-read.csv("202009-divvy-tripdata.csv")
octset<-read.csv("202010-divvy-tripdata.csv")
novset<-read.csv("202011-divvy-tripdata.csv")
decset<-read.csv("202012-divvy-tripdata.csv")
janset<-read.csv("202101-divvy-tripdata.csv")
febset<-read.csv("202102-divvy-tripdata.csv")
marset<-read.csv("202103-divvy-tripdata.csv")


allset<-rbind(aprilset,mayset,juneset,julyset,augustset,sepset,octset,novset,decset,janset,febset,marset)

str(allset)
summary(allset)

allset<-allset %>% drop_na() %>% distinct(.keep_all = TRUE) %>% select(-c(rideable_type,start_lat,start_lng,end_lat,end_lng))



allset <- allset %>% mutate(ride_length= difftime(ended_at,started_at,units="min"),member_casual = recode(member_casual
                           ,"Subscriber" = "member"
                           ,"Customer" = "casual")
, day_of_week=wday(started_at))%>%filter(ride_length > 0)#1=Sunday
head(allset)




member_min_max_avg <- allset %>% group_by(member_casual) %>% summarize(avg_ride_length=mean(ride_length),max_ride_length=max(ride_length),min_ride_length=min(ride_length))
head(member_min_max_avg)

allset %>% 
  mutate(day = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, day) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(day)		

allset %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+ggtitle("Number of rides by member vs casual")


allset %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+ggtitle("Average duration by member vs casual")



