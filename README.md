# Cyclistic-Bike-sharing-analysis
## Table of Contents
- [Background](#background)
- [Case study](#case-study)
- [Stakeholders](#stakeholders)
- [Data sources](#data-sources)
- [Documentation, cleaning and preparation of data for analysis](#documentation,-cleaning-and-preparation-of-data-for-analysis)
- [Tools for analysis](#tools-for-analysis)
- [Preparation of Data](#preparation-of-data)
- [Analyze data](#analyze-data)
- [Data Visualiation's](#data-visualiation's)
- [what does the data tell us?](#what-does-the-data-tell-us?)
- [key takeaways](#key-takeaways)
- [Recommendations](#recommendations)
- [Things to Consider](#things-to-consider)
- [Additional points that were not examined](#additional-points-that-were-not-examined)


### Background
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geo-tracked and locked into a network of 692 stations across Chicago.
Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members.

### Case study
This report will examine the business question: 'what is the most effective marketing strategy to converting Cyclistic’s casual riders to annul memberships?'
It is understood that the goal can be broken down into 3 main questions.
- How do annual members and casual riders use Cyclistic bikes differently?
- Why would casual riders buy Cyclistic annual memberships?
- How can Cyclistic use digital media to influence casual riders to become members?
This report will seek to deliver on the following objectives:
- How do annual members and casual riders use Cyclistic bikes differently?


### Stakeholders
This report also seeks to identify the important stakeholders that are involved in the overall analysis. This includes:
- cyclistic users,
- director of marketing,
- Cyclistic marketing team
- Cyclistic executive team


### Data sources
User data from the past 12 months, January 2021 - December 2021 has been made available. Each data set is in csv format and details every ride logged by Cyclistic customers. This data has been made publicly available via license by Motivate International Inc. and the city of Chicago available [here](https://divvy-tripdata.s3.amazonaws.com/index.html). All user’s personal data has been scrubbed for privacy.

### Documentation, cleaning and preparation of data for analysis

### Tools for analysis
R is being used due to the data size and visualizations needed to complete this analysis.

### Preparation of Data

```R
#Load the necessary libraries that will be utilized for the project
library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(ggplot2)

── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

✔ ggplot2 3.3.5     ✔ purrr   0.3.4
✔ tibble  3.1.5     ✔ dplyr   1.0.7
✔ tidyr   1.1.4     ✔ stringr 1.4.0
✔ readr   2.0.2     ✔ forcats 0.5.1

── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
Attaching package: ‘lubridate’


The following objects are masked from ‘package:base’:

    date, intersect, setdiff, union



Attaching package: ‘janitor’


The following objects are masked from ‘package:stats’:

    chisq.test, fisher.test
```

Load all the data, as well as combine every dataset

```R
trip20_Jan <- read.csv("../input/divvytrips2021/202101-divvy-tripdata.csv") #load data into R
trip20_Feb <- read.csv("../input/divvytrips2021/202102-divvy-tripdata.csv")
trip20_Mar <- read.csv("../input/divvytrips2021/202103-divvy-tripdata.csv")
trip20_Apr <- read.csv("../input/divvytrips2021/202104-divvy-tripdata.csv")
trip20_May <- read.csv("../input/divvytrips2021/202105-divvy-tripdata.csv")
trip20_Jun <- read.csv("../input/divvytrips2021/202106-divvy-tripdata.csv")
trip20_Jul <- read.csv("../input/divvytrips2021/202107-divvy-tripdata.csv")
trip20_Aug <- read.csv("../input/divvytrips2021/202108-divvy-tripdata.csv")
trip20_Sep <- read.csv("../input/divvytrips2021/202109-divvy-tripdata.csv")
trip20_Oct <- read.csv("../input/divvytrips2021/202110-divvy-tripdata.csv")
trip20_Nov <- read.csv("../input/divvytrips2021/202111-divvy-tripdata.csv")
trip20_Dec <- read.csv("../input/divvytrips2021/202112-divvy-tripdata.csv")
```

Combine every dataset to consolidate analysis

```R
trips20fill<- rbind(trip20_Jan, trip20_Feb, trip20_Mar, trip20_Apr, trip20_May, trip20_Jun, trip20_Jul, trip20_Aug, trip20_Sep, trip20_Oct, trip20_Nov, trip20_Dec)
```

View newly created dataset

```R
View(trips20fill) #combine all datasets
```

Firstly remove all the irrelevent columns that won't be used for analysis

```R
trips20fill <- trips20fill %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_id,end_station_id, end_station_name))
```

Review of the data and its parameters.

```R
colnames(trips20fill)  #List of column names
nrow(trips20fill)  #How many rows are in data frame?
dim(trips20fill)  #Dimensions of the data frame?
head(trips20fill, 6)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(trips20fill)  #See list of columns and data types (numeric, character, etc)
summary(trips20fill) #inspect the date and its dimensions before moving onto cleaning
```

Additional columns must be created for date and time.

```R
#The default format is yyyy-mm-dd
trips20fill$date <- as.Date(trips20fill$started_at)
trips20fill$month <- format(as.Date(trips20fill$date), "%m")
trips20fill$day <- format(as.Date(trips20fill$date), "%d")
trips20fill$year <- format(as.Date(trips20fill$date), "%Y")
trips20fill$day_of_week <- format(as.Date(trips20fill$date), "%A")
trips20fill$time <- format(trips20fill$started_at, format= "%H:%M")
trips20fill$time <- as.POSIXct(trips20fill$time, format= "%H:%M")
```

Calculated filed that shows the time of each unique ride

```R
#create calculated field to isolate time spent on every ride.
trips20fill$ride_length <- (as.double(difftime(trips20fill$ended_at, trips20fill$started_at))) /60
```

Check data structure. Confirm data types for time/date

```R
str(trips20fill) #confirm data type is double [True]
```

Alter data type for time

```R
trips20fill$ride_length <- as.numeric(as.character(trips20fill$ride_length)) #change datatype to numeric for further analysis
```

Remove all blank entries from the dataset

```R
trips20fill<- trips20fill[!(trips20fill$start_station_name == "HQ QR" | trips20fill$ride_length<0),]
```

Observe the newly created column for the backup dataset

```R
summary(trips20fill$ride_length)
```

### Analyze data
Calculating the mean, median, max, min - figures to determine statisical spead of membership type

```R
aggregate(trips20fill$ride_length ~ trips20fill$member_casual, FUN = mean)
aggregate(trips20fill$ride_length ~ trips20fill$member_casual, FUN = median)
aggregate(trips20fill$ride_length ~ trips20fill$member_casual, FUN = max)
aggregate(trips20fill$ride_length ~ trips20fill$member_casual, FUN = min)
```

Order day's of week within new dataset for future use

```R
trips20fill$day_of_week <- ordered(trips20fill$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```

Create a weekday field as well as view column specifics

```R
trips20fill %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, day_of_week ) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n())
```

### Data Visualiation's

```R
trips20fill$day_of_week  <- format(as.Date(trips20fill$date), "%A")
trips20fill %>%                              #total rides broken down by weekday
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(customer_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = customer_type)) + geom_col(position = "dodge") + 
  labs(x='Day of Week', y='Total Number of Rides', title='Rides per Day of Week', fill = 'Type of Membership') + 
  scale_y_continuous(breaks = c(250000, 400000, 550000), labels = c("250K", "400K", "550K"))
```

The rides per day of week show casual riders peak on the Saturday and Sunday while members peak Monday through Friday. This indicates members mainly use the bikes for their commutes and not leisure.


```R
trips20fill %>%   #total rides broken down by month
  group_by(member_casual, month) %>%  
  summarise(total_rides = n(),`average_duration_(mins)` = mean(ride_length)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=month, y=total_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x= "Month", y= "Total Number of Rides", title = "Rides per Month", fill = "Type of Membership") + 
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K")) + theme(axis.text.x = element_text(angle = 45))
```

The rides per month show that casual riders were a lot more active during the summer months than the long-term. Conversly, the winter months show very little activity on the part of the casual users. The long-term users are more active in the winter and spring months.

```R
trips20fill %>%    #looking at breakdown of bike types rented
  ggplot(aes(x = rideable_type, fill = member_casual)) + geom_bar(position = "dodge") + 
  labs(x= 'Type of Bike', y='Number of Rentals', title='Which bike works the most', fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000), labels = c("500K", "1Mil", "1.5Mil"))
```

The breakdown of which type of bike is the most popular among either type of user. Showing among the two types of bikes classic and electric. both types of memberships prefer using the classic bike more so than the electric bike. The long-term memebrs are also seen to be of the two types favours the classic bike.

```R
trips20fill %>%        #Find the average time spent riding by each membership type per individul day
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(x='Days of the week', y='Average duration - Hrs', title='Average ride time per week', fill='Type of Membership')
```

The average ride time shows a stark difference between the casuals and members. Casuals overall spend more time using the service than their full time member counter-parts.

### what does the data tell us?
### key takeaways
- Casual users tended to ride more so in the warmer months of Chicago, namely June- August. Their participation exceeded that of the long term members.
- To further that the Casual demographic spent on average a lot longer time per ride than their long-term counter-parts.
- The days of the week also further shows that causal riders prefer to use the service during the weekends as their usage peaked then. The long term members conversly utilised the service more-so throughout the typical work week i.e (Monday- friday)
- Long term riders tended to stick more so to classic bikes as opposed to the docked or electric bikes.

### Recommendations
*This report recommends the following:*
- Introducing plans thats may be more appealing to casuals for the summer months. This marketing should be done during the winter months in preperation.
- The casual users might be more interested in a memebrship option that allows for per-use balance card. Alternatively, the existing payment structure may be altered in order to make ---- single-use more costly to the casual riders as well as lowering the long-term membership rate.
- Membership rates specifically for the warmer months as well as for those who only ride on the weekends would assist in targeting the casual riders more specifically

### Things to Consider
### Additional points that were not examined
The report understands the scope of this analysis is extremely limited and because of that fact, additional data, as well as data points may have been able to contribute to this report offering an even more granular analysis. The following are data points that could have enhanced the report:

- Age and gender: This would add a dynamic to whether or not customers are being targeted across demograpic lines. Is the existing marketing effective? Is there potential for more inclusive targeting?
- Pricing structure: THe actual pricing plans data was not provided and would give further insight to which plans are the most popular and by (how much) when comparing them. It would also be effective to understanding the spending behaviour of casual user.
- Household income data: Pinpointing the average income of the long-term memebrs as compared to the casual counter-parts would allow for further analysis of what is the typical economic standing of each type of member, as well as providing the ability to analysis overall price sensitivity between the two different membership types.

# Thank you very much for your time!
🙂
