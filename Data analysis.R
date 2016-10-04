####################################################################
###
### Team Entrepreneur-sheep
### (Jungmin, Ginda, Christoph, Yuyao, Brenda)
### Final data analysis
###
####################################################################

library("sqldf")
library("PerformanceAnalytics")
library("ggplot2")

#Create Main Table
tripData <- read.csv("C:/Users/entrepreneurshipTeam/Desktop/YELLOW/2015-05-Zipcode.csv", 
                     header=TRUE, stringsAsFactors=FALSE)
weather <- read.csv("C:/Users/entrepreneurshipTeam/Desktop/YELLOW/central_park_weather.csv")
density <- read.csv("C:/Users/entrepreneurshipTeam/Desktop/YELLOW/2010_Census_zipcode_density.csv")
unemployment <- read.csv("C:/Users/entrepreneurshipTeam/Desktop/YELLOW/2010_Census_zipcode_unemployment.csv")
demographics <- read.csv("C:/Users/entrepreneurshipTeam/Desktop/YELLOW/2010_Census_zipcode_demographics.csv")
weatherByHour <- read.csv("C:/Users/entrepreneurshipTeam/Desktop/weather_data_hour.csv")

tripdata <- subset(sqldf("SELECT * FROM tripData 
                         JOIN weather ON tripData.pickup_date = weather.DATE  
                         JOIN density ON tripData.zipcode =density.zipcode
                         JOIN unemployment ON tripData.zipcode =unemployment.zip
                         JOIN demographics ON tripData.zipcode =demographics.zipcode
                         "))
tripdataW <- subset(sqldf("SELECT * FROM tripdata 
                         JOIN weatherByHour ON tripdata.pickup_dayhour = weatherByHour.dayhour 
                         "))
write.csv(tripdataW, file="yellowtaxiFullAggregated.csv", row.names=FALSE)

# Read the final dataset
yellow <- read.csv("C:/Users/entrepreneurshipTeam/Desktop/yellowtaxiFullAggregated.csv")



#Exploration
tripdata1 <- subset(sqldf("select fare_amount,tip_amount from tripdata"))
tripdata2 <- subset(sqldf("select fare_amount,tip_amount,prcp,tmin,tmax,awnd,snwd,trip_distance,tolls_amount from tripdata"))

tripdatadate <- subset(sqldf("select count(pickup_date) as countdate, PRCP,snow, tmax  from tripdata 
                             group by pickup_date"))
tripdataGeo <- subset(sqldf("SELECT ROUND(pickup_latitude, 4) AS lat,
                            ROUND(pickup_longitude, 4) AS long,
                            COUNT(*) AS num_pickups,
                            sum(fare_amount) AS total_revenue
                            FROM tripdata
                            WHERE fare_amount/trip_distance BETWEEN 2 AND 10
                            GROUP BY lat, long"))
tripdataZipcode1 <- subset(sqldf("SELECT count(*) as count, avg(passenger_count) AS avg_passenger_count,
                                 avg(trip_distance) AS trip_distance,
                                avg(tip_amount) AS avg_tip,avg(fare_amount) AS avg_fare,
                                avg(PRCP) as avg_prcp, avg(tmax) as avg_tmax, avg(unemprate) as avg_unemp,
                                avg(pctasian) as avg_pctasian, avg(pctwhite) as avg_pctwhite, 
                                avg(pcthispanic) as avg_pcthispanic, avg(housing) as avg_housing
                                FROM yellow
                                GROUP BY pickup_hour"))

tripdataZipcode2 <- subset(sqldf("SELECT pickup_weekday,
                                avg(fare_amount) AS avg_fare,avg(pickup_hour),
                                avg(PRCP), avg(tmax), avg(unemprate),
                                avg(pctasian), avg(pctwhite), avg(pcthispanic) , avg(housing)
                                FROM yellow
                                GROUP BY zipcode"))

tripdataZipcode3 <- subset(sqldf("SELECT count(*) as count, avg(SPD) AS avg_spd
                            FROM yellow
                                GROUP BY hr"))


# Table grouped by zipcode and table grouped by day
tripdataZipcode <- subset(sqldf("SELECT count(*) as count, sum(total_amount) as total_fare, avg(passenger_count) AS avg_passenger_count,
                                avg(trip_distance) AS trip_distance, 
                                avg(tip_amount) AS avg_tip, avg(fare_amount) AS avg_fare,
                                avg(PRCP) as avg_prcp, avg(tmax) as avg_tmax, avg(unemprate) as    avg_unemp,
                                avg(pctasian) as avg_pctasian, avg(pctwhite) as avg_pctwhite,  avg(pctblack) as avg_pctblack,  
                                avg(pcthispanic) as avg_pcthispanic, avg(housing) as avg_housing
                                FROM yellow
                                GROUP BY Zipcode"))

tripdataDay <- subset(sqldf("SELECT count(*) as count, sum(total_amount) as total_fare, avg(passenger_count) AS avg_passenger_count,
                                avg(trip_distance) AS trip_distance, 
                                avg(tip_amount) AS avg_tip, avg(fare_amount) AS avg_fare, avg(SPD) as wind_speed,
                                avg(PRCP) as avg_prcp, avg(tmax) as avg_tmax, avg(unemprate) as    avg_unemp,
                                avg(pctasian) as avg_pctasian, avg(pctwhite) as avg_pctwhite, 
                                avg(pcthispanic) as avg_pcthispanic, avg(housing) as avg_housing
                                FROM yellow
                                GROUP BY DA"))

#Plot map revenue
ggplot(tripdataGeo , aes(x=long, y=lat, z=total_revenue)) +
  geom_point(size=0.06, color="#999999") +
  stat_summary_hex(fun = sum, bins=100, alpha=0.7) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  scale_fill_gradient(low="#CCCCCC", high="#27AE60") +
  labs(title = "Total Revenue for NYC Yellow Taxis by pickup Zipcode, May 2015") +
  coord_equal()

# Plot and correlation
chart.Correlation(tripdataZipcode, histogram=TRUE, pch=19) 	
cor(x = tripdataZipcode$count, y = tripdataZipcode$avg_pcthispanic, use = "everything",method = c("pearson"))

# LinearModel grouped by Zipcode
trip_model <- lm(count ~  avg_unemp + avg_pctasian + avg_pctwhite + avg_pcthispanic, data=tripdataZipcode)

trip_model <- lm(avg_fare ~ avg_unemp + avg_pctasian + avg_pctwhite + avg_pcthispanic + avg_housing, data=tripdataZipcode)

trip_model <- lm(total_fare ~  avg_pctasian + avg_pctwhite + avg_pcthispanic + avg_pctblack, data=tripdataZipcode)


# LinearModel grouped by Day
trip_model <- lm(count ~ avg_prcp + avg_tmax + wind_speed, data=tripdataDay)

summary(trip_model) 

