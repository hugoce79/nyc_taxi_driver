####################################################################
###
### Team Entrepreneur-sheep
### (Jungmin, Ginda, Christoph, Yuyao, Brenda)
### Exploring dataset by using summary data
###
####################################################################
#raw data1
taxidata <- read.csv("C:/Program Files/RStudio/resources/Yello taxi project.csv", 
                     header=TRUE, stringsAsFactors=FALSE)
#raw data2-organized
taxidata_raw <- read.csv("C:/Program Files/RStudio/resources/Yello taxi project_3.csv", 
                         header=TRUE, stringsAsFactors=FALSE)

#day summary with weather
taxidata_summary <- read.csv("C:/Program Files/RStudio/resources/Yello taxi project_summary_by day.csv", 
                     header=TRUE, stringsAsFactors=FALSE)

#hour summary
taxidata_summary2 <- read.csv("C:/Program Files/RStudio/resources/Yello taxi project_summary_by hour.csv", 
                             header=TRUE, stringsAsFactors=FALSE)
#hour summary with weather
weather <- read.csv("C:/Program Files/RStudio/resources/weather data_hour.csv", 
                         header=TRUE, stringsAsFactors=FALSE)
#sub data
taxidata_raw_sub <- read.csv("C:/Program Files/RStudio/resources/taxidata_raw_sub.csv", 
                         header=TRUE, stringsAsFactors=FALSE)
#Zipcode summary
taxidata_zipcode <- read.csv("C:/Program Files/RStudio/resources/Taxidata_group by zipcode.csv", 
                             header=TRUE, stringsAsFactors=FALSE)

# precipitation_yes or no
taxidata_summary$precipitation_True<- as.logical(0)


for (i in 1:nrow(taxidata_summary)){
  if (taxidata_summary$precipitation[i]!=0)
    taxidata_summary$precipitation_True[i]<-as.logical(1)
}

# check correlation
cor(x = taxidata_summary$precipitation_True, y = taxidata_summary$Count.of.Zipcode, use = "everything",method = c("pearson"))
cor(x = taxidata_summary$max_temperature, y = taxidata_summary$Count.of.Zipcode, use = "everything",method = c("pearson"))
cor(x = taxidata_summary$min_temperature, y = taxidata_summary$Count.of.Zipcode, use = "everything",method = c("pearson"))
cor(x = taxidata_summary$average_wind_speed, y = taxidata_summary$Count.of.Zipcode, use = "everything",method = c("pearson"))
#cor(x = taxidata_summary$average_weekend, y = taxidata_summary$Count.of.Zipcode, use = "everything",method = c("pearson"))
#cor(x = taxidata_summary$average_Sunday, y = taxidata_summary$Count.of.Zipcode, use = "everything",method = c("pearson"))

# linear model gropued by day
Day_count1<- lm(Count.of.Zipcode ~ precipitation_True + max_temperature + min_temperature + average_wind_speed
                + Sunday, data=taxidata_summary)
#                  + weekend + Sunday, data=taxidata_summary)
summary(Day_count1)  

Day_count2<- lm(Count.of.Zipcode ~  average_wind_speed
               , data=taxidata_summary)
summary(Day_count2) # windy day people don't want to go out

# linear model gropued by hour
# Correlation
cor(x = taxidata_summary2$hour, y = taxidata_summary2$count, use = "everything",method = c("pearson"))

#barplot
barplot(taxidata_summary2$count)

#intial data setting(Sunday, weekend, hours)
taxidata_raw$sunday <- as.logical(0)
taxidata_raw$weekend <- as.logical(0)
taxidata_raw$hours <- as.numeric(0)

for (i in 1:nrow(taxidata_raw)){
  if (taxidata_raw$tpep_pickup_day[i]==3)
    taxidata_raw$sunday[i]<-as.logical(1)
  else if (taxidata_raw$tpep_pickup_day[i]==10)
    taxidata_raw$sunday[i]<-as.logical(1)
  else if (taxidata_raw$tpep_pickup_day[i]==17)
    taxidata_raw$sunday[i]<-as.logical(1)
  else if (taxidata_raw$tpep_pickup_day[i]==24)
    taxidata_raw$sunday[i]<-as.logical(1)
  
  if (taxidata_raw$tpep_pickup_day[i]==3)
    taxidata_raw$weekend[i]<-as.logical(1)
  else if (taxidata_raw$tpep_pickup_day[i]==10)
    taxidata_raw$weekend[i]<-as.logical(1)    
  else if (taxidata_raw$tpep_pickup_day[i]==17)
    taxidata_raw$weekend[i]<-as.logical(1)    
  else if (taxidata_raw$tpep_pickup_day[i]==24)
    taxidata_raw$weekend[i]<-as.logical(1)    
  else if (taxidata_raw$tpep_pickup_day[i]==2)
    taxidata_raw$weekend[i]<-as.logical(1)
  else if (taxidata_raw$tpep_pickup_day[i]==9)
    taxidata_raw$weekend[i]<-as.logical(1)  
  else if (taxidata_raw$tpep_pickup_day[i]==16)
    taxidata_raw$weekend[i]<-as.logical(1)  
  else if (taxidata_raw$tpep_pickup_day[i]==23)
    taxidata_raw$weekend[i]<-as.logical(1)  
  
  if (taxidata_raw$tpep_pickup_apm[i]=="PM")
    taxidata_raw$hours[i] <- taxidata_raw$tpep_pickup_hour[i] + 12
  else if (taxidata_raw$tpep_pickup_apm[i]=="AM")
    taxidata_raw$hours[i] <- taxidata_raw$tpep_pickup_hour[i]
}

# Write dataset not to process same things 
write.csv(taxidata_raw, file="taxidata_raw.csv", row.names=FALSE)

# merge dataset with weather dataset
taxiweather <- merge(x = taxidata_raw, y = weather, by = "DAHR", all.x = TRUE)

# make a subset grouped by hours
library("sqldf")
taxihour <- subset(sqldf("select  hours, count(hours) as number_of_taxi,avg(total_amount) as total_fare, avg(total_amount) as avg_fare from taxiweather group by hours"))

# initial setting(whether hour is between three and five, whether hour is between four and five)
taxihour$three_to_five <- as.logical(0)
taxihour$four_to_five <- as.logical(0)

for (i in 1:nrow(taxihour)){
  if (taxihour$hours[i]=="4")
    taxihour$three_to_five[i] <- as.logical(1)
  else if (taxihour$hours[i]=="5")
    taxihour$three_to_five[i] <- as.logical(1)
  else if (taxihour$hours[i]=="3")
    taxihour$three_to_five[i] <- as.logical(1)

  if (taxihour$hours[i]=="4")
    taxihour$four_to_five[i] <- as.logical(1)
  else if (taxihour$hours[i]=="5")
    taxihour$four_to_five[i] <- as.logical(1)
  
}

# Correlation
cor(x = taxihour$three_to_five, y = taxihour$total_fare, use = "everything",method = c("pearson"))
cor(x = taxihour$four_to_five, y = taxihour$avg_fare, use = "everything",method = c("pearson"))

# linear model grouped by hour
taxi_model3 <- lm(number_of_taxi ~ three_to_five, data=taxihour)
summary(taxi_model3)  

taxi_model4 <- lm(avg_fare ~  four_to_five, data=taxihour)
summary(taxi_model4)  

# try to find every relatioship by pairs function
library("car")

pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris)
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Iris Scatterplot Matrix")

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~tpep_pickup_day+trip_distance+RateCodeID+store_a0d_fwd_flag+payment_type+extra+mta_tax+tip_amount+tolls_amount+improvement_surcharge+fare_amount+passenger_count+sunday+hours+total_amount+Zipcode+weekend, data=taxidata_raw_sub,
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Yellow cab Scatterplot Matrix")

# Figuring out correlation between two variables
cor(x = taxidata_raw$total_amount, y = taxidata_raw$passenger_count, use = "everything",method = c("pearson"))
cor(x = taxidata_raw$total_amount, y = taxidata_raw$sunday, use = "everything",method = c("pearson"))
cor(x = taxidata_raw$total_amount, y = taxidata_raw$hours, use = "everything",method = c("pearson"))
cor(x = taxidata_raw$total_amount, y = taxidata_raw$weekend, use = "everything",method = c("pearson"))

cor(x = taxiweather$total_amount, y = taxiweather$SPD, use = "everything",method = c("pearson"))
cor(x = taxiweather$total_amount, y = taxiweather$GUS, use = "everything",method = c("pearson"))
cor(x = taxiweather$total_amount, y = taxiweather$CLG, use = "everything",method = c("pearson"))
cor(x = taxiweather$total_amount, y = taxiweather$VSB, use = "everything",method = c("pearson"))
cor(x = taxiweather$total_amount, y = taxiweather$TEMP, use = "everything",method = c("pearson"))
cor(x = taxiweather$total_amount, y = taxiweather$DEWP, use = "everything",method = c("pearson"))
cor(x = taxiweather$total_amount, y = taxiweather$PCP01, use = "everything",method = c("pearson"))

# Use Chi-square test if there is a categorical variable 
taxidata_raw$Zipcode <- as.factor(taxidata_raw$Zipcode)

cross.Zipcode_fare <- CrossTable(taxidata_raw$total_amount, taxidata_raw$Zipcode
                            , prop.r = FALSE, prop.chisq = FALSE)
cross.Zipcode_fare.count <- cross.Zipcode_fare$t
chisq.test(cross.Zipcode_fare.count) 

# linear model grouped by day and hour
taxi_model <- lm(total_amount ~ sunday + weekend + hours + passenger_count + Zipcode + payment_type 
#                 + SPD + GUS + CLG + VSB + TEMP + DEWP + PCP01, data=taxiweather)
 + GUS + TEMP , data=taxiweather)
summary(taxi_model)  

# Chi-square test grouped by Zipcode
library(gmodels)
taxidata_zipcode$Zipcode <- as.factor(taxidata_zipcode$Zipcode)
cross.zipcode <- CrossTable(taxidata_zipcode$Average.of.total_amount, taxidata_zipcode$Zipcode
                            , prop.r = FALSE, prop.chisq = FALSE)
cross.zipcode.count <- cross.zipcode$t
chisq.test(cross.zipcode.count)

cross.zipcode2 <- CrossTable(taxidata_zipcode$Number.of.taxi, taxidata_zipcode$Zipcode
                            , prop.r = FALSE, prop.chisq = FALSE)
cross.zipcode2.count <- cross.zipcode2$t
chisq.test(cross.zipcode2.count)
#cor(x = taxidata_zipcode$Average.of.total_amount, y = taxidata_zipcode$Number.of.taxi, use = "everything",method = c("pearson"))
