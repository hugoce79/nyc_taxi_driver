####################################################################
###
### Team Entrepreneur-sheep
### (Jungmin, Ginda, Christoph, Yuyao, Brenda)
### Convert longitude and latitude to Zipcode
### Clean invalid data
###
####################################################################
tripdata <- read.csv("C:/Program Files/RStudio/resources/Brenda_2502-5001.csv", 
                          header=TRUE, stringsAsFactors=FALSE)

tripdata_valid <- subset(tripdata, pickup_longitude != 0 & pickup_latitude != 0)


library("ggmap")
tripdata_valid$Zipcode <- as.character(0)
#tripdata_valid$Neighborhood <- as.character(0)
#tripdata_valid$Address <- as.character(0)

for (i in 1:nrow(tripdata_valid)){
# generate a single example address
lonlat_sample <- c(tripdata_valid$pickup_longitude[i], tripdata_valid$pickup_latitude[i])
lonlat_sample  # note the order is longitude, latitiude

res <- revgeocode(lonlat_sample, output="more")
# can then access zip and neighborhood where populated
tripdata_valid$Zipcode[i] <-  as.character(res$postal_code)
#tripdata_valid$Neighborhood[i] <- as.character(res$neighborhood)
#tripdata_valid$Address[i] <- as.character(res$address)
}

write.csv(tripdata_valid, file="yellowtaxi.csv", row.names=FALSE)
