################### STA141 Assignment 2 #####################
################### Juanjuan Hu #############################
list.files("~/Desktop/201509-12/STA141statisticalComputing/homework/hw2/NASA")

# Step 1
########################################################################################################
# ff is a list, each element is a vector containing files of one variable
ff = lapply(c("cloudhigh", "cloudmid", "cloudlow", "ozone", "pressure", "surftemp", "temperature"), function(p) 
  list.files("~/Desktop/201509-12/STA141statisticalComputing/homework/hw2/NASA", pattern = p))
class(ff)

########## deal with the first txt first ##########
ch1 = readLines('~/Desktop/201509-12/STA141statisticalComputing/homework/hw2/NASA/cloudhigh1.txt')
class(ch1)
length(ch1)
View(ch1)
# substract the unique longitude, latitude and observed values
long = unlist(strsplit(ch1[6], " +"))[-1] # ' +' takes care of multiple spaces
lat = unlist(lapply(ch1[8:31], function(x) unlist(strsplit(x, " +"))[2]))
val = unlist(lapply(ch1[8:31], function(x) unlist(strsplit(x, " +"))[-c(1:4)]))
length(val)
# replicate long and lat correponding to the observed values
long1 = (rep(long, 24))
lat1 = rep(lat, each = 24)
# combine the long, lat and val
dim(long_lat_val1)
# substrct the date information
date = unlist(strsplit(ch1[5], "+ "))
date = date[unlist(lapply(date, function(x) nchar(x) > 5))]
date1 = rep(d, 24^2)
length(date1)
# combine the date with long, lat and val
cloudhigh_df1 = cbind(date1, long1, lat1, val)

######## deal with all the cloudhigh #########
# based on what we did for the first cloudhigh file,
# write a function such that for each file name input, we have a output which is a 576*4 data frame
f_stack = 
  function (x) {
    path = "~/Desktop/201509-12/STA141statisticalComputing/homework/hw2/NASA/"
    # read file into a vector of string, each string corresponds to one line in the file
    ch = readLines(paste0(path, x))
    # substract the unique longitude, which has a length of 24
    long = unlist(strsplit(ch[6], " +"))[-1] # ' +' takes care of multiple spaces
    # substract the unique latitude, which has a length of 24
    lat = unlist(lapply(ch[8:31], function(x) unlist(strsplit(x, " +"))[2]))
    # substract the values, which has a length of 24*24
    val = unlist(lapply(ch[8:31], function(x) unlist(strsplit(x, " +"))[-c(1:4)]))
    # convert the value into numeric. In this step, missing values denoted by "..." are converted into "NA"
    val1 = as.numeric(val)
    # replicate the longitude corrsponding to the values, now long1 has a length of 24*24
    long1 = (rep(long, 24))
    # replicate the latitude corrsponding to the values, now long1 has a length of 24*24
    lat1 = rep(lat, each = 24)
    # substract the date, which has a length of 1
    date = unlist(strsplit(ch[5], "+ "))
    date = date[unlist(lapply(date, function(x) nchar(x) > 5))]
    # replicate the date, now date1 has a length of 24*24
    date1 = rep(date, 24^2)
    # substract the value name from the file name
    valuename = gsub("[0-9].*", "", x)
    df = data.frame(date1, lat1, long1, val1)
    names(df) = c("date", "lat", "long", valuename)
    df
  }

# the following function returns a list of 7 data frame
listofDF = lapply(1:length(ff), function(i) {
  # for each file in ff[i], call f_stack function to generate a tidied data frame with dim of 576 by 4 
  tmp = lapply(unlist(ff[i]), f_stack)
  # combine all the 72 data frame in ff[i], giving out a data frame with 41472 rows and 4 columns
  do.call('rbind', tmp)
})

# check
class(listofDF)
length(listofDF)
class(listofDF[[1]])
View(listofDF[[3]])

# assign names to df
names(listofDF) = c("cloudhigh", "cloudmid", "cloudlow", "ozone", "pressure", "surftemp", "temperature")
# check
head(listofDF[["surftemp"]])

# Step 2
####################################################################################################
# record the date from cloudhigh data frame into a vector
date_long_lat_cloudhigh = listofDF[["cloudhigh"]][, c("date", "long", "lat")]
dim(date_long_lat_cloudhigh) # check whether the length is 41472
# for each data frame in listofDF, see whether the date, longitude and latitude are identical to 
# date_long_lat_cloudhigh
lapply(listofDF, function(x) sum(x[, c("date", "long", "lat")] != date_long_lat_cloudhigh))
# The result shows that grid points are the same across variables, combine them
valueOnly = lapply(listofDF, function(df) df[,4])
allValues = as.data.frame(do.call('cbind', valueOnly))
finalDF = cbind(listofDF[[1]][,1:3], allValues)
dim(finalDF)
head(finalDF)
sum(is.na(finalDF))

# do.call('merge', listofDF) #??????

# Step 3
##################################################################################################
# read the intlvtn.dat into R
ii = read.table("~/Desktop/201509-12/STA141statisticalComputing/homework/hw2/NASA/intlvtn.dat", 
                header = TRUE)
class(ii)
dim(ii)
# substract the longitude into a vector
longofEve = unlist(lapply(colnames(ii), function(x) unlist(strsplit(x, "X."))[2]))
longofEve = -1*as.numeric(longofEve)
# substract the latitude into a vector
latofEve = as.numeric(rownames(ii))
# From step2, we know for the cloudhigh, cloudmid and other 5 variables, the longitude and latitude are identical,
# so we can compare the longitude from cloudhigh1.txt with the longitude from intlvtn.dat
ch1 = readLines('~/Desktop/201509-12/STA141statisticalComputing/homework/hw2/NASA/cloudhigh1.txt')
long_ch1 = unlist(strsplit(ch1[6], " +"))[-1] # ' +' takes care of multiple spaces
# transform the longitude of ch1 into integers
longofCh1 = -1*as.numeric(unlist(lapply(long_ch1, function(x) gsub("[A-Z]", "", x))))
# compare longofEve and long_ch1, if their difference are equal to or less than 0.05, we regard them equal
sum(round(abs(longofCh1-longofEve),14) > 0.05) # use 0.050001 instead of 0.05 here
# latitude from ch1
lat_ch1 = unlist(lapply(ch1[8:31], function(x) unlist(strsplit(x, " +"))[2]))
# create a vector with value of 1 or -1. If latitude ends with "N", assign 1 correspondingly, if with "S", assign -1.
north_south = ifelse(grepl("N", lat_ch1), 1, -1)
lat_ch1 = as.numeric(unlist(lapply(lat_ch1, function(x) gsub("[A-Z]", "", x))))
latofCh1 = lat_ch1*north_south
# compare latofEve and lat_ch1, if their difference are equal to or less than 0.05, regard them equal
sum(round(abs(latofCh1-latofEve),14) > 0.05)
# substract the elevation values and replicate corresponding to the finalDF
ele = unlist(lapply(1:nrow(ii), function(i) ii[i,]))
class(ele)
length(ele)
elevation = rep(ele, 72)
# add the elevation info into the big data frame
dfWithEle = cbind(finalDF, elevation)
head(dfWithEle)
dim(dfWithEle)

# transform the longitude and latitude in the big data frame into integers
lat_tmp = dfWithEle$lat
length(lat_tmp)
n_s = ifelse(grepl("N", lat_tmp), 1, -1)
length(n_s)
lat_tmp1 = as.numeric(unlist(lapply(lat_tmp, function(x) gsub("[A-Z]", "", x))))
latFromDf = lat_tmp1*n_s
# check
length(latFromDf)
sum(latFromDf < 0)
# replace the lat in finalDF
dfWithEle$lat = latFromDf
long_tmp = dfWithEle$long
long_tmp1 = -1*as.numeric(unlist(lapply(long_tmp, function(x) gsub("[A-Z]", "", x))))
dfWithEle$long = long_tmp1
tail(dfWithEle) # check

# Step 4
############################################################################################
###### Plot temperature versus pressure. Color code the points by the value of cloudlow.
library(ggplot2)
ggplot(dfWithEle, aes(x = temperature, y = pressure, colour = cloudlow)) +
  geom_point() +
  scale_color_continuous(low="blue", high="red", na.value = "black") +
  ggtitle("Plot for temperature and pressure")

###### For the points at the four corners of the spatial grid, display the values of temperature over time.
###### call f_stack function we define above to generate a list of 72 data frame for temperature values.
temp = lapply(ff[[7]], f_stack)
# check
class(temp)
length(temp)
temp[[1]]
# the four corner values for each data frame is the 1st, 24th, 553th(24*24-24+1), 576th element
# subset these value and their corresponding date information for each of data frame
cornerTemp_list = lapply(temp, function(x) x[c(1,24,553,576),c(1,4)])
# combine the 72 4*4 data frame
cornerTemp = do.call('rbind', cornerTemp_list)
# the date is now a factor, convert it to date type
cornerTemp$date = as.Date(cornerTemp$date, "%d-%b-%Y")
# sort the data frame by date
cornerTemp = cornerTemp[order(cornerTemp$date),]
dim(cornerTemp)
# adjust the rownames 
rownames(cornerTemp) = 1:288
# calculate average temperature for each date
aggdata = aggregate(cornerTemp, list(cornerTemp$date), mean, na.rm=TRUE)
# plot
ggplot(cornerTemp, aes(x = date, y = temperature)) +
  geom_point() +
  geom_point(data = aggdata, color = "red") +
  ggtitle("Plot for temperature over time")
# let's see the points from 4 corners respectively
cornerTemp_f = function(i) {
  cornerTemp_list = lapply(temp, function(x) x[i,c(1,4)])
  cornerTemp = do.call('rbind', cornerTemp_list)
  cornerTemp$date = as.Date(cornerTemp$date, "%d-%b-%Y")
  cornerTemp[order(cornerTemp$date),]
}
corners = lapply(c(1,24,553,576), cornerTemp_f)
names(corners) = c("top_left", "top_right", "bottom_left", "bottom_right")
par(mfrow = c(2,2))
lapply(names(corners), function(x) plot(corners[[x]]$date, corners[[x]]$temperature, 
                                        type = "l",
                                        main = paste0(x," temperature \nover time"),
                                        xlab = "date",
                                        ylab = "temperature"))

###### For all points on the grid, compute the average and standard deviation 
###### for each of the 7 variables across time.
listOfNames = c("cloudhigh", "cloudmid", "cloudlow", "ozone", "pressure", "surftemp", "temperature")
mean_sd_f = function(i) {
  data = dfWithEle[, c("lat", "long", listOfNames[i])]
  mean_data = aggregate(data, list(data$lat, data$long), mean, na.rm=TRUE)
  # calculate standard deviation of temperature for each date
  sd_data = aggregate(data, list(data$lat, data$long), sd, na.rm=TRUE)
  # combine mean and sd value together and rename the columns
  mean_sd_data = cbind(mean_data[3:5], sd_data[,5])
  colnames(mean_sd_data) = c("lat", "long", paste0("mean_", listOfNames[i]), paste0("sd_", listOfNames[i]))
  mean_sd_data
}
meanAndSd = lapply(1:length(ff), mean_sd_f)
names(meanAndSd) = listOfNames
# check
class(meanAndSd)
head(meanAndSd[["cloudhigh"]])

###### Display the average value for pressure computed in the previous question on a map.
meanLon = mean(meanAndSd[["pressure"]]$long)
meanLat = mean(meanAndSd[["pressure"]]$lat)

library(ggmap)
map = get_map(location = c(long = meanLon, lat = meanLat), zoom =3)
ggmap(map) +
  geom_point(aes(x = long, y = lat, colour = mean_pressure), data = meanAndSd[["pressure"]]) +
  xlim(c(min(meanAndSd[["pressure"]]$long), max(meanAndSd[["pressure"]]$long))) +
  ylim(c(min(meanAndSd[["pressure"]]$lat), max(meanAndSd[["pressure"]]$lat))) +
  ggtitle("Average pressures across time")

# contour plot
ggmap(map) +
  #ggplot(meanAndSd[["pressure"]], aes(long, lat, z = mean_pressure)) +
  stat_contour(data = meanAndSd[["pressure"]], aes(x = long, y = lat, z = mean_pressure, colour = ..level..),
               bins = 30) +
  scale_colour_gradient(name = "mean pressure", low = "red", high = "blue" ) +
  xlim(c(min(meanAndSd[["pressure"]]$long), max(meanAndSd[["pressure"]]$long))) +
  ylim(c(min(meanAndSd[["pressure"]]$lat), max(meanAndSd[["pressure"]]$lat))) +
  ggtitle("Average pressures across time")

###### Display average surface temperature versus elevation.
# sort the latitude for average surface temperature
ss = meanAndSd[["surftemp"]][order(meanAndSd[["surftemp"]]$lat, decreasing = TRUE),]
# check whether the latitudes for average surface temperature and the latitudes for elevation uniform
all.equal(ss$lat, dfWithEle$lat[1:576])
surf_ele = as.data.frame(cbind(ss$mean_surftemp, dfWithEle$elevation[1:576]))
colnames(surf_ele) = c("surftemp", "elevation")
ggplot(surf_ele, aes(x = surftemp, y = elevation)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Plot of relationship between \naverage surface temperature and elevation")

