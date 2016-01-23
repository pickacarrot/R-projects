# load data
Url = url('http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda')
load(Url)
library(ggplot2)

## Q1 ## Find at least 3 types of anomalies in the data. 
######## Provide succinct justification for identifying them as anomalies. 
######## Then correct the corresponding observations appropriately, again providing justification. 
######## What impact does this have on analyzing the data?

# 1. odometer
vposts_noMissingOdometer = vposts[!is.na(vposts$odometer),]
max(vposts_noMissingOdometer$odometer)
# imagine a car running 60 miles per hour for 24 hours every day for 100 years
cap = 24*60*115*365
subset(vposts_noMissingOdometer, odometer > cap)
# subset data with odometer higher than 999999
vposts_highOdometer = subset(vposts_noMissingOdometer, odometer > 999999)
# check how old are these cars with high odometer 
years = names(table(vposts_highOdometer$year))
# it is a character vector
class(years)
# subset vposts for corresponding years
temp = subset(vposts, year %in% as.integer(years) & odometer < 999999 & !is.na(odometer))
# for each year, calculate the median of odometer
odometer_median = aggregate(odometer ~ year, temp, median)
# 24 is the 24th element of each row of vposts_highOdometer which is the year information
fixed_odometer = apply(vposts_highOdometer, 1, function(x) x$odometer = odometer_median[odometer_median$year == x[24],2])
# fix the odometer
vposts_highOdometer$odometer = fixed_odometer
# create a new column to store the correced values
vposts$myOdometer = ifelse(vposts$id %in% vposts_highOdometer$id, vposts_highOdometer$odometer, vposts$odometer)
# check the new variable
max(vposts$myOdometer, na.rm = TRUE)
# comparison plots
ggplot(vposts, aes(x = odometer)) +
  geom_density() +
  geom_rug() +
  ggtitle("Distribution of odometer before correction")
ggplot(vposts, aes(x = myOdometer)) +
  geom_density() +
  geom_rug() +
  ggtitle("Distribution of odometer after correction")

# 2.odometer and condition
# 8 most common conditons
commonCondition = sort(table(vposts$condition), decreasing = TRUE)[1:8]
vposts$condition1 = ordered(as.character(vposts$condition),  levels = c("salvage", "fair", "good", "very good", 
                                                                        "excellent", "like new", "certified", "new"))
ggplot(vposts, aes(x = condition1, y = odometer)) +
  ylim(c(0, 999999)) +
  geom_boxplot(fill = 'purple', alpha = 0.2) +
  theme(axis.text=element_text(size=12)) +
  ggtitle("Relationship between condition and odometer")


new_highOdometer = subset(vposts, vposts$condition == "new" & vposts$odometer > 1000)
max(new_highOdometer$odometer)
# find the most common condition for cars within three groups devided by odometer: 1000 ~ 5000, 5000 ~ 10000, 10000 ~ 100000, 100000 ~ Inf
tapply(vposts$condition, cut(vposts$odometer, c(1000, 10000, 50000, 100000, Inf)), function (x) names(sort(table(x), decreasing = TRUE))[1])
# create a new column to store the correced values
vposts$myCondition = ifelse(vposts$id %in% new_highOdometer$id, "excellent", as.character(vposts$condition))
# plot after correction
commonCondition2 = sort(table(vposts$myCondition), decreasing = TRUE)[1:8]
vposts$condition2 = ordered(as.character(vposts$myCondition),  levels = c("salvage", "fair", "good", "very good", 
                                                                          "excellent", "like new", "certified", "new"))
ggplot(vposts, aes(x = condition2, y = odometer)) +
  ylim(c(0, 999999)) +
  geom_boxplot(fill = 'green', alpha = 0.2) +
  theme(axis.text=element_text(size=12)) +
  ggtitle("Relationship between \ncorrected condition and odometer")

# 3. fuel and year
# plot the year and fuel 
ggplot(vposts, aes(x = fuel, y = year)) +
  geom_point() +
  ylim(c(1900, 2015)) +
  ggtitle("Relationship of fuel and year")
# correction
subset(vposts, vposts$fuel == "electric" & vposts$year < 2000)$description
vposts$myFuel = ifelse(vposts$fuel == "electric" & vposts$year < 2000, "gas", as.character(vposts$fuel))
# plot after correction
ggplot(vposts, aes(x = myFuel, y = year)) +
  geom_point() +
  ylim(c(1900, 2015)) +
  ggtitle("Relationship of corrected fuel and year")

# 4. year

table(vposts$year)
subset(vposts, year ==4 | year == 2022)
# correct
vposts$myYear = ifelse(vposts$year == 4, 2004, ifelse(vposts$year == 2022, NA, vposts$year))
# check the new variable
table(vposts$myYear)

## Q2 ## Find at least 3 interesting insights/characteristics/features illustrated by the data. 
######## Explain in what way these insights are interesting (to whom? why?) 
######## and provide evidence for any inference/conclusions you draw. 
######## How generalizable are these insights to other vehicle sales data?


# 1. Relationship between drive and city
drive_city = as.data.frame(with(vposts, table(drive, city)))
ggplot(drive_city, aes(x = Freq, y = city, color = drive)) +
  geom_point(size = 8) +
  ggtitle("The number of sale for each drive type across cities")

# 2. For a certain maker, the distribution of price across the odometer groups for "sale by owner" and "sale by dealer"
maker_table = table(vposts$maker)
length(maker_table)
commonMaker = sort(maker_table, decreasing = TRUE)[1:10]
# subset cars made by ford
sub_ford = subset(vposts, maker == "ford")
# cut odometer into 5 groups
groupOfOdometer1 = cut(sub_ford$odometer, c(0, 1000, 10000, 50000, 100000, Inf))
# plot
ggplot(sub_ford, aes(x = groupOfOdometer1, y = price, fill = byOwner)) +
  geom_boxplot() +
  ylim(c(0, 200000)) +
  ggtitle("Ford's price distribution \nacross odometer for sale by owner and by dealer") +
  theme(axis.text=element_text(size=12))
# subset cars made by chevrolet
sub_chevy = subset(vposts, maker == "chevrolet")
groupOfOdometer2 = cut(sub_chevy$odometer, c(0, 1000, 10000, 50000, 100000, Inf))
ggplot(sub_chevy, aes(x = groupOfOdometer2, y = price, fill = byOwner)) +
  geom_boxplot() +
  ylim(c(0, 200000)) +
  ggtitle("Chevrolet's price distribution \nacross odometer for sale by owner and by dealer") +
  theme(axis.text=element_text(size=12))
# subset cars made by toyota
sub_toyota = subset(vposts, maker == "toyota")
groupOfOdometer3 = cut(sub_toyota$odometer, c(0, 1000, 10000, 50000, 100000, Inf))
ggplot(sub_toyota, aes(x = groupOfOdometer3, y = price, fill = byOwner)) +
  geom_boxplot() +
  ylim(c(0, 200000)) +
  ggtitle("Toyota's price distribution \nacross odometer for sale by owner and by dealer") +
  theme(axis.text=element_text(size=12))
# subset cars made by honda
sub_honda = subset(vposts, maker == "honda")
groupOfOdometer4 = cut(sub_honda$odometer, c(0, 1000, 10000, 50000, 100000, Inf))
ggplot(sub_honda, aes(x = groupOfOdometer4, y = price, fill = byOwner)) +
  geom_boxplot() +
  ylim(c(0, 200000)) +
  ggtitle("Honda's price distribution \nacross odometer for sale by owner and by dealer") +
  theme(axis.text=element_text(size=12))

# 3. the gap between posted time and updated time, and whether the gap is different 
# between for sale by owner and for sale by dealer.
# add a column to show the time diffrence between posted time and updated time
vposts$timeDiff = as.integer(difftime(vposts$updated, vposts$posted, units = "secs"))
typeof(vposts$timeDiff)
sub_timeDiff = vposts[!is.na(vposts$timeDiff),]
# check
head(sub_timeDiff, 5)
tail(sub_timeDiff, 5)
dim(subset(sub_timeDiff, byOwner == TRUE))
dim(subset(sub_timeDiff, byOwner == FALSE))
# plot
ggplot(sub_timeDiff, aes(x = byOwner, y = timeDiff)) +
  geom_boxplot() +
  ggtitle("The difference between posted time and updated time\nfor sale by owner and by dealer")



