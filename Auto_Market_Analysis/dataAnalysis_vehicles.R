################### STA141 Assignment 1 Part I #################
################### Juanjuan Hu ################################

# load the vehicles data into r
Url = url('http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda')
print(load(Url))


##Q1## How many observations are there in the data set?
# Investigate the class of data
class(vposts)
# For data frame, each line is one obs. So we look into the number of rows.
nrow(vposts)


##Q2## What are the names of the variables? and what is the class of each variable?
# Variables names are column names in a data frame
names(vposts)
# Use 'sapply' to return a list of class of each variable
classList = sapply(vposts, class)
str(classList)

##Q3## What is the average price of all the vehicles? the median price? and the deciles? 
###### Displays these on a plot of the distribution of vehicle prices.
# Check whether there is missing values in price
summary(vposts$price)
# Calculate mean, median and deciles by removing all missing price
avg = mean(vposts$price, na.rm = TRUE)
med = median(vposts$price, na.rm = TRUE)
dec = quantile(vposts$price, seq(0, 1, 0.1), na.rm = TRUE)
# There is a huge difference between mean and median. Mean is much bigger than median, 
# showing the dist of vehicle price is right skewed.

# Four vehicles are sold with prices above 1 million dollars! 
# Look into the body of the posts to get more details about these four cars.
vpostsWithExtremePrice = vposts[vposts$price > 10^6 & !is.na(vposts$price),]
vpostsWithExtremePrice$price
vpostsWithExtremePrice$body

# From the body of the posts, I think the first three high prices are mispresenatations of price ranges.
# They should be 6000 ~ 30000, 6000 ~ 30000, and 3000 ~ 2500. I will correct them with the middle price of the ranges.
# The last car owner posted like '$20 or best offer'. Instead of 9999999, $20 is more appropriate.
vposts$myPrice=ifelse(vposts$price == 600030000, 18000, 
                     ifelse(vposts$price == 30002500, 2750,
                            ifelse(vposts$price == 9999999, 20, vposts$price)))

# Recalculate the avergae price after correcting extemly high prices.
avg2 = mean(vposts$myPrice, na.rm = TRUE)
# Plot a histogram of vposts after correcting.
plot(density(vposts$myPrice, na.rm = TRUE))
#hist(vposts$myPrice)
rug(vposts$myPrice, col = 'red')
# It is still greatly right skewed. By looking at the rug, 
# boxplot(vposts$price)
# lower = quantile(vposts$price, 0.25, na.rm = TRUE)
# higher = quantile(vposts$price, 0.75, na.rm = TRUE)
# num = 1.5*(higher - lower)
vposts_sub = vposts[vposts$myPrice < 10^5,]
d = density(vposts_sub$myPrice, na.rm = TRUE)
plot(d, main = 'Distribution of vehicle price', xlab = 'Price (dollar)')
polygon(d, col='grey')
abline(v = dec, col = 'black')
abline(v = med, col = 'red')
abline(v = avg2, col = 'blue')
legend('topright', c("mean", "median", "deciles"), lty=c(1,1,1), 
       col=c('blue', 'red', 'black')) 

##Q4## What are the different categories of vehicles, i.e. the type variable/column? 
###### What is the proportion for each category ?
levels(vposts$type)
prop = table(vposts$type)/sum(table(vposts$type))
barplot(prop, xlab = 'type of vehicles', main = 'proportion for each vehicle type')


##Q5## Display the relationship between fuel type and vehicle type. 
###### Does this depend on transmission type?
table_type = with(vposts, table(type, fuel, transmission))
plot(table_type, xlab = 'type of vehicles', ylab = 'type of fuel', 
                  main = 'Relationship between fuel type and vehicle type')
library(ggplot2)
ggplot(as.data.frame(table_type), aes(x = type, y = Freq, fill = fuel)) +
  geom_bar(stat ='identity') +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~transmission)


#################################
#table_type -> data frame df1
df1 = as.data.frame(table_type)
#total count for each transmission+type
df2 = aggregate(Freq ~ transmission + type, df1, sum)
#join two data frames
df3 = merge(df1, df2, by = c('transmission', 'type'), all.x = TRUE) # all.x keeps all columns in df1
#Calc % for each row
df3['Perc'] = df3['Freq.x']/df3['Freq.y']

ggplot(df3, aes(x = transmission, y = Perc, fill = fuel)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~type) 
#################################


##Q6## How many different cities are represented in the dataset?
nlevels(vposts$city)

##Q7## Visually how the number/proportion of "for sale by owner" 
###### and "for sale by dealer" varies across city?
ggplot(vposts, aes(x = city, fill = byOwner)) +
  geom_bar(stat = 'bin', width = .5, position = "dodge") +
  ggtitle("The number of sale by owner/dealer across city")

# Alternatively
as.data.frame(with(vposts, table(city, byOwner))) %>%
  ggplot(aes(x = Freq, y = city, color = byOwner)) +
  geom_point()
 # geom_bar(stat = 'bin', width = .5, position = 'dodge') +
  scale_y_continuous(breaks = seq(0, 2500, 50)) +
  ggtitle("The number of sale by owner/dealer across city")

##Q8## What is the largest price for a vehicle in this data set? 
###### Examine this and fix the value. Now examine the new highest value for price.
max(vposts$price, na.rm = TRUE)
vposts$price[vposts$price == 600030000] = 18000
max(vposts$price, na.rm = TRUE)

##Q9## What are the three most common makes of cars in each city for "sale by owner" 
###### and for "sale by dealer"? Are they similar or quite different?
vposts_owner = vposts[vposts$byOwner == TRUE,]
vposts_dealer = vposts[vposts$byOwner == FALSE,]

vposts_owner_1 = as.data.frame(with(vposts_owner, table(maker, city)))
library(dplyr)
owner_commonMaker = 
vposts_owner_1 %>%
  group_by(city) %>%
  top_n(3, Freq) %>%
  ungroup()

vposts_dealer_1 = as.data.frame(with(vposts_dealer, table(maker, city)))
library(dplyr)
dealer_commonMaker = 
  vposts_dealer_1 %>%
  group_by(city) %>%
  top_n(3, Freq) %>%
  ungroup()

commonMaker = merge(owner_commonMaker, dealer_commonMaker, by = c('maker', 'city'))

##Q10## Visually compare the distribution of the age of cars for different cities 
####### and for "sale by owner" and "sale by dealer". 
####### Provide an interpretation of the plots, i.e., what are the key conclusions and insights?
# Create a new column named 'age' based on the infomation from the column 'year'
# The first car was invented in the year of 1768 (https://en.wikipedia.org/wiki/History_of_the_automobile),
# and now is year 2015, so we take years between 1768 and 2016 as valid input and assign years beyond this range as NA.
vposts$age = ifelse(vposts$year < 2016 & vposts$year > 1768, 2016 - vposts$year, NA)
sum(is.na(vposts$year))
sum(is.na(vposts$age))
vposts %>%
  ggplot(aes(x = age, color = city)) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 120, 5)) +
  ggtitle("Distribution of age of cars for different cities")

vposts %>%
  ggplot(aes(x = age, color = byOwner)) +
  geom_density() +
  scale_x_continuous(breaks = seq(0, 120, 5)) +
  ggtitle("Distribution of age of cars for 'Sale by owner/dealer'")

##Q11## Plot the locations of the posts on a map? What do you notice?
library(maps)
map('state')
with(vposts, points(long, lat, pch = '.', col = 'red'))

##Q12## Summarize the distribution of fuel type, drive, transmission, 
####### and vehicle type. Find a good way to display this information.
# display distribution for each variable
temp = as.data.frame(with(vposts, table(transmission, drive, type, fuel)))
temp %>%
  ggplot(aes(x =  fuel, y = Freq, fill = type)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  facet_grid(transmission ~ drive)
  



p1 = 
  ggplot(vposts, aes(x = fuel)) +
  geom_histogram(fill = 'blue')
p2 = 
  ggplot(vposts, aes(x = drive)) +
  geom_histogram(fill = 'red')
p3 = 
  ggplot(vposts, aes(x = transmission)) +
  geom_histogram(fill = 'green')
p4 = 
  ggplot(vposts, aes(x = type)) +
  geom_histogram(fill = 'purple')
library(grid)
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)


# display relationship between each two of these four variables
pairs(vposts[, c('fuel', 'drive', 'transmission', 'type')])

##Q13## Plot odometer reading and age of car? Is there a relationship? 
####### Similarly, plot odometer reading and price? Interpret the result(s). 
####### Are odometer reading and age of car related?
vposts[vposts$odometer < 5*10^6, ] %>%
  ggplot(aes(x = age, y = odometer)) +
  geom_point(col = 'blue', alpha = 0.5) +
  geom_rug(alpha = 0.1) 

vposts[vposts$odometer < 10^6 &vposts$price2 < 2*10^5, ] %>%
  ggplot(aes(x = odometer, y = price2)) +
  geom_point(col = 'red', alpha = 0.5) +
  geom_rug(alpha = 0.1) +
  geom_smooth(method = 'lm')

##Q14## Identify the "old" cars. What manufacturers made these? 
####### What is the price distribution for these?
#vposts %>%
#  ggplot(aes(x = age)) +
#  geom_density() +
#  scale_x_continuous(breaks = seq(0, 120, 5))
# we observe two peaks of distribution: one is around age of 4, another is around age of 10.
# definition of 'classic car' from Wikipedia is:
# "A classic car is an older automobile; the exact definition varies around the world. 
# The common theme is of an older car with enough historical interest to be collectable 
# and worth preserving or restoring rather than scrapping. 
# The Classic Car Club of America maintains that a car must be between 30 and 49 years old to be a classic, 
# while cars between 50 and 99 fall into a pre-antique class, and cars 100 years and older fall into the Antique Class. "
# I intend to follow this definiton can consider all cars older than 30 year to be 'old' car
oldCars = vposts[vposts$age > 30, ]

oldcarMaker = as.data.frame(with(oldCars, table(maker)))
head(oldcarMaker[order(-oldcarMaker$Freq),], 3)
# most old car makers are chevrolet, ford and volkswagen

oldCars %>%
  ggplot(aes(x = price2)) +
  geom_density()

##Q15## I have omitted one important variable in this data set. What do you think it is? 
####### Can we derive this from the other variables? If so, sketch possible ideas as to how we would compute this variable.
# I think it might be the mpg value
# we can derive it from 'body' part of this data frame
# do like this: for each row of data frame, see whether the body part includes strings like this "MPG: 27 city/ 38 highway".
# if it does, extract the two numbers and average them. Assign the average number to be the value of the new variable mpg
head(vposts$body)
color <- gsub('.*Exterior: (.*)Interior.*','\\1',vposts$body)



engine <- gsub('.*([0-9][.][0-9])L.*','\\1',vposts$body)

##Q16## Display how condition and odometer are related. Also how condition and price are related. 
####### And condition and age of the car. Provide a brief interpretation of what you find.

# Examine the condition
summary(vposts$condition)
# Substract 10 condition levels with most frequencies
eightCondition = sort(table(vposts$condition), decreasing = TRUE)[1:8]
names(eightCondition)
# Create a new column "myCondition". If the condition of a car belongs to the ten common conditions, copy it to "myConditon". If not, assign NA to "myCondtion". Also give levels in an order of increasing condition based on my personal experiences.
vposts$myCondition = ordered(as.character(vposts$condition),  levels = c("salvage", "fair", "good", "very good", 
                                                                         "excellent", "like new", "certified", "new"))
vposts%>%
  ggplot(aes(x = myCondition, y = odometer)) +
  ylim(c(0, 500000)) +
  geom_violin(fill = 'purple', alpha = 0.2) +
  ggtitle("Relationship between condition and odometer")

vposts%>%
  ggplot(aes(x = myCondition, y = myPrice)) +
  ylim(c(0, 100000)) +
  geom_violin(fill = 'blue', alpha = 0.2) +
  ggtitle("Relationship between condition and price")
# q16 type condition
cond = as.character(vposts$condition)
i = cond == "pre owned"
table(i)
cond[i] = "pre-owned"
cond[cond == "project car"] = "project"
cond[cond %in% c("honda", "ac/heater")] = NA
vposts$myCondition = factor(cond) # create a new column 



###############
test = vposts[c(1:100), c("price", "drive")]
test[order(test$price),]
library(ggplot2)
library(dplyr)
sp <- ggplot(vposts, aes(x=vposts$drive, y=vposts$fuel)) + geom_point(shape=1) + facet_grid(vposts$transmission ~ vposts$type)
