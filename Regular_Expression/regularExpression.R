########################## STA141 Assignment 4 ##########################
########################## Juanjuan Hu ##################################
# 1.
# Extract the price being asked for the vehicle from the body column, if it is present, 
# and check if it agrees with the actual price in the price column.
load("/Users/PullingCarrot/Desktop/201509-12/STA141statisticalComputing/homework/data/vehicles.rda")
head(vposts)
names(vposts)
summary(vposts$price)

# before extracting prices, we want to check how many price-like strings in each body
# gg is a 34677 long list, each element's first integer is the start position of a substring which starts
# with $ and followed by one or more numbers. -1 is given if no string matches.
gg = gregexpr('\\$[0-9]+', vposts$body)
# summarize how many posts have one price, how many have two prices, etc.
table(sapply(gg, function(x) length(unique(x))))
len = sapply(1:nrow(vposts), function(i) {length(gg[[i]])})
vposts[len==40,]$title

removedPatterns = 'reduced from \\$|\\$[0-9]+ below|original base sticker price: \\$|was \\$'
# 144 rows contains such pattern which we do not want extract
table(grepl(removedPatterns, vposts$body, ignore.case = TRUE))
body1 = gsub(removedPatterns, '', vposts$body, ignore.case = TRUE)
gg1 = gregexpr('\\$[0-9]+', body1)
table(sapply(gg1, function(x) length(unique(x))))

# make sure that in the original posts, no ":;&:;&" exists
sum(grepl(":;&:;&", body1))

# write a function to extract multiple matches and give unique value in the end
extract_multiple = function(pattern) {
  a = gsub(pattern, "\\1:;&:;&", vposts$body, ignore.case = TRUE)
  ll = sapply(a, function(x) strsplit(x, ":;&:;&"))
  ll = sapply(1:length(ll), function(i) ll[[i]][1:length(ll[[i]])-1])
  sapply(ll, unique)
}
pattern1 = ".*?\\$([0-9,]+)"

llprice = extract_multiple(pattern1)

# clean the prices
cut_year = function(priceList, year) {
  a = sapply(priceList, function(x) {
    tmp = gsub(",", "", x) # remove comma
    # if the price's last four digits are identical to its correponding year, remove them
    if (nchar(tmp)>4){
      last4 = substr(tmp, nchar(tmp)-3, nchar(tmp))
      last4 = as.integer(last4)
      if (last4 == year)
        as.integer(substr(tmp, 1, nchar(tmp)-4))
      else tmp}
    else tmp
  })
  unlist(a)
}

llprice2 = sapply(1:nrow(vposts), function(i) cut_year(llprice[[i]], vposts$year[i]))

lenPrice = sapply(llprice2, length)
table(lenPrice)

# for those body which contains a single price, extract the price 
index = which(lenPrice==1)
# initialize the priceFromBody column
priceFromBody = rep(-1, nrow(vposts))
priceFromBody[index] = as.integer(unlist(llprice2[index]))
vposts$priceFromBody = priceFromBody

# for thoese rows which contain multiple prices, extract the highest price
index2 = which(lenPrice > 1)
pp = sapply(llprice2[index2], function(x) max(as.integer(unlist(x)), na.rm = TRUE))
priceFromBody[index2] = pp
priceTag = ifelse(priceFromBody == vposts$price, 1, 0)
validated = sum(priceTag, na.rm = TRUE)

# propotion of validated price
prop = validated/sum(lenPrice>0)



# 2.
# Extract a Vehicle Identication Number (VIN) from the body, if it is present.  
# Add the VIN, if available, to the data frame. How many postings include the VIN?
# https://en.wikipedia.org/wiki/Vehicle_identification_number
pattern2 = '.*?([0-9a-zA-Z]{13}[0-9]{4})'
llvin = extract_multiple(pattern2)
lenVin = sapply(llvin, length)
# some rows have multiple vin
table(lenVin)

# initialize the vin column
vposts$vin = rep(-1, nrow(vposts))
# for unique vin, extract them
indexVin = which(lenVin == 1)
vin = unlist(llvin[indexVin])
vposts$vin[indexVin] = vin
# for multiple vins, extract the first one
indexVin2 = which(lenVin > 1)
vin2 = sapply(llvin[indexVin2], function(x) unlist(x)[1])
vposts$vin[indexVin2] = vin2
# how many postings include VIN
sum(vposts$vin != -1)


# 3.
# Extract phone numbers from the body column, and again add these as a new column. 
# How many posts include a phone number?
# After screening some of the postings, I found that phone numbers follow these patterns:
# (508) 205-1046; 603-475-0270; 650.445.0890; 9704394902 or having hidding numbers: (978)319-two885
# Some posts contain both phone number and tax number. In general, phone number come first
nums = "(zero|one|two|three|four|five|six|seven|eight|nine|[0-9])"
rx = sprintf('(%s ?){3}[-|.|)]? ?(%s ?){3}[-|.] ?(%s ?){4}', nums, nums, nums)
pattern3 = sprintf('.*?((%s ?){3}[-|.|)]? ?(%s ?){3}[-|.] ?(%s ?){4})', nums, nums, nums)
llphone = extract_multiple(pattern3)
lenphone = sapply(llphone, length)
# some posts contain multiple phone numbers
table(lenphone)
indexPhone = which(lenPhone > 0)
phone = sapply(llphone[indexPhone], function(x) unlist(x)[1])

# change the number expressed in words into digits
wordToDigit = function(text) {
  a1 = gsub("zero", "0", text, ignore.case = TRUE)
  a2 = gsub("one", "1", a1, ignore.case = TRUE)
  a3 = gsub("two", "2", a2, ignore.case = TRUE)
  a4 = gsub("three", "3", a3, ignore.case = TRUE)
  a5 = gsub("four", "4", a4, ignore.case = TRUE)
  a6 = gsub("five", "5", a5, ignore.case = TRUE)
  a7 = gsub("six", "6", a6, ignore.case = TRUE)
  a8 = gsub("seven", "7", a7, ignore.case = TRUE)
  a9 = gsub("eight", "8", a8, ignore.case = TRUE)
  gsub("nine", "9", a9, ignore.case = TRUE)
}
phone1 = wordToDigit(phone)
# remove the symboles 
phone2 = gsub('[^0-9]', '', phone1)
# initialize the phone number column
vposts$phoneNum = rep(-1, nrow(vposts))
vposts$phoneNum[indexPhone] = phone2
# how many posting contain phone number
sum(vposts$phoneNum!=-1)

# 4.
# Extract email addresses from the body column, and again add these as a new column. 
# How many posts include an email address?
pattern4 = '.*?[^0-9a-zA-Z]([0-9a-zA-Z]+@[0-9a-zA-Z]+\\.?[0-9a-zA-Z]*\\.(com|org|net|edu))'
llemail = extract_multiple(pattern4)
lenEmail = sapply(llemail, length)
# there is one posting containing two email
table(lenEmail)
# look at the outlier and we find that the first email is more reasonable
outlier = vposts$body[which(lenEmail==2)]
indexEmail = which(lenEmail > 0)
email = sapply(llemail[indexEmail], function(x) unlist(x)[1])
vposts$email = rep(-1, nrow(vposts))
vposts$email[indexEmail] = email
# how many emails
sum(vposts$email !=-1)

# 5.
# Find the year in the description or body and compare it with the value in the year column.
year1 = gsub('^[^0-9]*([1-2][0-9]{3}).*', "\\1", vposts$description)
# nine NAs
sum(is.na(year1))
year2 = gsub('^[^0-9]*([0-9]{2})[^0-9].*', "\\1", vposts$description)
newYear = ifelse(year2 == vposts$description, year1, year2) 
Year = rep(-1, nrow(vposts))
Year = ifelse(newYear == vposts$description, -1, newYear)
# adjust NAs into -1
Year[is.na(Year)] = -1
# convert two digits' year into four digits
correctedYear = sapply(Year, function(x) {
  y = as.integer(x)
  if (y<17 & y >0)
    return (paste0('20', x))
  else
    if (y>16 & y<99)
      return (paste0('19', x))
    else return(x)
  })
vposts$Year = correctedYear

# how many years are validated
yearTag = ifelse(vposts$Year == vposts$year, 1, 0)
validatedYear = sum(yearTag, na.rm = TRUE)

# propotion of validated year
prop1 = validatedYear/sum(!is.na(vposts$year))
prop1

# 6.
# Determine the model of the car, e.g., S60, Boxter, Cayman, 911, Jetta.
# extract the third word of headers 
# Almosts every header starts with year
table(grepl('^[0-9]{4}', vposts$header))
vposts$headerlow = tolower(vposts$header)
# remove the - 
vposts$headerlow = gsub("-", "", vposts$headerlow)
llmodel = sapply(vposts$headerlow, function(x) unlist(strsplit(x, " +"))[3])
# there are 1767 missing values
sum(is.na(llmodel))
# for missing values, go back and pull out the second word in headerlow
temp = sapply(vposts$headerlow, function(x) unlist(strsplit(x, " +"))[2])
llmodel[which(is.na(llmodel))] = temp[which(is.na(llmodel))]
# for model named "grand", see what makers made tham
table(vposts$maker[llmodel == "grand"])
# Buick: grand national
# chrysler: grand voyager
# dodge: grand caravan
# jeep: grand cherokee
# mercury: grand marquis
# nissan: grand blanc
# plymouth: grand 15
# pontiac: grand am
# suzuke: grand vitara

# for these model "grand", go back and pull out the fourth word also
temp1 = sapply(vposts$headerlow, function(x) unlist(strsplit(x, " +"))[4])
llmodel[llmodel == "grand"] = paste(llmodel[llmodel == "grand"], temp1[llmodel == "grand"])


# create a column
vposts$model = as.character(llmodel)
# count the frequency
sort(table(llmodel), decreasing = TRUE)[1:20]
# group vopsts by maker
vposts1 = vposts[!is.na(vposts$maker),]
groupByMaker = split(vposts1, vposts1$maker)

# correct model within maker group
correctByGroup = function(i) {
  b = groupByMaker[i]
  a = b[[1]]
  df = as.data.frame(table(a$model))
  df$Var1 = as.character(df$Var1)
  # consider model with frequency more than 5
  highFreqModel = subset(df, Freq > 5)
  # regards these "high frequency models" as "accurate models"
  # record these models as a reference model data base
  accModel = highFreqModel$Var1
  if (length(accModel) == 0)
    return (a$model)
  else {
    corrected = unlist(sapply(a$model, function(x) correct(x, accModel)))
    return (corrected)
  }
}
  
# loop over 72 makers
groups = do.call(rbind,groupByMaker)
dim(groups)
tmp = sapply(1: length(groupByMaker), function(i) correctByGroup(i))
corrects = unlist(tmp)
# check how many are corrected in total
sum(groups$model!=corrects) # 422 are corrected
groups$correctedModel = corrects
# combine groups to those data with missing makers
vposts2 = vposts[is.na(vposts$maker),]
vposts2$correctedModel = vposts2$model
newVposts = rbind(vposts2, groups)
dim(newVposts)
  
# For each model in this group, check whether its "distance" with any model in accModel
# is equal to or less than one. If so, change it to that model (we consider mis-spelling here). 
# Otherwise, do not change it.
correct = function(x, accModel) {
  # x is the model we are considering whether to correct it
  a = adist(x, accModel)
  if (min(a, na.rm = TRUE) < 1.5)
    return (accModel[which.min(a)])
  else return(x)
}

#################################################################################################
# Pick two models of cars, each for a different car maker, e.g., Toyota or Volvo. 
# For each of these, separately explore the relationship between the price being asked for the vehicle, 
# the number of miles (odometer), age of the car and condition. Does location (city) have an effect on this? 
# Use a statistical model to be able to suggest the appropriate price for such a car given its age, mileage, and condition.
# You might consider a linear model, k-nearest neighbors, or a regression tree.
# You need to describe why the method you chose is appropriate? 
# what assumptions are needed and how reasonable they are? and how well if performs and how you determined this? 
# Would you use it if you were buying or selling this type of car?

# create age column from year
newVposts$age = ifelse(newVposts$year < 2016 & newVposts$year > 1768, 2016 - newVposts$year, NA)
# tidy the condition column
eightCondition = sort(table(newVposts$condition), decreasing = TRUE)[1:8]
names(eightCondition)
# Create a new column "myCondition". If the condition of a car belongs to the ten common conditions, copy it to "myConditon". If not, assign NA to "myCondtion". Also give levels in an order of increasing condition based on my personal experiences.
newVposts$myCondition = ordered(as.character(newVposts$condition),  levels = c("salvage", "fair", "good", "very good", 
                                                                         "excellent", "like new", "certified", "new"))
# remove outliers of odometer and price
newVposts$odometer[newVposts$odometer > 300000] = NA
newVposts$price[newVposts$price > 100000] = NA

# Honda civic
civic = newVposts[newVposts$correctedModel == "civic", ]
# make sure all civic are from honda
table(civic$maker)
civic_sub = civic[, c("odometer", "age", "price", "myCondition", "city")]
# check the class for each variable
sapply(civic_sub, class)
# convert the class of condition to factor with no order
civic_sub$myCondition = factor(civic_sub$myCondition, ordered = FALSE)
# for quantitative variable, see the scatterplot matrix and correlation matrix
civic_sub1 = civic_sub[, c("odometer", "age", "price")]
pairs(civic_sub1)
cor(na.omit(civic_sub1))
# for qualitative
boxplot(civic_sub$price ~ civic_sub$myCondition, main = "Boxplot of condition",
        xlab = "condition levels", ylab = "price $", col = rainbow(8))
boxplot(civic_sub$price ~ civic_sub$city, main = "Boxplot of cities",
        xlab = "cities", ylab = "price $", col = rainbow(7))
#### fit the linear model

#####
civicNoNa = na.omit(civic_sub)
# we will use civicNoNa as our traing set for lm model
null=lm(price ~ 1, data = civicNoNa)
null
full=lm(price ~ age + myCondition + odometer + city, data = civicNoNa)
full
step(null, scope=list(lower=null, upper=full), direction="forward")
fit = lm(price ~ age + factor(myCondition) + odometer + factor(city), data = civicNoNa)
summary(fit) # Ra = 0.8711
plot(fit, which = 1) # obvious nonlinear pattern
# add the quadratic terms of quantitative varible into the data set
civicQ = civicNoNa
civicQ$ageQ = (civicQ$age)^2
civicQ$odometerQ = (civicQ$odometer)^2
# perform variable selection again
null1=lm(price ~ 1, data = civicQ)
full1=lm(price ~ ., data = civicQ)
step(null1, scope=list(lower=null1, upper=full1), direction="forward")
## final model
finalModel = lm(formula = price ~ age + ageQ + city + myCondition + odometerQ, 
                data = civicQ)
summary(finalModel)
plot(finalModel, which = 1)

### 
# toyota camry
camry = newVposts[newVposts$correctedModel == "camry", ]
table(camry$maker)
camry_sub = camry[, c("odometer", "age", "price", "myCondition", "city")]
# convert the class of condition to factor with no order
camry_sub$myCondition = factor(camry_sub$myCondition, ordered = FALSE)
# for quantitative variable, see the scatterplot matrix and correlation matrix
camry_sub1 = camry_sub[, c("odometer", "age", "price")]
pairs(camry_sub1)
cor(na.omit(camry_sub1))
# for qualitative
boxplot(camry_sub$price ~ camry_sub$myCondition, main = "Boxplot of condition",
        xlab = "condition levels", ylab = "price $", col = rainbow(8))
boxplot(camry_sub$price ~ camry_sub$city, main = "Boxplot of cities",
        xlab = "cities", ylab = "price $", col = rainbow(7))
#### fit the linear model

#####
camryNoNa = na.omit(camry_sub)
# we will use civicNoNa as our traing set for lm model
null.1=lm(price ~ 1, data = camryNoNa)
null.1
full.1=lm(price ~ age + myCondition + odometer + city, data = camryNoNa)
full.1
step(null.1, scope=list(lower=null.1, upper=full.1), direction="forward")
fit.1 = lm(price ~ age + factor(myCondition) + odometer + factor(city), data = camryNoNa)
summary(fit.1) # Ra = 0.8711
plot(fit.1, which = 1) # obvious nonlinear pattern
# add the quadratic terms of quantitative varible into the data set
camryQ = camryNoNa
camryQ$ageQ = (camryQ$age)^2
camryQ$odometerQ = (camryQ$odometer)^2
# perform variable selection again
null.11=lm(price ~ 1, data = camryQ)
full.11=lm(price ~ ., data = camryQ)
step(null.11, scope=list(lower=null.11, upper=full.11), direction="forward")
## final model
finalModel.1 = lm(formula = price ~ age + ageQ + city + myCondition + odometerQ, 
                data = camryQ)
summary(finalModel.1)
plot(finalModel.1, which = 1)













