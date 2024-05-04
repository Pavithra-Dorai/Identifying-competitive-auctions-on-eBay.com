
## 0 load the package and suppress scientific notation 

# load packages 
library(caret)
library(gains)

# not display scientific notation
options(scipen = 999)


## 1 create a data frame

# load the data
ebay.df <- read.csv("eBayAuctions.csv", stringsAsFactors = FALSE)

# first six rows
head(ebay.df)

# variable names 
names(ebay.df)



## 2 apply a function to each group 

# mean of the binary outcome for each day of the week of auction close   
mean_by_day <- tapply(ebay.df$Competitive., ebay.df$endDay, mean)
mean_by_day

# mean of the binary outcome for each auction category  
mean_by_category <- tapply(ebay.df$Competitive., ebay.df$Category, mean)
mean_by_category



## 3 reduce levels in the day of week that the auction closed  

# combine Sunday and Friday into a single category called Sun_Fri
ebay.df$endDay[ebay.df$endDay %in% c("Sun", "Fri")] <- "Sun_Fri"



## 4 reduce levels in auction categories 

# combine Business/Industrial and Computers into a single category called Computers
ebay.df$Category[ebay.df$Category %in% c("Business/Industrial", "Computer")] <- "Computer"

# combine Antique/Art/Craft and Collectibles into a single category called collectibles
ebay.df$Category[ebay.df$Category %in% c("Antique/Art/Craft", "Collectibles")] <- "Collectibles"


## 5 convert Duration to a categorical or factor variable 

# convert Duration from a numeric variable to a categorical or factor variable  
ebay.df$Duration <- as.factor(ebay.df$Duration)

# variable types 
str(ebay.df)



## 6 data partition

# set seed for reproducing the partition 
set.seed(1)
total_rows <- dim(ebay.df)[1]
train_size <- 0.6 * total_rows

# takes a sample of row numbers for the training set 
train.index <- sample(1:total_rows, train_size)

# training set 
train.df <- ebay.df[train.index, ]

# test set 
test.df <- ebay.df[-train.index, ]



## 7 fit a logistic regression model 

# logistic regression using all the variables
reg <- glm(Competitive. ~ ., data = train.df, family = "binomial")

# summary table 
summary(reg)


## 8 generate predicted probabilities for records in the test set 

# predicted probabilities 
pred <- predict(reg, newdata = test.df, type = "response")

# first six values 
head(pred)


## 9 confusion matrix 

threshold <- 0.5
predicted_classes <- ifelse(pred > threshold, 1, 0)
observed_classes <- as.factor(test.df$Competitive.)
predicted_classes <- as.factor(predicted_classes)
conf_matrix <- confusionMatrix(predicted_classes, observed_classes, positive = "1")
conf_matrix



## 10 creating a gain table 

# gain table 
library(gains)
groups <- 10
gain <- gains(test.df$Competitive., pred, groups = groups)

# cumulative percentage of competitive auctions 
cumulative_percentage <- gain$cume.pct.of.total
cumulative_percentage

# cumulative number of auctions 
cumulative_auctions <- gain$cume.obs
cumulative_auctions



## 11 plot a lift chart

# plot the cumulative number of competitive auctions against the cumulative number of auctions

total_competitive_auctions <- sum(test.df$Competitive.)
y_values <- c(0, gain$cume.pct.of.total * total_competitive_auctions)

plot(c(0, dim(test.df)[1]), c(0, max(y_values)), type = "n",
     xlab = "Cumulative Number of Auctions", ylab = "Cumulative Number of Competitive Auctions")

# add a baseline curve 
lines(y_values, type = "l")
lines(c(0, dim(test.df)[1]), c(0, dim(test.df)[1]))

