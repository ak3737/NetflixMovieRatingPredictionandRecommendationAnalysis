
library(car)
library(ggplot2)
library(pls)
library(lars)
library(caret)
library(MASS)
library(lme4)
library(outliers)
library(dplyr)
library(reshape2)

getwd()

#load data 
trainData <- read.csv(file = 'train_movie.csv')
View(trainData)
summary(trainData)


#numeric data
numTrain <- trainData %>% select_if(is.numeric)
numTrain <- numTrain[, -c(1)]
glimpse(numTrain)

#get factor data
facTrain <- trainData %>% select_if(Negate(is.numeric))
facTrain <- facTrain %>% mutate_if(is.character, as.factor)
facTrain %>% select_if(is.factor) %>% sapply(levels) %>% sapply(length)

#recombine data
training <- cbind(numTrain, facTrain)
set.seed(5103)
train_sample <- sample_n(training, 50000)

#baseline model
ols_fit<-lm(data=train_sample, Rating ~ Cust_Id + genre + country + release_year + actor1 + production_company)
summary(ols_fit)$r.squared

#test data and prediction
testData <- read.csv(file = 'test_movie.csv')
summary(testData)

#numeric data
numTest <- testData %>% select_if(is.numeric)
numTest <- numTest[, -c(1)]
glimpse(numTest)

#get factor data
facTest <- testData %>% select_if(Negate(is.numeric))
facTest <- facTest %>% mutate_if(is.character, as.factor)
facTest %>% select_if(is.factor) %>% sapply(levels) %>% sapply(length)

#recombine data
test <- cbind(numTest, facTest)
set.seed(5103)

test_sample <- sample_n(test, 50000)
glimpse(test_sample)

#set rating to be 0
test_sample_final <- test_sample
test_sample_final$Rating[test_sample_final$Rating>0] <- 0
glimpse(test_sample_final)


#apply model to test sample data
test_predictions <- predict(ols_fit, data=test_sample)
glimpse(test_predictions)

#create data frame to compare ratings
comparison <- as.data.frame(test_sample$Cust_Id)
names(comparison)[names(comparison) == "test_sample$Cust_Id"] <- "Cust_Id"
comparison$actual_rating <- test_sample$Rating
comparison$predicted_rating <- test_predictions
comparison$movie <- test_sample$name

#round predicted ratings to 3 decimal places to make it easier to work with 
comparison$predicted_rating <- round(comparison$predicted_rating, 3)
comparison

#get number of ratings
num_ratings <- comparison %>% count(Cust_Id)
num_ratings
#num_cust_ratings <- aggregate(num_ratings$n, by=list(Cust_Id = num_ratings$Cust_Id), FUN=sum)
names(num_ratings)[names(num_ratings) == "n"] <- "num_ratings"
max(num_ratings$num_ratings, na.rm=TRUE)
num_ratings_ordered <- num_ratings %>% arrange(desc(num_ratings))
num_ratings_ordered

#exploration of customer 305344
cust_305344_predict <- comparison[comparison$Cust_Id == 305344,]
cust_305344_predict

#exploration of customer 1001129
cust_1001129_predict <- comparison[comparison$Cust_Id == 1001129,]


cust_1001129_info <- testData[testData$Cust_Id == 1001129,]
cust_1001129_predict
cust_1001129_info


# Plotting 
plot(comparison$actual_rating, comparison$predicted_rating)

p0 <- ggplot(data = comparison,
             mapping = aes(x = log(actual_rating), y = predicted_rating))
p1 <- p0 + geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", aes(color = "OLS", fill = "OLS"))

p1      
model_colors <- RColorBrewer::brewer.pal(3, "Set1")
model_colors

p1 + scale_color_manual(name = "Models", values = model_colors) +
  scale_fill_manual(name = "Models", values = model_colors) +
  theme(legend.position = "top")
p1

# Another Method 

plot(comparison$actual_rating[comparison$movie == "Frida"],
     comparison$predicted_rating[comparison$movie=="Frida"],
     xlim=c(1,5), ylim =c(1,5),
      pch = 19,
     xlab= "Actual Rating" , ylab="Predicted Rating",

     main = "Predicted vs Actual Rating - Frida")





