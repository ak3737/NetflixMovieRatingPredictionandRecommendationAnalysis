
#libraries
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
library(e1071)
library(lsa)
library(tidyr)

#load data
customer1 <- read.csv(file = 'combined_data_1.csv')

#bring in movie data and combine with customer data 
movie_titles <- read.csv(file = 'movie_titles.csv', header=FALSE)
colnames(movie_titles) <- c("Movie_Id", "release_year", "name")
movie_titles$Movie_Id <- as.numeric(movie_titles$Movie_Id)
movie_titles$Movie_Id <- as.integer(movie_titles$Movie_Id)
str(movie_titles)
combined <- inner_join(customer1, movie_titles, by="Movie_Id")
sample_n(combined, 2) #confirm joined worked

#drop initial column with row numbers
combined <- combined[-c(1)]

#get average number of ratings
num_ratings <- combined %>% count(Cust_Id, Rating)
num_cust_ratings <- aggregate(num_ratings$n, by=list(Cust_Id = num_ratings$Cust_Id), FUN=sum)
sample_n(num_cust_ratings, 2)
mean(num_cust_ratings$x) #51 is the average number of ratings

above_mean <- num_cust_ratings[num_cust_ratings$x > mean(num_cust_ratings$x), 1]

#remove customers with lower than the average number of ratings
combined2 <- combined[combined$Cust_Id %in% above_mean,]
str(combined2)
str(customer1)

View(combined2)
unique(combined2$name)

# IMDB data

imdb <- read.csv("IMDb movies.csv")
head(imdb)

combined3 <- inner_join(combined2, imdb, by=c("name", "release_year"))
View(sample_n(combined3, 20))
cbind(lapply(lapply(combined3, is.na), sum))

train_size <- floor(0.75*nrow(combined3))
set.seed(123)
train_ind <- sample(seq_len(nrow(combined3)), size = train_size)

#create new test and train dataset
trainData <- combined3[train_ind, ]
testData <- combined3[-train_ind, ]

## Initial Data Analysis

combine <- rbind(trainData, testData)
combine <- combine %>% select(-Date)

summary(combine)

avg_summary <- combine %>% group_by(name) %>% summarize(avg_rating = mean(Rating)) %>%
  arrange(desc(avg_rating, by_group=TRUE))
head(avg_summary)
tail(avg_summary)

#Top Rated: Harakiri (4.27)
#Least Rated: Midnight Mass (1.58)

cust_summary <- combine %>% group_by(Cust_Id) %>% summarize(avg_rat_cust = mean(Rating)) %>%
  arrange(desc(avg_rat_cust, by_group=TRUE))
head(cust_summary)
tail(cust_summary)
summary(cust_summary)

# Data Visualization 
ggplot(avg_summary, aes(x=avg_rating)) + geom_histogram() +
  labs(title = "Histogram of Movie Rating Average")

ggplot(cust_summary, aes(x=avg_rat_cust)) + geom_histogram() +
  labs(title = "Histogram of Movie Rating Average by Customer")

length(unique(combine$name)) # 546 movies
length(unique(combine$Cust_Id)) # 143,453 customers
length(combine$Rating) # 3,625,073 ratings given

rate1 <- length(combine$Rating[combine$Rating == 1])
rate2 <- length(combine$Rating[combine$Rating == 2])
rate3 <- length(combine$Rating[combine$Rating == 3])
rate4 <- length(combine$Rating[combine$Rating == 4])
rate5 <- length(combine$Rating[combine$Rating == 5])

counts <- data.frame(Rating=c(1, 2, 3, 4, 5), Counts=c(rate1, rate2, rate3, rate4, rate5))
counts <- counts %>% mutate(Percentage = round(Counts/sum(Counts) *100,2))
counts 

ggplot(data=counts, aes(x=Rating, y=Percentage)) +
  geom_bar(stat="identity", width=0.7)+ 
  geom_text(aes(label=Percentage), vjust=-0.2, size=3.5) +
  labs(title="Movie Rating Given by Customers")

## Data Preparation for History Check
head(trainData)
# Extracting just the CustId, Rating, and Title
train2 <- trainData[, c("Cust_Id", "Rating", "name")]
head(train2)

## History Check

# Let's see what random user 54333 liked in the past
# The user liked (rated 5 on) In Good Company, Shrek 2, Bad Boys, and Coach Carter 
user_54333_5 <- train2[(train2['Cust_Id']==54333) & (train2['Rating'] == 5),]
user_54333_5

# Let's see what random user 54333 moderately liked in the past
# The user has 20 movies rated as 3 ranging from Mean Girls, 2 Fast 2 Furious, to Hercules
user_54333_3 <- train2[(train2['Cust_Id']==54333) & (train2['Rating'] == 3),]
user_54333_3

# Let's see what random user 54333 disliked in the past
# The user disliked "Napoleon Dynamite" and rated it as 1
user_54333_1 <- train2[(train2['Cust_Id']==54333) & (train2['Rating'] == 1),]
user_54333_1

## Model building

#numeric data
numTrain <- trainData %>% select_if(is.numeric)
numTrain <- numTrain[, ]
glimpse(numTrain)

#get factor data
facTrain <- trainData %>% select_if(Negate(is.numeric))
facTrain <- facTrain %>% mutate_if(is.character, as.factor)
facTrain %>% select_if(is.factor) %>% sapply(levels) %>% sapply(length)

#recombine data and sample
training <- cbind(numTrain, facTrain)
set.seed(5103)
train_sample <- sample_n(training, 50000)

#baseline model
ols_fit<-lm(data=train_sample, Rating ~ Cust_Id + genre + country + release_year + actor1 + production_company)
summary(ols_fit)$r.squared
cor(train_sample$Rating, ols_fit$fitted.values)^2

#SVM model
memory.limit(size=15000)
svm_fit<-svm(Rating~., data=train_sample, kernel="radial", gamma=1, cost=1)
summary(svm_fit)

#test data and prediction
summary(testData)

#numeric data
numTest <- testData %>% select_if(is.numeric)
numTest <- numTest[, ]
glimpse(numTest)

#get factor data
facTest <- testData %>% select_if(Negate(is.numeric))
facTest <- facTest %>% mutate_if(is.character, as.factor)
facTest %>% select_if(is.factor) %>% sapply(levels) %>% sapply(length)

#recombine data
test <- cbind(numTest, facTest)
set.seed(5103)
test_sample <- sample_n(test,50000)
glimpse(test_sample)

#set rating to be 0
test_sample_final <- test_sample
test_sample_final$Rating[test_sample_final$Rating>0] <- 0
glimpse(test_sample_final)

#apply model to test sample data
test_predictions_ols <- predict(ols_fit, data=test_sample)
test_predictions_svm <- predict(svm_fit, data=test_sample, type="Rating")
glimpse(test_predictions)

## Result Exploration

#create data frame to compare ratings
comparison <- as.data.frame(test_sample$Cust_Id)
names(comparison)[names(comparison) == "test_sample$Cust_Id"] <- "Cust_Id"
comparison$actual_rating <- test_sample$Rating
comparison$predicted_rating <- test_predictions_ols
comparison$movie <- test_sample$name

#round predicted ratings to 3 decimal places to make it easier to work with 
comparison$predicted_rating <- round(comparison$predicted_rating, 3)
comparison

#get number of ratings
num_ratings <- comparison %>% count(Cust_Id)
num_ratings
num_cust_ratings <- aggregate(num_ratings$n, by=list(Cust_Id = num_ratings$Cust_Id), FUN=sum)
names(num_ratings)[names(num_ratings) == "n"] <- "num_ratings"
max(num_ratings$num_ratings, na.rm=TRUE)
num_ratings_ordered <- num_ratings %>% arrange(desc(num_ratings))
num_ratings_ordered

#exploration of customer 2439493
head(comparison)
cust_2439493_predict <- comparison[comparison$Cust_Id == 2439493,] %>% mutate(name = movie)
cust_2439493_info <- test_sample[test_sample$Cust_Id == 2439493,] 
cust_2439493_predict <- cust_2439493_predict 
combined_2439493 <- inner_join(cust_2439493_predict, cust_2439493_info, by="name")
combined_2439493 <- combined_2439493[, -c(4, 7, 8, 9, 10, 11, 12, 13, 16, 19, 20)]
combined_2439493

#exploration of customer 684531
cust_684531_predict <- comparison[comparison$Cust_Id == 684531,] %>% mutate(name = movie)
cust_684531_info <- test_sample[test_sample$Cust_Id == 684531,]
combined_684531 <- inner_join(cust_684531_predict, cust_684531_info, by="name")
cust_684531_predict
cust_684531_info
combined_684531 <- combined_684531[, -c(4, 7, 8, 9, 10, 11, 12, 13, 16, 19, 20)]
combined_684531

#exploration of customer 305344
cust_305344_predict <- comparison[comparison$Cust_Id == 305344,] %>% mutate(name = movie)
cust_305344_info <- test[test$Cust_Id == 305344,]
combined_305344 <- inner_join(cust_305344_predict, cust_305344_info, by="name")
cust_305344_predict
cust_305344_info
combined_305344 <- combined_305344[, -c(4, 7, 8, 9, 10, 11, 12, 13, 16, 19, 20)]
combined_305344
