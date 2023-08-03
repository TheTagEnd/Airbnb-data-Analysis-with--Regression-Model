#data Exploration
df <- read.csv('C:\\Users\\vashu\\OneDrive\\Desktop\\Programming\\R\\trial\\Airbnb_Open_Data.csv')
#Filling Empty String With Na
df[df == ''] <- NA
print(df)
column_names <- colnames(df)
print(column_names)
df_dimensions <- dim(df)
print(df_dimensions)
summary_df <- summary(df)
print(summary_df)
df_info <- str(df)
print(df_info)
# Count the number of missing values in each column
missing_counts <- colSums(is.na(df))
print(missing_counts)
# Data Cleaning
print(df_dimensions)
print(column_names)
df['reviews_per_month']
print(names(df))
new_df <- df[, !(colnames(df) %in% c("host_name", "NAME" , "country" , "country_code" ,  "review_rate_number"  ,           "calculated_host_listings_count" , "availability.365" ,
"house_rules"  ,"license" ))]
working_df <- new_df
print(colnames(working_df))
#converting char price to int
# Remove the dollar sign and any additional spaces
working_df$price <- gsub("[ $]", "", working_df$price)
working_df$price<- as.integer(working_df$price)
working_df$minimum_nights <- as.integer(working_df$minimum_nights)
# Remove the dollar sign and any additional spaces
working_df$service_fee <- gsub("[ $]", "", working_df$service_fee)
working_df$service_fee <- as.integer(working_df$service_fee)
# Count missing values in each column
missing_counts <- colSums(is.na(working_df))
# Print the result
print(missing_counts)
# Count the occurrences of each unique value in the column
value_counts <- table(working_df$host_identity_verified)
# Print the result
print(value_counts)
### Replacing null values in "host_identity_verified" with Unconfirmed assuming the they are the ones who are also not verified users
working_df$host_identity_verified <- ifelse(is.na(working_df$host_identity_verified), "unconfirmed", working_df$host_identity_verified)
# Count missing values in each column
missing_counts <- colSums(is.na(working_df))
# Print the result
print(missing_counts)
table(working_df$neighbourhood_group)
## replacing manhatan to Manhattan and broklyn to Brooklyn
working_df$neighbourhood_group <- ifelse((working_df$neighbourhood_group == 'manhatan') , "Manhattan" ,working_df$neighbourhood_group )
working_df$neighbourhood_group <- ifelse((working_df$neighbourhood_group == 'brookln') , "Brooklyn" ,working_df$neighbourhood_group )
# check after replacing
table(working_df$neighbourhood_group)
table(working_df$neighbourhood)
temp = working_df  %>% group_by(neighbourhood_group , neighbourhood ) %>%
summarise(total_count =n(),.groups = 'drop')
library(biglm)
library(caTools)
library(dplyr)
library(gbm)
library(ModelMetrics)
library(plotrix)
library(stringr)
library(tidyr)
#data Exploration
df <- read.csv('C:\\Users\\vashu\\OneDrive\\Desktop\\Programming\\R\\trial\\Airbnb_Open_Data.csv')
#Filling Empty String With Na
df[df == ''] <- NA
print(df)
column_names <- colnames(df)
print(column_names)
df_dimensions <- dim(df)
print(df_dimensions)
summary_df <- summary(df)
print(summary_df)
df_info <- str(df)
print(df_info)
# Count the number of missing values in each column
missing_counts <- colSums(is.na(df))
print(missing_counts)
# Data Cleaning
print(df_dimensions)
print(column_names)
df['reviews_per_month']
print(names(df))
new_df <- df[, !(colnames(df) %in% c("host_name", "NAME" , "country" , "country_code" ,  "review_rate_number"  ,           "calculated_host_listings_count" , "availability.365" ,
"house_rules"  ,"license" ))]
working_df <- new_df
print(colnames(working_df))
#converting char price to int
# Remove the dollar sign and any additional spaces
working_df$price <- gsub("[ $]", "", working_df$price)
working_df$price<- as.integer(working_df$price)
working_df$minimum_nights <- as.integer(working_df$minimum_nights)
# Remove the dollar sign and any additional spaces
working_df$service_fee <- gsub("[ $]", "", working_df$service_fee)
working_df$service_fee <- as.integer(working_df$service_fee)
# Count missing values in each column
missing_counts <- colSums(is.na(working_df))
# Print the result
print(missing_counts)
# Count the occurrences of each unique value in the column
value_counts <- table(working_df$host_identity_verified)
# Print the result
print(value_counts)
### Replacing null values in "host_identity_verified" with Unconfirmed assuming the they are the ones who are also not verified users
working_df$host_identity_verified <- ifelse(is.na(working_df$host_identity_verified), "unconfirmed", working_df$host_identity_verified)
# Count missing values in each column
missing_counts <- colSums(is.na(working_df))
# Print the result
print(missing_counts)
table(working_df$neighbourhood_group)
## replacing manhatan to Manhattan and broklyn to Brooklyn
working_df$neighbourhood_group <- ifelse((working_df$neighbourhood_group == 'manhatan') , "Manhattan" ,working_df$neighbourhood_group )
working_df$neighbourhood_group <- ifelse((working_df$neighbourhood_group == 'brookln') , "Brooklyn" ,working_df$neighbourhood_group )
# check after replacing
table(working_df$neighbourhood_group)
table(working_df$neighbourhood)
temp = working_df  %>% group_by(neighbourhood_group , neighbourhood ) %>%
summarise(total_count =n(),.groups = 'drop')
temp
### Replacing the null values of "neighbourhood group" with "Brooklyn" because for most of the null neighbourhood the "neighbourhood group" is "Brooklyn"
working_df$neighbourhood_group <- ifelse(is.na(working_df$neighbourhood_group) , "Brooklyn" ,working_df$neighbourhood_group )
table(working_df$neighbourhood)
# Check the number of null values in each column
null_counts <- colSums(is.na(working_df))
# Print the null counts
print(null_counts)
neighborhoods <- c("Greenpoint", "Crown Heights", "East Village", "West Village", "Elmhurst", "Flatiron District", "Upper West Side")
for (neighborhood in neighborhoods) {
lat_mode <- mode(working_df$lat[working_df$neighbourhood == neighborhood])
long_mode <- mode(working_df$long[working_df$neighbourhood == neighborhood])
working_df$lat[working_df$neighbourhood == neighborhood & is.na(working_df$lat)] <- lat_mode
working_df$long[working_df$neighbourhood == neighborhood & is.na(working_df$long)] <- long_mode
}
# Check the number of null values in each column
null_counts <- colSums(is.na(working_df))
# Print the null counts
print(null_counts)
table(working_df$instant_bookable)
### Replacing Null values with instant_bookable is false
working_df$instant_bookable <- ifelse(is.na(working_df$instant_bookable) , "FALSE" ,working_df$instant_bookable )
# Check the number of null values in each column
null_counts <- colSums(is.na(working_df))
# Print the null counts
print(null_counts)
# Filling "cancellation_policy" null with "Moderate" as for False "instant_bookable" the max value counts is "Moderate"
working_df$cancellation_policy <- ifelse(is.na(working_df$cancellation_policy) , "moderate" ,working_df$cancellation_policy )
# Check the number of null values in each column
null_counts <- colSums(is.na(working_df))
# Print the null counts
print(null_counts)
# fill na in neighbourhood
# Replacing Null value wrt "Brooklyn" with "Bedford-Stuyvesant" and "Manhattan" with "Two Bridges"
working_df$neighbourhood[working_df$neighbourhood_group == "Brooklyn"] <- ifelse(is.na(working_df$neighbourhood[working_df$neighbourhood_group == "Brooklyn"]), "Bedford-Stuyvesant", working_df$neighbourhood[working_df$neighbourhood_group == "Brooklyn"])
working_df$neighbourhood[working_df$neighbourhood_group == "Manhattan"] <- ifelse(is.na(working_df$neighbourhood[working_df$neighbourhood_group == "Manhattan"]), "Two Bridges", working_df$neighbourhood[working_df$neighbourhood_group == "Manhattan"])
# Check the number of null values in each column
null_counts <- colSums(is.na(working_df))
# Print the null counts
print(null_counts)
#finding mean of price AND SERVICE FEE for filling na values
working_df$price <- ifelse(is.na(working_df$price) , 0 ,working_df$price )
working_df$service_fee <- ifelse(is.na(working_df$service_fee) , 0 ,working_df$service_fee)
price_mean =  mean(working_df$price)
price_mean
service_fee_mean =  mean(working_df$service_fee)
service_fee_mean
working_df$price <- ifelse((working_df$price == 0) , price_mean ,working_df$price )
working_df$service_fee <- ifelse((working_df$service_fee == 0 ) , service_fee_mean ,working_df$service_fee)
# filling na for minimum_nights
mode_value <- mode(working_df$minimum_nights)
mode_value
#fill with mode value
working_df$minimum_nights <- ifelse(is.na(working_df$minimum_nights) , mode_value ,working_df$minimum_nights)
# filling na of  number of review with  median value of all col
working_df$number_of_reviews <- ifelse(is.na(working_df$number_of_reviews) , 0 ,working_df$number_of_reviews )
median =  median(working_df$number_of_reviews)
median
working_df$number_of_reviews <- ifelse((working_df$number_of_reviews  ==  0 ) , median ,working_df$number_of_reviews )
# filling last_review to no review
working_df$last_review <- ifelse(is.na(working_df$last_review) , "No  Review" ,working_df$last_review )
# filling na of construction to no available
working_df$Construction_year <- ifelse(is.na(working_df$Construction_year) , "Not Available" ,working_df$Construction_year)
# fillin na of review_per_month with mean value
# Calculate the mean of the 'reviews_per_month' column
mean_value <- mean(working_df$reviews_per_month, na.rm = TRUE)
mean_value
# Replace NA values in the 'reviews_per_month' column with the mean value
working_df$reviews_per_month[is.na(working_df$reviews_per_month)] <- mean_value
# Check the number of null values in each column
null_counts <- colSums(is.na(working_df))
print(null_counts)
# Calculate price per night
working_df$price<- as.integer(working_df$price)
working_df$minimum_nights <- as.integer(working_df$minimum_nights)
working_df$price_per_night <- working_df$price / working_df$minimum_nights
working_df$price_per_night
# Distance from a popular Landmark
# Function to calculate distance using Haversine formula
haversine_distance <- function(lat1, lon1, lat2, lon2) {
# Convert degrees to radians
lat1_rad <- lat1 * pi / 180
lon1_rad <- lon1 * pi / 180
lat2_rad <- lat2 * pi / 180
lon2_rad <- lon2 * pi / 180
# Radius of the Earth in kilometers
radius <- 6371
# Haversine formula
dlat <- lat2_rad - lat1_rad
dlon <- lon2_rad - lon1_rad
a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
c <- 2 * atan2(sqrt(a), sqrt(1-a))
distance <- radius * c
return(distance)
}
# Assuming your dataset is named 'airbnb' and latitude and longitude columns are 'lat' and 'long'
landmark_lat <- 143
landmark_lon <- 233
# Calculate the distance from the landmark for each location
working_df$distance_from_landmark <- haversine_distance(as.numeric(working_df$lat),as.numeric(working_df$long), landmark_lat, landmark_lon)
working_df$distance_from_landmark
# 5. ---------------------------------------------------Modeling-------------------------------
# Install and load the 'caret' package
library(caret)
# Assuming your data is stored in a data frame called 'data'
# Set the seed for reproducibility
set.seed(123)
# Split the data into a training set and a testing set
train_indices <- createDataPartition(working_df$price, p = 0.7, list = FALSE)
training_set <- working_df[train_indices, ]
testing_set <- working_df[-train_indices, ]
# 70 percent for training and 30 for testing
library(randomForest)
working_df <- na.omit(working_df)
# random forest
# Train the Random Forest model with handling missing values
rf_model <- randomForest(price ~ ., data = training_set, na.action = na.exclude)
rf_model
# Calculate R-squared
rf_r_squared <- caret::R2(aligned_predictions, aligned_testing_set$price)
# Make predictions on the testing set
rf_predictions <- predict(rf_model, newdata = testing_set)
# Subset the testing set to align with the predictions
aligned_testing_set <- testing_set[!is.na(rf_predictions), ]
# Subset the predictions to align with the testing set
aligned_predictions <- rf_predictions[!is.na(rf_predictions)]
rf_rmse <- caret::RMSE(aligned_predictions, aligned_testing_set$price)
# Print the RMSE
print(paste("Random Forest RMSE:", rf_rmse))
# Create a data frame with actual and predicted prices
results <- data.frame(Actual = testing_set$price, Predicted = rf_predictions)
# Calculate R-squared
rf_r_squared <- caret::R2(aligned_predictions, aligned_testing_set$price)
# Calculate Mean Absolute Error (MAE)
rf_mae <- caret::MAE(aligned_predictions, aligned_testing_set$price)
# Calculate Mean Percentage Error (MAPE)
rf_mape <- mean(abs((aligned_predictions - aligned_testing_set$price) / aligned_testing_set$price)) * 100
# Print the evaluation metrics
print(paste("Random Forest R-squared:", rf_r_squared))
print(paste("Random Forest MAE:", rf_mae))
print(paste("Random Forest MAPE:", rf_mape))