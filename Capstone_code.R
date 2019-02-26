# Data cleaning

#Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#Load in dataset
wine_reviews <- read.csv("~/Desktop/winemag-data-130k-v2.csv", stringsAsFactors=FALSE)

#Separate year from title column
wine_reviews <- separate(wine_reviews, title, c("title", "type"), sep = "\\(")

#Remove unnecessary columns 
wine_reviews <- subset( wine_reviews, select = -c(description, type, region_2, taster_name, taster_twitter_handle, designation))

#Replace NAs with "NA"
wine_reviews[is.na(wine_reviews)] <- "NA"

#Replace blanks with "NA"
wine_reviews[wine_reviews == ""] <- "NA"

#Remove leading and trailing spaces in title column
wine_reviews$title <- str_trim(wine_reviews$title)

#Remove rows where price is not recorded
wine_reviews <- subset(wine_reviews, price !="NA")

#Change data type of price to numeric
wine_reviews$price <- as.numeric(wine_reviews$price)

#Add value column
wine_reviews <- mutate(wine_reviews, value = (points/price))

#Add continent column
wine_reviews <- wine_reviews %>%
  mutate(continent = case_when(country %in% 
                                 c("Italy","Spain","Austria","Romania","Slovenia","Georgia","Moldova","Ukraine","Portugal","France","Hungary","Luxembourg","Serbia","Bulgaria","Switzerland","Slovakia","Germany","Armenia","Greece","Czech Republic","Croatia","England","Cyprus","Bosnia and Herzegovina","Macedonia") ~ "Europe",
                               country %in% c("Argentina","Chile","Uruguay","Brazil","Peru") ~ "South America", 
                               country %in% c("Israel","India","China","Morocco","Egypt", "Turkey", "Lebanon") ~ "Asia",
                               country %in% c("South Africa") ~ "Africa", 
                               country %in% c("Mexico", "US", "Canada") ~ "North America", 
                               country %in% c("New Zealand", "Australia") ~ "Oceania",
                               TRUE ~ "Other"))


#Add year from gsub
wine_reviews <- mutate(wine_reviews, year =gsub("[^0-9]","",title))

#Add year2 column 
yearList <- append(
  paste0("19",str_pad(seq(0,99,1),2,side = "left",pad = "0")),
  paste0("20",str_pad(seq(0,19,1),2,side = "left",pad = "0")))

#Create list of possible years
new_years <- yearList %>%
  subset(yearList > 1950 & yearList < 2018)

#Create year2 column 
wine_reviews <- wine_reviews %>%
  mutate(year2 = case_when(
    substr(year,1,4) %in% new_years ~ substr(year,1,4),
    substr(year,2,5) %in% new_years ~ substr(year,2,5), 
    substr(year,3,6) %in% new_years ~ substr(year,3,6),
    substr(year,4,7) %in% new_years ~ substr(year,4,7),
    substr(year,5,8) %in% new_years ~ substr(year,5,8),
    substr(year,6,9) %in% new_years ~ substr(year,6,9),
    substr(year,7,10) %in% new_years ~ substr(year,7,10),
    substr(year,8,11) %in% new_years ~ substr(year,8,11),
    substr(year,9,12) %in% new_years ~ substr(year,9,12),
    substr(year,10,13) %in% new_years ~ substr(year,10,13),
    substr(year,11,14) %in% new_years ~ substr(year,11,14),
    TRUE ~ "NA"
  ))
#Add years that meet the requirements to year column and drop working column year2
wine_reviews$year <- as.numeric(wine_reviews$year2)
wine_reviews <- subset( wine_reviews, select = -c(year2))


# Data exploration and visualization


#Plot distribution of prices
wine_reviews %>%
  ggplot(aes(x = price)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of prices") +
  xlab("price") +
  ylab("count")

#Plot distribution of points
wine_reviews %>%
  ggplot(aes(x= points)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Distribution of points") +
  xlab("points") +
  ylab("count")

#Anderson-Darling test of normality
library(nortest)
ad.test(wine_reviews$points)

#Plot points vs price
wine_reviews %>%
  ggplot(aes(x=points, y = price, color = continent)) +
  geom_point() +
  expand_limits(y= 0) +
  ggtitle("Points vs price")

# Pearson's correlation coefficient

# Value by continent
wine_reviews %>%
  filter(continent !="Other") %>%
  ggplot(aes(x = value, color = continent)) +
  geom_density()+
  facet_wrap(~ continent) +
  ggtitle("Proportion of value by continent") 

#Machine Learning

#Creating training and testing sets for linear regression 

library(caret)
set.seed(36)

# Shuffle row indices: rows
rows <- sample(nrow(wine_reviews))

# Randomly order data
wine_reviews <- wine_reviews[rows, ]

# Determine row to split on: split
split <- round(nrow(wine_reviews) * .75)

# Create train
train <- wine_reviews[1:split, ]

# Create test
test <- wine_reviews[(split + 1):nrow(wine_reviews), ]

#Testing linear regression models
model_1 <- lm(value ~ country, data = train
model_2 <- lm(value ~ points, data = train)
model_3 <- lm(value ~ points + price + country, data = train
model_4 <- lm(value ~ points + price + province, data = train)
model_5 <- lm(value ~ points + price + variety, data = train)
model_6 <- lm(value ~ points + price + province + year, data = train)
model_7 <- lm(value ~ points + price + year + continent, data = train)
model_8 <- lm(value ~ points + price + year + continent + province, data = train)
model_9 <- lm(value ~ points + price + year + country + province, data = train)
model_10 <- lm(value ~ points + price + year + province + variety, data = train)
model_11 <- lm(value ~ points + price + year + variety + region_1, data = train)

#Creating a dataset without missing values for independent variables

model_set <- wine_reviews %>%
  filter(region_1 !="NA") %>%
  filter(year !="NA") %>%
  filter(variety !="NA") %>%
  filter(province !="NA")

# Linear regression on the full model dataset
model_cv <- train(
  value ~ points + price + year + province + variety + region_1, model_set,
  method = "lm",
  na.action = na.pass,
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# Tukey's method for outliers

#Determining the 3rd quartile value of price
summary(wine_reviews$price)

#Removing outliers above 1.5x the 3rd quartile (there are no outliers on the lower end of the data) to
# create modified model
mod_wine <- wine_reviews %>%
  filter( price <= 63)

#Creating modified model for linear regression
mod_model <- mod_wine %>%
  filter(region_1 !="NA") %>%
  filter(year !="NA") %>%
  filter(variety !="NA") %>%
  filter(province !="NA")

#Running cross-validated linear regression on modified model set
modified_cv <- train(
  value ~ points + price + year + province + variety + region_1, mod_model,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)