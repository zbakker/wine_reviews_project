#Load libraries
library(dplyr)
library(tidyr)
library(stringr)
#Load in dataset
wine_reviews <- read.csv("~/Desktop/winemag-data-130k-v2.csv", stringsAsFactors=FALSE)

#Separate year from title column
wine_reviews <- separate(wine_reviews, title, c("title", "type"), sep = "\\(")

#Remove unnecessary columns 

wine_reviews <- subset( wine_reviews, select = -c(description, type, region_2, taster_name, taster_twitter_handle))

#Replace NAs with "NA"

wine_reviews[is.na(wine_reviews)] <- "NA"

#Replace blanks with "NA"

wine_reviews[wine_reviews == ""] <- "NA"

#Remove leading and trailing spaces in title column
wine_reviews$title <- str_trim(wine_reviews$title)
