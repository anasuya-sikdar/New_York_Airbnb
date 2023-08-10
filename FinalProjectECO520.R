install.packages("tidyr")
install.packages("caret")
install.packages("xgboost")
install.packages(c( "ggpubr", "tidyverse", "broom", "AICcmodavg"))
install.packages("vcd")
library('dplyr')
library('tidyr')
library('ggplot2')
library('ggpubr')
library('tidyverse')
library('broom')
library('AICcmodavg')
library('caret')
library(xgboost)
library(vcd)
setwd("/Users/anasuyasikdar/Library/CloudStorage/OneDrive-DePaulUniversity/ECO 520 Tools 2")

### import data file and merge
# read the files using read.csv
indata2 <- read.csv(file = "listings.csv",fileEncoding = 'UTF-8-BOM')

#Examine the dataset indata2
summary(indata2)
dim(indata2)
colnames(indata2)
str(indata2)
head(indata2)
tail(indata2)

#Find the count of distinct neighbourhoods
distinct_count <- length(unique(indata2$neighbourhood))
distinct_count

#{r Identifying the factor varibales}
#$ neighbourhood_group , $ neighbourhood , $ room_type, $ host_name 
indata2$neighbourhood_group <- as.factor(indata2$neighbourhood_group)
indata2$neighbourhood <- as.factor(indata2$neighbourhood)
indata2$room_type <- as.factor(indata2$room_type)
indata2$host_name <- as.factor(indata2$host_name)


#{r Formating the last_review data}
as.Date("07/08/2009", "%m/%d/%Y")
indata2$last_review<-as.Date(indata2$last_review,"%m/%d/%Y")



#Sampling the data 
sample_data <- sample_n(indata2, 20000)
str(sample_data)

#Summarize sample_data
summary(sample_data)


#Correlation Analysis

cor(sample_data[,unlist(lapply(sample_data, is.numeric))])

#Correlation Analysis for Categorical Variables
# Calculate Cramer's V statistic between two factor variables
cramers_v <- assocstats(table(sample_data$room_type, sample_data$price))$cramer
cramers_v1 <- assocstats(table(sample_data$room_type, sample_data$neighbourhood_group))$cramer
cramers_v2 <- assocstats(table(sample_data$neighbourhood_group, sample_data$price))$cramer
cramers_v3 <- assocstats(table(sample_data$room_type, sample_data$minimum_nights))$cramer
cramers_v4 <- assocstats(table(sample_data$price, sample_data$minimum_nights))$cramer
cramers_v5 <- assocstats(table(sample_data$room_type, sample_data$reviews_per_month))$cramer
cramers_v6 <- assocstats(table(sample_data$room_type, sample_data$availability_365))$cramer

# Print the result
print(cramers_v)
print(cramers_v1)
print(cramers_v2)
print(cramers_v3)
print(cramers_v4)
print(cramers_v5)
print(cramers_v)

#one-way ANOVA
one.way <- aov(price ~ room_type, data = sample_data)

summary(one.way)

#two_way ANOVA
two.way <- aov(price ~ room_type + minimum_nights, data = sample_data)

summary(two.way)

#Adding interactions between variables
interaction <- aov(price ~ room_type*minimum_nights, data = sample_data)

summary(interaction)

#Adding a blocking variable
blocking <- aov(price ~ room_type + minimum_nights + neighbourhood_group, data = sample_data)

summary(blocking)

#Best fit for the data
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")
aictab(model.set, modnames = model.names) 

#Step4.Check for homoscedasticity
par(mfrow=c(2,2))
plot(blocking)
par(mfrow=c(1,1))

#count the listings based on neighbourhood
neighborhood_count <- table(sample_data$neighbourhood_group)

# Print count of neighborhoods
print(neighborhood_count)

#Descriptive Statistics
indata3 <- data.frame(neighbourhood_group = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"),
                      count=c(21661, 20104, 5666,1091,373),
                     color = c("#F8766D", "#00BFC4", "#C77CFF", "#7CAE00", "#FFA500"))

# Create barplot
ggplot(indata3, aes(x = neighbourhood_group, y=count, fill = color)) +
  geom_bar(stat = "identity", color = "black") + 
  labs(x = "Neighborhood Group", y = "Count", title = "Number of Listings by Neighborhood Group") 

#Create Stacked barplot for neighbourhood_group='Manhattan' vs price based on neighbourhood

gm<- sample_data %>%
  filter(neighbourhood_group== 'Manhattan') %>%
  group_by(neighbourhood_group, neighbourhood)

ggplot(gm, aes(x = neighbourhood_group, y=price, fill=neighbourhood)) +
  geom_bar(position="dodge", stat = "identity") + 
  labs(x = "Manhattan Neighborhood", y = "Prices", title = "Price Listings by Neighborhood Manhattan") 

#Create Stacked barplot for neighbourhood_group='Brooklyn' vs price based on neighbourhood

gm1<- sample_data %>%
  filter(neighbourhood_group== 'Brooklyn') %>%
  group_by(neighbourhood_group, neighbourhood)

ggplot(gm1, aes(x = neighbourhood_group, y=price, fill=neighbourhood)) +
  geom_bar(position="dodge", stat = "identity") + 
  labs(x = "Brooklyn Neighborhood", y = "Prices", title = "Price Listings by Neighborhood Brooklyn") 

#Create Stacked barplot for neighbourhood_group='Manhattan' vs price based on neighbourhood

gm2<- sample_data %>%
  filter(neighbourhood_group== 'Queens') %>%
  group_by(neighbourhood_group, neighbourhood)

ggplot(gm2, aes(x = neighbourhood_group, y=price, fill=neighbourhood)) +
  geom_bar(position="dodge", stat = "identity") + 
  labs(x = "Queens Neighborhood", y = "Prices", title = "Price Listings by Neighborhood Queens") 
#Create Stacked barplot for neighbourhood_group='Manhattan' vs price based on neighbourhood

gm3<- sample_data %>%
  filter(neighbourhood_group== 'Bronx') %>%
  group_by(neighbourhood_group, neighbourhood)

ggplot(gm3, aes(x = neighbourhood_group, y=price, fill=neighbourhood)) +
  geom_bar(position="dodge", stat = "identity") + 
  labs(x = "Bronx Neighborhood", y = "Prices", title = "Price Listings by Neighborhood Bronx") 
#Create Stacked barplot for neighbourhood_group='Manhattan' vs price based on neighbourhood

gm4<- sample_data %>%
  filter(neighbourhood_group== 'Staten Island') %>%
  group_by(neighbourhood_group, neighbourhood)

ggplot(gm4, aes(x = neighbourhood_group, y=price, fill=neighbourhood)) +
  geom_bar(position="dodge", stat = "identity") + 
  labs(x = "Staten Island Neighborhood", y = "Prices", title = "Price Listings by Neighborhood Staten Island") 

  
#Bar Plot to count of room_type demand
ggplot(sample_data, aes(x = room_type)) +
  geom_bar(color = "black",fill="lightpink") + 
  theme_dark()+
  labs(x = "Type of Airbnb Rooms", y = "Count", title = "Room types selected by Guests")        

#Barplot of room_type based on number of reviews
ggplot(data=sample_data, aes(x=room_type, y=number_of_reviews, fill=room_type))+
  geom_bar(stat="identity")+
  scale_fill_grey()+
  labs(x = "Type of Airbnb Rooms", y = "number_of_reviews", title = "Room types selected by Guests based on Reviews")

#Scatterplot of price vs number_of_reviews
ggplot(data=sample_data, aes(x=price, y=number_of_reviews, group=room_type, color=room_type)) +
  geom_point() +
  labs(x = "Price per night", y = "Number or Reviews", title = "Number of Reviews of Guests based on the price of Airbnb per night")

#Scatter point based on price vs minimum nights
ggplot(data=sample_data, aes(x=price, y=minimum_nights, group=room_type, color=room_type)) +
  geom_point() +
  labs(x = "Price per night", y = "Minimum Nights", title = "Price per night vs Minimum nights")






