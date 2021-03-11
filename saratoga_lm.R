library(tidyverse)
library(ggplot2)
library(modelr)
library(rsample)
library(mosaic)
data(SaratogaHouses)

glimpse(SaratogaHouses)

####
# Compare out-of-sample predictive performance
####

# Split into training and testing sets
saratoga_split = initial_split(SaratogaHouses, prop = 0.8)
saratoga_train = training(saratoga_split)
saratoga_test = testing(saratoga_split)

# Fit to the training data
# Sometimes it's easier to name the variables we want to leave out
# The command below yields exactly the same model.
# the dot (.) means "all variables not named"
# the minus (-) means "exclude this variable"
lm1 = lm(price ~ lotSize + bedrooms + bathrooms, data=saratoga_train)
lm2 = lm(price ~ . - pctCollege - sewer - waterfront - landValue - newConstruction, data=saratoga_train)
lm3 = lm(price ~ (. - pctCollege - sewer - waterfront - landValue - newConstruction)^2, data=saratoga_train)

coef(lm1) %>% round(0)
coef(lm2) %>% round(0)
coef(lm3) %>% round(0)

# Predictions out of sample
# Root mean squared error
rmse(lm1, saratoga_test)
rmse(lm2, saratoga_test)
rmse(lm3, saratoga_test)

# Can you hand-build a model that improves on all three?
# Remember feature engineering, and remember not just to rely on a single train/test split


library(readxl)
library(ggplot2)
library(tidyverse)
library(modelr)
library(rsample)
library(mosaic)
library(caret) 
library(MASS)
library(pROC)
library(caret)
library(Metrics)
library(ROCR)


Q4_dev <- read_excel("C:/Users/cheny/Desktop/UIL 2021 Spring/ECO395M/A2/hotels_dev.xlsx")

Q4_dev$children=factor(Q4_dev$children)
Q4_dev$is_repeated_guest=factor(Q4_dev$is_repeated_guest)

Q4_dev_split = initial_split(Q4_dev, prop = 0.9)
Q4_dev_train = training(Q4_dev_split)
Q4_dev_test = testing(Q4_dev_split)

glm1 = glm(children ~ market_segment + adults +customer_type
           +is_repeated_guest,family=binomial(), data=Q4_dev_train)
glm2 = glm(children ~. -arrival_date, family=binomial(),data=Q4_dev_train)

glm3 = glm(children ~ 
             reserved_room_type:meal + average_daily_rate + 
             poly(total_of_special_requests,2) + assigned_room_type + market_segment + 
             hotel +  poly(adults,3) + booking_changes + customer_type + 
             previous_bookings_not_canceled + poly(lead_time,3) + distribution_channel + 
             is_repeated_guest + poly(stays_in_weekend_nights,3) +
             required_car_parking_spaces +
             poly(days_in_waiting_list,3),family=binomial(), data=Q4_dev_train)


Q4_val <- read_excel("C:/Users/cheny/Desktop/UIL 2021 Spring/ECO395M/A2/hotels_val.xlsx")
Q4_val$children=factor(Q4_val$children)
Q4_val$is_repeated_guest=factor(Q4_val$is_repeated_guest)


num_groups = 20

iris %>% 
  group_by((row_number()-1) %/% (n()/num_groups)) %>%
  nest %>% pull(data)


n <- 20
nr <- nrow(df)
split(df, rep(1:ceiling(nr/n), each=n, length.out=nr))

log_predict <- predict(glm3,newdata = Q4_val,type = "response")
log_predict1 <- ifelse(log_predict > 0.1,1,0)
log_predict2 <- ifelse(log_predict > 0.5,1,0)
log_predict3 <- ifelse(log_predict > 0.8,1,0)

pr1 <- prediction(log_predict1,Q4_val$children)
perf1 <- performance(pr1,measure = "tpr",x.measure = "fpr") 

pr2 <- prediction(log_predict2,Q4_val$children)
perf2 <- performance(pr2,measure = "tpr",x.measure = "fpr") 

pr3 <- prediction(log_predict3,Q4_val$children)
perf3 <- performance(pr3,measure = "tpr",x.measure = "fpr") 

attach(mtcars)
par(mfrow=c(3,1))
plot(perf1, col="red", pch=19,main = "TPR vs. FPR (t=0.1)")
plot(perf2, col="blue", pch=19,main = "TPR vs. FPR (t=0.5)")
plot(perf3, col="green", pch=19,main = "TPR vs. FPR (t=0.8)")