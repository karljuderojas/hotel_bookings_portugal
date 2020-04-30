library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(caret)
library(randomForest)
library(pROC)
library(ResourceSelection)
library(corrplot)
install.packages('Rcpp')

## Reading CSV and getting summary stats

hotelbooking <-  read.csv("C:/Users/karlj/Dropbox/MIS 581/hotel_bookings.csv")

str(hotelbooking)
summary(hotelbooking)

##Cleaning up data types

hotelbooking[sapply(hotelbooking, is.character)] <-
  lapply(hotelbooking[sapply(hotelbooking, is.character)], as.factor)

summary(hotelbooking)
str(hotelbooking)

##Checking to see if data was not corrupted

table(hotelbooking$hotel)

cor(hotels)
str(hotels)
df <- subset(hotels, select = -c(reservation_status_date, children) )

telly_cor <- cor(df)
corrplot(telly_cor, method = 'color')
## This pulls breakdown between the hotel and resport

ggplot(data = hotelbooking, aes(x=hotel)) +
  geom_bar(stat = "count") +
  labs(title = "Booking Requests",
        x = "Hotel Type",
        y = "Number of bookings") +
  theme_light()

##This pulls the breakdowns for cancelled bookings per hotel type


       ggplot(data = hotelbooking,
              aes(
                x = hotel,
                y = prop.table(stat(count)),
                fill = factor(is_canceled),
                label = scales::percent(prop.table(stat(count)))
              )) +
         geom_bar(position = position_dodge()) +
         geom_text(
           stat = "count",
           position = position_dodge(.9),
           vjust = -0.5,
           size = 3
         ) +
         scale_y_continuous(labels = scales::percent) +
         labs(title = "Cancellation X Hotel Type",
              x = "Hotel Type",
              y = "Count") +
         theme_bw() +
         scale_fill_discrete(
           name = "Booking Status",
           breaks = c("0", "1"),
           labels = c("Cancelled", "Not Cancelled")
         )


##Explore cancellation based off of lead time

       summary(hotelbooking$lead_time)

       ggplot(data = hotelbooking, aes(
         x = hotel,
         y = lead_time,
         fill = factor(is_canceled)
       )) +
         geom_boxplot(position = position_dodge())+
         labs(
           title = 'Cancellation by Hotel Type based on Lead Time',
           x = 'Hotel Type',
           y = 'Lead Time (Days)'
         ) +
         stat_summary(geom="text", fun.y=quantile, aes(label=sprintf("%1.1f", ..y..)))

##Lead time is_cancelled

       ggplot(data = hotelbooking , aes(lead_time)) + geom_histogram(binwidth = 0.8) + facet_wrap(~ is_canceled)


##Here we look at how travellers booked


       ggplot(data = hotelbooking,
              aes(
                x = hotel,
                y = prop.table(stat(count)),
                fill = factor(market_segment),
                label = scales::percent(prop.table(stat(count)))
              )) +
         geom_bar(position = position_dodge()) +
         geom_text(
           stat = "count",
           position = position_dodge(.9),
           vjust = -0.5,
           size = 3
         ) +
         scale_y_continuous(labels = scales::percent) +
         labs(title = "Market Segment X Hotel Type",
              x = "Hotel Type",
              y = "Count") +
         theme_bw() +
         scale_fill_discrete(
           name = "Booking Status"
         )

## Now the portion of the bookings that got cancelled

       p <- ggplot(data = hotelbooking,
              aes(
                x = market_segment,
                y = prop.table(stat(count)),
                fill = factor(is_canceled),
                label = scales::percent(prop.table(stat(count)))
              )) +
         geom_bar(position = position_dodge()) +
         geom_text(
           stat = "count",
           position = position_dodge(.9),
           vjust = -0.5,
           size = 3
         ) +
         scale_y_continuous(labels = scales::percent) +
         labs(title = "Market Segment X Hotel Type",
              x = "Hotel Type",
              y = "Count") +
         theme_bw() +
         scale_fill_discrete(
           name = "Booking Status"
         )
##Clean up a bit
p <-p + theme(axis.text.x = element_text(angle = 90))
p

# Logistic Regression
# where F is a binary factor and
# x1-x3 are continuous predictors
##fit <- glm(F~x1+x2+x3,data=mydata,family=binomial())
#https://www.theanalysisfactor.com/r-tutorial-glm1/

##
fit <- glm(is_canceled~lead_time+total_of_special_requests+adr+required_car_parking_spaces+
             children+babies+booking_changes, data = hotel_bookings, family = binomial)


###
sample <- floor(0.80*nrow(hotel_bookings))
train_ind <- sample(seq_len(nrow(hotel_bookings)), size = sample)
train <- hotelbooking[train_ind,]
test <- hotelbooking[-train_ind,]

model_lead_time <- glm(is_canceled~lead_time, data = train, binomial())
summary(model_lead_time)


lead_pred <- predict(model_lead_time, test, type = 'response')
lead_pred<- ifelse(lead_pred > 0.5, 1,0)
lead_pred

lead_matrix <- table(lead_pred, test$is_canceled )
lead_matrix
confusionMatrix((lead_matrix))

ggplot(train, aes (x = is_canceled, y = lead_time)) + geom_point() + geom_line()+
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)


plot(test$lead_time, lead_pred)
curve(predict(model_lead_time, data.frame(lead_time=x), type="response"), add=TRUE)


####MARKET SEGMENT
model_market_segment <- glm(is_canceled~market_segment, data = train, family = binomial)
summary(model_market_segment)


mkt_pred <- predict(model_market_segment, test, type = 'response')
mkt_pred <- ifelse(mkt_pred > 0.5, 1,0)
mkt_pred

mkt_matrix <- table(mkt_pred, test$is_canceled )
mkt_matrix
confusionMatrix((mkt_matrix))
