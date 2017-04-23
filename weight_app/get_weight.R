library(prophet)
library(ggplot2)
library(MASS) 
library(dplyr)
library(gridExtra)

get_weight                     <- function(training_length,inputDate){
#read-in data
dta <- read.csv('C:/Users/celine.regis/Desktop/Project/Firma/upwork/R-shiny/Weight.csv')
date <- as.Date(dta$day, format="%Y-%m-%d")

weight<-dta$value
data<-data.frame(date,weight)

#split dataset
#cutoff_date <- as.Date("2012-07-12") + training_length
cutoff_date <- as.Date(inputDate)+ training_length
trainset<-subset(data, date>=as.Date(inputDate) & date<=cutoff_date)
testset <-subset(data,date>cutoff_date & date<="2017-04-17")

ds<-trainset$date
y <- trainset$weight
df <-data.frame(ds,y)

m <- prophet(df,changepoint.prior.scale = 0.001)
# Future objects
future <- make_future_dataframe(m, periods=365)
forecast<- predict(m,future)

return(list(m,forecast,data))

}