require(jsonlite)
require(httr)
require(data.table)
library(xts)
library(zoo)
library(ggfortify)
library(ggplot2)
library(fpp2)
install.packages("tsbox")

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=T){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}


check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://167.172.183.67'

u_name = "Group8"
p_word = "D0rlLLrPf3oSfFIl"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

data[data==(-1)]<-NA
coronavirusdata<-fread("C:/Users/Nirvana/Desktop/Proje/new_cases.csv", select =c("date","Turkey"))
coronavirusdata$date=as.POSIXct(coronavirusdata$date)
coronavirusdata<-xts(coronavirusdata$Turkey, order.by=coronavirusdata$date)
dates <- seq(from=as.Date("2019-04-30"),to=as.Date("2019-12-30"), by="days")
zeros<-rep(0,length(dates))
extendeddata<-xts(zeros, order.by=dates)
coronavirusdata<-rbind(coronavirusdata,extendeddata)
x<-nrow(data)/8
coronavirusdata<-coronavirusdata[1:x,]
coronavirusdata[is.na(coronavirusdata)] <- 0

#product1
product1<-data[data$product_content_id=='3904356']
product1$price<-na.locf(product1$price)
product1$price<-na.locf(product1$price, fromLast=TRUE)

price_difference <- diff(product1$price)
price_difference<-c(0,price_difference)
product1<-product1[,price_difference:=price_difference]

product1<-product1[,coronavirusdailycases:= coronavirusdata]
a<-head(which(product1[,"visit_count"]>0),n=1)
product1<-product1[-(1:(a-1)),]
product1<-product1[,c("category_visits","ty_visits", "product_content_id"):=NULL]
#category_visits and ty_visits are removed because they have their values are NA in the first couple of months. product_content_id is removed because it is not a value that helps predicting the output.
product1<-na.locf(product1)
product1<-na.locf(product1, fromLast=TRUE)
library(leaps)
cor(product1[,-"event_date"])
#favored_count and visit_count are highly correlated, there is no need to include both of them as predictors. We will remove favored_count because it is less correlated with sold_count.
cor(product1[,-c("event_date","favored_count")])

#category_brand_sold and category_sold are highly correlated, there is no need to include both of them as predictors. We will remove category_brand_sold because it is less correlated with sold_count.
cor(product1[,-c("event_date","favored_count","category_brand_sold")])

#Correlations seem fine now.

product1xts<-xts(product1[,-2], order.by=product1$event_date)
trainxreg<-product1xts[,c("price_difference", "coronavirusdailycases", "visit_count", "basket_count", "price", "category_sold")]

arimafit<-auto.arima(product1xts[,"sold_count"],xreg=trainxreg)
summary(arimafit)
summary(auto.arima(product1xts[,"sold_count"]))
#Using regression significantly improves our model, we have lower AIC, AICc, and BIC when the input variables are used in the Arima model.

checkresiduals(arimafit)  
# p-value of the Ljung-Box test shows that residuals look like white noise.

summary(ets(product1xts[,"sold_count"]))


farima <- function(x, h, xreg) {
  forecast(auto.arima(x, xreg = xreg), h=h)}
fets <- function(x, h) {
  forecast(ets(x), h = h)}
e1 <- tsCV(product1xts[,"sold_count"], farima, h=1, xreg=trainxreg)

e2 <- tsCV(product1xts[,"sold_count"], fets, h=1)
mean(e1^2, na.rm=TRUE)

mean(e2^2, na.rm=TRUE)


# By comparing errors, we see that the Arima model gives better results, that’s why we will go ahead with it.

library(tsbox)
product1ts <-ts_ts(product1xts)
product1ts<-ts(product1ts, frequency=7) 
A<-(forecast(tbats(product1ts[,"price_difference"]),h=1))$mean[1]
B<-(forecast(tbats(product1ts[,"coronavirusdailycases"]),h=1))$mean[1]
C<-(forecast(tbats(product1ts[,"visit_count"]),h=1))$mean[1]
D<-(forecast(tbats(product1ts[,"basket_count"]),h=1))$mean[1]
E<-(forecast(tbats(product1ts[,"price"]),h=1))$mean[1]
F<-(forecast(tbats(product1ts[,"category_sold"]),h=1))$mean[1]

product1prediction <- round(forecast(arimafit, xreg=cbind(A,B,C,D,E,F))$mean[1], digit=0)
product1prediction
predictions[3,forecast:=product1prediction]
#0


#product2
product2<-data[data$product_content_id=='6676673']
product2$price<-na.locf(product2$price)
product2$price<-na.locf(product2$price, fromLast=TRUE)

price_difference <- diff(product2$price)
price_difference<-c(0,price_difference)
product2<-product2[,price_difference:=price_difference]

product2<-product2[,coronavirusdailycases:= coronavirusdata]
a<-head(which(product2[,"visit_count"]>0),n=1)
product2<-product2[-(1:(a-1)),]
product2<-product2[,c("category_visits","ty_visits", "product_content_id"):=NULL]
#category_visits and ty_visits are removed because they have their values are NA in the first couple of months. product_content_id is removed because it is not a value that helps predicting the output.
product2<-na.locf(product2)
product2<-na.locf(product2, fromLast=TRUE)
library(leaps)
cor(product2[,-"event_date"])
#favored_count and visit_count are highly correlated, there is no need to include both of them as predictors. We will remove favored_count because it is less correlated with sold_count.
cor(product2[,-c("event_date","favored_count")])

#category_brand_sold and category_sold are highly correlated, there is no need to include both of them as predictors. We will remove category_brand_sold because it is less correlated with sold_count.
cor(product2[,-c("event_date","favored_count","category_brand_sold")])

#Correlations seem fine now.

product2xts<-xts(product2[,-2], order.by=product2$event_date)
trainxreg<-product2xts[,c("price_difference", "coronavirusdailycases", "visit_count", "basket_count", "price", "category_sold")]

arimafit<-auto.arima(product2xts[,"sold_count"],xreg=trainxreg)
summary(arimafit)
summary(auto.arima(product2xts[,"sold_count"]))
#Using regression significantly improves our model, we have lower AIC, AICc, and BIC when the input variables are used in the Arima model.

checkresiduals(arimafit)  

# p-value of the Ljung-Box test shows that residuals look like white noise.

summary(ets(product2xts[,"sold_count"]))


farima <- function(x, h, xreg) {
  forecast(auto.arima(x, xreg = xreg), h=h)}
fets <- function(x, h) {
  forecast(ets(x), h = h)}
e1 <- tsCV(product2xts[,"sold_count"], farima, h=1, xreg=trainxreg)

e2 <- tsCV(product2xts[,"sold_count"], fets, h=1)
mean(e1^2, na.rm=TRUE)

mean(e2^2, na.rm=TRUE)

# By comparing errors, we see that the Arima model gives better results, that’s why we will go ahead with it.

library(tsbox)
product2ts <-ts_ts(product2xts)
product2ts<-ts(product2ts, frequency=7) 
A<-(forecast(tbats(product2ts[,"price_difference"]),h=1))$mean[1]
B<-(forecast(tbats(product2ts[,"coronavirusdailycases"]),h=1))$mean[1]
C<-(forecast(tbats(product2ts[,"visit_count"]),h=1))$mean[1]
D<-(forecast(tbats(product2ts[,"basket_count"]),h=1))$mean[1]
E<-(forecast(tbats(product2ts[,"price"]),h=1))$mean[1]
F<-(forecast(tbats(product2ts[,"category_sold"]),h=1))$mean[1]

product2prediction <- round(forecast(arimafit, xreg=cbind(A,B,C,D,E,F))$mean[1], digit=0)
product2prediction
predictions[6,forecast:= product2prediction]
#375

#product3
product3<-data[data$product_content_id=='4066298']

product3$price<-na.locf(product3$price)
product3$price<-na.locf(product3$price, fromLast=TRUE)

price_difference <- diff(product3$price)
price_difference<-c(0,price_difference)
product3<-product3[,price_difference:=price_difference]

product3<-product3[,coronavirusdailycases:= coronavirusdata]
a<-head(which(product3[,"visit_count"]>0),n=1)
product3<-product3[-(1:(a-1)),]
product3<-product3[,c("category_visits","ty_visits", "product_content_id"):=NULL]
#category_visits and ty_visits are removed because they have their values are NA in the first couple of months. product_content_id is removed because it is not a value that helps predicting the output.
product3<-na.locf(product3)
product3<-na.locf(product3, fromLast=TRUE)
library(leaps)
cor(product3[,-"event_date"])
#favored_count and visit_count are highly correlated, there is no need to include both of them as predictors. We will remove favored_count because it is less correlated with sold_count.
cor(product3[,-c("event_date","favored_count")])

#category_brand_sold and category_sold are highly correlated, there is no need to include both of them as predictors. We will remove category_brand_sold because it is less correlated with sold_count.
cor(product3[,-c("event_date","favored_count","category_brand_sold")])

#Correlations seem fine now.

product3xts<-xts(product3[,-2], order.by=product3$event_date)
trainxreg<-product3xts[,c("price_difference", "coronavirusdailycases", "visit_count", "basket_count", "price", "category_sold")]

arimafit<-auto.arima(product3xts[,"sold_count"],xreg=trainxreg)
summary(arimafit)
summary(auto.arima(product3xts[,"sold_count"]))

#Using regression significantly improves our model, we have lower AIC, AICc, and BIC when the input variables are used in the Arima model.

checkresiduals(arimafit)  

# p-value of the Ljung-Box test shows that residuals look like white noise.

summary(ets(product3xts[,"sold_count"]))


farima <- function(x, h, xreg) {
  forecast(auto.arima(x, xreg = xreg), h=h)}
fets <- function(x, h) {
  forecast(ets(x), h = h)}
e1 <- tsCV(product3xts[,"sold_count"], farima, h=1, xreg=trainxreg)

e2 <- tsCV(product3xts[,"sold_count"], fets, h=1)
mean(e1^2, na.rm=TRUE)

mean(e2^2, na.rm=TRUE)


# By comparing errors, we see that the Arima model gives better results, that’s why we will go ahead with it.

library(tsbox)
product3ts <-ts_ts(product3xts)
product3ts<-ts(product3ts, frequency=7) 
A<-(forecast(tbats(product3ts[,"price_difference"]),h=1))$mean[1]
B<-(forecast(tbats(product3ts[,"coronavirusdailycases"]),h=1))$mean[1]
C<-(forecast(tbats(product3ts[,"visit_count"]),h=1))$mean[1]
D<-(forecast(tbats(product3ts[,"basket_count"]),h=1))$mean[1]
E<-(forecast(tbats(product3ts[,"price"]),h=1))$mean[1]
F<-(forecast(tbats(product3ts[,"category_sold"]),h=1))$mean[1]

product3prediction <- round(forecast(arimafit, xreg=cbind(A,B,C,D,E,F))$mean[1], digit=0)
product3prediction
predictions[4,forecast:=product3prediction]



#product4
product4<-data[data$product_content_id=='32939029']
product4$price<-na.locf(product4$price)
product4$price<-na.locf(product4$price, fromLast=TRUE)

price_difference <- diff(product4$price)
price_difference<-c(0,price_difference)
product4<-product4[,price_difference:=price_difference]

product4<-product4[,coronavirusdailycases:= coronavirusdata]
a<-head(which(product4[,"visit_count"]>0),n=1)
product4<-product4[-(1:(a-1)),]
product4<-product4[,c("category_visits","ty_visits", "product_content_id"):=NULL]
#category_visits and ty_visits are removed because they have their values are NA in the first couple of months. product_content_id is removed because it is not a value that helps predicting the output.
product4<-na.locf(product4)
product4<-na.locf(product4, fromLast=TRUE)
library(leaps)
cor(product4[,-"event_date"])
#favored_count and visit_count are highly correlated, there is no need to include both of them as predictors. We will remove favored_count because it is less correlated with sold_count.
cor(product4[,-c("event_date","favored_count")])

#category_brand_sold and category_sold are highly correlated, there is no need to include both of them as predictors. We will remove category_brand_sold because it is less correlated with sold_count.
cor(product4[,-c("event_date","favored_count","category_brand_sold")])

#Correlations seem fine now.

product4xts<-xts(product4[,-2], order.by=product4$event_date)
trainxreg<-product4xts[,c("price_difference", "coronavirusdailycases", "visit_count", "basket_count", "price", "category_sold")]

arimafit<-auto.arima(product4xts[,"sold_count"],xreg=trainxreg)
summary(arimafit)
summary(auto.arima(product4xts[,"sold_count"]))

#Using regression significantly improves our model, we have lower AIC, AICc, and BIC when the input variables are used in the Arima model.
checkresiduals(arimafit)  

# p-value of the Ljung-Box test shows that residuals look like white noise.

summary(ets(product4xts[,"sold_count"]))


farima <- function(x, h, xreg) {
  forecast(auto.arima(x, xreg = xreg), h=h)}
fets <- function(x, h) {
  forecast(ets(x), h = h)}
e1 <- tsCV(product4xts[,"sold_count"], farima, h=1, xreg=trainxreg)

e2 <- tsCV(product4xts[,"sold_count"], fets, h=1)
mean(e1^2, na.rm=TRUE)


mean(e2^2, na.rm=TRUE)

# By comparing errors, we see that the Arima model gives better results, that’s why we will go ahead with it.

library(tsbox)
product4ts <-ts_ts(product4xts)
product4ts<-ts(product4ts, frequency=7) 
A<-(forecast(tbats(product4ts[,"price_difference"]),h=1))$mean[1]
B<-(forecast(tbats(product4ts[,"coronavirusdailycases"]),h=1))$mean[1]
C<-(forecast(tbats(product4ts[,"visit_count"]),h=1))$mean[1]
D<-(forecast(tbats(product4ts[,"basket_count"]),h=1))$mean[1]
E<-(forecast(tbats(product4ts[,"price"]),h=1))$mean[1]
F<-(forecast(tbats(product4ts[,"category_sold"]),h=1))$mean[1]

product4prediction <- round(forecast(arimafit, xreg=cbind(A,B,C,D,E,F))$mean[1], digit=0)
product4prediction
predictions[2,forecast:= product4prediction]


#product5
product5<-data[data$product_content_id=='85004']
product5$price<-na.locf(product5$price)
product5$price<-na.locf(product5$price, fromLast=TRUE)

price_difference <- diff(product5$price)
price_difference<-c(0,price_difference)
product5<-product5[,price_difference:=price_difference]

product5<-product5[,coronavirusdailycases:= coronavirusdata]
a<-head(which(product5[,"visit_count"]>0),n=1)
product5<-product5[-(1:(a-1)),]
product5<-product5[,c("category_visits","ty_visits", "product_content_id"):=NULL]
#category_visits and ty_visits are removed because they have their values are NA in the first couple of months. product_content_id is removed because it is not a value that helps predicting the output.
product5<-na.locf(product5)
product5<-na.locf(product5, fromLast=TRUE)
library(leaps)
cor(product5[,-"event_date"])
#favored_count and visit_count are highly correlated, there is no need to include both of them as predictors. We will remove favored_count because it is less correlated with sold_count.
cor(product5[,-c("event_date","favored_count")])

#category_brand_sold and category_sold are highly correlated, there is no need to include both of them as predictors. We will remove category_brand_sold because it is less correlated with sold_count.
cor(product5[,-c("event_date","favored_count","category_brand_sold")])

#Correlations seem fine now.

product5xts<-xts(product5[,-2], order.by=product5$event_date)
trainxreg<-product5xts[,c("price_difference", "coronavirusdailycases", "visit_count", "basket_count", "price", "category_sold")]

arimafit<-auto.arima(product5xts[,"sold_count"],xreg=trainxreg)
summary(arimafit)
summary(auto.arima(product5xts[,"sold_count"]))

#Using regression significantly improves our model, we have lower AIC, AICc, and BIC when the input variables are used in the Arima model.

checkresiduals(arimafit)  

# p-value of the Ljung-Box test shows that residuals look like white noise.

summary(ets(product5xts[,"sold_count"]))


farima <- function(x, h, xreg) {
  forecast(auto.arima(x, xreg = xreg), h=h)}
fets <- function(x, h) {
  forecast(ets(x), h = h)}
e1 <- tsCV(product5xts[,"sold_count"], farima, h=1, xreg=trainxreg)

e2 <- tsCV(product5xts[,"sold_count"], fets, h=1)
mean(e1^2, na.rm=TRUE)

mean(e2^2, na.rm=TRUE)


#By comparing errors, we see that the Arima model gives better results, that’s why we will go ahead with it.


library(tsbox)
product5ts <-ts_ts(product5xts)
product5ts<-ts(product5ts, frequency=7) 
A<-(forecast(tbats(product5ts[,"price_difference"]),h=1))$mean[1]
B<-(forecast(tbats(product5ts[,"coronavirusdailycases"]),h=1))$mean[1]
C<-(forecast(tbats(product5ts[,"visit_count"]),h=1))$mean[1]
D<-(forecast(tbats(product5ts[,"basket_count"]),h=1))$mean[1]
E<-(forecast(tbats(product5ts[,"price"]),h=1))$mean[1]
F<-(forecast(tbats(product5ts[,"category_sold"]),h=1))$mean[1]

product5prediction <- round(forecast(arimafit, xreg=cbind(A,B,C,D,E,F))$mean[1], digit=0)
product5prediction
predictions[8,forecast:= product5prediction]

#
#product6
product6<-data[data$product_content_id=='7061886']

product6$price<-na.locf(product6$price)
product6$price<-na.locf(product6$price, fromLast=TRUE)

price_difference <- diff(product6$price)
price_difference<-c(0,price_difference)
product6<-product6[,price_difference:=price_difference]

product6<-product6[,coronavirusdailycases:= coronavirusdata]
a<-head(which(product6[,"visit_count"]>0),n=1)
product6<-product6[-(1:(a-1)),]
product6<-product6[,c("category_visits","ty_visits", "product_content_id"):=NULL]
#category_visits and ty_visits are removed because they have their values are NA in the first couple of months. product_content_id is removed because it is not a value that helps predicting the output.
product6<-na.locf(product6)

product6<-na.locf(product6, fromLast=TRUE)
library(leaps)
cor(product6[,-"event_date"])
#favored_count and visit_count are highly correlated, there is no need to include both of them as predictors. We will remove favored_count because it is less correlated with sold_count.
cor(product6[,-c("event_date","basket_count")])

#category_brand_sold and category_sold are highly correlated, there is no need to include both of them as predictors. We will remove category_brand_sold because it is less correlated with sold_count.
cor(product6[,-c("event_date","basket_count","category_brand_sold")])

#Correlations seem fine now.

product6xts<-xts(product6[,-2], order.by=product6$event_date)
trainxreg<-product6xts[,c("price_difference", "coronavirusdailycases", "visit_count", "basket_count", "price", "category_sold")]

arimafit<-auto.arima(product6xts[,"sold_count"],xreg=trainxreg)
summary(arimafit)
# Regression with ARIMA(1,0,0).  

summary(auto.arima(product6xts[,"sold_count"]))

#Using regression significantly improves our model, we have lower AIC, AICc, and BIC when the input variables are used in the Arima model.

checkresiduals(arimafit)  

# p-value of the Ljung-Box test shows that residuals look like white noise.

summary(ets(product6xts[,"sold_count"]))

# ETS(A,N,N) Our data has neither trend nor seasonality, and it has additive errors.

farima <- function(x, h, xreg) {
  forecast(auto.arima(x, xreg = xreg), h=h)}
fets <- function(x, h) {
  forecast(ets(x), h = h)}
e1 <- tsCV(product6xts[,"sold_count"], farima, h=1, xreg=trainxreg)

e2 <- tsCV(product6xts[,"sold_count"], fets, h=1)
mean(e1^2, na.rm=TRUE)

mean(e2^2, na.rm=TRUE)

#By comparing errors, we see that the Arima model gives better results, that’s why we will go ahead with it.


library(tsbox)
product6ts <-ts_ts(product6xts)
product6ts<-ts(product6ts, frequency=7) 
A<-(forecast(tbats(product6ts[,"price_difference"]),h=1))$mean[1]
B<-(forecast(tbats(product6ts[,"coronavirusdailycases"]),h=1))$mean[1]
C<-(forecast(tbats(product6ts[,"visit_count"]),h=1))$mean[1]
D<-(forecast(tbats(product6ts[,"basket_count"]),h=1))$mean[1]
E<-(forecast(tbats(product6ts[,"price"]),h=1))$mean[1]
F<-(forecast(tbats(product6ts[,"category_sold"]),h=1))$mean[1]

product6prediction <- round(forecast(arimafit, xreg=cbind(A,B,C,D,E,F))$mean[1], digit=0)
product6prediction
predictions[7,forecast:=product6prediction]
#54


#product7
product7<-data[data$product_content_id=='5926527']
product8$price<-na.locf(product7$price)
product7$price<-na.locf(product7$price, fromLast=TRUE)

price_difference <- diff(product7$price)
price_difference<-c(0,price_difference)
product7<-product7[,price_difference:=price_difference]

product7<-product7[,coronavirusdailycases:= coronavirusdata]
a<-head(which(product7[,"visit_count"]>0),n=1)
product7<-product7[-(1:(a-1)),]
product7<-product7[,c("category_visits","ty_visits", "product_content_id"):=NULL]
#category_visits and ty_visits are removed because they have their values are NA in the first couple of months. product_content_id is removed because it is not a value that helps predicting the output.
product7<-na.locf(product7)

product7<-na.locf(product7, fromLast=TRUE)
library(leaps)
cor(product7[,-"event_date"])
#favored_count and visit_count are highly correlated, there is no need to include both of them as predictors. We will remove favored_count because it is less correlated with sold_count.
cor(product7[,-c("event_date","favored_count")])

#category_brand_sold and category_sold are highly correlated, there is no need to include both of them as predictors. We will remove category_brand_sold because it is less correlated with sold_count.
cor(product7[,-c("event_date","favored_count","category_brand_sold")])

#Correlations seem fine now.

product7xts<-xts(product7[,-2], order.by=product7$event_date)
trainxreg<-product7xts[,c("price_difference", "coronavirusdailycases", "visit_count", "basket_count", "price", "category_sold")]

arimafit<-auto.arima(product7xts[,"sold_count"],xreg=trainxreg)
summary(arimafit)
summary(auto.arima(product7xts[,"sold_count"]))

#Using regression significantly improves our model, we have lower AIC, AICc, and BIC when the input variables are used in the Arima model.

checkresiduals(arimafit)  

# p-value of the Ljung-Box test shows that residuals look like white noise.

summary(ets(product7xts[,"sold_count"]))


farima <- function(x, h, xreg) {
  forecast(auto.arima(x, xreg = xreg), h=h)}
fets <- function(x, h) {
  forecast(ets(x), h = h)}
e1 <- tsCV(product7xts[,"sold_count"], farima, h=1, xreg=trainxreg)

e2 <- tsCV(product7xts[,"sold_count"], fets, h=1)
mean(e1^2, na.rm=TRUE)

mean(e2^2, na.rm=TRUE)

#By comparing errors, we see that the Arima model gives better results, that’s why we will go ahead with it.

library(tsbox)
product7ts <-ts_ts(product7xts)
product7ts<-ts(product7ts, frequency=7) 
A<-(forecast(tbats(product7ts[,"price_difference"]),h=1))$mean[1]
B<-(forecast(tbats(product7ts[,"coronavirusdailycases"]),h=1))$mean[1]
C<-(forecast(tbats(product7ts[,"visit_count"]),h=1))$mean[1]
D<-(forecast(tbats(product7ts[,"basket_count"]),h=1))$mean[1]
E<-(forecast(tbats(product7ts[,"price"]),h=1))$mean[1]
F<-(forecast(tbats(product7ts[,"category_sold"]),h=1))$mean[1]

product7prediction <- round(forecast(arimafit, xreg=cbind(A,B,C,D,E,F))$mean[1], digit=0)
product7prediction
predictions[5,forecast:=product7prediction]
#0

#product8
product8<-data[data$product_content_id=='31515569']
product8$price<-na.locf(product8$price)
product8$price<-na.locf(product8$price, fromLast=TRUE)

price_difference <- diff(product8$price)
price_difference<-c(0,price_difference)
product8<-product8[,price_difference:=price_difference]

product8<-product8[,coronavirusdailycases:= coronavirusdata]
a<-head(which(product8[,"visit_count"]>0),n=1)
product8<-product8[-(1:(a-1)),]
product8<-product8[,c("category_visits","ty_visits", "product_content_id"):=NULL]
#category_visits and ty_visits are removed because they have their values are NA in the first couple of months. product_content_id is removed because it is not a value that helps predicting the output.
product8<-na.locf(product8)

product8<-na.locf(product8, fromLast=TRUE)
library(leaps)
cor(product8[,-"event_date"])
#favored_count and visit_count are highly correlated, there is no need to include both of them as predictors. We will remove favored_count because it is less correlated with sold_count.
cor(product8[,-c("event_date","favored_count")])

#category_brand_sold and category_sold are highly correlated, there is no need to include both of them as predictors. We will remove category_brand_sold because it is less correlated with sold_count.
cor(product8[,-c("event_date","favored_count","category_brand_sold")])

#Correlations seem fine now.

product8xts<-xts(product8[,-2], order.by=product8$event_date)
trainxreg<-product8xts[,c("price_difference", "coronavirusdailycases", "visit_count", "basket_count", "price", "category_sold")]

arimafit<-auto.arima(product8xts[,"sold_count"],xreg=trainxreg)
summary(arimafit)
summary(auto.arima(product8xts[,"sold_count"]))

#Using regression significantly improves our model, we have lower AIC, AICc, and BIC when the input variables are used in the Arima model.

checkresiduals(arimafit)  

# p-value of the Ljung-Box test shows that residuals look like white noise.

summary(ets(product8xts[,"sold_count"]))


farima <- function(x, h, xreg) {
  forecast(auto.arima(x, xreg = xreg), h=h)}
fets <- function(x, h) {
  forecast(ets(x), h = h)}
e1 <- tsCV(product8xts[,"sold_count"], farima, h=1, xreg=trainxreg)

e2 <- tsCV(product8xts[,"sold_count"], fets, h=1)
mean(e1^2, na.rm=TRUE)

mean(e2^2, na.rm=TRUE)

#By comparing errors, we see that the Arima model gives better results, that’s why we will go ahead with it.

library(tsbox)
product8ts <-ts_ts(product8xts)
product8ts<-ts(product8ts, frequency=7) 
A<-(forecast(tbats(product8ts[,"price_difference"]),h=1))$mean[1]
B<-(forecast(tbats(product8ts[,"coronavirusdailycases"]),h=1))$mean[1]
C<-(forecast(tbats(product8ts[,"visit_count"]),h=1))$mean[1]
D<-(forecast(tbats(product8ts[,"basket_count"]),h=1))$mean[1]
E<-(forecast(tbats(product8ts[,"price"]),h=1))$mean[1]
F<-(forecast(tbats(product8ts[,"category_sold"]),h=1))$mean[1]

product8prediction <- round(forecast(arimafit, xreg=cbind(A,B,C,D,E,F))$mean[1], digit=0)
product8prediction
predictions[1,forecast:= product8prediction]



predictions=unique(data[,list(product_content_id)])
predictions[1,forecast:=product8prediction]
predictions[2,forecast:=product4prediction]
predictions[3,forecast:=product1prediction]
predictions[4,forecast:=product3prediction]
predictions[5,forecast:=product7prediction]
predictions[6,forecast:=product2prediction]
predictions[7,forecast:=product6prediction]
predictions[8,forecast:=product5prediction]


send_submission(predictions, token, url=subm_url, submit_now=TRUE)
