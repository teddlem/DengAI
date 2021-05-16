#DengAI competition

#New computer, new installation of packages
install.packages("fpp2")
install.packages("lubridate")
install.packages("zoo")
install.packages("quantmod")
install.packages("eventstudies")
install.packages("smooth")
install.packages("dplyr")
install.packages("vars")
install.packages("Amelia")
library(vars)
library(Amelia)
library(forecast)
library(smooth)
library(fpp2) 
library(lubridate)
library(dplyr)
library(zoo)
library(quantmod)
library(eventstudies)


#Importing datasets 
d_train <- read.csv("C:/Users/teddl/Downloads/dengue_features_train.csv", header=TRUE) 
d_test <- read.csv("C:/Users/teddl/Downloads/dengue_features_test.csv", header = TRUE) 
d_label <-read.csv("C:/Users/teddl/Downloads/dengue_labels_train.csv", header = TRUE)
submission_format<-read.csv("C:/Users/teddl/Downloads/submission_format.csv", header = TRUE)

#View(d_train) 
#View(d_test) 
#View(d_label)

#Merging Train and Label 
d_train$id <- paste(d_train$city, d_train$year,d_train$weekofyear) 
d_label$id <- paste(d_label$city, d_label$year,d_label$weekofyear) 
train<-merge(x=d_train, y=d_label, all.x = TRUE)

#Cleaning data
sum(is.na(train$precipitation_amt_mm)) #13
sum(is.na(train$reanalysis_precip_amt_kg_per_m2)) #10
sum(is.na(train$reanalysis_dew_point_temp_k)) #10


#Separating San Juan and Iquitos test set
sd_test<-d_test[1:260,]
id_test<-d_test[261:416,]

#Adding month variable, which for TRAIN proved to be slightly LESS predictive than existing "weekofyear" 
#variable, until I converted it from an number to a factor. 8% vs. 14.5% AdjR2 difference. 
train$month<-month(train$week_start_date) 
train$month<-as.factor(train$month) 
id_test$month<-month(id_test$week_start_date) 
id_test$month<-as.factor(id_test$month)
sd_test$month<-month(sd_test$week_start_date) 
sd_test$month<-as.factor(sd_test$month)
#str(train)

#Splitting San Juan and Iquito data, "strain" and "itrain" 
itrain <-train[train$city %in% "iq",] 
strain <-train[train$city %in% "sj",]

#Checking for significant variables
lmi<-lm(total_cases~station_min_temp_c, data=itrain)
summary(lmi)
#Variable list: 
"""
total_cases~reanalysis_air_temp_k+station_max_temp_c+station_min_temp_c+ reanalysis_avg_temp_k +
reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+station_avg_temp_c+ 
station_diur_temp_rng_c+reanalysis_tdtr_k,ndvi_ne+ndvi_se+ndvi_nw+ndvi_sw,reanalysis_air_temp_k+station_max_temp_c+station_min_temp_c+ 
reanalysis_avg_temp_k +reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+station_avg_temp_c+ 
station_diur_temp_rng_c+reanalysis_tdtr_k,station_precip_mm+precipitation_amt_mm+reanalysis_sat_precip_amt_mm+ reanalysis_dew_point_temp_k+
reanalysis_relative_humidity_percent+ reanalysis_specific_humidity_g_per_kg+reanalysis_precip_amt_kg_per_m2
"""
#Conclusion - Variables worth using (>3%Adj.R2) are: month, reanalysis_specific_humidity_g_per_kg, reanalysis_dew_point_temp_k,
#station_avg_temp_c, station_min_temp_c 

#Splitting into train and validation sets 
strain_ts <- ts(strain$total_cases) 
subtrain <-ts(strain$total_cases[1:800])

itrain_ts <- ts(itrain$total_cases) 
subtrain2 <-ts(itrain$total_cases[1:800])


#Linear model on temperature: AdjR2=11% 
lm1 <-lm(total_cases~reanalysis_air_temp_k+station_max_temp_c+station_min_temp_c+ reanalysis_avg_temp_k +
           reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+station_avg_temp_c+ 
           station_diur_temp_rng_c+reanalysis_tdtr_k, data=train) summary(lm1)

#Linear model on ndvi vegetation index: AdjR2=06% 
lm2 <-lm(total_cases~ndvi_ne+ndvi_se+ndvi_nw+ndvi_sw, data=train) 
summary(lm2)

#Linear model on precipitation: AdjR2=09% 
lm3 <-lm(total_cases~station_precip_mm+precipitation_amt_mm+reanalysis_sat_precip_amt_mm+ reanalysis_dew_point_temp_k+
           reanalysis_relative_humidity_percent+ reanalysis_specific_humidity_g_per_kg+reanalysis_precip_amt_kg_per_m2,
         data=train) 
summary(lm3)

#Linear model on city: AdjR2=9%, but look at the differential! 
lm4 <-lm(total_cases~city, data=train) 
summary(lm4)

#San Juan linear model on temperature: AdjR2=7% 
lm5 <-lm(total_cases~reanalysis_air_temp_k+station_max_temp_c+station_min_temp_c+ 
           reanalysis_avg_temp_k +reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+station_avg_temp_c+ 
           station_diur_temp_rng_c+reanalysis_tdtr_k, data=strain) 
summary(lm5)

#Iquito linear model on temperature: AdjR2=6% 
lm6 <-lm(total_cases~reanalysis_air_temp_k+station_max_temp_c+station_min_temp_c+ 
           reanalysis_avg_temp_k +reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+station_avg_temp_c+ 
           station_diur_temp_rng_c+reanalysis_tdtr_k, data=itrain) 
summary(lm6)

mean(itrain$total_cases) #7.6 
mean(strain$total_cases) #34.2 
median(itrain$total_cases) #5 
median(strain$total_cases) #19

#Daily temperature differential is essentially useless, with an AdjR2 value of 0.01% for San #Juan, 0.1% for Iquito
lm7 <-lm(total_casesstation_diur_temp_rng_c, data=strain) 
summary(lm7) 
lm.87 <-lm(total_casesstation_diur_temp_rng_c, data=itrain) 
summary(lm.87)

#Weekofyear AdjR2=8% for San Juan, negative for Iquito (essentially the same for month as number) 
lm9 <-lm(total_cases~weekofyear, data=itrain) 
summary(lm9)
lm10 <-lm(total_cases~weekofyear, data=strain) 
summary(lm10)

#Linear model on month as factor variable 
lm11 <-lm(total_cases~month, data=strain) #AdjR2=14.5% Wow! 
summary(lm11)

#Trying a generic ETS forecast (bleh!) 
subtrain_ets <-forecast(subtrain, h=136) 
subtrain_ets %>% forecast() %>% autoplot () + autolayer(strain_ts)
accuracy(subtrain_ets)  #MAE=12.50
checkresiduals(subtrain_ets) 
#Several statistically significant variances, normal distribution

#Trying a generic auto.arima forecast (equally bleh!) 
subtrain_arima <-auto.arima(subtrain) 
subtrain_arima %>% forecast(h=136) %>% autoplot() + autolayer(strain_ts)
accuracy(subtrain_arima) #MAE=12.80
checkresiduals(subtrain_arima)
#Several statistically significant variances, normal distribution

#Slice large and small spikes and find if there's some variable 
#that accounts for this (including lag)
spike <- strain[212:216,]
nspike <- strain[20:24,]
summary(spike)
summary(nspike)
View(train)
#Conclusion: It looks like precipitation is a lagged variable by about 1 month

#Get median by month, Min=April(7), Max=October(43)
ssubtrain <- strain[1:800,]
median_month<-ssubtrain%>%
  group_by(month)%>% 
  summarise(median=median(total_cases))
summary(median_month)

#Putting monthly median into file for forecast
median_strain<-merge(x=ssubtrain, y=median_month, all.x = TRUE)
median <-ts(median_strain$median)
cases <- ts(median_strain$total_cases)
fc<-VAR(cbind(cases, median))
summary(fc) #AdjR2 = 0.9932
fc %>% forecast() %>% autoplot()

#Building a monthly median model
rm(median)
agg<-aggregate(total_cases~month, FUN=median, data=itrain)
mmerge<-merge(id_test, agg, by="month", all.x=TRUE)
mmerge$total_cases<-as.integer(mmerge$total_cases)
mymerge3<-mmerge[,c(2,3,4,26)]

myagg<-aggregate(total_cases~month, FUN=median, data=strain)
mymerge<-merge(sd_test, myagg, by="month", all.x=TRUE)
mymerge$total_cases<-as.integer(mymerge$total_cases)
mymerge2<-mymerge[,c(2,3,4,26)]
mymerge2_ts <- ts(mymerge[,4])
mymerge4<-full_join(x=mymerge2, y=mymerge3, all.x=TRUE)
mymerge4[order(mymerge4$year,mymerge4$weekofyear), ]
mymerge4_ts<-ts(mymerge4_ts)
View(mymerge4_ts)
submission_format$total_cases<-mymerge4$total_cases
View(submission_format)
write.csv(submission_format, "mediantest.csv", row.names=FALSE) #MAE=29.877, which
  #is five worse than the benchmark (in Python) on the website.
autoplot(mymerge4_ts)
strain_ts %>% autoplot() + autolayer(mymerge4_ts)


View(strain_ts)
strain_med<-ts(median_strain$median)
subtrain_med <-forecast(strain_med, h=136) 
subtrain_med %>% forecast() %>% autoplot () + autolayer(strain_ts)
accuracy(subtrain_med)  #MAE=12.50
checkresiduals(subtrain_med)
