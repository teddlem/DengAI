# DengAI
DengAI competition 

library(fpp2)
library(lubridate)

#Importing datasets
d_train <- read.csv("C:/Users/teddl/Downloads/dengue_features_train.csv", header=TRUE)
d_test <- read.csv("C:/Users/teddl/Downloads/dengue_features_test.csv", header = TRUE)
d_label <-read.csv("C:/Users/teddl/Downloads/dengue_labels_train.csv", header = TRUE)

#View(d_train)
#View(d_test)
#View(d_label)

#Merging Train and Label
d_train$id <- paste(d_train$city, d_train$year,d_train$weekofyear)
d_label$id <- paste(d_label$city, d_label$year,d_label$weekofyear)
train<-merge(x=d_train, y=d_label, all.x = TRUE)

#Adding month variable, which proved to be slightly LESS predictive than existing "weekofyear" variable, 
#until I converted it from an number to a factor. 8% vs. 14.5% AdjR2 difference.
train$month<-month(train$week_start_date)
train$month<-as.factor(train$month)
str(train)

#Linear model on temperature: AdjR2=11%
lm1 <-lm(total_cases~reanalysis_air_temp_k+station_max_temp_c+station_min_temp_c+
         reanalysis_avg_temp_k +reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+station_avg_temp_c+
           station_diur_temp_rng_c+reanalysis_tdtr_k,  data=train)
summary(lm1)

#Linear model on ndvi vegetation index: AdjR2=06%
lm2 <-lm(total_cases~ndvi_ne+ndvi_se+ndvi_nw+ndvi_sw,  data=train)
summary(lm2)

#Linear model on precipitation: AdjR2=09%
lm3 <-lm(total_cases~station_precip_mm+precipitation_amt_mm+reanalysis_sat_precip_amt_mm+
           reanalysis_dew_point_temp_k+reanalysis_relative_humidity_percent+
           reanalysis_specific_humidity_g_per_kg+reanalysis_precip_amt_kg_per_m2,  data=train)
summary(lm3)

#Linear model on city: AdjR2=9%, but look at the differential!
lm4 <-lm(total_cases~city,  data=train)
summary(lm4)

#Splitting San Juan and Iquito data, "strain" and "itrain"
itrain <-train[train$city %in% "iq",]
strain <-train[train$city %in% "sj",]

#San Juan linear model on temperature: AdjR2=7%
lm5 <-lm(total_cases~reanalysis_air_temp_k+station_max_temp_c+station_min_temp_c+
           reanalysis_avg_temp_k +reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+station_avg_temp_c+
           station_diur_temp_rng_c+reanalysis_tdtr_k,  data=strain)
summary(lm5)

#Iquito linear model on temperature: AdjR2=6%
lm6 <-lm(total_cases~reanalysis_air_temp_k+station_max_temp_c+station_min_temp_c+
           reanalysis_avg_temp_k +reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+station_avg_temp_c+
           station_diur_temp_rng_c+reanalysis_tdtr_k,  data=itrain)
summary(lm6)

mean(itrain$total_cases) #7.6
mean(strain$total_cases) #34.2
median(itrain$total_cases) #5
median(strain$total_cases) #19

#Daily temperature differential is essentially useless, with an AdjR2 value of 0.01% for San
#Juan, 0.1% for Iquito

lm7 <-lm(total_cases~station_diur_temp_rng_c, data=strain)
summary(lm7) 
lm.87 <-lm(total_cases~station_diur_temp_rng_c, data=itrain)
summary(lm.87)

#Weekofyear AdjR2=8% for San Juan, negative for Iquito (essentially the same for month as number)
lm9 <-lm(total_cases~weekofyear, data=itrain)
summary(lm9)  
lm10 <-lm(total_cases~weekofyear, data=strain)
summary(lm10) 

#Linear model on month as factor variable
lm11 <-lm(total_cases~month, data=strain) #AdjR2=14.5% Wow!
summary(lm11) 

#Splitting into train and validation sets
strain_ts <- ts(strain$total_cases)
subtrain <-ts(strain$total_cases[1:800])

#Trying a generic ETS forecast (bleh!)
subtrain_ets <-forecast(subtrain, h=136) 
subtrain_ets %>% forecast() %>% autoplot () +
  autolayer(strain_ts)

#Trying a generic auto.arima forecast (equally bleh!)
subtrain_ets <-auto.arima(subtrain) 
subtrain_ets %>% forecast(h=136) %>% autoplot () +
  autolayer(strain_ts)
