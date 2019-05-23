#Alright, in the first part it is just a proof of concpect, I am trying a few ARIMA models just to see if this in fact is going to work
#It is a pretty cool plot at the end, I will try and post somewhere (I am also pretty novice at using Github)
############################################################################################################################################

library(readxl)
library(forecast)
fake_baseline_data <- read_excel("Use fake_baseline_data.xlsx Here", sheet = "Sheet2", col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
head(fake_baseline_data)
plot(`Floor Enter`, type = "l")

attach(fake_baseline_data)
floor_enter <- c(fake_baseline_data$'Floor Enter')
data.ts<-ts(floor_enter, frequency = 144)
tsclean(data.ts) #not needed but hey wanted to see what it did
#first model
mod1<-Arima(data.ts,order=c(1, 0, 0),
            seasonal=list(order=c(2, 1, 0), period=12))
print(mod1)
mod2<-auto.arima(data.ts, seasonal = "TRUE")
print(mod2)
plot(fitted(mod1), mod2$residuals)
plot(fitted(mod1), mod2$residuals, type="l")
plot(mod2$x, col='red')
lines(fitted(mod2), col='blue')
mod3<-auto.arima(data.ts,D=1)
plot(mod3$residuals)
print(mod3)
#mod4<-auto.arima(data.ts, d =5)

 #alright so i have a few models, using predict here because that is what the first example I came across used
 
#predict1(mod1, n.ahead = 288)
mod1.predict <- predict(mod1, n.ahead = 288)
plot(data.ts,xlim = c(0,8))
lines(mod1.predict$pred, col="blue")
#lines(mod1.predict$pred - 1.96*mod1.predict$se, col + "red")
#lines(mod1.predict$pred + 1.96*mod1.predict$se, col = "red")

#predict2(mod2, n.ahead = 288)
mod2.predict <- predict(mod2, n.ahead = 288)
plot(data.ts,xlim = c(0, 8))
lines(mod2.predict$pred, col="blue")
#lines(mod2.predict$pred - 1.96*mod1.predict$se, col + "red")
#lines(mod2.predict$pred + 1.96*mod1.predict$se, col = "red")

#So it looks the like ARMIA model were i force seasonality is the only one that gives me usable answers

#predict3(mod3, n.ahead = 288)
mod3.predict <- predict(mod3, n.ahead = 288)
plot(data.ts,xlim = c(0, 8))
lines(mod3.predict$pred, col="blue")
#lines(mod3.predict$pred - 1.96*mod1.predict$se, col + "red")
#lines(mod3.predict$pred + 1.96*mod1.predict$se, col = "red")



###############################################################Some More Good Stuff##########################
#Okay so now I have 1500 obervations from a day that you represent 1500 uses of a single elevator in a day
#So the basic data does not have a bunch of stuff, so I have to add some new variables
#################################################################################################################

library(readr)
library(dplyr)
new_data2 <- read_csv("Use new_data.csv", col_types = cols(date = col_date(format = "%Y/%m/%d"), time = col_time(format = "%H:%M")))

z <- c(runif(1500,1,10))
a <- round(z)
enter3 <- new_data2 %>% mutate(enter = if_else(arrive == 1, 2, a))
enter4 <- enter3 %>% mutate(exit = if_else(arrive ==0, 2, a))
ts(enter4)
enter5 <- enter4 %>% mutate(current_loc = lag(exit, n = 1L))
enter6 <- enter5 %>% mutate(current_loc_diff = enter - current_loc)
enter7 <- enter6 %>% mutate(current_loc_time_sec = (3 + 1*current_loc_diff))
enter8 <- enter7 %>% mutate(ran_loc = round(runif(1500,1,10)))
enter8 <- enter8 %>% mutate(ran_loc_diff = lag(exit, n = 1L))
enter9 <- enter8 %>% mutate(ran_loc_time_sec = (3 + 1*ran_loc_diff))
attach(enter9)
#looks like the random floor does worse than the first lag
sum(current_loc_time_sec, na.rm = TRUE)
sum(ran_loc_time_sec, na.rm = TRUE)

###########################################################Now I have to be able to collapse it##################

round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
enter10 <- enter9 %>% mutate(round_time = round_any(time_fraction, (1/144)))
enter11 <- aggregate(enter10, by = list(round_time), FUN = "mean")
enter12 <- enter11 %>% mutate(rounded_enter = round_any(enter, 1))

attach(enter12)
plot(rounded_enter, type = "l")

####Alright, had some issues with my join, could not get my vars lined up

library(sqldf)
time_slot_beta <- c(1:144)
time_slot <- (time_slot_beta/144) 
base_enter <- data.frame(c("enter_beta" <- 2), c(time_slot))
base_enter <- base_enter %>% mutate(id2 <- c(1:144))
names(base_enter)[3] <- "ind2"

#the syntax was a little tricky here so I just did it in a query
entersql <- sqldf('Select enter, round_time, ind2, "c.time_slot."
                   from base_enter
                   left join enter12  on round_time = "c.time_slot."')

####Okay So I need to generate some new varibles for times when nobody was there 

enter13 <- enter12 %>% mutate(round_time_144 <- c(round_time * 144))
names(enter13)[19] <- "round_time_144"
base_enter5 <- base_enter %>% mutate(round_time_base_144 <- c(c.time_slot. * 144))
names(base_enter5)[5] <- "round_time_base_144"
enter13 <- enter13 %>% mutate(final_round_time_144 <- round_any(round_time_144, 1))
names(enter13)[20] <- "final_round_time_144"

entersql2 <- sqldf('Select enter, round_time, ind2, round_time_144, round_time_base_144
                  from base_enter5
                  left join enter13  on final_round_time_144 = round_time_base_144')
                  
#that was messy but I finally got my join
#######alright changing my data (NA to 2)###########################################################

entersql2$enter <- ifelse(is.na(entersql2$enter), 2, entersql2$enter)
plot(entersql2$enter, type = "l")
#oh shit that was not the rounded_enter -> ah okay
entersql2$rounded_enter <- ifelse(is.na(entersql2$rounded_enter), 2, entersql2$rounded_enter)
plot(entersql2$rounded_enter, type = "l")

#that went well
######################################################################################################
####So now i have to my data, now I just need to run the forcast on my new data (Currently working on this I will post it when it works)
######################################################################################################
rounded_enter_final <- c(entersql2$rounded_enter)

entersql2.ts<-ts(rounded_enter_final, freq=143)
plot(entersql2.ts)
#so I had some problems with the arima when I had 144 variables and a freq of 144, so I dropped the freq to 143.
#did not have a problem when i was using mutiple days however.
mod13<-auto.arima(entersql2.ts, D=1)
#using forcast here becuase it is easier (need to look to see if it is any difference than predict)
forcast_entersql2.ts <- forecast(mod13, h=144)
plot(forcast_entersql2.ts)

#that took a while but I finally got my results



######################################################################################################
####Alright now I need to un-collapse everything and see if my results are better --> this is started from the "pretty R script" so will
####not run all at once nicely anymore, but that is what the final script is for anyway
######################################################################################################

forcast_entersql2_mean <- data.frame(forcast_entersql2.ts)
print(forcast_entersql2_mean$id)
b = 1:144
forcast_entersql2_mean <- forcast_entersql2_mean %>% mutate(merge1 = (b/144))
names(forcast_entersql2_mean) [1]<-"mean_forecast"


better_data_mean_test <- sqldf('select round_time, enter, exit, mean_forecast
                                from better_data
                                left join forcast_entersql2_mean on merge1 = round_time')
print(better_data_mean_test)

###shit, okay so kind of the same problem as before, my merges are not adding up 
###already have round_time_144 
forcast_entersql2_mean <- forcast_entersql2_mean %>% mutate(round_time_forecast_144 <- c(merge1 * 144))
names(forcast_entersql2_mean) [7]<-"round_time_forecast_144"
forcast_entersql2_mean <- forcast_entersql2_mean %>% mutate(round_time_forecast_1442 <- round_any(forcast_entersql2_mean$round_time_forecast_144, 1))
names(forcast_entersql2_mean) [8]<-"round_time_forecast_1442"
better_data <- better_data %>% mutate(round_time_join_144 <- c(round_time * 144))
names(better_data) [17] <- "round_time_join_144"
better_data <- better_data %>% mutate(round_time_join_1442 <- round_any(round_time_join_144,1))
names(better_data) [18] <- "round_time_join_1442"  

better_data_mean_test <- sqldf('select round_time, enter, exit, mean_forecast
                                from better_data
                                left join forcast_entersql2_mean on round_time_forecast_1442 = round_time_join_1442')
attach(better_data_mean_test)
better_data_mean_test <- better_data_mean_test %>% mutate(mean_diff = (better_data_mean_test$enter - better_data_mean_test$mean_forecast))
better_data_mean_test <- better_data_mean_test %>% mutate(mean_diff = abs(mean_diff))
sum(mean_diff, na.rm = TRUE)
#welp shit I got 2486
