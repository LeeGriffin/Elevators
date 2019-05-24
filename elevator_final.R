#Here is my project on elevators
#Below are the packages that I used
library(readxl)
library(forecast)
library(dplyr)
library(sqldf)

#five days of random data to prove the concept
#use the file path to the "fake baseline data" in place of my file path
fake_baseline_data <- read_excel("C:/Users/lee/Desktop/fake_baseline_data.xlsx")

#1500 observations over the corse of a "day"
#same as above, yes i know one is a csv and one is in an excel sheet, your just going to have to deal with it
better_data <- read_csv("C:/Users/lee/Desktop/new_data.csv")


head(fake_baseline_data)
plot(`Floor Enter`, type = "l")

#turning it into a time series 
floor_enter <- c(fake_baseline_data$'Floor Enter')
floor_enter.ts<-ts(floor_enter, frequency = 144)
#tsclean(data.ts)

#I am not testing for stationartiy or trend because I do not beleive that it applies in this situation

#first "thrid" model, forces seasonality
mod3<-auto.arima(floor_enter.ts,D=1)
forcast_mod3 <- forecast(mod3, h=288)
plot(forcast_mod3)



###############################################################Some More Good Stuff##########################
# new data represents 1500 uses of a single elevator in a day
#Okay so I need to generate some new vars 
#################################################################################################################

z <- c(runif(1500,1,10))
a <- round(z)

better_data <- better_data %>% mutate(enter = if_else(arrive == 1, 2, a))
better_data <- better_data %>% mutate(exit = if_else(arrive ==0, 2, a))
better_data <- better_data %>% mutate(current_loc = lag(exit, n = 1L))
better_data <- better_data %>% mutate(current_loc_diff = enter - current_loc)
better_data <- better_data %>% mutate(current_loc_time_sec = (3 + 1*current_loc_diff))
better_data <- better_data %>% mutate(ran_loc = round(runif(1500,1,10)))
better_data <- better_data %>% mutate(ran_loc_diff = lag(exit, n = 1L))
better_data <- better_data %>% mutate(ran_loc_time_sec = (3 + 1*ran_loc_diff))

#looks like if the elevator was on a random floor it would be better than moving from the previous floor
#It should be noted that because there are lots of random values that these numbers will change, but I have ran this 
#several times and I tend to get that random misses by less total floors
better_data <- better_data %>% mutate(current_loc_diff = abs(current_loc_diff))
sum(better_data$current_loc_diff, na.rm = TRUE) #5264
mean(better_data$current_loc_diff, na.rm = TRUE) #3.511

better_data <- better_data %>% mutate(ran_loc_diff = abs(ran_loc_diff))
sum(better_data$ran_loc_diff, na.rm = TRUE) #5070
mean(better_data$ran_loc_diff, na.rm = TRUE) #3.382


###########################################################Now I have to be able to collapse it##################

round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
better_data <- better_data %>% mutate(round_time = round_any(time_fraction, (1/144)))
plot(better_data$round_time)
better_data_collapse <- aggregate(better_data, by = list(round_time2 = better_data$round_time), FUN = "mean")
better_data_collapse <- better_data_collapse %>% mutate(rounded_enter = round_any(enter, 1))


plot(rounded_enter, type = "l")

####Alright, had some issues with my join, could not get my vars lined up

time_slot_beta <- c(1:144)
time_slot <- (time_slot_beta/144) 
data_join <- data.frame(c("enter_beta" <- 2), c(time_slot))
data_join <- data_join %>% mutate(id2 <- c(1:144))
names(data_join)[3] <- "ind2"

#the syntax was a little tricky here so I just did it in a query
entersql <- sqldf('Select enter, round_time, ind2, "c.time_slot."
                   from data_join
                   left join better_data_collapse  on round_time = "c.time_slot."')

####Okay So I need to generate some new variables for times when nobody was there

better_data_collapse <- better_data_collapse %>% mutate(round_time_144 <- c(round_time * 144))
names(better_data_collapse)[19] <- "round_time_144"
data_join <- data_join %>% mutate(round_time_base_144 <- c(c.time_slot. * 144))
names(data_join)[4] <- "round_time_base_144"
better_data_collapse <- better_data_collapse %>% mutate(final_round_time_144 <- round_any(round_time_144, 1))
names(better_data_collapse)[20] <- "final_round_time_144"

entersql2 <- sqldf('Select enter, round_time, ind2, round_time_144, round_time_base_144, rounded_enter
                  from data_join
                  left join better_data_collapse  on final_round_time_144 = round_time_base_144')

#######alright changing my data (NA to 2)###########################################################

entersql2$enter <- ifelse(is.na(entersql2$enter), 2, entersql2$enter)
plot(entersql2$enter, type = "l")
#oh shit that was not the rounded_enter -> ah okay
entersql2$rounded_enter <- ifelse(is.na(entersql2$rounded_enter), 2, entersql2$rounded_enter)
plot(entersql2$rounded_enter, type = "l")

######################################################################################################
####So now i have to my data, need to finish this forecast
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
#I have since realized that if I wanted to do this in a real life situation I would probably need to break the entire week into 10 min intervals to account for weekends

######################################################################################################
####Alright now I need to un-collapse everything and see if my results are better
######################################################################################################

forcast_entersql2_mean <- data.frame(forcast_entersql2.ts)
b = 1:144
forcast_entersql2_mean <- forcast_entersql2_mean %>% mutate(merge1 = (b/144))
names(forcast_entersql2_mean) [1]<-"mean_forecast"

###shit, okay so kind of the same problem as before, my joins are not adding up 

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
#seeing if all of this work was for anything
better_data_mean_test <- better_data_mean_test %>% mutate(mean_diff = (better_data_mean_test$enter - better_data_mean_test$mean_forecast))
better_data_mean_test <- better_data_mean_test %>% mutate(mean_diff = abs(mean_diff))

#again because of the use of random numbers these numbers will not be exact
sum(better_data_mean_test$mean_diff, na.rm = TRUE) #2450
mean(better_data_mean_test$mean_diff, na.rm = TRUE) #1.6333 floors off on average
#whoop whoop! It seems to have worked
