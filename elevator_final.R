#Here is my project on elevators

library(readxl)
library(forecast)
library(dplyr)
library(sqldf)

#five days of random data to prove the concept
fake_baseline_data <- read_excel("Use fake_baseline_data.xlsx Here", sheet = "Sheet2", col_types = c("date", "numeric", "numeric", "numeric", "numeric"))
#1500 observations over the corse of a "day"
better_data <- read_csv("Use new_data.csv", col_types = cols(date = col_date(format = "%Y/%m/%d"), time = col_time(format = "%H:%M")))

attach(fake_baseline_data)

head(fake_baseline_data)
plot(`Floor Enter`, type = "l")

#turning it into a time series 
floor_enter <- c(fake_baseline_data$'Floor Enter')
floor_enter.ts<-ts(floor_enter, frequency = 144)
#tsclean(data.ts)

#I am not testing for stationartiy or trend because I do not beleive that it applies in this situation

#first "thrid" model, forces seasonality
mod3<-auto.arima(data.ts,D=1)
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

#looks like the random floor does worse than the first lag
sum(current_loc_time_sec, na.rm = TRUE)
sum(ran_loc_time_sec, na.rm = TRUE)

###########################################################Now I have to be able to collapse it##################

round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
better_data <- better_data %>% mutate(round_time = round_any(time_fraction, (1/144)))
better_data <- better_data %>% aggregate(by = list(round_time), FUN = "mean")
better_data <- better_data %>% mutate(rounded_enter = round_any(enter, 1))

attach(better_data)
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
                   left join better_data  on round_time = "c.time_slot."')

####Okay So I need to generate some new varibles for times when nobody was there 

better_data <- better_data %>% mutate(round_time_144 <- c(round_time * 144))
names(better_data)[19] <- "round_time_144"
data_join <- data_join %>% mutate(round_time_base_144 <- c(c.time_slot. * 144))
names(data_join)[5] <- "round_time_base_144"
better_data <- better_data %>% mutate(final_round_time_144 <- round_any(round_time_144, 1))
names(better_data)[20] <- "final_round_time_144"

entersql2 <- sqldf('Select enter, round_time, ind2, round_time_144, round_time_base_144
                  from data_join
                  left join better_data  on final_round_time_144 = round_time_base_144')

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
