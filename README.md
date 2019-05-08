# Elevators!!!

Okay so I hate the elevators in our building (they are old slow and terrible) but I want to make them more better.
I cannot do a lot about most of it. But what if I could forecast the floor that an elevator should be on? So in this project I am going to try and take some (fake made up by me) elevator data and try to forecast what floor it should stop on based on the time of day.

######################################################################################### 
And this is an excuse to learn some R (because it will be obvious soon that this is my first project in R).

My end goal is to be able to take one days worth of data (when people get to the elevator, what floor they get on) collapse that (by mean) to 144 10 min "sections" of time, round those awnsers so I have this nice data set. Then run a seasonal ARIMA model to predcit where the elevator should be on the next day.

I will try to be as step by step in my acutal script as possible, but pretty much my steps are.
  1) Proff of conpect with some super random data
  2) Important some new data and adding some columns of varibles (some cleaning)
  3) Collapsing my data into 10 min averages and further cleaning
  4) Running an ARIMA model on this new data (current)
  5) More stuff
