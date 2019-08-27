## Power test to examine the minimum sample size needed to find possible significance in advance of data collection ##
## Code by Amelia Ritger at UCSB on August 26, 2019 ##

library(readr)
library(tidyverse)
Data <- read_csv("code/prep/Data_Urchin.csv")

cory <- as.factor(Data$corynactis)
kelp <- as.numeric(Data$kelp_consumed_square_cm)
plot(kelp~cory)
hungry <- filter(Data, hungry_fed=="hungry")
cory <-as.factor(hungry$corynactis)
kelp <-as.numeric(hungry$kelp_consumed_square_cm)
yes <- filter(hungry, corynactis=="yes")
no <- filter(hungry, corynactis=="no")
plot(kelp~cory)
t.test(kelp~cory)
m1<- mean(yes$kelp_consumed_square_cm) #12.305
m2<- mean(no$kelp_consumed_square_cm) #23.32006
s1<- sd(yes$kelp_consumed_square_cm) #12.15613
s2<- sd(no$kelp_consumed_square_cm) #17.79539
## cohen's d
d <- (m2-m1)/sqrt(((s1^2)+(s2^2))/2)
#power test
library(pwr)
pwr.t2n.test(n2= 60, d = d , sig.level = 0.05, power = 0.75) #just comparing controls to all other trials with Corynactis combined, need a n of 17 controls to find significance