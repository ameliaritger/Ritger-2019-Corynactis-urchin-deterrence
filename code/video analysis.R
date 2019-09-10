## Data analysis for Corynactis-Urchin deterrence experiment - video trials ##
## Code by Amelia Ritger at UCSB on September 10, 2019 ##

library(readxl)
library(tidyverse)

# LOAD DATA
data <- read_excel("data/raw.xlsx", sheet = "Videos")

# TIDY DATA
#remove instances where urchin never came in contact with tile
data <- subset(data, !data$`Urchin deterred during video?`=="urchin never tried")

#assign variables
Y1 <- data$`Time to reach kelp (min)`
Y2 <- data$`Times deterred from Cory`
Y3 <- data$`Time to cross Cory, first (min)`
Y4 <- data$`Percent time with Kelp`
treat <- as.factor(data$Treatment)
