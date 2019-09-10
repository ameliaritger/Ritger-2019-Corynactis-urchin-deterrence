## Data analysis for Corynactis-Urchin deterrence experiment - video trials ##
## Code by Amelia Ritger at UCSB on September 10, 2019 ##

library(readxl)
library(tidyverse)

# LOAD DATA
data <- read_excel("data/raw.xlsx", sheet = "Kelp consumption")

# TIDY DATA
# remove trials without videos
data <- subset(data, !`Tank number`>6 & !`Trial number`==46)
# remove instances where urchin never came in contact with tile
data <- subset(data, !data$`Urchin deterred during video?`=="urchin never tried")
# To compare across treatments, presence/absence of Corynactis
data$corynactis_binary <- ifelse(data$`Treatment Number`>1, "present", "absent")

# assign variables
Y1 <- data$`Time to reach kelp (min)`
Y2 <- data$`Times deterred from tile`
Y3 <- data$`Time to cross tile, first (min)`
Y4 <- data$`Percent time with Kelp`
deter <- data$`Urchin sucessfully deterred during video?`
treat <- as.factor(data$Treatment)
cory <- as.factor(data$corynactis_binary)

Y1name <- as.character("Time to reach kelp")
Y2name <- as.character("# times urchin was deterred")
Y3name <- as.character("Time to cross tile (first, min)")
Y4name <- as.character("% time spent with kelp")

#Univariate analysis for Y
hist(Y4, main="", xlab=Y4name)
boxplot(Y4, xlab=Y4name)
qqnorm(Y)
qqline(Y)

plot(data2$`Urchin Size (mm)`~data2$`Total video time (min)`)
plot(data$`Urchin Size (mm)`~data$`Total video time (min)`)

# Create variables: Kelp consumed, percent kelp consumed
data <- data %>%
  mutate(area_consumed_cm2=(`Kelp Before (cm^2)`-`Kelp After (cm^2)`)) %>%
  mutate(percent_consumed=(area_consumed_cm2/`Kelp Before (cm^2)`))

plot(data$area_consumed_cm2~Y4, xlab=Yname)
