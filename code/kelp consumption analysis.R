## Data analysis for Corynactis-Urchin deterrence experiment ##
## Code by Amelia Ritger at UCSB on August 26, 2019 ##

#load data
library(readxl)
data <- read_excel("data/raw.xlsx", sheet = "Kelp consumption")

# assign the predictor variables to X
treat <- as.factor(data$Treatment)
size <- data$`Urchin Size (mm)`
tile <- as.factor(data$`Cory Tile Number`)
date <- as.factor(data$`Julian date`)
temp <- data$`Water Temperature`
loc <- data$`Kelp Location`
starv <- data$`Urchin Starvation Time (days)`

# assign the response variable to Y
Y <- data$`Kelp Consumed (cm^2)`
Yname <- as.character("Kelp Consumed")

library(tidyverse)
library(zoo)

# take averages (kelp consumption) of each tile
data <- data %>%
  group_by(`Cory Tile Number`) %>%                                     # group tile numbers together
  mutate(avg_tile=mean(`Kelp Consumed (cm^2)`, na.rm=TRUE))  %>%       # take mean of each tile group
  ungroup()                                                            # ungroup data REALLY IMPT when using group_by

#Untransformed univariate analyses for Y 
hist(Y, main="", breaks=XXX, xlab=Yname)
boxplot(Y, xlab=Yname)
qqnorm(Y)

# Visualize Y vs. Xs
plot(XXX, Y, main='', ylab=Yname)
plot(XXX, Y, main='', ylab=Yname)

####################################################
# General two-way ANOVA: Mixed Model
####################################################

library(lmerTest)
m1 <- lmer(Y ~ 1 + XXX + (1|XXX) + (1|XXX:XXX))

summary(m1)
anova(m1)
anova(m1, ddf="lme4")
st <- step(m1)
plot(m1)