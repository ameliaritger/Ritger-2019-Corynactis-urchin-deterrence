## Data analysis for Corynactis-Urchin deterrence experiment - video trials ##
## Code by Amelia Ritger at UCSB on September 10, 2019 ##

library(readxl)
library(tidyverse)
library(ggplot2)
library(GGally)

# LOAD DATA
data <- read.csv("data/raw.csv")

# TIDY DATA
# my own OCD, fix "Trial" issue
colnames(data)[colnames(data)=="ï..Trial"] <- "Trial"
# remove trials without videos
data <- subset(data, !Tank>6 & !Trial==46)
# remove instances where urchin never came in contact with tile
data <- subset(data, !data$Urchin_deter_success=="Urchin never moved")
# To compare across treatments, presence/absence of Corynactis
data$corynactis_binary <- ifelse(data$Treatment_numb>1, "present", "absent")
# Standardize "instances of action" across trials because different video lengths
data <- data %>%
  mutate(contact_per_min=(Numb_contact/Total_video)) %>%
  mutate(deter_per_min=(Numb_deter/Total_video)) %>%
  mutate(cross_per_min=(Numb_cross/Total_video)) 

## VISUALIZE DATA
# assign variables
Y <- log(data$Time_to_kelp)
Y <- log(data$Time_cross_first)
Y <- data$Percent_alone
Y <- data$Percent_kelp
Y <- log(data$contact_per_min)
Y <- data$deter_per_min
Y <- data$cross_per_min
outcome <- factor(as.factor(data$Urchin_deter_success))
treat <- as.factor(data$Treatment)
cory <- as.factor(data$corynactis_binary)

Yname <- as.character("# cross per min")

#Univariate analysis for Y
hist(Y, main="", xlab=Yname)
boxplot(Y, xlab=Yname)
qqnorm(Y)
qqline(Y)

#look for correlations among Ys
ggpairs(data[, c("Time_to_kelp", "Time_cross_first", "Percent_alone", "Percent_kelp", "contact_per_min", "deter_per_min", "cross_per_min")])



####################################################
# Chi-Square test - How does treatment type affect outcome? 
####################################################
# treatment type doesn't affect outcome - but in general, urchins always crossed tile when Cory weren't present (p = 0.0393)
library(corrplot)

t1 <- table(cory, outcome)
cs <- chisq.test(cory, outcome) #p = 0.0393 - Urchins always crossed tile when it didn't have Corynactis significantly more than expected 

t1 <-table(treat, outcome)
rowSums(t1) #number of times each treatment tile was used
colSums(t1) #number of times each outcome occurred
prop.table(t1)*100 #probability distribution table
cs <- chisq.test(treat,outcome)
cs$p.value #p value = 0.2054, outcome and treatment are independent
cs$observed #observed values
round(cs$expected,2) #expected values
round(cs$residuals, 3) #pearsons residuals
corrplot(cs$residuals, is.cor = FALSE) #visualize pearsons residuals; (blue color means positively associated with variables, red color means negatively associated with variables); controls driving a lot of this trend, pink and red also important
contrib <- 100*cs$residuals^2/cs$statistic #contribution of residuals in %
round(contrib, 3) #visualize % contribution of pearsons residuals for each variable





plot(data$`Times deterred from tile`~as.factor(data$Treatment))
data$`Times deterred from tile`
class(data$`Times deterred from tile`)


plot(data2$`Urchin Size (mm)`~data2$`Total video time (min)`)
plot(data$`Urchin Size (mm)`~data$`Total video time (min)`)


plot(data$area_consumed_cm2~Y4, xlab=Yname)
