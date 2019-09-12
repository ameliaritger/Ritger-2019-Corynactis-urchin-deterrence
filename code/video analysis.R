## Data analysis for Corynactis-Urchin deterrence experiment - video trials ##
## Code by Amelia Ritger at UCSB on September 10, 2019 ##

library(readxl)
library(tidyverse)
library(ggplot2)
library(GGally)
library(lmerTest)

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
data$cross_binary <- ifelse(data$Numb_cross>0, 1, 0)
# Standardize "instances of action" across trials because different video lengths
data <- data %>%
  mutate(contact_per_hr=((Numb_contact/Total_video)*60)) %>%
  mutate(deter_per_hr=((Numb_deter/Total_video)*60)) %>%
  mutate(cross_per_hr=((Numb_cross/Total_video)*60)) %>%
  mutate(deter_per_hr = replace(deter_per_hr, deter_per_hr == 0, 0.000001)) %>%
  mutate(percent_deter=(deter_per_hr/contact_per_hr)) %>%
  mutate(percent_cross=(cross_per_hr/contact_per_hr))

## VISUALIZE DATA
# assign variables
Y <- log10(data$Time_to_kelp)
Y <- log10(data$Time_cross_first)
Y <- data$Percent_alone
Y <- data$Percent_kelp
Y <- log10((data$contact_per_hr))
Y <- (data$deter_per_hr)
Y <- (data$cross_per_hr)
Y <- data$percent_deter
Y <- data$percent_cross
Y <- data$cross_binary
outcome <- factor(as.factor(data$Urchin_deter_success))
treat <- as.factor(data$Treatment)
cory <- as.factor(data$corynactis_binary)
tile <- data$Cory_numb

Yname <- as.character("%percent kelp")

#Univariate analysis for Y
hist(Y, main="", xlab=Yname)
boxplot(Y, xlab=Yname)
qqnorm(Y)
qqline(Y)

summary(glm(Y~treat, family=binomial))



#look for correlations among Ys
ggpairs(data[, c("Time_to_kelp", "Time_cross_first", "Percent_alone", "Percent_kelp", "contact_per_min", "deter_per_min", "cross_per_min")])
# % time with kelp is - corr with % time with alone (-.99) and both are corr with # deterrence (0.46, -0.46)
# time to get to kelp is + corr with time to cross cory tile
# # contacts with tile is + corr with # deterrence events (0.8) and # crosses (0.4)

#interesting things to look at separately - # deterrence, # crosses, time to cross tile

b <- Y~treat
a<-plot(b, ylab=Yname, xlab="Treatment")
ggsave("figures/treatment_percentkelp.pdf",a,width=5,height=5)
summary(lm(b))
TukeyHSD(aov(lm(b))) 
anova(lm(b))

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
