## Data analysis for Corynactis-Urchin deterrence experiment - video trials ##
## Code by Amelia Ritger at UCSB on September 10, 2019 ##

library(tidyverse)
library(ggplot2)
library(GGally)
library(lmerTest)

# LOAD DATA
video <- read.csv("data/raw.csv")

# TIDY DATA
# my own OCD, fix "Trial" issue
colnames(video)[colnames(video)=="ï..Trial"] <- "Trial"
# remove trials without videos
video <- subset(video, !Tank>6 & !Trial==46)
# remove instances where urchin never came in contact with tile during video (regardless of kelp consumption after video ended)
#video <- subset(video, !video$Urchin_deter=="urchin never tried")
# remove instances where urchin never came in contact with tile during video (but keep instances where kelp consumption occurred after video ended because otherwise n=3 for control treatment)
video <- subset(video, !video$Urchin_deter_success=="Urchin never moved")
# To compare across treatments, presence/absence of Corynactis
video$corynactis_binary <- ifelse(video$Treatment_numb>1, "present", "absent")
# Create binary variable: urchin crossed (yes/no)
video$cross_binary <- ifelse(video$Numb_cross>0, 1, 0)
# Create variables: kelp area consumed, percent kelp consumed
video <- video %>%
  mutate(area_consumed=(Kelp_b-Kelp_a)) %>%
  mutate(percent_consumed=(area_consumed/Kelp_b))
# Now filter out values where urchins didn't actually consume kelp
video$area_corrected <- ifelse(video$Visible_consumption=="no", 0, video$area_consumed)
video$percent_corrected <- ifelse(video$Visible_consumption=="no", 0, video$percent_consumed)
# To compare consumption (yes, no) across treatments
video$consumption_binary <- ifelse(video$percent_corrected>0, 1, 0)
# Average urchin body weight before and after trials
video <- video %>%
  mutate(urchin_avg_g = rowMeans(cbind(Urchin_wt_b,Urchin_wt_a)))
# Standardize "instances of action" across trials because different video lengths
video <- video %>%
  mutate(contact_per_hr=((Numb_contact/Total_video)*60)) %>%
  mutate(deter_per_hr=((Numb_deter/Total_video)*60)) %>%
  mutate(cross_per_hr=((Numb_cross/Total_video)*60)) %>%
  #mutate(deter_per_hr = replace(deter_per_hr, deter_per_hr == 0, 0.000001)) %>%
  mutate(percent_deter=(deter_per_hr/contact_per_hr)) %>%
  mutate(percent_cross=(cross_per_hr/contact_per_hr))
# Clean up data frame, only keep variables you want to work with
video <- video %>%
  select(Julian.date, Kelp, urchin_avg_g, Treatment, corynactis_binary, consumption_binary, area_corrected, percent_corrected, Time_cross_first, Percent_alone, Percent_kelp, When_consumption, Urchin_deter, Urchin_deter_success, cross_binary, contact_per_hr, deter_per_hr, cross_per_hr, percent_deter, percent_cross)

## VISUALIZE DATA
#look for correlations among variables
ggpairs(video[, c("Julian.date", "Kelp", "urchin_avg_g", "consumption_binary", "area_corrected", "percent_corrected", "Time_cross_first", "Percent_alone", "Percent_kelp", "When_consumption", "Urchin_deter", "Urchin_deter_success", "cross_binary", "contact_per_hr", "deter_per_hr", "cross_per_hr", "percent_deter", "percent_cross")])
#look at Treatment with everything
#look at corynactis_binary with everything
#come back to this when making final conclusions, because lots of the Ys are highly correlated with eachother (obviously) so you want to avoid being  repetitive by commenting on different relationships with correlated variables
# % time with kelp is - corr with % time with alone (-.98) and both are corr with # deterrence (0.58, -0.57)
# number of contacts with tile is + corr with number deterrence events (0.81) and number crosses (0.38)
#interesting things to look at separately - # deterrence, # crosses, time to cross tile

####################################################
# Chi-Square test - How does presence of Corynactis affect outcome? 
####################################################
library(corrplot)
X <- as.factor(video$corynactis_binary)
outcome <- factor(as.factor(video$Urchin_deter_success))

t1 <- table(X, outcome)
cs <- chisq.test(X, outcome) #p = 0.0393, yes/no Corynactis and outcome are dependent
# Urchins always crossed tile when it didn't have Corynactis significantly more than expected

####################################################
# Chi-Square test - How does treatment type affect outcome?
####################################################
X <- as.factor(video$Treatment)

t1 <-table(X, outcome)
rowSums(t1) #number of times each treatment tile was used
colSums(t1) #number of times each outcome occurred
prop.table(t1)*100 #probability distribution table
cs <- chisq.test(X,outcome)
cs$p.value #p = 0.2054, outcome and treatment are independent
cs$observed #observed values
round(cs$expected,2) #expected values
round(cs$residuals, 3) #pearsons residuals
corrplot(cs$residuals, is.cor = FALSE) #visualize pearsons residuals; (blue color means positively associated with variables, red color means negatively associated with variables); controls driving a lot of this trend, pink and red also important
contrib <- 100*cs$residuals^2/cs$statistic #contribution of residuals in %
round(contrib, 3) #visualize % contribution of pearsons residuals for each variable
# treatment type doesn't affect outcome

####################################################
#now let's plot these possibly correlated variables
#urchin_deter/percent_corrected and kelp
#urchin_avg and percent_alone/percent_kelp/date/Treatment
deter <- video$Urchin_deter
kelp_percent <- video$percent_corrected
kelp_loc <- as.factor(video$Kelp)
urchin_avg <- video$urchin_avg_g
percent_alone <- video$Percent_alone
percent_kelp <- video$Percent_kelp
date <- video$Julian.date
treat <- video$Treatment


# assign variables
Y <- log10(video$Time_to_kelp)
Y <- log10(video$Time_cross_first)
Y <- log10((video$contact_per_hr))
Y <- (video$deter_per_hr)
Y <- (video$cross_per_hr)
Y <- video$percent_deter
Y <- video$percent_cross
Y <- video$cross_binary

#Univariate analysis for Y
hist(Y, main="")
boxplot(Y)
qqnorm(Y)
qqline(Y)

summary(glm(Y~treat, family=binomial))

b <- Y~treat
a<-plot(b, ylab=Yname, xlab="Treatment")
ggsave("figures/treatment_percentkelp.pvideo",a,width=5,height=5)
summary(lm(b))
TukeyHSD(aov(lm(b))) 
anova(lm(b))
