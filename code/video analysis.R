## Data analysis for Corynactis-Urchin deterrence experiment - video trials ##
## Code by Amelia Ritger at UCSB on September 10, 2019 ##

library(tidyverse)
library(ggplot2)
library(GGally)
library(janitor)

# LOAD DATA
data <- read.csv("data/raw.csv")

# TIDY DATA
# my own OCD, fix "Trial" issue
colnames(data)[colnames(data)=="ï..Trial.number"] <- "Trial"
# Clean names for R
data <- data %>%
  clean_names()
# remove trials without videos
data <- subset(data, !tank_number>6 & !trial==46)
# remove instances where urchin never came in contact with tile during video (regardless of kelp consumption after video ended)
#data <- subset(data, !urchin_sucessfully_deterred_during_video="Uchin never moved")
# remove instances where urchin never came in contact with tile during video (yes, n=3 for control treatment)
data <- subset(data, !urchin_deterred_during_video=="urchin never tried")
# To compare across treatments, presence/absence of Corynactis
data$corynactis_binary <- ifelse(data$treatment_number>1, "present", "absent")
# Create binary variable: urchin crossed (yes/no)
data$cross_binary <- ifelse(data$times_crossing_cory>0, 1, 0)
# Create variables: kelp area consumed, percent kelp consumed
data <- data %>%
  mutate(area_consumed=(kelp_before_cm_2-kelp_after_cm_2)) %>%
  mutate(percent_consumed=(area_consumed/kelp_before_cm_2))
# Now filter out values where urchins didn't actually consume kelp
data$area_corrected <- ifelse(data$kelp_visibly_consumed=="no", 0, data$area_consumed)
data$percent_corrected <- ifelse(data$kelp_visibly_consumed=="no", 0, data$percent_consumed)
# To compare consumption (yes, no) across treatments
data$consumption_binary <- ifelse(data$percent_corrected>0, 1, 0)
# Create variable for binary deterrence (was urchin ever deterred)
data$deter_binary <- ifelse(data$times_deterred_from_cory>0, 1, 0)
# Average urchin body weight before and after trials
data <- data %>%
  mutate(urchin_avg_g = rowMeans(cbind(urchin_weight_before_g,urchin_weight_after_g)))
# Standardize "instances of action" across trials because different video lengths
data <- data %>%
  mutate(contact_per_hr=((times_in_contact_with_cory/total_video_time_min)*60)) %>%
  mutate(deter_per_hr=((times_deterred_from_cory/total_video_time_min)*60)) %>%
  mutate(cross_per_hr=((times_crossing_cory/total_video_time_min)*60)) %>%
  #mutate(deter_per_hr = replace(deter_per_hr, deter_per_hr == 0, 0.000001)) %>%
  mutate(percent_deter=(deter_per_hr/contact_per_hr)) %>%
  mutate(percent_cross=(cross_per_hr/contact_per_hr))
# Clean up data frame, only keep variables you want to work with
data <- data %>%
  dplyr::select(julian_date, kelp_location, urchin_size_mm, urchin_avg_g, treatment, corynactis_binary, consumption_binary, area_corrected, percent_corrected, time_to_cross_cory_first_min, percent_time_alone, percent_time_with_kelp, when_was_kelp_consumed, deter_binary, urchin_deterred_during_video, urchin_sucessfully_deterred_during_video, contact_per_hr, deter_per_hr, cross_per_hr, percent_deter, percent_cross)

## VISUALIZE DATA
#look for correlations among variables
ggpairs(data[, c("julian_date", "kelp_location", "urchin_size_mm", "urchin_avg_g", "treatment", "corynactis_binary", "consumption_binary", "area_corrected", "percent_corrected", "time_to_cross_cory_first_min", "percent_time_alone", "percent_time_with_kelp", "when_was_kelp_consumed", "urchin_deterred_during_video", "urchin_sucessfully_deterred_during_video", "contact_per_hr", "deter_per_hr", "cross_per_hr", "percent_deter", "percent_cross")])
#look at treatment with everything
#look at corynactis_binary with everything
#come back to this when making final conclusions, because lots of the Ys are highly correlated with eachother (obviously) so you want to avoid being  repetitive by commenting on different relationships with correlated variables
# % time with kelp is - corr with % time with alone (-.98) and both are corr with # deterrence (0.58, -0.57)
# number of contacts with tile is + corr with number deterrence events (0.81) and number crosses (0.38)
#interesting things to look at separately - # deterrence, # crosses, time to cross tile

####################################################
# How does presence of Corynactis affect "outcome" (see data frame)?
####################################################
X <- as.factor(data$corynactis_binary)
outcome <- factor(as.factor(data$urchin_sucessfully_deterred_during_video))

t1 <- table(outcome, X) #because data is binary and sample size is small (controls n = 3), use N-1 Two Proportion Test (below)
prop.test(t1, correct=TRUE) # correct=TRUE tells R to correct for small sample size p = 0.0393
fisher.test(t1) #fisher's exact test is useful when sample sizes are particularly small... p=0.0505 when n=5, p=0.255 when n=3
# Urchins always crossed tile when it didn't have Corynactis...but not significant because n=3

####################################################
# How does treatment type affect "outcome" (see data frame)?
####################################################
library(corrplot)
X <- as.factor(data$treatment)

t1 <-table(X, outcome)
t1 = t1[-1,] #because "Control" is driving whole relationship
cs<-chisq.test(t1)
cs$p.value #p = 0.2054, outcome and treatment are independent; when n=3, p=0.197
corrplot(cs$residuals, is.cor = FALSE) #visualize pearsons residuals; (blue color means positively associated with variables, red color means negatively associated with variables); controls driving a lot of this trend, pink and red also important
# treatment type doesn't affect outcome

####################################################
# How does presence of Corynactis affect whether or not urchin was deterred (regardless of ultimate outcome)? 
####################################################
X <- as.factor(data$corynactis_binary)
deter <- as.factor(data$deter_binary)

t1 <- table(X, deter)
prop.test(t1, correct=TRUE) #two proportion test p = 0.01524; when n=3, p=0.0782
fisher.test(t1) # fisher's test p = 0.0067; when n=3, p=0.041
#urchin was never detered from tile when Corynactis was absent, urchins were likely to be deterred when Corynactis present

####################################################
# How does type of Corynactis affect whether or not urchin was deterred (regardless of ultimate outcome)?
####################################################
X <- as.factor(data$treatment)

t1 <- table(X, deter)
t1 = t1[-1,] # because "Control" is driving the whole relationship
prop.test(t1, correct=TRUE) #p=0.8174
#treatment type definitely doesn't affect whether or not an urchin was deterred at least 1x

####################################################
#Time with kelp: correlation with amount of kelp consumed? (and is there an urchin size effect? -> follow up in kelp consumption analysis code)
####################################################
time <- data$percent_time_with_kelp
area <- data$area_corrected
size <- data$urchin_avg_g
size_mm <- data$urchin_size_mm
hist(time)
hist(area)
hist(size)
hist(size_mm)

plot(area~size_mm)
plot(time~size_mm)
plot(time~area)

m1<- lm(area~time+size+(time:size))
summary(m1) #n=3, p=0.00369
plot(area~size)
abline(lm(area~size))
summary(lm(area~size)) # p=0.74 >> follow up on this, especially fixed effects model

####################################################
#Presence of Corynactis: effect on time to cross tile (urchin size effect?)
####################################################
cross <- data %>%
  dplyr::select(treatment, corynactis_binary, time_to_cross_cory_first_min, urchin_avg_g, urchin_size_mm) %>%
  filter(!is.na(time_to_cross_cory_first_min)) %>%
  filter(time_to_cross_cory_first_min<100) #remove this one point, it's a huge outlier
Y <- log10(cross$time_to_cross_cory_first_min) #to meet assumptions of normality
X <- as.factor(cross$corynactis_binary)
hist(Y)
library(car)
qqPlot(Y) #good enough

## importantly... is there an effect of urchin size?
plot(Y~cross$Urchin_size)
plot(Y~cross$urchin_avg_g)
summary(lm(Y~cross$urchin_avg_g)) #p=0.8554, definitely no effect of urchin size on time it takes them to cross the tile

plot(Y~X)
t.test(Y~X) #p=0.039, When Corynactis present, time to cross tile was significantly longer

####################################################
#Type of Corynactis: effect on time to cross tile
####################################################
X <-  as.factor(cross$treatment)
plot(Y~X)
m1 <- lm(Y~X)
a1 <- aov(Y~X)
summary(a1) #p=0.0034
TukeyHSD(a1) #all the treatments are significantly different from control, but there is no signficant difference amongst colors

####################################################
# Presence of Corynactis: effect on % of contacts that resulted in deterrence ("percent_deter")? (urchin size effect?)
####################################################
library(car)
data$deter_corrected <- ifelse(data$percent_deter=="NaN", 0, data$percent_deter) #two control instances where urchin never moved are zero deterrence...
deter <- data %>%
  dplyr::select(treatment, corynactis_binary, deter_corrected, urchin_avg_g, urchin_size_mm)
Y <- deter$deter_corrected
X <- as.factor(deter$corynactis_binary)
hist(Y)
qqPlot(Y)
## >> there are lots of zeros and ones, need to correct for that or account for that in the analysis

library(gamlss)
b1 <- gamlss(Y~X,  family = BEINF, trace = F) #BEINF because zero and one values are present in beta distribution
summary(b1)

means_b1 <- lpred(b1, type='response', what='mu', se.fit=T)

df_fit <- data.frame(CORY = deter$corynactis_binary, M = means_b1$fit, SE = means_b1$se.fit)

ggplot(df_fit, aes(CORY, M)) + geom_pointrange(aes(ymin=M-SE, ymax=M+SE)) + 
  labs(x="Corynactis presence/absence",y="% contacts that resulted in deterrence")# +
  scale_y_continuous(labels=scales::percent)
## something doesn't look right, take a look at this later.

####################################################
# Type of Corynactis: effect on % of contacts that resulted in deterrence ("percent_deter")? (urchin size effect?)
####################################################





  
  
  
####################################################
###Analysis I still want to do
####################################################
#let's plot these possibly correlated variables
#urchin_deter/percent_corrected and kelp location
#urchin_avg and percent_alone/percent_kelp/date/treatment
deter <- data$Urchin_deter
kelp_percent <- data$percent_corrected
kelp_loc <- as.factor(data$Kelp)
urchin_avg <- data$urchin_avg_g
percent_alone <- data$Percent_alone
percent_kelp <- data$Percent_kelp
date <- data$Julian.date
treat <- data$treatment

par(mfrow=c(1,5))
for(i in 7:11) {
  boxplot(data[,i],main=names(data)[i])
}
for(i in 15:19) {
  boxplot(data[,i],main=names(data)[i])
}

# assign variables
Y <- log10(data$Time_to_kelp)
Y <- log10(data$Time_cross_first)
Y <- log10((data$contact_per_hr))
Y <- (data$deter_per_hr)
Y <- (data$cross_per_hr)
Y <- data$percent_deter
Y <- data$percent_cross
Y <- data$cross_binary

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
