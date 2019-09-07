## Data analysis for Corynactis-Urchin deterrence experiment ##
## Code by Amelia Ritger at UCSB on September 6, 2019 ##

library(tidyverse)
library(zoo)
library(readxl)

## LOAD DATA
data <- read_excel("data/raw.xlsx", sheet = "Kelp consumption")

## TIDY DATA
# because I'm not done with trials yet, remove lines for trials not yet run
data <- data %>%
  filter(`Urchin Size (mm)`>0)

# To compare across treatments, presence/absence of Corynactis
data$present_absent <- ifelse(data$`Treatment Number`>1, "present", "absent")

# look at "minimum % kelp consumed cutoff" contradictions between ImageJ and visual analysis
datan <- subset(data, `Kelp visibly consumed?`=="no")
datay <- subset(data, `Kelp visibly consumed?`=="yes")
sort(datan$`Percent of Kelp Consumed`) #If any percent consumption is less than 0.04, we assume there was no consumption (aka 'remove' ImageJ analysis error)...minus the one piece of kelp that definitely had a tiny bit of urchin chewing it
#data$percent_corrected <- ifelse(data$`Percent of Kelp Consumed`>0.04, data$`Percent of Kelp Consumed`, 0)
data$percent_corrected <- ifelse(data$`Kelp visibly consumed?`=="no", 0, data$`Percent of Kelp Consumed`)

# To compare consumption (yes, no) across treatments
data$consumption <- ifelse(data$percent_corrected>0, "1", "0")
# Average urchin body weight before and after trials
data <- data %>%
  mutate(urchin_avg_g = rowMeans(cbind(`Urchin weight before (g)`,`Urchin weight after (g)`)))
# To see if balcony versus wet lab location had an effect
data$experiment_location <- ifelse(data$`Tank number`>6, "Balcony", "Lab")
# To keep all control values rather than average them, assign each control trial a unique number
data$`Cory Tile Number`[data$`Cory Tile Number`==0] <- c(-1:-15)

# take averages (kelp consumption) of each tile, create new data frame from subsetted averages
data_avg <- data %>%
  group_by(`Cory Tile Number`) %>%                                     # group tile numbers together
  mutate(avg_area=mean(`Kelp Consumed (cm^2)`, na.rm=TRUE))  %>%       # take mean kelp consumption of each tile group
  mutate(avg_percent=mean(`Percent of Kelp Consumed`, na.rm=TRUE)) %>% # take mean % kelp consumption of each tile group
  ungroup() %>%                                                        # ungroup data REALLY IMPT when using group_by
  distinct(`Cory Tile Number`, .keep_all = TRUE)                       # remove duplicate tiles

## ORGANIZE CODE
# assign the response variable to Y
Y <- as.numeric(data$percent_corrected) #binomial distribution?
Yname <- as.character("% Kelp Consumed")
lY <- log(as.numeric(data$percent_corrected))
lYname <- as.character("log(% Kelp Consumed)")

# assign the predictor variables to X
treat <- as.factor(data$Treatment)
size <- data$urchin_avg_g
tile <- as.factor(data$`Cory Tile Number`)
date <- as.factor(data$`Julian date`)
temp <- data$`Water Temperature (°C)`
kloc <- as.factor(data$`Kelp Location`)
tloc <- as.factor(data$experiment_location)
starv <- data$`Urchin Starvation Time (days)`
used <- as.factor(data$`Urchin "type"`)

## ANALYSIS 
#Univariate analyses for Y 
hist(Y, main="", xlab=Yname)
boxplot(Y, xlab=Yname)
qqnorm(Y)
qqline(Y)

# quick look at kelp consumption by treatment
ggplot(data,aes(x=Treatment,y=percent_corrected))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Treatment")+
  ylab("% kelp area consumed")+
  theme_bw()

#rough model to look at effects of treatment on kelp consumption
m1 <- lm(Y ~ data$Treatment)
summary(m1)

# quick look at kelp consumption with and without Corynactis
ggplot(data,aes(x=present_absent,y=percent_corrected))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Treatment")+
  ylab("% kelp area consumed")+
  theme_bw()

#rough model to look at effects of Corynactis on kelp consumption
m2 <- lm(Y~data$present_absent)
summary(m2)

## now to do things properly...
# Visualize Y vs. Xs - are there any unpredicted effects?
plot(Y~treat, ylab=Yname)
plot(Y~size, ylab=Yname)
plot(Y~tile, ylab=Yname)
plot(Y~date, ylab=Yname)
plot(Y~temp, ylab=Yname)
plot(Y~kloc, ylab=Yname)
plot(Y~tloc, ylab=Yname)
plot(Y~starv, ylab=Yname)
plot(Y~used, ylab=Yname)

# look at "minimum % kelp consumed cutoff" contradictions between ImageJ and visual analysis
datan <- subset(data, `Kelp visibly consumed?`=="no")
datay <- subset(data, `Kelp visibly consumed?`=="yes")
sort(datay$`Percent of Kelp Consumed`) #% consumption under 0.05 can be considered "not eaten" (minus 2 blades where kelp was "visibly" consumed)

####################################################
# General one-way ANOVA: Mixed Model
####################################################

library(lmerTest)
m3 <- lmer(Y ~ treat + (1|tile))

summary(m3)
anova(m3)
anova(m3, ddf="lme4")
st <- step(m3)
plot(m3)