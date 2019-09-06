## Data analysis for Corynactis-Urchin deterrence experiment ##
## Code by Amelia Ritger at UCSB on August 26, 2019 ##

library(tidyverse)
library(zoo)
library(readxl)

#load data
data <- read_excel("data/raw.xlsx", sheet = "Kelp consumption")

# because I'm not done with trials yet, remove lines for trials not yet run
data <- data %>%
  filter(`Urchin Size (mm)`>0)

# to keep all control values rather than average them, assign each control trial a unique number
data$`Cory Tile Number`[data$`Cory Tile Number`==0] <- c(-1:-15)

# take averages (kelp consumption) of each tile, create new data frame from subsetted averages
data_avg <- data %>%
  group_by(`Cory Tile Number`) %>%                                     # group tile numbers together
  mutate(avg_area=mean(`Kelp Consumed (cm^2)`, na.rm=TRUE))  %>%       # take mean kelp consumption of each tile group
  mutate(avg_percent=mean(`Percent of Kelp Consumed`, na.rm=TRUE)) %>% # take mean % kelp consumption of each tile group
  ungroup() %>%                                                        # ungroup data REALLY IMPT when using group_by
  distinct(`Cory Tile Number`, .keep_all = TRUE)                       # remove duplicate tiles

# assign the response variable to Y
Y <- log(as.numeric(1+data$`No zero`))
Yname <- as.character("log(% Kelp Consumed)")
#binomial distrib

#Untransformed univariate analyses for Y 
hist(Y, main="", xlab=Yname)
boxplot(Y, xlab=Yname)
qqnorm(Y)
qqline(Y) #neither area nor percent don't need transformation

# quick look at kelp consumption by treatment
ggplot(data,aes(x=Treatment,y=`No zero`))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Treatment")+
  ylab("log(% kelp area consumed)")+
  theme_bw()

ggsave("figures/second_to_last_area.pdf",b,width=5,height=5)

# assign the predictor variables to X
treat <- as.factor(data$Treatment)
size <- data$`Urchin Size (mm)`
tile <- as.factor(data$`Cory Tile Number`)
date <- as.factor(data$`Julian date`)
temp <- data$`Water Temperature (°C)`
loc <- as.factor(data$`Kelp Location`)
site <- as.factor(data$`Experiment location`)
starv <- data$`Urchin Starvation Time (days)`
urch <- as.factor(data$`Urchin "type"`)
area <- data$`Kelp Consumed (cm^2)`
pcnt <- data$`Percent of Kelp Consumed`

# Visualize Y vs. Xs - are there any unpredicted effects?
plot(Y~treat, ylab=Yname)
plot(Y~size, ylab=Yname)
plot(Y~tile, ylab=Yname)
plot(Y~date, ylab=Yname)
plot(Y~temp, ylab=Yname)
plot(Y~loc, ylab=Yname)
plot(Y~site, ylab=Yname)
plot(Y~starv, ylab=Yname)
plot(Y~urch, ylab=Yname)

# look at "minimum % kelp consumed cutoff" contradictions between ImageJ and visual analysis
datan <- subset(data, `Kelp visibly consumed?`=="no")
datay <- subset(data, `Kelp visibly consumed?`=="yes")
summary(datan$`Percent of Kelp Consumed`) #% consumption under 0.05 can be considered "not eaten" (minus 2 blades where kelp was "visibly" consumed)

####################################################
# General two-way ANOVA: Mixed Model
####################################################

library(lmerTest)
m1 <- lmer(Y ~ data$Treatment + 1|data$`Cory Tile Number`)

summary(m1)
anova(m1)
anova(m1, ddf="lme4")
st <- step(m1)
plot(m1)