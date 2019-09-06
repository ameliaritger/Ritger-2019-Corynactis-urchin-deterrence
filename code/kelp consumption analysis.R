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
data$`Cory Tile Number`[data$`Cory Tile Number`==0] <- c(-1:-14)

# take averages (kelp consumption) of each tile, create new data frame from subsetted averages
data_avg <- data %>%
  group_by(`Cory Tile Number`) %>%                                     # group tile numbers together
  mutate(avg_area=mean(`Kelp Consumed (cm^2)`, na.rm=TRUE))  %>%       # take mean kelp consumption of each tile group
  mutate(avg_percent=mean(`Percent of Kelp Consumed`, na.rm=TRUE)) %>% # take mean % kelp consumption of each tile group
  ungroup() %>%                                                        # ungroup data REALLY IMPT when using group_by
  distinct(`Cory Tile Number`, .keep_all = TRUE)                       # remove duplicate tiles

# assign the response variable to Y
Yp <- as.numeric(data_avg$avg_percent)
Ypname <- as.character("% Kelp Consumed")

Ya <- as.numeric(data_avg$avg_area)
Yaname <- as.character("Kelp area Consumed")

#Untransformed univariate analyses for Y 
hist(Yp, main="", xlab=Ypname)
boxplot(Yp, xlab=Ypname)
qqnorm(Yp)
qqline(Yp) #neither area nor percent don't need transformation

# quick look at kelp consumption by treatment
a <- ggplot(data_avg,aes(x=Treatment,y=avg_percent))+
  geom_boxplot()+
  geom_point()+
  xlab("Treatment")+
  ylab("% kelp area consumed, averaged by tile")+
  theme_bw()

ggsave("figures/first_run_percent.pdf",a,width=5,height=5)

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

# assign the response variable to Y
Y <- as.numeric(data$`Percent of Kelp Consumed`)
Yname <- as.character("% Kelp Consumed")

# look at "minimum % kelp consumed cutoff" contradictions between ImageJ and visual analysis
datan <- subset(data, `Kelp visibly consumed?`=="no") 
datay <- subset(data, `Kelp visibly consumed?`=="yes")
summary(datay$`Percent of Kelp Consumed`) #% consumption under 0.05 can be considered "not eaten" (minus 2 blades where kelp was visibly consumed)

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

a <- ggplot(data_avg,aes(x=Treatment,y=avg_percent))+
  geom_boxplot()+
  geom_point()+
  xlab("Treatment")+
  ylab("% kelp area consumed, averaged by tile")+
  theme_bw()

ggsave("figures/first_run_percent.pdf",a,width=5,height=5)

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