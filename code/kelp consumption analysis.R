## Data analysis for Corynactis-Urchin deterrence experiment ##
## Code by Amelia Ritger at UCSB on August 26, 2019 ##

#load data
library(readxl)
data <- read_excel("data/raw.xlsx", sheet = "Kelp consumption")

# because I'm not done with trials yet, remove lines for trials not yet run
data <- data %>%
  filter(`Urchin Size (mm)`>0)

# assign the predictor variables to X
treat <- as.factor(data$Treatment)
size <- data$`Urchin Size (mm)`
tile <- as.factor(data$`Cory Tile Number`)
date <- as.factor(data$`Julian date`)
temp <- data$`Water Temperature (°C)`
loc <- as.factor(data$`Kelp Location`)
site <- as.factor(data$`Experiment location`)
starv <- data$`Urchin Starvation Time (days)`

# assign the response variable to Y
Y <- as.numeric(data$`Percent of Kelp Consumed`)
Yname <- as.character("% Kelp Consumed")

library(tidyverse)
library(zoo)

# look at "minimum % kelp consumed cutoff" contradictions between ImageJ and visual analysis
datan <- subset(data, `Kelp visibly consumed?`=="no") 
datay <- subset(data, `Kelp visibly consumed?`=="yes")
summary(datay$`Percent of Kelp Consumed`) #% consumption under 0.05 can be considered "not eaten" (minus 2 blades where kelp was visibly consumed)

# take averages (kelp consumption) of each tile
data <- data %>%
  group_by(`Cory Tile Number`) %>%                                     # group tile numbers together
  mutate(avg_tile=mean(`Kelp Consumed (cm^2)`, na.rm=TRUE))  %>%       # take mean of each tile group
  ungroup()                                                            # ungroup data REALLY IMPT when using group_by

#Untransformed univariate analyses for Y 
hist(Y, main="", xlab=Yname)
boxplot(Y, xlab=Yname)
qqnorm(Y)
qqline(Y)

#Transform Y
lY <- log10(Y)
lYname <- as.character("log(% Kelp Consumed)")
boxplot(lY)
qqnorm(lY)
qqline(lY) #not perfect, but more normal

# Visualize Y vs. Xs
plot(Y~treat, main='', ylab=Yname)
plot(Y~tile)
plot(Y~site)

plot <- ggplot(data,aes(x=treat,y=Y))+
  geom_boxplot()+
  geom_point()+
  xlab("Treatment")+
  ylab("% kelp area consumed")+
  theme_bw()

ggsave("figures/first_run.pdf",plot,width=5,height=5)

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