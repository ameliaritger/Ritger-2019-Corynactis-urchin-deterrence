## Data analysis for Corynactis-Urchin deterrence experiment - kelp consumption ##
## Code by Amelia Ritger at UCSB on September 7, 2019 ##

library(tidyverse)
library(zoo)
library(readxl)

## LOAD DATA
data <- read_excel("data/raw.xlsx", sheet = "Kelp consumption")

## TIDY DATA
# To compare across treatments, presence/absence of Corynactis
data$corynactis_binary <- ifelse(data$`Treatment Number`>1, "present", "absent")
# Create variables: Kelp consumed, percent kelp consumed
data <- data %>%
  mutate(area_consumed_cm2=(`Kelp Before (cm^2)`-`Kelp After (cm^2)`)) %>%
  mutate(percent_consumed=(area_consumed_cm2/`Kelp Before (cm^2)`))

# look at "minimum % kelp consumed cutoff" contradictions between ImageJ and visual analysis
datan <- subset(data, `Kelp visibly consumed?`=="no")
datay <- subset(data, `Kelp visibly consumed?`=="yes")
sort(datan$percent_consumed) #If any percent consumption is less than 0.04, we assume there was no consumption (aka 'remove' ImageJ analysis error)...minus the one piece of kelp that definitely had a tiny bit of urchin chewing it
#data$percent_corrected <- ifelse(data$`Percent of Kelp Consumed`>0.04, data$`Percent of Kelp Consumed`, 0)
data$percent_corrected <- ifelse(data$`Kelp visibly consumed?`=="no", 0, data$percent_consumed)
data$area_corrected <- ifelse(data$`Kelp visibly consumed?`=="no", 0, data$area_consumed_cm2)

# To compare consumption (yes, no) across treatments
data$consumption_binary <- ifelse(data$percent_corrected>0, 1, 0)
# Average urchin body weight before and after trials
data <- data %>%
  mutate(urchin_avg_g = rowMeans(cbind(`Urchin weight before (g)`,`Urchin weight after (g)`)))
# To see if balcony versus wet lab location had an effect
data$experiment_location <- ifelse(data$`Tank number`>6, "Balcony", "Lab")
# To keep all control values rather than average them, assign each control trial a unique number
data$`Cory Tile Number`[data$`Cory Tile Number`==0] <- c(-1:-16)

# take averages (kelp consumption) of each tile, create new data frame from subsetted averages
data_avg <- data %>%
  group_by(`Cory Tile Number`) %>%                                     # group tile numbers together
  mutate(avg_area=mean(area_consumed_cm2, na.rm=TRUE))  %>%       # take mean kelp consumption of each tile group
  mutate(avg_percent=mean(percent_consumed, na.rm=TRUE)) %>% # take mean % kelp consumption of each tile group
  ungroup() %>%                                                        # ungroup data REALLY IMPT when using group_by
  distinct(`Cory Tile Number`, .keep_all = TRUE)                       # remove duplicate tiles


## ORGANIZE CODE
# assign the response variable to Y
Y <- as.numeric(data$percent_corrected) #binomial distribution? needs transformation
Yname <- as.character("% Kelp Consumed")

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


## FIRST-PASS ANALYSIS 
#Univariate analysis for Y 
hist(Y, main="", xlab=Yname)
boxplot(Y, xlab=Yname)
qqnorm(Y)
qqline(Y)

# quick look at kelp consumption with and without Corynactis
ggplot(data,aes(x=corynactis_binary,y=percent_corrected))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Treatment")+
  ylab("% kelp area consumed")+
  theme_bw()
#ggsave("figures/present_absent.pdf",a,width=5,height=5)

# rough model to look at effects of Corynactis on kelp consumption
m2 <- lm(Y~data$corynactis_binary)
summary(m2)

# quick look at kelp consumption by treatment
ggplot(data,aes(x=treat,y=area_corrected))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Treatment")+
  ylab("kelp area consumed")+
  theme_bw()

# rough model to look at effects of treatment on kelp consumption
m1 <- lm(Y ~ treat)
summary(m1)

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

####################################################
# General one-way ANOVA: Mixed Model
####################################################
#### TO DO: deal with non-normal distribution of data

library(lmerTest)
m3 <- lmer(Y ~ treat + (1|tile))

summary(m3)
anova(m3)
anova(m3, ddf="lme4")
st <- step(m3)
plot(m3)

####################################################
# Chi-square test
###################################################
# Results: Red color morph is not necessarily more likely to have no consumption, since treatment and %consumption are independent

# Chi-square test for kelp consumption versus no consumption for yes/no Corynactis
t1<-table(data$corynactis_binary, data$consumption_binary)
rowSums(t1) #number of times Corynactis were or weren't used
colSums(t1) #number of times urchins did or did not eat
prop.table(t1)*100 #probability distribution table
cs <- chisq.test(data$corynactis_binary, data$consumption_binary)
cs$p.value #p value = 0.1326, yes/no consumption and yes/no Corynactis are independent
cs$observed #observed values
round(cs$expected,2) #expected values
round(cs$residuals, 3) #pearsons residuals
library(corrplot)
corrplot(cs$residuals, is.cor = FALSE) #visualize pearsons residuals; (blue color means positively associated with variables, red color means negatively associated with variables); controls driving a lot of this trend
contrib <- 100*cs$residuals^2/cs$statistic #contribution of residuals in %
round(contrib, 3) #visualize % contribution of pearsons residuals for each variable
corrplot(contrib, is.cor = FALSE) #visualize contribution - dependency is heavy on controls

# Chi-square test for kelp consumption versus no consumption across treatments
t2<-table(data$Treatment, data$consumption_binary)
#visualize contingency table
library(gplots)
balloonplot(t(t2), main ="", xlab ="", ylab="", label = FALSE, show.margins = FALSE)
cs <- chisq.test(data$Treatment, data$consumption_binary) #yes/no consumption and treatment are independent p = 0.1104, red morph positively associated with no consumption control positively associated with kelp consumption, control and red are strongly influencing dependency 
