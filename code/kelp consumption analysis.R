## Data analysis for Corynactis-Urchin deterrence experiment - kelp consumption ##
## Code by Amelia Ritger at UCSB on September 7, 2019 ##

library(tidyverse)
library(zoo)
library(GGally)

## LOAD DATA
data <- read.csv("data/raw.csv") 

## TIDY DATA
# my own OCD, fix "Trial" issue
colnames(data)[colnames(data)=="ï..Trial"] <- "Trial"
# To remove instances where we have video evidence of urchins not moving (and not eating kelp)
data <- data[(data$Urchin_deter!="urchin never tried" | data$Visible_consumption=="yes"),]
# To compare across treatments, presence/absence of Corynactis
data$corynactis_binary <- ifelse(data$Treatment_numb>1, "present", "absent")
# Create variables: kelp area consumed, percent kelp consumed
data <- data %>%
  mutate(area_consumed=(Kelp_b-Kelp_a)) %>%
  mutate(percent_consumed=(area_consumed/Kelp_b))

# look at "minimum % kelp consumed cutoff" contradictions between ImageJ and visual analysis
#datan <- subset(data, Visible_consumption=="no")
#datay <- subset(data, Visible_consumption=="yes")
#sort(datan$percent_consumed) #If any percent consumption is less than 0.04, we assume there was no consumption (aka 'remove' ImageJ analysis error)...minus the one piece of kelp that definitely had a tiny bit of urchin chewing it
#data$percent_corrected <- ifelse(data$`Percent of Kelp Consumed`>0.04, data$`Percent of Kelp Consumed`, 0)
data$percent_corrected <- ifelse(data$Visible_consumption=="no", 0, data$percent_consumed)
data$area_corrected <- ifelse(data$Visible_consumption=="no", 0, data$area_consumed)

# To compare consumption (yes, no) across treatments
data$consumption_binary <- ifelse(data$percent_corrected>0, 1, 0)
# Average urchin body weight before and after trials
data <- data %>%
  mutate(urchin_avg_g = rowMeans(cbind(Urchin_wt_b,Urchin_wt_a)))
# To see if balcony versus wet lab location had an effect
data$experiment_location <- ifelse(data$Tank>6, "Balcony", "Lab")
# take averages (kelp consumption) of each tile, create new data frame from subsetted averages
# keep all control values rather than average them (if I pursue averaging repeated tiles), assign each control trial a unique number
data$Cory_numb[data$Cory_numb==0] <- c(-1:-15)
data_avg <- data %>%
  group_by(Cory_numb) %>%                                     # group tile numbers together
  mutate(avg_area=mean(area_consumed, na.rm=TRUE))  %>%       # take mean kelp consumption of each tile group
  mutate(avg_percent=mean(percent_consumed, na.rm=TRUE)) %>% # take mean % kelp consumption of each tile group
  ungroup() %>%                                                        # ungroup data REALLY IMPT when using group_by
  distinct(Cory_numb, .keep_all = TRUE)                       # remove duplicate tiles

# create new data frame for instances where urchins ate something (not nothing)
data_consumption <- subset(data, data$area_corrected>0)

## VISUALIZE DATA
# assign the response variable to Y
Y <- as.numeric(data$area_corrected)
Yname <- as.character("Kelp Consumed")

# use kelp area or percent consumption?
#plot(data_consumption$area_corrected~data_consumption$Kelp_b)
#abline(lm(data_consumption$area_corrected~data_consumption$Kelp_b))
#summary(lm(data_consumption$area_corrected~data_consumption$Kelp_b)) #area is fine because no relationship between kelp blade size and amount consumed 

# assign the predictor variables to X
treat <- as.factor(data$Treatment)
size <- data$urchin_avg_g
tile <- as.factor(data$Cory_numb)
date <- as.factor(data$Julian.date)
temp <- data$Temp
kloc <- as.factor(data$Kelp)
tloc <- as.factor(data$experiment_location)
starv <- data$Urchin_starve
used <- as.factor(data$Urchin_age)
eat <- data$consumption_binary
cory <- data$corynactis_binary

####################################################
# Tiered Analysis: Chi-Square test/glm - Urchins that ate vs didn't eat kelp
####################################################
library(corrplot)
library(boot)
library(spdep)
library(DCluster)

# Chi-square test for kelp consumption versus no consumption for yes/no Corynactis
t1 <-table(cory, eat)
rowSums(t1) #number of times Corynactis were or weren't used
colSums(t1) #number of times urchins did or did not eat
prop.table(t1)*100 #probability distribution table
cs <- chisq.test(cory,eat)
cs$p.value #p value = 0.1372, yes/no consumption and yes/no Corynactis are independent
cs$observed #observed values
round(cs$expected,2) #expected values
round(cs$residuals, 3) #pearsons residuals
corrplot(cs$residuals, is.cor = FALSE) #visualize pearsons residuals; (blue color means positively associated with variables, red color means negatively associated with variables); controls driving a lot of this trend
contrib <- 100*cs$residuals^2/cs$statistic #contribution of residuals in %
round(contrib, 3) #visualize % contribution of pearsons residuals for each variable
corrplot(contrib, is.cor = FALSE) #visualize contribution - dependency is heavy on controls

# Chi-square test for kelp consumption versus no consumption across treatments
t1 <- table(treat, eat)
cs <- chisq.test(treat,eat) #yes/no consumption and treatment are independent p = 0.115, red morph positively associated with no consumption control positively associated with kelp consumption, control and red are strongly influencing dependency 

# no significance across color morphs, but it looks like red has a big effect - let's take a look
#subset only controls and red
data_red <- subset(data, Treatment_numb==1|Treatment_numb==2)

#re-assign variable names
treat_red <- factor(data_red$Treatment)
eat_red <- data_red$consumption_binary
tile_red <- data_red$Cory_numb
# Chi-square test for kelp consumption versus no consumption between Red and control
t1 <- table(treat_red, eat_red)
cs <- chisq.test(treat_red,eat_red) #p = 0.0428, yes/no consumption and red/control are dependent
prop.test(cs$observed) #proportion test - proportions amongst groups are the same

###### BUT CHI-SQUARED TEST ASSUMES INDEPENDENCE BETWEEN SAMPLES! AND WE HAVE REPLICATES AMONG TILES
#so...
#bootstrap amongst tile replicates? none of the following accomplishes this
Observed=cs$observed[2,]
Expected=round(cs$expected[2,],2)
boot_r <- data.frame(cbind(Observed, Expected))
#Multinomial model
chq.mboot<-boot(boot_r, statistic=achisq.pboot, sim="parametric", ran.gen=multinom.sim,  R=1000)
plot(chq.mboot)#Display results
chq.perboot<-boot(boot_r, statistic=achisq.boot, R=1000)
plot(chq.perboot)#Display results
boot.ci(chq.perboot)

Observed=as.numeric(cs$observed) #observed chi-square values
Expected=as.numeric(round(cs$expected,2)) #expected chi-square values
boot_c <- data.frame(Observed, Expected)
row.names(boot_c) <- cbind("no consumption control", "no consumption red", "consumption control", "consumption red")
chq.mboot<-boot(boot_c, statistic=achisq.pboot, sim="parametric", ran.gen=multinom.sim,  R=1000)
plot(chq.mboot)#Display results
chq.perboot<-boot(boot_c, statistic=achisq.boot, R=1000)
plot(chq.perboot)


# total "eat vs not eat 0-1" for each tile - to visualize
red <- red %>%
  group_by(Tile) %>%                                     # group tile numbers together
  mutate(sum=sum(Eat, na.rm=TRUE)) %>%
  mutate(avg=mean(Eat, na.rm=TRUE)) %>%
  ungroup() %>%                                                        # ungroup data REALLY IMPT when using group_by
  distinct(Tile, .keep_all = TRUE)                       # remove duplicate tiles
red$sum #doesn't look like there's any bias amongst tiles... some are always eat = 0, some are sometimes 1/0, one is always 1




##############################################
# how about mixed effects logistic regression?
ggpairs(data[,c("Julian.date", "Kelp", "Urchin_starve", "Tank")])

########## with bart
library(lme4)
data$Cory_numb_f <- as.factor(data$Cory_numb)
m1 <- glm(consumption_binary ~ Treatment, data=data, family=binomial)
m <- glmer(consumption_binary ~ Treatment + (1 | Cory_numb_f), data = data, family = binomial)
summary(m1)

hist(resid(m))
######## with bart                                         
                                        


m <- glmer(consumption_binary ~ Treatment + (1 | Cory_numb), data = data_red, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
print(m)
se <- sqrt(diag(vcov(m)))
#table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 *
                se))

#next step - multilevel bootstrapping
sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}

set.seed(20)
tmp <- sampler(data_red, "Treatment", reps = 100)
bigdata <- cbind(tmp, data_red[tmp$RowID, ])

f <- fixef(m)
r <- getME(m, "theta")

require(parallel)

cl <- makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))

myboot <- function(i) {
  object <- try(glmer(consumption_binary ~ Treatment + (1 | Cory_numb), data = bigdata, subset = Replicate == i, family = binomial,
                      nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
  if (class(object) == "try-error")
    return(object)
  c(fixef(object), getME(object, "theta"))
}

start <- proc.time()
res <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = myboot)
end <- proc.time()
stopCluster(cl)

success <- sapply(res, is.numeric)
mean(success)

# combine successful results
bigres <- do.call(cbind, res[success])

# calculate 2.5th and 97.5th percentiles for 95% CI
(ci <- t(apply(bigres, 1, quantile, probs = c(0.025, 0.975))))


####################################################
# Tiered Analysis: Mixed-model - Among the urchins that ate, how does Corynactis matter in how much they ate?
####################################################

#something like lm(area consumed) compared to binary analysis above using glm(proportion data)

# create new data frame for instances where urchins ate something (not nothing)
data_consumption <- subset(data, data$area_corrected>0)

# assign the response variable to Y
Y0 <- as.numeric(data_consumption$area_corrected)
Y0name <- as.character("Kelp Consumed, when consumed")

# do the same for those cases where urchins ate something
treat <- as.factor(data_consumption$Treatment)
size <- data_consumption$urchin_avg_g
tile <- as.factor(data_consumption$Cory_numb)
date <- as.factor(data_consumption$Julian.date)
temp <- data_consumption$Temp
kloc <- as.factor(data_consumption$Kelp)
tloc <- as.factor(data_consumption$experiment_location)
starv <- data_consumption$Urchin_starve
used <- as.factor(data_consumption$Urchin_age)

# Univariate analysis for Y
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
ggplot(data_zero,aes(x=Treatment,y=area_corrected))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Treatment")+
  ylab("kelp area consumed")+
  theme_bw()

# rough model to look at effects of treatment on kelp consumption
m1 <- lm(Y ~ treat)
summary(m1)

m<- lm(data_zero$area_corrected~as.factor(data_zero$Treatment))
summary(m)

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
# General two-way ANOVA: Mixed Model
####################################################
#### TO DO: deal with non-normal distribution of data (zero-inflated? binomial distribution), nested design

library(lmerTest)
require(ggplot2)
require(pscl)
require(MASS)
require(boot)

summary(Y)

m1 <- glmer(percent_corrected ~ (1|tile) + treat, weights= `Kelp After (cm^2)`, data = data)

################### with bart
gls?m?(kelp_after, weights = kelp before #normal distrib), fitting beta distrib after removing zeros, no random effects
      
  
  Y2~treat + (1|tile), family=binomial(link="logit"))
summary(m1)


m3 <- lmer(Y ~ treat + (1|tile))

summary(m3)
anova(m3)
anova(m3, ddf="lme4")
st <- step(m3)
plot(m3)



summary((glm(data$consumption_binary~treat, family=binomial)))
