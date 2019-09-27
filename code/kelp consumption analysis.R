## Data analysis for Corynactis-Urchin deterrence experiment - kelp consumption ##
## Code by Amelia Ritger at UCSB on September 7, 2019 ##

library(tidyverse)
library(zoo)
library(GGally)
library(ggplot2)

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
#data$Cory_numb[data$Cory_numb==0] <- c(-1:-15)

## VISUALIZE DATA
# assign the response variable to Y
Y <- as.numeric(data$area_corrected)
Yname <- as.character("Kelp Consumed")

# use kelp area or percent consumption?
#plot(data_consumption$area_corrected~data_consumption$Kelp_b)
#abline(lm(data_consumption$area_corrected~data_consumption$Kelp_b))
#summary(lm(data_consumption$area_corrected~data_consumption$Kelp_b)) #area is fine because no relationship between kelp blade size and amount consumed 

#visualize correlations among variables
ggpairs(data[,c("Julian.date", "Temp", "Kelp", "Urchin_starve", "Tank", "experiment_location", "urchin_avg_g")]) #good thing, nothing here
ggpairs(data[,c("Treatment", "Cory_numb", "corynactis_binary", "urchin_avg_g", "consumption_binary", "area_corrected")])
#treatment doesn't have an effect on area kelp consumed, might be an effect on yes/no consumption (though it looks weird, lots of overlap)
#tile number doesn't have any strong correlations
#yes/no Corynactis doesn't have an effect on area kelp consumed, yes/no consumption

####################################################
# Tiered Analysis: Urchins that ate vs didn't eat kelp
# Ignore the next 120 lines of code
####################################################
library(corrplot)
library(boot)
library(spdep)
library(DCluster)

# assign variables
treat <- as.factor(data$Treatment)
eat <- data$consumption_binary
cory <- data$corynactis_binary

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

#BUT CHI-SQUARED TEST ASSUMES INDEPENDENCE BETWEEN SAMPLES. AND WE HAVE REPLICATES AMONG TILES
#so... bootstrap amongst tile replicates?
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


##############################################
# Logistic regression, binomial distribution (really Bernoulli)
##############################################
## Aight. Let's look at the effect of color on whether or not urchins ate, "averaged" amongst tiles
# because not every tile was actually used twice (thanks to urchins not moving, etc), get the total counts each tile was used and add it as a column to data frame
data <- data[order(data$Cory_numb),] #sort whole data frame by tile number
df <- data # separate this organization from the whole data frame in case you mess something up
df <- df[order(df$Cory_numb),] #sort subsetted data frame by tile number
df$Cory_numb[df$Cory_numb==0] <- c(-1:-15) #change all controls to unique negative numbers to get count values
new <- count(df, Cory_numb) #count values for each tile number
duptimes <- c(new$n) #how many replicates I want of each row
indx <- rep(1:nrow(new), duptimes) # Create an index of the rows I want with duplications
dupdf <- new[indx,] # Use that index to generate new data frame
dupdf$Cory_numb[1:15] <-0 #change control values back to tile number "0"
dupdf$n[1:15] <- 15 #change duplications for control tiles to 15
data$count_per_tile <- dupdf$n #add the counts to the whole data frame

#now put it all together 
#library(dplyr)
data_avg <- data %>%
  group_by(Cory_numb) %>%                                     # group tile numbers together
  mutate(success=sum(consumption_binary, na.rm=TRUE)) %>%     #number of successes
  mutate(failure=ifelse(count_per_tile==2, abs(success-2), ifelse(count_per_tile==1,abs(success-1), 3))) %>%  #number of failures, given number of times tile was used
  mutate(binary_avg=mean(consumption_binary, na.rm=TRUE)) %>% #average successes/total attempts
  mutate(avg_area=mean(area_consumed, na.rm=TRUE))  %>%       # mean kelp consumption of each tile group
  mutate(avg_percent=mean(percent_consumed, na.rm=TRUE)) %>%  # mean % kelp consumption of each tile group
  ungroup() %>%                                               # ungroup data REALLY IMPT when using group_by
  distinct(Cory_numb, .keep_all = TRUE) %>%                   # remove duplicate tiles
  dplyr::select(Treatment, Cory_numb, urchin_avg_g, success, failure, binary_avg, count_per_tile, Kelp_b, Kelp_a, area_corrected) # clean up! extract only columns you want

#check your work
table(data_avg$Treatment,data_avg$failure)
table(data$Cory_numb, data$consumption_binary) # nice work!

#now to create the model
library(lme4)
#treating the data as bernoulli...but m1 here doesn't address non-independence of each trial
#data$consumption_yn <- ifelse(data$consumption_binary>0, "Yes", "No")
#data_bern <- data[c("Treatment", "Cory_numb", "consumption_binary", "consumption_yn")]
#m1 <- glm(consumption_binary ~ Treatment + Cory_numb, family = binomial, data = data_bern)
#m2 <- glm(consumption_binary ~ Treatment, family = binomial, data = data_bern)

#this is actually realistically treating the data as bernoulli, sucesses given failures amongst treatments and tiles
m1 <- glm(cbind(success, count_per_tile-success) ~ Treatment, family = binomial , data = data_avg)
summary(m1) 
#red color morphs are significantly different from control tiles (p = 0.0226), red color morphs are more likely to result in urchin failures

#data$Cory_numb_f <- as.factor(data$Cory_numb)
#m <- glmer(consumption_binary ~ Treatment + (1 | Cory_numb_f), data = data, family = binomial)


####################################################
# Tiered Analysis: Among the urchins that ate, how does Corynactis matter in how much they ate?
####################################################
###??? should I do something like lm(area consumed) compared to binary analysis above using glm(proportion data)?

# create new data frame for instances where urchins ate something (not nothing)
data_consumption <- subset(data, data$area_corrected>0)

#visualize correlations among variables
ggpairs(data_consumption[,c("Treatment", "Cory_numb", "corynactis_binary", "urchin_avg_g", "area_corrected")])
#urchin size, corynactis presence/absence, nor treatment determined how much kelp was consumed

# Univariate analysis for Y
hist(data_consumption$area_corrected, main="") #looks fine
boxplot(data_consumption$area_corrected) #2 outliers
qqnorm(data_consumption$area_corrected)
qqline(data_consumption$area_corrected) #could be better, but let's look at the model/residuals

## OPTIONAL...
#remove outliers
outliers <- boxplot(data_consumption$area_corrected)$out
data_consumption <- data_consumption[-which(data_consumption$area_corrected %in% outliers),]

# "fancy" plot kelp consumption with and without Corynactis
ggplot(data_consumption,aes(x=corynactis_binary,y=area_corrected))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Treatment")+
  ylab("kelp area consumed, when consumed")+
  theme_bw()
#ggsave("figures/present_absent.pdf",a,width=5,height=5)

# "fancy" plot kelp consumption by treatment
ggplot(data_consumption,aes(x=Treatment,y=area_corrected))+
  geom_boxplot()+
  geom_jitter()+
  xlab("Treatment")+
  ylab("kelp area consumed, when consumed")+
  theme_bw()

# super quick linear regression model
m2 <- lm(area_corrected ~ Treatment, data=data_consumption)
summary(m2)

# add tile as a random effect
library(lmerTest)
m3 <- lmer(area_corrected ~ Treatment + (1|Cory_numb), data = data_consumption)
summary(m3)
anova(m3)

# let's look at the residuals
data_consumption$predicted <- predict(m2)
data_consumption$residuals <- residuals(m2)
plot(residuals~predicted, data=data_consumption) #for m2, looks fine
abline(0, 0)
plot(m3)#for m3, looks fine

library(psycho)
results <- analyze(m3, CI = 95)
summary(results) %>% 
  mutate(p = psycho::format_p(p))
print(results)
#as suspected, there was no effect of treatment on amount of kelp consumed by urchins, when any kelp was consumed

# from bart
#library(betareg)
#summary(betareg(percent_corrected~Treatment, data=data_consumption)) #but I think I can just do a simple mixed model regression looking at area consumed, no??

####################################################
## SO, in conclusion:
## Red color morph increases probability urchin will fail to eat any kelp
## But there was NO EFFECT of treatment on amount of kelp consumed, when urchins did consume kelp
####################################################