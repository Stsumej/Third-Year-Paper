rm(list=ls())

library(dplyr)
library(foreign)
library(descr)
library(knitr)
library(stargazer)
library(ggplot2)
library(tidyverse) 
library(sandwich)
library(lmtest)
library(readstata13)
library(data.table)
library(score)
library(MASS)
library(coefplot)
library(pscl)
library(visreg)
library(interactionTest)
library(interplot)
library(productplots)
library(gridExtra)
library(car)
library(visreg)
library(texreg)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(plyr)

anes <- read.dta13("anes_timeseries_2012.dta")
anes[anes<0 ] <- NA


#NATID, need to figure out how to reverse this and 

anes$nat <- sort(anes$patriot_amident, decreasing = TRUE)
anes$nat <- as.numeric(anes$nat)

hist(anes$nat)
table(anes$nat)

#Race
anes$allrace <- as.factor(anes$dem_raceeth_x); table(anes$allrace)
levels(anes$allrace) <- c("White", "Black", "Asian", 
                          "Native American/Alaska Native", "Hispanic", "Other")
anes$black <- as.numeric(anes$dem_racecps_black)
anes$white <- as.numeric(anes$dem_racecps_white)
anes$asian <- as.numeric(anes$dem_racecps_asian)
anes$latino <- as.numeric(anes$allrace == "Hispanic")

anes$dblack <- as.numeric(anes$ident_blackid)
anes$dwhite <- as.numeric(anes$ident_whiteid)
anes$dasian <- as.numeric(anes$ident_asianid)
anes$dlatino <- as.numeric(anes$ident_hispid)

#Linked Fate
anes$blf <- as.numeric(anes$link_black == 1)
anes$wlf <- as.numeric(anes$link_white == 1)
anes$llf <- as.numeric(anes$link_hisp == 1)


#Controls (note all respondents in the ANES are citizens)
anes$age <- anes$dem_age_r_x
anes$ed <- anes$dem_edu
anes$native <- anes$dem_nativity
anes$income <- anes$incgroup_prepost_x
anes$south <- as.numeric(anes$sample_region=="3. South")
anes$gender <- anes$gender_respondent_x
anes$fem <- as.numeric(anes$gender == "2. Female")
anes$pid7 <- anes$pid_x
anes$partisan <- as.numeric(anes$pid_self)
anes$partisan <- ifelse(anes$pid7, 1:2, 1)-1
table(anes$partisan)

#######################################################################################
#Descriptive Statistics for National Identity in ANES
#######################################################################################
crosstab(anes$allrace, anes$nat, prop.r = TRUE)

crosstab(anes$nat, anes$blf, prop.r = TRUE)


b <- ggplot(anes, aes(anes$allrace, fill = anes$patriot_amident))
b <- b + geom_bar(position = "fill") + theme_classic();b
b <- b + labs(title = "National Identity Orientation: Belonging",
              x = "Race", y = "Proportion") + theme(plot.title = element_text(hjust = 0.5))
b <- b + scale_fill_grey(start = 0.2, end = 0.8, na.value = "red", name = "Intensity", 
                         labels = c("High", "Moderate", "Low", "None"));b



#######################################################################################
#Subset to Blacks Only
#######################################################################################

bnes <- anes[complete.cases(anes), ]
bnes <- filter(anes, anes$dem_racecps_black == "1. Selected by R")

#NATID


#Linked Fate
bnes$blf <- as.numeric(bnes$link_black == 1); table(anes$blf)



#Controls (note all respondents in the bnes are citizens)
bnes$age <- bnes$dem_age_r_x
bnes$ed <- bnes$dem_edu
bnes$native <- bnes$dem_nativity
bnes$income <- bnes$incgroup_prepost_x
bnes$south <- as.numeric(bnes$sample_region=="3. South")
bnes$gender <- bnes$gender_respondent_x
bnes$fem <- as.numeric(bnes$gender == "2. Female")
bnes$pid7 <- bnes$pid_x
bnes$partisan <- as.numeric(bnes$pid_self)
bnes$partisan <- ifelse(bnes$pid7, 1:2, 1)-1

black <-glm(nat ~ blf + fem + native + south + income + partisan + pid7 + ed + age, 
             family="gaussian", data=bnes); screenreg(black)


zero

#######################################################################################
#Subset to Whites Only
#######################################################################################

bmps <- filter(cmps, cmps$s2_3 == 1)

