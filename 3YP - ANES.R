rm(list=ls())
setwd("/Users/jenniferst.sume/Documents/Fall_18_Data_Files/Third Year Paper")
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
anes[anes < 0 ] <- NA

#NATID
anes$nat <- anes$ident_amerid

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
anes$blf <- anes$link_black
anes$wlf <- anes$link_white
anes$alf <- anes$link_asian
anes$llf <- anes$link_hisp

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
anes$partisan <- ifelse(anes$pid7, 1:2, 1)
table(anes$partisan)

