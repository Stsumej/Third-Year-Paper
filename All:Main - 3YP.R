rm(list=ls())

setwd("~/Documents/Fall_18_Data_Files")
output <- "~/Documents/Fall_18_Data_Files"
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

cmps <- read.dta13("CMPS2016-book-ms.dta")
cmps[cmps<0] <-  NA

#National identity index
cmps$c107 <- as.numeric(cmps$c107) #belong
cmps$c108 <- as.numeric(cmps$c108) #respect and value
cmps$c109 <- as.numeric(cmps$c109) #how much do you feel like an outsider in the US
cmps$c110 <- as.numeric(cmps$c110) #excluded

cmps$natid <-(rev(cmps$c107) + rev(cmps$c108) + cmps$c109 + cmps$c110)/4
cmps$natid <- cmps$natid
summary(cmps$natid) #come back to this specification

table(cmps$allrace)
#IVS
cmps$allrace <- cmps$ethnic_quota
cmps$allrace <- fct_drop(cmps$allrace, c("Middle Eastern or Arab", "American Indian/Native American", "Other" ))
cmps$allrace <- revalue(cmps$allrace, c("Hispanic or Latino" = "Latinx", "Black or African American" = "Black",
                        "White Non Hispanic" = "White", "Asian American" = "Asian"))
table(cmps$allrace)
cmps$allrace <- cmps$allrace
cmps$Black <- cmps$s2_3
cmps$White <- cmps$s2_1
cmps$Latino <- cmps$s2_2
cmps$Asian <- cmps$s2_4

#Controls
cmps$income <-as.numeric(cmps$c383)
cmps$south <- cmps$south
cmps$citizen <-cmps$citizen
cmps$religion <- cmps$c129
cmps$age <- cmps$age
cmps$age <- cut_number(age,10)
cmps$ed <- as.numeric(cmps$c381)
cmps$native <- cmps$s7
cmps$native <- ifelse(native == "United States", 1, 0)#Note that this does not include Puerto Rico. This is a separate category. Excluded because irrelevant to my theory. 
cmps$native <- as.numeric(cmps$native)
cmps$party <- cmps$c25 #3-point measure, secondary measure of intensity below  
cmps$party <- cmps$party 
cmps$party <- as.numeric(cmps$party)
cmps$party <- recode(cmps$party,"c(3,4)= 0 ;c(1,2)= 1")
party <- cmps$party 
cmps$Dem <- as.numeric(cmps$c25=="Democrat")
cmps$partisan <- cmps$c26
cmps$partisan <- fct_rev(cmps$partisan)
cmps$partisan <- as.numeric(cmps$partisan)-1
cmps$lf <- as.numeric(cmps$c150) -1;lf
cmps$dlf <- as.numeric(fct_rev(cmps$c151))
cmps$c.dlf <- fct_rev(cmps$c151)
cmps$female <- cmps$s3
cmps$female <- ifelse(female == "Male", 0, 1)
cmps$female <- as.numeric(female)
table(c.dlf)
#######################################################################################
#Descriptive Statistics for National ID
#######################################################################################

#National identity index
belong <- cmps$gg.c107 <- as_factor(cmps$c107)  #belong
respect <-  cmps$gg.c108 <- as_factor(cmps$c108)  #respect and value
cmps$out <- cmps$gg.c109 <- as_factor(cmps$c109)  #how much do you feel like an outsider in the US
cmps$exc <- cmps$gg.c110 <- as_factor(cmps$c110)  #excluded
cmps$ggnatid <- cmps$natid
crosstab(cmps$allrace, cmps$gg.c107, prop.r = TRUE)
crosstab(cmps$allrace, cmps$gg.c108, prop.r = TRUE)
crosstab(cmps$allrace, cmps$gg.c109, prop.r = TRUE)
crosstab(cmps$allrace, cmps$gg.c110, prop.r = TRUE)
100-5.4



b <- ggplot(cmps, aes(allrace, fill = gg.c107))
b <- b + geom_bar(position = "fill") + theme_classic();b
b <- b + labs(title = "National Identity Orientation: Belonging",
              x = "Race", y = "Proportion") + theme(plot.title = element_text(hjust = 0.5))
b <- b + scale_fill_grey(start = 0.2, end = 0.8, na.value = "red", name = "Intensity", 
                         labels = c("High", "Moderate", "Low", "None"));b
 
r <- ggplot(cmps, aes(allrace, fill = gg.c108))
r <- r + geom_bar(position = "fill") + theme_classic();r
r <- r + labs(title = "National Identity Orientation: Respect and Value",
              x = "Race", y = "Proportion") + theme(plot.title = element_text(hjust = 0.5))
r <- r + scale_fill_grey(start = 0.2, end = 0.8, na.value = "red", name = "Intensity", 
                         labels = c("High", "Moderate", "Low", "None"));r


ot <- ggplot(cmps, aes(allrace, fill = gg.c109))
ot <- ot + geom_bar(position = "fill") + theme_classic();ot
ot <- ot + labs(title = "National Identity Orientation: Outsider",
              x = "Race", y = "Proportion") + theme(plot.title = element_text(hjust = 0.5))
ot <- ot + scale_fill_grey(start = 0.2, end = 0.8, na.value = "red", name = "Intensity", 
                         labels = c("None", "Low","Moderate","High"));ot

e <- ggplot(cmps, aes(allrace, fill = gg.c110))
e <- e + geom_bar(position = "fill") + theme_classic();e
e <- e + labs(title = "National Identity Orientation: Excluded",
              x = "Race", y = "Proportion") + theme(plot.title = element_text(hjust = 0.5))
e <- e + scale_fill_grey(start = 0.2, end = 0.8, na.value = "red", name = "Intensity", 
                         labels = c("None", "Low","Moderate","High"));e

all <-  grid.arrange(b,r,ot,e); all
ggsave("allracenatid.png", width = 7.5, height = 5)

#######################################################################################
#Descriptive Statistics for Linked Fate
#######################################################################################


crosstab(allrace,c.dlf, prop.r=TRUE) #percentage of High LF for all racial groups
1849/6344 #percentage of those who reported High LF/all respondents
766/1849 #percentage of Blacks that reported High LF/all High LF

plot(allrace, c.dlf)
cmps$gg.dlf <- cmps$c151
p <- ggplot(cmps, aes(allrace, fill = gg.dlf)) 
p <- p + geom_bar(position = "fill") + theme_classic()
p <- p + labs(title = "Degree of Linked Fate Across Racial Groups",
              x = "Race", y = "Proportion") + theme(plot.title = element_text(hjust = 0.5))
p <- p + scale_fill_grey(start = 0.2, end = 0.8, na.value = "1", name = "Degree of Linked Fate", 
                         labels = c("High", "Moderate", "Low"));p


ggcoefnames <- c("National Identity", "Black", "Citizen", "Female", "Native Born","South", 
                  "Income", "Linked Fate", "Partisanship", "Education", "Age")

stargazer(cmps[c("natid", "Black", "citizen", "female", "native", "south", 
                 "income", "lf", "c.dlf", "partisan", "ed", "age")],title = "Descriptive Statistics",
          covariate.labels = ggcoefnames, digits = 2, type = "html",  out = "descriptives.htm")
summary(c.dlf)


#######################################################################################
#Multivariate model for BLACK/NONBLACk
#######################################################################################

black <-glm(natid ~ Black + citizen + female + native + south + income + lf + partisan + ed + age, 
                  family="gaussian", data=cmps); screenreg(black)

dblack <-glm(natid ~ Black + citizen + female + native + south + income + c.dlf + partisan + ed + age, 
            family="gaussian", data=cmps); screenreg(dblack)



bcoefnames <- c("Intercept", "Black", "Citizen", "Female", "Native Born","South", 
                "Income", "Linked Fate", "Partisanship", "Education", "Age")
bdcoefnames <- c("Intercept", "Black", "Citizen", "Female", "Native Born","South", 
                 "Income", "Moderate LF","High LF","Partisanship", "Education", "Age")


plotreg(black, omit.coef = "Intercept", custom.coef.names = bcoefnames, custom.model.names = "Collectivism and Perception of American National Identity")
plotreg(dblack, omit.coef = "Intercept", custom.coef.names = bdcoefnames, 
        custom.model.names = "Degree of Collectivism and Perception of\n American National Identity")

bnbcoefnames <- c("Black", "Citizen", "Female", "Native Born","South", 
                  "Income", "Linked Fate", "Moderate LF","High LF","Partisanship", "Education", "Age")

stargazer(black,dblack, title = c("Black Collectivism and National Identity"),
          covariate.labels = bnbcoefnames, dep.var.labels = "National Identity",
          se=list(NULL), omit.stat = c("LL", "ser", "f", "adj.rsq"), digits = 2, type = "text", out = "blacknonblack.htm")


