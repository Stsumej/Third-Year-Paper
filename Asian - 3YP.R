######################################################################################
#Multivariate model subet to only Asian 
#######################################################################################
amps <- cmps[complete.cases(cmps), ]
amps <- filter(cmps, cmps$s2_4 == 1)

#National identty index
amps$c107 <- as.numeric(amps$c107)
amps$c108 <- as.numeric(amps$c108)
amps$c109 <- as.numeric(amps$c109)
amps$c110 <- as.numeric(amps$c110)

natid <-(rev(amps$c107) + rev(amps$c108) + amps$c109 + amps$c110)/4
natid <- natid-1
hist(natid)
summary(natid)

#IVS
amps$allrace <- amps$ethnic_quota
allrace <- amps$allraces

#Controls
income <-as.numeric(amps$c383)
south <- amps$south
citizen <-amps$citizen
religion <- amps$c129
age <- amps$age
age <- cut_number(age,10)
ed <- as.numeric(amps$c381)
native <- amps$s7
native <- ifelse(native == "United States", 1, 0) #Note that this does not include Puerto Rico. This is a separate category. Excluded because irrelevant to my theory. 
amps$party <- amps$c25 #3-point measure, secondary measure of intensity below  
amps$party <- amps$party 
amps$party <- as.numeric(amps$party)
amps$party <- recode(amps$party,"c(3,4)= 0 ;c(1,2)= 1")
party <- amps$party 
amps$Dem <- as.numeric(amps$c25=="Democrat"); table(amps$Dem)
amps$partisan <- amps$c26
amps$partisan <- fct_rev(amps$partisan)
amps$partisan <- as.numeric(amps$partisan)-1
lf <- as.numeric(amps$c150) -1;lf
dlf <- as.numeric(fct_rev(amps$c151))
female <- amps$s3
female <- as.numeric(female)
female <- ifelse(female == "Male", 0, 1)

sAsian <-glm(natid ~ citizen + female + native + south + income + lf + partisan + ed + age, 
             family="gaussian", data=amps); summary(sAsian)

sdAsian <-glm(natid ~ citizen + female + native + south + income + dlf + partisan + ed + age, 
              family="gaussian", data=amps); summary(sdAsian)
log(exp(coef(sdAsian)))

plotreg(sAsian, omit.coef = "Intercept")
plotreg(sdAsian, omit.coef = "Intercept")
