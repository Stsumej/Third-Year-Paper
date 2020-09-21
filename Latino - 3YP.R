######################################################################################
#Multivariate model subet to only Latino 
#######################################################################################
lmps <- cmps[complete.cases(cmps), ]
lmps <- filter(cmps, cmps$s2_2 == 1)

#National identty index
lmps$c107 <- as.numeric(lmps$c107)
lmps$c108 <- as.numeric(lmps$c108)
lmps$c109 <- as.numeric(lmps$c109)
lmps$c110 <- as.numeric(lmps$c110)

natid <-(rev(lmps$c107) + rev(lmps$c108) + lmps$c109 + lmps$c110)/4
natid <- natid-1
hist(natid)
summary(natid)

#IVS
lmps$allrace <- lmps$ethnic_quota
allrace <- lmps$allrace
lmps$Latino <- lmps$s2_3
lmps$White <- lmps$s2_1
lmps$Latino <- lmps$s2_2
lmps$Asian <- lmps$s2_4

#Controls
income <-as.numeric(lmps$c383)
south <- lmps$south
citizen <-lmps$citizen
religion <- lmps$c129
age <- lmps$age
age <- cut_number(age,10)
ed <- as.numeric(lmps$c381)
native <- lmps$s7
native <- ifelse(native == "United States", 1, 0) #Note that this does not include Puerto Rico. This is a separate category. Excluded because irrelevant to my theory. 
lmps$party <- lmps$c25 #3-point measure, secondary measure of intensity below  
lmps$party <- lmps$party 
lmps$party <- as.numeric(lmps$party)
lmps$party <- recode(lmps$party,"c(3,4)= 0 ;c(1,2)= 1")
party <- lmps$party 
lmps$Dem <- as.numeric(lmps$c25=="Democrat"); table(lmps$Dem)
lmps$partisan <- lmps$c26
lmps$partisan <- fct_rev(lmps$partisan)
lmps$partisan <- as.numeric(lmps$partisan)-1
lf <- as.numeric(lmps$c150) -1;lf
dlf <- as.numeric(fct_rev(lmps$c151))
female <- lmps$s3
female <- as.numeric(female)
female <- ifelse(female == "Male", 0, 1)

sLatino <-glm(natid ~ citizen + female + native + south + income + lf + partisan + ed + age, 
             family="gaussian", data=lmps); summary(sLatino)

sdLatino <-glm(natid ~ citizen + female + native + south + income + dlf + partisan + ed + age, 
              family="gaussian", data=lmps); summary(sdLatino)
log(exp(coef(sdLatino)))

multiplot(sLatino, sdLatino, title = "Degree of Collectivism and National Identity", intercept = FALSE)
plotreg(sLatino, omit.coef = "Intercept")
plotreg(sdLatino, omit.coef = "Intercept")