######################################################################################
#Multivariate model subet to only Asian 
#######################################################################################
amps <- cmps[complete.cases(cmps), ]
amps <- subset(cmps, cmps$s2_4 == 1)

amps$natid <-(rev(amps$c107) + rev(amps$c108) + amps$c109 + amps$c110)/4
amps$natid <- amps$natid-1
#IVS
amps$allrace <- amps$ethnic_quota
amps$allrace <- amps$allrace
amps$Black <- amps$s2_3
amps$White <- amps$s2_1
amps$Latino <- amps$s2_2
amps$Asian <- amps$s2_4

#Controls
amps$income <-as.numeric(amps$c383)
amps$south <- amps$south
amps$citizen <-amps$citizen
amps$religion <- amps$c129
amps$age <- as.numeric(amps$age)
amps$ed <- as.numeric(amps$c381)
amps$native <- amps$s7
amps$native <- ifelse(amps$native == "United States", 1, 0) #Note that this does not include Puerto Rico. This is a separate category. Excluded because irrelevant to my theory. 
amps$native <- as.numeric(amps$native)
amps$party <- amps$c25 #3-point measure, secondary measure of intensity below  
amps$party <- amps$party 
amps$party <- as.numeric(amps$party)
amps$party <- recode(amps$party,"c(3,4)= 0 ;c(1,2)= 1")
amps$party <- amps$party 
amps$Dem <- as.numeric(amps$c25=="Democrat")
amps$partisan <- amps$c26
amps$partisan <- fct_rev(amps$partisan)
amps$lf <- as.numeric(amps$c150) -1
amps$c.dlf <- fct_rev(amps$c151)
amps$female <- amps$s3
amps$female <- as.numeric(ifelse(amps$female == "Male", 0, 1))
amps$female <- as.numeric(amps$female)


#Modeling
asian <-lm(natid ~ citizen + female + native + south + income + partisan + ed + age, data=amps)

sasian <-lm(natid ~ citizen + female + native + south + income + lf + partisan + ed + age, data=amps)

sdasian <-lm(natid ~ citizen + female + native + south + income + c.dlf + partisan + ed + age, data=amps)
#Descriptive Statistics
ggbcoefnames <- c("National Identity", "Citizen", "Female", "Native Born", "South",
                  "Income", "Linked Fate", "Partisanship", "Education", "Age")


stargazer(amps[c("natid", "citizen", "female", "native", "south", 
                 "income", "lf", "partisan", "ed", "age")] ,title = c("Descriptive Statistics"), covariate.labels = ggbcoefnames, digits = 2, type = "latex", header=FALSE)
plotreg(asian, omit.coef = "Intercept")
plotreg(sasian, omit.coef = "Intercept")
plotreg(sdasian, omit.coef = "Intercept")
plotreg(black, omit.coef = "Intercept")
