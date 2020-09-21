######################################################################################
#Multivariate model subet to only White 
#######################################################################################
wmps <- cmps[complete.cases(cmps), ]
wmps <- filter(cmps, cmps$s2_1 == 1)

#National identty index
wmps$c107 <- as.numeric(wmps$c107)
wmps$c108 <- as.numeric(wmps$c108)
wmps$c109 <- as.numeric(wmps$c109)
wmps$c110 <- as.numeric(wmps$c110)

wmps$natid <-(rev(wmps$c107) + rev(wmps$c108) + wmps$c109 + wmps$c110)/4
wmps$natid <- wmps$natid-1
hist(wmps$natid)
summary(wmps$natid)

#IVS
wmps$allrace <- wmps$ethnic_quota
wmps$allrace <- wmps$allrace

#Controls
wmps$income <-as.numeric(wmps$c383)
wmps$south <- wmps$south
wmps$citizen <-wmps$citizen
wmps$religion <- wmps$c129
wmps$age <- as.numeric(wmps$age)
wmps$ed <- as.numeric(wmps$c381)
wmps$native <- wmps$s7
wmps$native <- ifelse(wmps$native == "United States", 1, 0)#Note that this does not include Puerto Rico. This is a separate category. Excluded because irrelevant to my theory. 
wmps$party <- wmps$c25 #3-point measure, secondary measure of intensity below  
wmps$party <- wmps$party 
wmps$party <- as.numeric(wmps$party)
wmps$party <- recode(wmps$party,"c(3,4)= 0 ;c(1,2)= 1")
wmps$party <- wmps$party 
wmps$Dem <- as.numeric(wmps$c25=="Democrat"); table(wmps$Dem)
wmps$partisan <- wmps$c26
wmps$partisan <- fct_rev(wmps$partisan)
wmps$partisan <- as.numeric(wmps$partisan)-1
wmps$lf <- as.numeric(wmps$c150) -1;wmps$lf
wmps$c.dlf <- fct_rev(wmps$c151)
wmps$female <- wmps$s3
wmps$female <- ifelse(wmps$female == "Male", 0, 1)
######################################################################################
#Modeling
#######################################################################################

sWhite <-glm(natid ~ citizen + female + native + south + income + lf + partisan + ed + age, 
             family="gaussian", data=wmps); screenreg(sWhite)


sdWhite <-glm(natid ~ citizen + female + south + income + c.dlf + partisan + ed + age, 
              family="gaussian", data=wmps); screenreg(sdWhite) #dropped native because it seemed collinear among Whites

w.scoefnames <- c("Intercept", "Citizen", "Female", "Native Born","South", 
                "Income", "Linked Fate", "Partisanship", "Education", "Age")
w.sdcoefnames <- c("Intercept", "Citizen", "Female", "South", 
                 "Income", "Moderate LF","High LF","Partisanship", "Education", "Age")



plotreg(sWhite, omit.coef = "Intercept", custom.coef.names = w.scoefnames, custom.model.names = "White Collectivism and \n Perception of American National Identity")
plotreg(sdWhite, use.se = FALSE, omit.coef = "Intercept", custom.coef.names = w.sdcoefnames, 
        custom.model.names = "Degree of White Collectivism and \n Perception of American National Identity")


w.ascoefnames <- c("Citizen", "Female", "Native Born","South", 
                 "Income", "Linked Fate", "Moderate LF","High LF","Partisanship", "Education", "Age", "Intercept")

stargazer(sWhite,sdWhite, title = c("White Collectivism and National Identity"),
          covariate.labels = w.ascoefnames, dep.var.labels = "National Identity",
          se=list(NULL), omit.stat = c("LL", "ser", "f", "adj.rsq"), digits = 2, type = "text", out = "whitesubsets.htm")




