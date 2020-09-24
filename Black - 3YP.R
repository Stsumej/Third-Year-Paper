######################################################################################
#Multivariate model subet to only Blacks 
#######################################################################################
bmps <- cmps[complete.cases(cmps), ]
bmps <- filter(cmps, cmps$s2_3 == 1)

#National identty index
bmps$c107 <- as.numeric(bmps$c107)
bmps$c108 <- as.numeric(bmps$c108)
bmps$c109 <- as.numeric(bmps$c109)
bmps$c110 <- as.numeric(bmps$c110)

bmps$natid <-(rev(bmps$c107) + rev(bmps$c108) + bmps$c109 + bmps$c110)/4
bmps$natid <- bmps$natid-1

summary(bmps$natid)

#IVS
bmps$allrace <- bmps$ethnic_quota
bmps$allrace <- bmps$allrace
bmps$Black <- bmps$s2_3
bmps$White <- bmps$s2_1
bmps$Latino <- bmps$s2_2
bmps$Asian <- bmps$s2_4

#Controls
bmps$income <-as.numeric(bmps$c383)
bmps$south <- bmps$south
bmps$citizen <-bmps$citizen
bmps$religion <- bmps$c129
bmps$age <- as.numeric(bmps$age)
bmps$ed <- as.numeric(bmps$c381); summary(bmps$ed)
bmps$native <- bmps$s7
bmps$native <- ifelse(bmps$native == "United States", 1, 0) #Note that this does not include Puerto Rico. This is a separate category. Excluded because irrelevant to my theory. 
bmps$native <- as.numeric(bmps$native)
bmps$party <- bmps$c25 #3-point measure, secondary measure of intensity below  
bmps$party <- bmps$party 
bmps$party <- as.numeric(bmps$party)
bmps$party <- recode(bmps$party,"c(3,4)= 0 ;c(1,2)= 1")
bmps$party <- bmps$party 
bmps$Dem <- as.numeric(bmps$c25=="Democrat"); table(bmps$Dem)
bmps$partisan <- bmps$c26; table(bmps$partisan)
bmps$partisan <- fct_rev(bmps$partisan)
bmps$partisan <- as.numeric(bmps$partisan)-1; summary(bmps$partisan)
bmps$lf <- as.numeric(bmps$c150) -1; table(bmps$lf)
bmps$c.dlf <- fct_rev(bmps$c151)
bmps$female <- bmps$s3
bmps$female <- as.numeric(ifelse(bmps$female == "Male", 0, 1))
bmps$female <- as.numeric(bmps$female)


######################################################################################
#Modeling
#######################################################################################
ggbcoefnames <- c("National Identity", "Citizen", "Female", "Native Born", "South",
                 "Income", "Linked Fate", "Partisanship", "Education", "Age")


stargazer(bmps[c("natid", "citizen", "female", "native", "south", 
                 "income", "lf", "partisan", "ed", "age")] ,title = c("Descriptive Statistics"),
          covariate.labels = ggbcoefnames, digits = 2, type = "html",  out = "blackdescriptives.htm")


######################################################################################
#Modeling
#######################################################################################

black <-glm(natid ~ citizen + female + native + south + income + partisan + ed + age, 
             family="gaussian", data=bmps); screenreg(black)

sblack <-glm(natid ~ citizen + female + native + south + income + lf + partisan + ed + age, 
             family="gaussian", data=bmps); screenreg(sblack)


sdblack <-glm(natid ~ citizen + female + native + south + income + c.dlf + partisan + ed + age, 
              family="gaussian", data=bmps); screenreg(sdblack)

scoefnames <- c("Intercept", "Citizen", "Female", "Native Born","South", 
                "Income", "Linked Fate", "Partisanship", "Education", "Age")
sdcoefnames <- c("Intercept", "Citizen", "Female", "Native Born","South", 
                 "Income", "Moderate LF", "High LF","Partisanship", "Education", "Age")

table(c.dlf)
780/2286
coefplot(sdblack)
plotreg(sblack, omit.coef = "Intercept", custom.coef.names = scoefnames, custom.model.names = "Black Collectivism and Perception of American National Identity")
plotreg(sdblack, use.se = FALSE, omit.coef = "Intercept", custom.coef.names = sdcoefnames, 
        custom.model.names = "Degree of Black Collectivism and \n Perception of American National Identity")

ascoefnames <- c("Citizen", "Female", "Native Born","South", 
                 "Income", "Linked Fate", "Moderate LF","High LF","Partisanship", "Education", "Age", "Intercept")

stargazer(black,sblack,sdblack, title = c("Black Collectivism and National Identity"),
          covariate.labels = ascoefnames, dep.var.labels = "National Identity",
          se=list(NULL), omit.stat = c("LL", "ser", "f", "adj.rsq"), digits = 2, type = "text", out = "blacksubsets.htm")

