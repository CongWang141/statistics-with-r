
# REGRESSION WITH PANEL DATA ----------------------------------------------

#LOAD PACKAGES

library(AER)
library(plm)


#Load the dataset

data("Fatalities") #Dataset used as example on the textbook of Stock and Watson
dim(Fatalities) #Dimension of dataset
str(Fatalities)

head(Fatalities) #First rows. Useful to a first glimpse

summary(Fatalities)
summary(Fatalities[,c(1,2)]) #Summary of first two columns

length(Fatalities$state)
#State is a Factor with 48 levels
#Year is a factor with 7 levels

#Initially we are interested in checking the relationship between beer tax and
#traffic fatality rate (fatalities/10000 inhabitants')

# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")

# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)
summary(fatal1982_mod)
summary(fatal1988_mod)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")
coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")

#The coefficient in 1988 is 3x higher than in 1982

# plot the observations and add the estimated regression line for 1982 data
dev.new() #Open new window to plot
par(mfrow=c(2,1))
plot(x = Fatalities1982$beertax, 
     y = Fatalities1982$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1982_mod, lwd = 1.5)


# plot observations and add estimated regression line for 1988 data
plot(x = Fatalities1988$beertax, 
     y = Fatalities1988$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1988_mod, lwd = 1.5)

dev.off()
# The difference is possibly due to omitted variable bias.


# BEFORE AND AFTER COMPARISONS - A PANEL WITH 2 TIME PERIODS--------------------


#Let's suppose we have information about just two periods: 1982 and 1988.

# compute the differences 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")

# plot the differenced data
par(mfrow=c(1,1))
plot(x = diff_beertax, 
     y = diff_fatal_rate, 
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")

# add the regression line to plot
abline(fatal_diff_mod, lwd = 1.5)

mean(Fatalities$fatal_rate) # It shows the effect might be overestimated.

#So far, information from 1983 to 1987 were not considered.
#If we have access to these information, what can be done?


# POOLED OLS --------------------------------------------------------------

# IT IS A SIMPLE OLS PERFORMED ON PANEL DATA
attach(Fatalities)
Fatalities_pooled=lm(fatal_rate~ beertax+year)
summary(Fatalities_pooled)

#The estimated effect is 0.3663 and it seems to be statically significant
#What's the main issue with this model?



attach(Fatalities)
# FIRST DIFFERENCE ESTIMATOR ----------------------------------------------

#1ST - Manually

#Get the first difference

diff_fatal_rate=ave(Fatalities$fatal_rate, state, FUN=function(x) lag(x)) # Vector, group, function
#Through the vector fatal rate, we're grouping by state and getting 1st diff
#Default of FUN is mean
?ave
diff_beer_tax=ave(beertax, state)

#Create a new data frame
Fatalities_diff=data.frame(fatal=diff_fatal_rate,
                           beer=diff_beer_tax,
                           year=year)
#Run OLS
Fatalities_FD=lm(fatal~beer, data=Fatalities_diff)
summary(Fatalities_FD)
#Estimated effect is 0.0136, not statistically significant


#2nd way - INBUILT FUNCTION

summary(plm(fatal_rate~beertax, data=Fatalities, model='fd'))
#Same value

#We can include time effects as well
summary(lm(fatal~beer+year, data=Fatalities_diff)) #Manually
summary(plm(fatal_rate~beertax+year, data=Fatalities, model='fd')) #Package

#In this way we are controlling for variables that are constant across entities
#(states) but vary over time.



# ESTIMATING THE FIXED EFFECTS MODEL --------------------------------------
##3 ways to estimate the fixed effects model

##1ST WAY - APPLYING OLS TO THE DEMEANED DATA (Yi-mean(Yi))

#1.1 obtain averages by group

#Average of fatal rate(dependent variable)
avg_fatal_by_state=ave(fatal_rate, state) #The average of fatal rate by state
sum(fatal_rate[1:7])/7 #Average of first state to double check
avg_fatal_by_state[1:10]    #First ten values of our object

#Average of beer tax
avg_beer=ave(beertax, state)


#1.2 Remove the average from the observations
fatal_rate_demeaned=fatal_rate-avg_fatal_by_state
beer_tax_demeaned=beertax-avg_beer

#1.3 Create a new dataset and run OLS

Fatalities_demeaned <- data.frame(fatal_rate = fatal_rate_demeaned,
                                       beertax = beer_tax_demeaned,
                                  year=year)
View(Fatalities_demeaned)


# estimate the regression
fatal_fe_1=lm(fatal_rate ~ beertax-1 , data = Fatalities_demeaned)
summary(fatal_fe_1)

#The estimated effect is -0.6559!!


##2ND WAY - LSDV (LEAST SQUARE DUMMY VARIABLE)

#wE WILL INCLUDE A DUMMY VARIABLE TO EACH STATE

fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state-1 , data = Fatalities)

#State is a categorical variable, so R will create dummies, as we learned before
#By including the option "-1" all states are included and there's no intercept
#If one want to estimate with the constant, just remove the argument

summary(fatal_fe_lm_mod)

#The estimated effect is the same -0.6559, exactly the same!
#The benefit of using LSDV is that we can estimate the effect of each state. 
#Naturally it's not a good idea when we have a large number of units.


## 3rd WAY - INBUILT FUNCTION

fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")
summary(fatal_fe_mod)

#The estimated effect is the same!

#Let's include time effects in all 3 models

fatal_fe_1=lm(fatal_rate ~ beertax+year - 1, data = Fatalities_demeaned)
summary(fatal_fe_1)
fatal_fe_lm_mod <- lm(fatal_rate ~ beertax +year+ state-1 , data = Fatalities)
summary(fatal_fe_lm_mod)
fatal_fe_mod <- plm(fatal_rate ~ beertax+year, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")
summary(fatal_fe_mod)


# RANDOM EFFECTS MODEL ----------------------------------------------------

fatal_re_mod <-  plm(fatal_rate ~ beertax+year, 
                     data = Fatalities,
                     index = c("state", "year"), 
                     model = "random")
summary(fatal_re_mod)

#THE RANDOM EFFECTS MODEL HAS A STRONG ASSUMPTION (Cov(ai,Xit=0)), WHICH RARELY
#HOLDS. SO, IT'S UNLIKELY TO BEST THE BEST CHOICE.
#TO TEST BETWEEN RE AND FE WE CAN PERFORM A HAUSMAN-TEST


# HAUSMAN TEST ------------------------------------------------------------




phtest(fatal_fe_mod, fatal_re_mod)
#NULL HYPOTHESIS: THE PREFERRED MODEL IS THE RANDOM ONE.

#HAUSMAN TEST MANUALLY (SINGLE COEFFICIENT)


fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")

fatal_re_mod <-  plm(fatal_rate ~ beertax, 
                     data = Fatalities,
                     index = c("state", "year"), model = 'random')
                     
numerator= (fatal_fe_mod$coefficients - fatal_re_mod$coefficients[2])**2 
denominator= vcov(fatal_fe_mod) - vcov(fatal_re_mod)[2,2]
W=numerator/denominator
W
pchisq(W,df=1, lower.tail = FALSE)
phtest(fatal_fe_mod, fatal_re_mod)



# ENHANCING OUR MODEL -----------------------------------------------------

#SO FAR WE HAVEN'T USED ECONOMIC CONDITIONS AND DRINKING LAWS, BUT THESE VARIABLES ARE AVAILABLE

# discretize the minimum legal drinking age
Fatalities$drinkagec <- cut(Fatalities$drinkage,
                            breaks = 18:22, 
                            include.lowest = TRUE, 
                            right = FALSE)
?cut
# set minimum drinking age [21, 22] to be the baseline level
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")


# mandaTory jail or community service?
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")))

# the set of observations on all variables for 1982 and 1988
Fatalities_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988), ]

# estimate all seven models
fatalities_mod1 <- lm(fatal_rate ~ beertax, data = Fatalities)

fatalities_mod2 <- plm(fatal_rate ~ beertax + state, data = Fatalities)

fatalities_mod3 <- plm(fatal_rate ~ beertax + state + year,
                       index = c("state","year"),
                       model = "within",
                       effect = "twoways", 
                       data = Fatalities)

fatalities_mod4 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod5 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles,
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod6 <- plm(fatal_rate ~ beertax + year + drinkage 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod7 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities_1982_1988)

library(stargazer)

# gather clustered standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(fatalities_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod6, type = "HC1"))),
               sqrt(diag(vcovHC(fatalities_mod7, type = "HC1"))))

# generate the table
stargazer(fatalities_mod1, fatalities_mod2, fatalities_mod3, 
          fatalities_mod4, fatalities_mod5, fatalities_mod6, fatalities_mod7, 
          digits = 3,
          header = FALSE,
                    se = rob_se,
          title = "Linear Panel Regression Models of Traffic Fatalities due to Drunk Driving",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"), 
          type='html', out='table.html')







# HANDS ON!!! -------------------------------------------------------------

#1)
#CONSIDER THE GENERAL MODEL PRESENTED ON THE SECOND SLIDE.
#SUPPOSE WE ARE INTERESTED IN CHECK THE EFFECT OF CRIME RATES IN HOUSE PRICES ACROSS CITIES.
#GENERATE A RANDOM PANEL WITH DATA FROM 10 CITIES ACROSS 6 YEARS AND ESTIMATE ALL
#THE MODELS WE SAW IN THIS LESSON (POOLED OLS, FD, FE, RE) INCLUDING JUST ENTITY EFFECTS.
#FD AND FE MUST BE ESTIMATED MANUALLY AND USING THE INBUILT FUNCTION. THE VALUES MUST BE THE SAME!
# REPEAT EVERYTHING INCLUDING ALSO TIME EFFECTS.
# DON'T FORGET TO INTERPRET THE MODELS
# PERFORM THE HAUSMAN TEST

##Some Instructions:
  #Generate a vector with random cities (names repeated 8 times)
  #Generate the vector X (crimes rates, size=60) drawing from any normal
  #Generate the vector of alphas (city characteristics, size=10)
  #Vector of gammas, size=6, variable that is time dependent
  #Choose values to B0, B1 and generate the error term(N(0,1))
  #Create the dataframe term and have fun!

#2) Reduce your dataframe to just TWO time periods and check that, in this case,
# FE and FD are identical

#3) CHALLENGE TO GAIN EXTRA POINTS:
#PERFORM THE HAUSMAN TEST CONSIDERING ESTIMATIONS WITH MORE THAN ONE COEFFICIENT 
#WITHOUT USING THE INBUILT FUNCTION, THAT IS, MANUALLY!
#HINT: TAKE A LOOK IN OUR FIRST LESSON.


