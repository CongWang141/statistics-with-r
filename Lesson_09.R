library(plm)
# REVIEW - LESSON 08 ------------------------------------------------------

countries= rep(c('Japan', 'China', 'Russia','Italy', 'France'),each=4)
time <- rep(c('2012', '2013','2014','2015'),5)
panel=data.frame(Country=countries, Year=time)
View(panel)
panel$movement <- rnorm(20,5,1)
panel$annual_effect <- rep(c(1,2,2.3,1.5),5)
panel$contagious <-  5 +4*panel$movement+panel$annual_effect+rnorm(20)

attach(panel)
#POLS
lm(contagious~movement+Year)
#Endogeneity!

#FD

plm(contagious~movement+Year, data=panel, model = 'fd')

panel$lag_contagious=ave(contagious, Country, FUN = dplyr::lag)
panel$lag_movement=ave(movement, Country, FUN = dplyr::lag)

panel$diff_contagious=panel$contagious-panel$lag_contagious
panel$diff_movement=panel$movement-panel$lag_movement

lm(diff_contagious~diff_movement+Year)

#FE

lm(contagious~movement+Year+Country)
plm(contagious~movement+Year, data = panel, model = "within")


rep(c(1,2,3),each=3)


# LESSON_09_REGRESSION WITH A BINARY DEPENDENT VARIABLE -------------------

library(AER)
library(stargazer)
data(HMDA)
?HMDA
# convert 'deny' to numeric
HMDA$deny <- as.numeric(HMDA$deny) - 1
# deny is an indicator for whether an applicant's mortgage application has been accepted
# pirat is the size of the anticipated total monthly loan payments relative 
#to the the applicant's income



# 1- LINEAR PROBABILITY MODEL ---------------------------------------------


# estimate a simple linear probability model
denymod1 <- lm(deny ~ pirat, data = HMDA)
denymod1

# plot the data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Scatterplot Mortgage Application Denial and the Payment-to-Income Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add the estimated regression line
abline(denymod1, 
       lwd = 1.8, 
       col = "steelblue")

# print robust coefficient summary
coeftest(denymod1, vcov. = vcovHC, type = "HC1")
#Linear probability models are ALWAYS heteroskedastic.

#B1 is the CHANGE in the probability that Yi=1, holding constant the other regressors

#R2 has no meaningful interpretation.

#According to the estimated model, a payment-to-income ratio of 1 is associated 
#with an expected probability of mortgage application denial of roughly 50%
0.603535*1-0.07991

#An increase of 1% in P/I ratio leads to an increase in the prob. of a loan
#denial by 0.604 * 0.01 = 0.00604 = 0.6%

#Including a new variable

# rename the variable 'afam' 
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"

# estimate the model
denymod2 <- lm(deny ~ pirat + black, data = HMDA)
coeftest(denymod2, vcov. = vcovHC)

#if being black has a significant (positive) influence on the probability of a 
#loan denial when we control for factors that allow for an objective assessment 
#of an applicants credit worthiness, this is an indicator for discrimination.

#What is the major flaw of the linear probability model?


# PROBIT AND LOGIT REGRESSION ---------------------------------------------

####
###PROBIT###
####


# estimate the simple probit model (function glm)
denyprobit <- glm(deny ~ pirat, 
                  family = binomial(link = "probit"), 
                  data = HMDA)

coeftest(denyprobit, vcov. = vcovHC, type = "HC1")
summary(denyprobit)

#Computing the predicted probability (p/i ratio=0.5)

z=(denyprobit$coefficients[1]+denyprobit$coefficients[2]*0.5)
z
pnorm(z)


# plot data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Probit Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.85)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(denyprobit, list(pirat = x), type = "response")

lines(x, y, lwd = 2.5, col = "steelblue")


# 1. compute predictions for P/I ratio = 0.3, 0.4
predictions <- predict(denyprobit, 
                       newdata = data.frame("pirat" = c(0.3, 0.4)),
                       type = "response")

predictions
#type="response" option tells R to output probabilities of the form P(Y = 1|X)

# 2. Compute difference in probabilities
diff(predictions)

#Manually
z1=(denyprobit$coefficients[1]+denyprobit$coefficients[2]*0.3)

pnorm(z1)

z2=(denyprobit$coefficients[1]+denyprobit$coefficients[2]*0.4)

pnorm(z2)

pnorm(z2)-pnorm(z1) #Difference

pnorm(z2)-pnorm(z1)==diff(predictions) #Check!!!

#We find that an increase in the payment-to-income ratio from 0.3 to 0.4
#is predicted to increase the probability of denial by approximately 6.2%.

#Including another variable

denyprobit2 <- glm(deny ~ pirat + black, 
                   family = binomial(link = "probit"), 
                   data = HMDA)

coeftest(denyprobit2, vcov. = vcovHC, type = "HC1")

#African Americans have a higher probability of denial than white applicants,
#holding constant the payments-to-income ratio and second, applicants with a
#high payments-to-income ratio face a higher risk of being rejected.

#PREDICTIONS

# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denyprobit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

# 2. compute difference in probabilities
diff(predictions)

#The value changes in other levels of P/I ratio
predictions <- predict(denyprobit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.5, 0.5)),
                       type = "response")

# 2. compute difference in probabilities
diff(predictions)

####
###LOGIT###
####

denylogit <- glm(deny ~ pirat, 
                 family = binomial(link = "logit"), 
                 data = HMDA)

coeftest(denylogit, vcov. = vcovHC, type = "HC1")

#Difference in probability from p/i ratio 0.3 to 0.4

#Automatically
predictions <- predict(denylogit, 
                       newdata = data.frame("pirat" = c(0.3, 0.4)),
                       type = "response")
diff(predictions)

#Manually (exponential function)
#z= b0+b1x1+...

z1=denylogit$coefficients[1]+denylogit$coefficients[2]*0.3

p1=1/(1+ exp(-z1))
p1

z2=denylogit$coefficients[1]+denylogit$coefficients[2]*0.4
p2=1/(1+ exp(-z2))
p2

p2-p1

#Manually (function plogis)
p11=plogis(z1);p11
p22=plogis(z2);p22


p22-p11


# plot data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Probit and Logit Models Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.9)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line of Probit and Logit models
x <- seq(0, 3, 0.01)
y_probit <- predict(denyprobit, list(pirat = x), type = "response")
y_logit <- predict(denylogit, list(pirat = x), type = "response")

lines(x, y_probit, lwd = 2, col = "steelblue")
lines(x, y_logit, lwd = 2, col = "black", lty = 2)

# add a legend
legend("topleft",
       horiz = TRUE,
       legend = c("Probit", "Logit"),
       col = c("steelblue", "black"), 
       lty = c(1, 2))

# estimate a Logit regression with multiple regressors
denylogit2 <- glm(deny ~ pirat + black, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

coeftest(denylogit2, vcov. = vcovHC, type = "HC1")

# Compute predictions for P/I ratio = 0.3
predictions <- predict(denylogit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

predictions


# Compute difference in probabilities
diff(predictions)

# Compute predictions for P/I ratio = 0.5
predictions <- predict(denylogit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.5, 0.5)),
                       type = "response")

predictions


# Compute difference in probabilities
diff(predictions)

#Both models are quite similar and equally easy to estimate on R.


# APPLICATIONS - REPRODUCING TEXTBOOK EXAMPLE-----------------------------------
?HMDA
# define low, medium and high loan-to-value ratio
HMDA$lvrat <- factor(
  ifelse(HMDA$lvrat < 0.8, "low",
         ifelse(HMDA$lvrat >= 0.8 & HMDA$lvrat <= 0.95, "medium", "high")),
  levels = c("low", "medium", "high"))

# convert credit scores to numeric
HMDA$mhist <- as.numeric(HMDA$mhist)
HMDA$chist <- as.numeric(HMDA$chist)


# estimate all 6 models for the denial probability
lpm_HMDA <- lm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
               + insurance + selfemp, data = HMDA)

logit_HMDA <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                  + insurance + selfemp, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

probit_HMDA_1 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                     + insurance + selfemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_2 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                     + insurance + selfemp + single + hschool + unemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_3 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist 
                     + phist + insurance + selfemp + single + hschool + unemp + condomin 
                     + I(mhist==3) + I(mhist==4) + I(chist==3) + I(chist==4) + I(chist==5) 
                     + I(chist==6), 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_4 <- glm(deny ~ black * (pirat + hirat) + lvrat + chist + mhist + phist 
                     + insurance + selfemp + single + hschool + unemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)


rob_se <- list(sqrt(diag(vcovHC(lpm_HMDA, type = "HC1"))),
               sqrt(diag(vcovHC(logit_HMDA, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_1, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_2, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_3, type = "HC1"))),
               sqrt(diag(vcovHC(probit_HMDA_4, type = "HC1"))))

stargazer(lpm_HMDA, logit_HMDA, probit_HMDA_1, 
          probit_HMDA_2, probit_HMDA_3, probit_HMDA_4,  
          digits = 3,
          header = FALSE,
          se = rob_se,
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),type='html',
          out='mytable.html')



# PREDICTIONS  ------------------------------------------------------------

# comppute regressor values for an AVERAGE black person
new <- data.frame(
  "pirat" = mean(HMDA$pirat),
  "hirat" = mean(HMDA$hirat),
  "lvrat" = "low",
  "chist" = mean(HMDA$chist),
  "mhist" = mean(HMDA$mhist),
  "phist" = "no",
  "insurance" = "no",
  "selfemp" = "no",
  "black" = c("no", "yes"),
  "single" = "no",
  "hschool" = "yes",
  "unemp" = mean(HMDA$unemp),
  "condomin" = "no")

# self-employed
prop.table(table(HMDA$selfemp))

# difference predicted by the LPM
predictions <- predict(lpm_HMDA, newdata = new)
diff(predictions)


# differnce predicted by the logit model
predictions <- predict(logit_HMDA, newdata = new, type = "response")
diff(predictions)


# difference predicted by probit model (3)
predictions <- predict(probit_HMDA_1, newdata = new, type = "response")
diff(predictions)


# difference predicted by probit model (4)
predictions <- predict(probit_HMDA_2, newdata = new, type = "response")
diff(predictions)


# difference predicted by probit model (5)
predictions <- predict(probit_HMDA_3, newdata = new, type = "response")
diff(predictions)


# difference predicted by probit model (6)
predictions <- predict(probit_HMDA_4, newdata = new, type = "response")
diff(predictions)

# HANDS ON! ---------------------------------------------------------------

#Using the admissions dataset, estimate a model to check how the variables of
#test scores (GRE and GPA) and the prestige of the college institute impact
#on a Master's course admission.
#Be careful: rank must be converted in a categorical variable (factor).

#Estimate all 3 models (LPM, LOGIT and PROBIT), interpret, make predictions
#and plot the graph to all of them.

