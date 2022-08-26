
# ASSIGNMENT - LESSON 06 --------------------------------------------------

library(MASS)
data("Boston")
View(Boston)
attach(Boston)
model1 <- lm(medv~lstat+crim+age)
summary(model1) #Summary of model
summary(model1)$coefficients #Coefficients, SE, T-statistic, p-value

###1) HYPOTHESIS TESTING
##1.1 Compute t-statistic for each coefficient
tstat=numeric(4) #Empty vector
names(tstat) <- names(model1$coefficients) #Set names
tstat
#1st - automatically

for (i in 1:4) {
  tstat[i] <- summary(model1)$coefficients[i,3]
  
}
tstat
#2nd Manually

tstat2 <- numeric(4)
names(tstat2) <- names(model1$coefficients)
for (i in 1:4) {
  tstat2[i] <- model1$coefficients[i]/summary(model1)$coefficients[i,2]
  
}
tstat2
tstat2==tstat #Vectors are equal

##1.2 P-value
#Automatically
pval <- numeric(4)
names(pval) <- names(model1$coefficients)
for (i in 1:4) {pval[i] <- summary(model1)$coefficients[i,4]}


#Manually
pval2 <- numeric(4)
names(pval2) <- names(model1$coefficients)
for (i in 1:4) {
  pval2[i] <- 2*pt(-abs(summary(model1)$coefficients[i,3]), df=nrow(Boston)-3-1) #Pnorm also works here
  
}
pval2==pval
pval
##1.3 Significance level
pval<0.01


###2) CONFIDENCE INTERVAL
library(AER)

confint(model1, level = 0.99)

confint(model1, level = 0.98)


###3) ROBUST HYPOTHESIS TESTING

##2.1 Print
coeftest(model1, vcoc.=vcovHC)
##2.2 AccesS the coefficients to test at 99%
coeftest(model1, vcoc.=vcovHC)[,4]<0.01

###4) JOINT HYPOTHESIS TESTING

##4.1 Estimate the restricted model and save it in `model_res`
model_res <- lm(medv ~ lstat + I(crim + age), data = Boston)
#We are considering the coefficients are equal, so they can be merged in one


## 4.2 Compute the SSR of the restricted model and assign it to `RSSR`
RSSR <- sum(model_res$residuals^2)

## 4.3 Estimate the unrestricted model and save it in `model_unres`
model_unres <- lm(medv ~ lstat + crim + age, data = Boston)

## 4.4 Compute the SSR of the unrestricted model and assign it to `USSR`
USSR <- sum(model_unres$residuals^2)

### 5 JOINT HYPOTHESIS TEST 2

## 5.1 Compute the F-statistic and assign it to `Fstat`
Fstat <- ((RSSR-USSR)/1)/(USSR/(nrow(Boston)-3-1))

## 5.2 Compute the p-value and assign it to `pval`
pval <- 1 - pf(Fstat, df1 = 1, df2 = nrow(Boston)-3-1)

## Check whether the null is rejected at the 1% significance level
pval < 0.01


### 6 - CONFIDENCE SET


## 6.1 Construct a 99% confidence set for the coefficients of `crim` and `lstat`
confidenceEllipse(model1, which.coef = c("crim", "lstat"), levels = 0.99)

## 6.2 conduct the corresponding F-test
linearHypothesis(model1, c("crim = 0", "lstat = 0")) #The null is rejected



# LESSON 07 -NONLINEAR REGRESSION FUNCTIONS -------------------------------

# OUR PREVIOUS MODEL
library(AER)                                                    
data(CASchools)
CASchools$size <- CASchools$students/CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math) / 2   

#New variable: income

cor(CASchools$income, CASchools$score)

# fit a simple linear model
linear_model<- lm(score ~ income, data = CASchools)

# plot the observations
plot(CASchools$income, CASchools$score,
     col = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)", 
     ylab = "Test Score",
     cex.main = 0.9,
     main = "Test Score vs. District Income and a Linear OLS Regression Function")

# add the regression line to the plot
abline(linear_model, 
       col = "red", 
       lwd = 2)

# The linear regression line seems to overestimate the true relationship 

# fit the quadratic Model
quadratic_model <- lm(score ~ income + I(income^2), data = CASchools)

# obtain the model summary
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")

# draw a scatterplot of the observations for income and test score
plot(CASchools$income, CASchools$score,
     col  = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(linear_model, col = "black", lwd = 2)

# add quadratic function to the plot
order_id <- order(CASchools$income)

lines(x = CASchools$income[order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "red", 
      lwd = 2) 

#Which line fits better?

#We can generalize the model to a polynomial of degree r
# estimate a cubic model
cubic_model <- lm(score ~ poly(income, degree = 3, raw = TRUE), data = CASchools)

summary(cubic_model) #We do not reject b3=0

coeftest(cubic_model, vcov. = vcovHC, type = "HC1")

##INTERPRETATION OF COEFFICIENTS - COMPARING TWO PREDICTIONS


# set up data for prediction- Changing income in 1 point
new_data <- data.frame(income = c(10, 11))

# do the prediction
Y_hat <- predict(quadratic_model, newdata = new_data)

# compute the difference
diff(Y_hat)

# Again, changin 1 point
new_data <- data.frame(income = c(40, 41))

# do the prediction
Y_hat <- predict(quadratic_model, newdata = new_data)

# compute the difference
diff(Y_hat)

### LOGARITHMS ###

#1- LEVEL-LOG MODEL

# estimate a level-log model
LinearLog_model <- lm(score ~ log(income), data = CASchools)

# compute robust summary
coeftest(LinearLog_model, 
         vcov = vcovHC, type = "HC1")

# draw a scatterplot
plot(score ~ income, 
     col = "steelblue",
     pch = 20,
     data = CASchools,
     main = "Linear-Log Regression Line")

# add the linear-log regression line
order_id  <- order(CASchools$income)

lines(CASchools$income[order_id],
      fitted(LinearLog_model)[order_id], 
      col = "red", 
      lwd = 2)

summary(LinearLog_model)
# HOW TO INTERPRET THE COEFFICIENT?
#1% increase in income is associated with 0.01*36.42
0.01*36.42
# set up new data
new_data <- data.frame(income = c(10, 11, 40, 41))

# predict the outcomes 
Y_hat <- predict(LinearLog_model, newdata = new_data)

# compute the expected difference
Y_hat_matrix <- matrix(Y_hat, nrow = 2, byrow = TRUE)
Y_hat_matrix[, 2] - Y_hat_matrix[, 1]


#2- LOG-LEVEL MODEL

# estimate a log-linear model 
LogLinear_model <- lm(log(score) ~ income, data = CASchools)

# obtain a robust coefficient summary
coeftest(LogLinear_model, 
         vcov = vcovHC, type = "HC1")

#HOW TO INTERPRET?

#3- LOG-LOG MODEL

# estimate the log-log model
LogLog_model <- lm(log(score) ~ log(income), data = CASchools)

# print robust coefficient summary to the console
coeftest(LogLog_model, 
         vcov = vcovHC, type = "HC1")

#HOW TO INTERPRET?


# generate a scatterplot
plot(log(score) ~ income, 
     col = "steelblue", 
     pch = 20, 
     data = CASchools,
     main = "Log-Linear Regression Function")

# add the log-linear regression line
order_id  <- order(CASchools$income)


lines(CASchools$income[order_id], 
      fitted(LogLinear_model)[order_id], 
      col = "red", 
      lwd = 2)

# add the log-log regression line
lines(sort(CASchools$income), 
      fitted(LogLog_model)[order(CASchools$income)], 
      col = "green", 
      lwd = 2)

# add a legend
legend("bottomright",
       legend = c("log-linear model", "log-log model"),
       lwd = 2,
       col = c("red", "green"))




#We can mix the polynomial with log

# estimate the polylog model
polyLog_model <- lm(score ~ log(income) + I(log(income)^2) + I(log(income)^3), 
                    data = CASchools)

# print robust summary to the console
coeftest(polyLog_model, 
         vcov = vcovHC, type = "HC1")

# generate a scatterplot
plot(score ~ income, 
     data = CASchools,
     col = "steelblue", 
     pch = 20,
     main = "Linear-Log and Cubic Regression Functions")

# add the linear-log regression line
order_id  <- order(CASchools$income)

lines(CASchools$income[order_id],
      fitted(LinearLog_model)[order_id], 
      col = "darkgreen", 
      lwd = 2)

# add the cubic regression line
lines(x = CASchools$income[order_id], 
      y = fitted(cubic_model)[order_id],
      col = "darkred", 
      lwd = 2)

# add a legend
legend("bottomright",
       legend = c("linear-log model", "cubic model"),
       lwd = 2,
       col = c("darkgreen", "darkred"))

#Although they seem to be similar, linear-log may be preferable 


### INTERACTIONS BETWEEN VARIABLES ###
## wE WILL CONSIDER 3 CASES:
# INTERACTION BETWEEN TWO BINARY VARIABLES
# BETWEEN BINARY AND CONTINUOUS
# BETWEEN TWO CONTINUOUS

#CREATING TWO DUMMIES
# append HiSTR to CASchools
CASchools$HiSTR <- as.numeric(CASchools$size >= 20)

# append HiEL to CASchools
CASchools$HiEL <- as.numeric(CASchools$english >= 10)

# estimate the model with a binary interaction term
bi_model <- lm(score ~ HiSTR * HiEL, data = CASchools)

summary(bi_model) #single variables and interaction included

#How to interpret?

# Robust summary of the coefficients
coeftest(bi_model, vcov. = vcovHC, type = "HC1")

#Let's predict all possible combinations
# estimate means for all combinations of HiSTR and HiEL

# 1.
predict(bi_model, newdata = data.frame("HiSTR" = 0, "HiEL" = 0))

# 2.
predict(bi_model, newdata = data.frame("HiSTR" = 0, "HiEL" = 1))

# 3.
predict(bi_model, newdata = data.frame("HiSTR" = 1, "HiEL" = 0))

# 4.
predict(bi_model, newdata = data.frame("HiSTR" = 1, "HiEL" = 1))

#2nd CONTUNUOUS AND BINARY

# estimate the model
bci_model <- lm(score ~ size + HiEL + size * HiEL, data = CASchools)
summary(bci_model)

#Interpretation?

# Robust summary of coefficients
coeftest(bci_model, vcov. = vcovHC, type = "HC1")

#It's possible to plot both cases

# identify observations with PctEL >= 10
id <- CASchools$english >= 10

# plot observations with HiEL = 0 as red dots
plot(CASchools$size[!id], CASchools$score[!id],
     xlim = c(0, 27),
     ylim = c(600, 720),
     pch = 20,
     col = "red",
     main = "",
     xlab = "Class Size",
     ylab = "Test Score")

# plot observations with HiEL = 1 as green dots
points(CASchools$size[id], CASchools$score[id],
       pch = 20,
       col = "green")

# read out estimated coefficients of bci_model
coefs <- bci_model$coefficients

# draw the estimated regression line for HiEL = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# draw the estimated regression line for HiEL = 1
abline(coef = c(coefs[1] + coefs[3], coefs[2] + coefs[4]),
       col = "green", 
       lwd = 1.5 )

# add a legend to the plot
legend("topright", 
       pch = c(20, 20), 
       col = c("red", "green"), 
       legend = c("HiEL = 0", "HiEL = 1"))

## 3rd - CONTINUOUS x CONTINUOUS

# Interaction between 'PctEL' and 'size'
cci_model <- lm(score ~ size + english + english * size, data = CASchools) 
summary(cci_model)

#Interpretation
summary(CASchools$english)

#If pctEL is at its median (8.78), the slope of the regression function is:
# -1.12 + 0.0012*8,78=-1.11


## DIFFERENT MODELS TO TEST EFFECTS ON SCORE


# estimate all models
TestScore_mod1 <- lm(score ~ size + english + lunch, data = CASchools)
summary(TestScore_mod1)
TestScore_mod2 <- lm(score ~ size + english + lunch + log(income), data = CASchools)
summary(TestScore_mod2)
TestScore_mod3 <- lm(score ~ size + HiEL + HiEL:size, data = CASchools)
summary(TestScore_mod3)
TestScore_mod4 <- lm(score ~ size + HiEL + HiEL:size + lunch + log(income), data = CASchools)
summary(TestScore_mod4)
TestScore_mod5 <- lm(score ~ size + I(size^2) + I(size^3) + HiEL + lunch + log(income), data = CASchools)
summary(TestScore_mod5)
TestScore_mod6 <- lm(score ~ size + I(size^2) + I(size^3) + HiEL + HiEL:size + HiEL:I(size^2) + HiEL:I(size^3) + lunch + log(income),
                     data = CASchools)
summary(TestScore_mod6)
TestScore_mod7 <- lm(score ~ size + I(size^2) + I(size^3) + english + lunch + log(income), data = CASchools)
summary(TestScore_mod7)
# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(TestScore_mod1, type = "HC1"))),
               sqrt(diag(vcovHC(TestScore_mod2, type = "HC1"))),
               sqrt(diag(vcovHC(TestScore_mod3, type = "HC1"))),
               sqrt(diag(vcovHC(TestScore_mod4, type = "HC1"))),
               sqrt(diag(vcovHC(TestScore_mod5, type = "HC1"))),
               sqrt(diag(vcovHC(TestScore_mod6, type = "HC1"))),
               sqrt(diag(vcovHC(TestScore_mod7, type = "HC1"))))
library(stargazer)
# generate a LaTeX table of regression outputs
stargazer(TestScore_mod1, 
          TestScore_mod2, 
          TestScore_mod3, 
          TestScore_mod4, 
          TestScore_mod5, 
          TestScore_mod6, 
          TestScore_mod7,
          digits = 3,
          dep.var.caption = "Dependent Variable: Test Score",
          se = rob_se,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"), type = "text")

#INTERPRETATIONS?
summary(TestScore_mod7)

# HANDS ON!!! -------------------------------------------------------------

#"Repeat" the previous example with the Boston Dataset!
#Considering the model from the previous assignment, estimate different models
#including quadratic/cubic variables, interaction between variables e log of it.
#Print a table comparing all the models and INTEPRET ALL OF THEM.

#Do the same procedure but considering log(medv) as dependent variable.




