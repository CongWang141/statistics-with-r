
# ASSIGNMENT --------------------------------------------------------------

# HANDS ON! MONTE CARLO SIMULATIONS ---------------------------------------
# Setting a confidence level of 95%, in about 5% of cases we will wrongly reject the null.
# However, the fraction of rejections will be greater if we do not use robust
# standard errors when errors are heteroskedastic!
#Test this, via Monte Carlo Simulations!

#Loop Routine:
#1 - Consider the simple linear regression model Y=B0+B1*X+u 
#2-Generate X, u and Y and violating the homoskedasticity assumption. Sample size=1000
#3-Estimate the model
#4-Perform t-test for B1 using non-robust and robust standard errors,  
# checking to false rejection. 
#Tip1:Use the function linearHypothesis()
#Tip1: store the results in logic vectors
#Compare the fraction of false rejections in each case
#What are the implications of not using robust standard errors in this case?


# initialize vectors t and t.rob
t <- c()
t.rob <- c()
# loop sampling and estimation
for (i in 1:1000) {
  # sample data
  X <- 1:1000
  u <- rnorm(1000, 0, 0.5*X)
  Y <- 5-3*X+u
  # estimate regression model
  reg <- lm(Y ~ X) 
  # homoskedasdicity-only significance test
  t[i] <- linearHypothesis(reg, "X = -3")$'Pr(>F)'[2] < 0.05 # WRONG REJECTION
  # robust significance test
  t.rob[i] <- linearHypothesis(reg, "X = -3", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05
}

# compute the fraction of false rejections
cbind(t = mean(t), t.rob = mean(t.rob))




# REGRESSION MODELS WITH MULTIPLE REGRESSORS ------------------------------

library(AER)
library(MASS)
library(mvtnorm)


#Let's consider our model with another variable, % of English learners

data(CASchools)


CASchools$STR <- CASchools$students/CASchools$teachers #Size of classes       
CASchools$score <- (CASchools$read + CASchools$math)/2 #Score
mod <- lm(score ~ STR, data = CASchools) 
summary(mod)

#Measure correlations
cor(CASchools$STR, CASchools$score) 
cor(CASchools$STR, CASchools$english) #Possible omitted variable bias.
#What if we estimate the model with both variables?

# estimate multiple regression model

mult.mod <- lm(score ~ STR + english, data = CASchools)
summary(mod)
summary(mult.mod)
#

# MEASURES OF FIT IN MULTIPLE REGRESSION ----------------------------------

#Compute by hand measures provided by the package

# define the components
n <- nrow(CASchools)                            # number of observations (rows)
k <- 2                                          # number of regressors

y_mean <- mean(CASchools$score)                 # mean of avg. test-scores

SSR <- sum(residuals(mult.mod)^2)               # sum of squared residuals
TSS <- sum((CASchools$score - y_mean )^2)       # total sum of squares
ESS <- sum((fitted(mult.mod) - y_mean)^2)       # explained sum of squares

# compute the measures

SER <- sqrt(1/(n-k-1) * SSR)                    # standard error of the regression
Rsq <- 1 - (SSR / TSS)                          # R^2
adj_Rsq <- 1 - (n-1)/(n-k-1) * SSR/TSS          # adj. R^2

# print the measures to the console
c("SER" = SER, "R2" = Rsq, "Adj.R2" = adj_Rsq)


# OLS ASSUMPTIONS IN MULTIPLE REGRESSION ----------------------------------

###MULTICOLINEARITY (2 OR MORE REGRESSORS ARE STRONGLY CORRELATED)

#Strong multicolinearity makes the variance of OLS to be large
#Perfect multicolinearity makes it impossible to estimate the model
#EXAMPLE:

# define the fraction of English learners        
CASchools$FracEL <- CASchools$english / 100 #fRACEL is a linear combination of 'english'
# estimate the model
mult.mod <- lm(score ~ STR + english + FracEL, data = CASchools) 

# obtain a summary of the model
summary(mult.mod)                                                 

#ANOTHER EXAMPLE:
# if STR smaller 12, NS = 0, else NS = 1
CASchools$NS <- ifelse(CASchools$STR < 12, 0, 1) #IFELSE

# estimate the model
mult.mod <- lm(score ~ computer + english + NS, data = CASchools)

# obtain a model summary
summary(mult.mod)         #What happened?

table(CASchools$NS)



# DUMMY VARIABLE TRAP -----------------------------------------------------

set.seed(1)

# generate artificial data on location
CASchools$direction <- sample(c("West", "North", "South", "East"), 
                              420, 
                              replace = T)

# estimate the model
mult.mod <- lm(score ~ STR + english + direction, data = CASchools)
summary(mult.mod)         

#Alternative
install.packages('fastDummies')
library(fastDummies)

CASchools$DUMMIE_DIRECTION <- dummy_cols(CASchools$direction) #Get dummies from a cat. variable
names(CASchools)

mult.mod <- lm(score ~ STR + english + DUMMIE_DIRECTION$.data_North
               +DUMMIE_DIRECTION$.data_South+DUMMIE_DIRECTION$.data_East
               +DUMMIE_DIRECTION$.data_West, data = CASchools)
summary(mult.mod) #R automatically drops one dummy

#The other way is removing the constant
mult.mod <- lm(score ~ 0 +STR + english + direction, data = CASchools)
summary(mult.mod) 

# BIAS-VARIANCE TRADEOFF -------------------------------------------------



#New function: rmvnorm - Draw from multivariate normal density
?rmvnorm
rmvnorm(n=10, mean = c(1,3), sigma = cbind(c(1,5),c(5,2)))

#first column(X1)= N(1,1)
#second column(X2)= N(3,2)
#Cov(x1,x2)=5
var_cov=cbind(c(1,5),c(5,2));var_cov


#Consider the model: Y=5 + 2.5*X1 + 3*X2 + u.
#Let's estimate it changing the value of Cov(X1,X2)

# set number of observations
n <- 50

# initialize vectors of coefficients
coefs1 <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))
coefs2 <- coefs1

# set seed
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  
  # for cov(X_1,X_2) = 0.25
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs1[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
  # for cov(X_1,X_2) = 0.85
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 8.5), c(8.5, 10)))
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs2[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}

# obtain variance estimates
diag(var(coefs1)) #In this case, 
diag(var(coefs2))
cbind(c(10, 8.5), c(8.5, 10))
par(mfrow=c(1,2))
hist(coefs1[,1], xlim = c(1,4), ylim = c(0,2000))
hist(coefs2[,1], xlim = c(1,4), ylim = c(0,2000))
# THE DISTRIBUTION OF OLS ESTIMATORS IN MULTIPLE REGRESSION ---------------



# set sample size
n <- 50

# initialize vector of coefficients
coefs <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))

# set seed for reproducibility
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs[i,] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}

# compute density estimate
kde <- kde2d(coefs[, 1], coefs[, 2])

# plot the density estimate
persp(kde, 
      theta = 310, 
      phi = 30, 
      xlab = "beta_1", 
      ylab = "beta_2", 
      zlab = "Est. Density"
      ,ticktype="detailed")

#Bivariate normal density 
library(mnormt)
x     <- seq(-5, 5, 0.25) 
y     <- seq(-5, 5, 0.25)
mu    <- c(0, 0)
sigma <- matrix(c(2, -1, -1, 2), nrow = 2)
f     <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f) #Outer product. Arguments: X,Y, FUNCTION
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "gold", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")


# HANDS ON! ESTIMATING A MULTIPLE LINEAR REGRESSION MODEL ---------------------

#Load the dataset 'mtcars' (check the help for information about it)
#Check the descriptive statistics of the variables and the correlation
#Estimate a multiple linear model to identify the variables that impact 
#the fuel consumption of a car
#Test the assumption of heteroskedasticity
#Interpret the results of the model
#From the same model, include another variable and repeat the tests and interpretations
#What was the effect of including this new variable? Would you keep it in the model?
#Create a dummy variable and include it in your model, commenting the result.
#Generate random data for a new car and predict the values of it's fuel consumption
#based on your model


