library(AER) #Provides some datasets
library(MASS) #Collection of functions for applied statistics
install.packages("MASS")

# ASSIGNMENT - LESSON 2 ------------------------------------------------

#a)Draw a sample of size=100 from a Standard Normal Distribution

sample1 <- rnorm(100)

#b)Perform a t-test "by hand" (without the R function)
t_by_hand <- mean(sample1)/sqrt(var(sample1)/100)
t_by_hand

#c)Perform the t-test using the function from R and compare the values
t.test(sample1)

t.test(sample1)$statistic==mean(sample1)/sqrt(var(sample1)/100) #Check!

#d)Interpret the result of the t-test


#NUll hypothesis: true mean is equal to zero
#According to the statistics, we do not reject the null hypothesis at the level of 95%

#d) Repeat step a) 1000 times, storing the mean on a vector and plot the histogram

#2 ways
#1st - function replicate
vector1 <- replicate(expr = mean(rnorm(100)), n=1000)
hist(vector1)

#2nd way - using a loop

vector2 <- numeric(1000)
for (i in 1:1000) {
  vector2[i] <- mean(rnorm(100))
  
}

hist(vector2)
par(mfrow=c(1,2)) #Command to plot more than 1 figure at the same time
hist(vector1)
hist(vector2)
par(mfrow=c(1,1)) #Back to default

# SIMPLE LINEAR REGRESSION ------------------------------------------------

STH <- c(12,15,10,20,8,12,16,13,5,9,12) #Hours studied
TestScore <- 0.5*STH + rnorm(11,0,1) #Exam score as a function of studied hours + random error

#A simple linear regression is Y=B1.X + e

plot(STH, TestScore, xlim = c(0,20), ylim = c(0,15))
abline(a=0, b=0.5)  #a=intercept b=slope. 

#Including an intercept
TestScore <- 2+ 0.5*STH + rnorm(11,0,1) # Data Generation Process of Exam score
plot(STH, TestScore, xlim = c(0,20), ylim = c(0,15))
abline(a=2, b=0.5)  


# ESTIMATING THE COEFFICIENTS ---------------------------------------------

##Exploring the Dataset
data(CASchools) #california Schools Dataset
View(CASchools)
###Create a new Variable, Students-teacher ratio.

#Compute STR and append it to CASchools
CASchools$STR <- CASchools$students/CASchools$teachers 

#Compute TestScore (average between Math and Read) and append it to the dataframe
CASchools$score <- (CASchools$read + CASchools$math)/2     

#Compute sample averages of STR and score
avg_STR <- mean(CASchools$STR) 
avg_score <- mean(CASchools$score)

#Compute sample standard deviations of STR and score
sd_STR <- sd(CASchools$STR) 
sd_score <- sd(CASchools$score)

#Creating a summary dataframe

# set up a vector of percentiles and compute the quantiles 
quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
quant_STR <- quantile(CASchools$STR, quantiles) #Arguments: series and vector of percentiles
quant_score <- quantile(CASchools$score, quantiles)

# gather everything in a data.frame 
DistributionSummary <- data.frame(Average = c(avg_STR, avg_score), 
                                  StandardDeviation = c(sd_STR, sd_score), 
                                  quantile = rbind(quant_STR, quant_score))
#rbind combines two or more vectors

DistributionSummary

#Plot
plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")

cor(CASchools$STR, CASchools$score) #Correlation. Negative, but weak

###OLS ESTIMATION 

#1ST - BY HAND!
attach(CASchools) # allows to use the variables contained in CASchools directly.
#BE CAREFUL! REMEMBER TO DETACH

#B1 hat
beta_1 <- sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2)


#B0 hat
beta_0 <- mean(score) - beta_1 * mean(STR)

# print the results to the console 
beta_1
beta_0

detach(CASchools)

#2ND- USING THE PACKAGE
linear_model <- lm(score ~ STR, data = CASchools) 
summary(linear_model)

linear_model$coefficients
beta_0 ; beta_1
#The same values!

# plot the data
plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)",
     xlim = c(10, 30),
     ylim = c(600, 720))

# add the regression line
abline(linear_model) 




# MEASURES OF FIT ---------------------------------------------------------

summary(linear_model)
ls(summary(linear_model)) #Check available outputs
r_squared <- summary(linear_model)$"r.squared" #Accessing a single output
mod_summary <- summary(linear_model) #Put the outputs in an object


attach(CASchools)

#R^2 by hand
SSR <- sum(mod_summary$residuals^2)
TSS <- sum((score - mean(score))^2)
R2 <- 1 - SSR/TSS; R2 
summary(linear_model)$"r.squared" 
#Same value!
detach(CASchools)




# THE LEAST SQUARE ASSUMPTIONS --------------------------------------------

###ASSUMPTION 1

# set a seed to make the results reproducible
set.seed(321)

#Generate the data 
X <- runif(50, min = -5, max = 5)
u <- rnorm(50, 0,1)  

# Data generating process (The true relationship)
Y <- X^2 + 2 * X + u                

# estimate a simple regression model 
mod_simple <- lm(Y ~ X)

# predict using a quadratic model 
prediction <- predict(lm(Y ~ X + I(X^2)), data.frame(X = sort(X)))

# plot the results
plot(Y ~ X)
abline(mod_simple, col = "red")
lines(sort(X), prediction)
#E(Ei|Xi) varies with the Xi.


###ASSUMPTION 2

# set seed
set.seed(123)

# generate a date vector
Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")

# initialize the employment vector
X <- c(5000, rep(NA, length(Date)-1))

# generate time series observations with random influences
for (i in 2:length(Date)) {
  
  X[i] <- -50 + 0.98 * X[i-1] + rnorm(n = 1, sd = 200)
  
}

#plot the results
plot(x = Date, 
     y = X, 
     type = "l", 
     col = "steelblue", 
     ylab = "Workers", 
     xlab = "Time")

#The  level of today's employment is correlated with tomorrows employment level. 
#Thus, the i.i.d. assumption is violated.

##Assumption 3

# set seed
set.seed(123)

# generate the data
X <- sort(runif(10, min = 30, max = 70))
Y <- rnorm(10 , mean = 200, sd = 50)
Y[9] <- 2000  #Create an outlier


# fit model with outlier
fit <- lm(Y ~ X)

# fit model without outlier
fitWithoutOutlier <- lm(Y[-9] ~ X[-9])

# plot the results

plot(Y ~ X,pch=16)
abline(fit)
abline(fitWithoutOutlier, col = "red")


# HANDS ON! MONTE CARLO SIMULATIONS  -------------------------------------------------

#We can check the unbiasedness and consistency of OLS estimators via MONTE CARLO simulations

#Routine:
#1-Consider the model Y=b0+b1*x+u
#2-Define a value to the parameters b0 and b1 creating these objects
#3-Generate the variable x, drawing a sample (size=50) from a normal distribution
#4-Generate the variable u, drawing from the standard normal distribution
#5-Create the variable Y=b0+b1*x+u
#6-Estimate the parameters via OLS.
#7-Get the coefficients. The values might be close to the parameters.
#SKIP NEXT STEP
#8-Check how close they are, performing a t-test and interpreting it.Tip: use pnorm()

#Simulations:
#Repeat the previous routine (steps 3 to 7) 1000 times. Tip: use "for" 
#For every estimation, store the coefficients b0 and b1 in empty vectors previously created.
#Get the mean of each vector (b0 and b1). 
#Are they close to the true parameters? Are the OLS estimators biased?
#Plot the histogram of each of them

#REPEAT ALL THE SIMULATIONS, BUT WITH SAMPLE SIZE EQUAL TO 200, THEN EQUAL TO 1000.
#Plot the 3 histograms (sample size =50, 200 and 1000) of b0 side by side to compare.
#Do the same with the 3 histograms of b1.
#TIP: USE THE SAME RANGE ON X-AXIS (argument xlim= ) TO A BETTER COMPARISON
#Are the OLS estimators consistent?


