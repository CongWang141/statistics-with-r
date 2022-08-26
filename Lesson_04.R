
# ASSIGNMENT LESSON 3 -----------------------------------------------------


#Check unbiasedness and consistency of OLS estimators via MONTE CARLO simulations

#Routine:
#1-Consider the model Y=b0+b1*x+u
#2-Define a value to the parameters b0 and b1 creating these objects

b0=3
b1=4
#3-Generate the variable x, drawing a sample (size=50) from a normal distribution
x=rnorm(n=50, mean = 5, sd=1)
#4-Generate the variable u, drawing from the standard normal distribution
u=rnorm(50)
#5-Create the variable Y=b0+b1*x+u
Y=b0+b1*x+u
#6-Estimate the parameters via OLS.
model1 <- lm(Y~x)
#7-Get the coefficients. The values might be close to the parameters.
model1$coefficients
model1$coefficients[1] #b0
model1$coefficients[2] #b1


#Simulations:
#Repeat the previous routine (steps 3 to 7) 1000 times. Tip: use "for" 
#For every estimation, store the coefficients b0 and b1 in empty vectors previously created.
vector_b0 <- numeric(1000)
vector_b1 <- numeric(1000)
for (i in 1:1000) {
  x=rnorm(50, mean = 5, sd=1) 
  u=rnorm(50) 
  Y=b0+b1*x+u 
  model1 <- lm(Y~x)
  vector_b0[i] <- model1$coefficients[1] #get the estimate of b0 and store
  vector_b1[i] <- model1$coefficients[2] #get the estimate of b1 and store
}



#Get the mean of each vector (b0 and b1). 
mean(vector_b0)
mean(vector_b1)
#Are they close to the true parameters? Are the OLS estimators biased?

#Plot the histogram of each of them
par(mfrow=c(1,2))
hist(vector_b0, main = expression(beta[0]),col='steelblue')
hist(vector_b1, main = expression(beta[1]), col = 'red')

#REPEAT ALL THE SIMULATIONS, BUT WITH SAMPLE SIZE EQUAL TO 200, THEN EQUAL TO 1000.
# SAMPLE SIZE=200
vector_b0_200 <- numeric(1000)
vector_b1_200 <- numeric(1000)
for (i in 1:1000) {
  x=rnorm(200, mean = 5, sd=1) 
  u=rnorm(200) 
  Y=b0+b1*x+u 
  model1 <- lm(Y~x)
  vector_b0_200[i] <- model1$coefficients[1] #get the estimate of b0 and store
  vector_b1_200[i] <- model1$coefficients[2] #get the estimate of b1 and store
}

#SAMPLE SIZE =1000
vector_b0_1000 <- numeric(1000)
vector_b1_1000 <- numeric(1000)
for (i in 1:1000) {
  x=rnorm(1000, mean = 5, sd=1) 
  u=rnorm(1000) 
  Y=b0+b1*x+u 
  model1 <- lm(Y~x)
  vector_b0_1000[i] <- model1$coefficients[1] #get the estimate of b0 and store
  vector_b1_1000[i] <- model1$coefficients[2] #get the estimate of b1 and store
}

#Plot the 3 histograms (sample size =50, 200 and 1000) of b0 side by side to compare.
#Do the same with the 3 histograms of b1.
#TIP: USE THE SAME RANGE ON X-AXIS (argument xlim= ) TO A BETTER COMPARISON
par(mfrow=c(1,3))
hist(vector_b0,xlim = c(1,6), main = "Sample=50")
hist(vector_b0_200, xlim = c(1,6),main = "Sample=200")
hist(vector_b0_1000,xlim = c(1,6), main = "Sample=1000")
#b1
hist(vector_b1, xlim = c(3.4,4.6),main = "Sample=50")
hist(vector_b1_200, xlim = c(3.4,4.6),main = "Sample=200")
hist(vector_b1_1000, xlim = c(3.4,4.6),main = "Sample=1000")

par(mfrow=c(1,1))
#Are the OLS estimators consistent?




##HYPOTHESIS TEST AND CONFIDENCE INTERVALS IN THE SIMPLE LINEAR REGRESSION MODEL-----
library(AER)
library(scales)

####Testing two-sided hypothesis



# load the `CASchools` dataset
data(CASchools)

# add student-teacher ratio
CASchools$STR <- CASchools$students/CASchools$teachers

# add average test-score
CASchools$score <- (CASchools$read + CASchools$math)/2

# T-TEST OF ESTIMATES
# t=(estimated value - hypothesized value)/standard error of the estimator
# estimate the model and get coefficients
linear_model <- lm(score ~ STR, data = CASchools) 
linear_model$coefficients
B1=linear_model$coefficients[2]

#STANDARD ERROR OF B1hat
summary(linear_model)
ls(summary(linear_model)) #ls gives us the possible outcomes
summary(linear_model)$coefficients  #just "coef" can be used as well
summary(linear_model)$coef
linear_model$coefficients
coef1=summary(linear_model)$coefficients

coef1[2] #B1
coef1[,2] #Access second column, standard errors
SEB1= coef1[,2][2] #Second element of second column
#or
SEB11=coef1[4] #4th element in general
SEB1==SEB11


#T-statistic manually
(t_by_hand=(B1-0)/SEB1)
coef1[,3][2] #Access from regression results
(t_by_hand=(B1-0)/SEB1)==coef1[,3][2] #Same value


#Get the p-value manually

#Degrees of freedom = n-k-1
df=420-1-1

p_value=2*pt(t_by_hand, df=df) #Pt function provides the student distribution
p_value
#NOTE: As we are dealing with a large sample, the normal density could be used as well:
2*pnorm(t_by_hand)
#We reject the null and conclude that the coefficient is significantly different from zero.
#Null: the class size has no influence on the students test scores at 5% level.




### PLOTTING ###

# Plot the standard normal on the support [-6,6]
t <- seq(-6, 6, 0.01)

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     yaxs = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=-4.75"), 
     cex.lab = 0.7,
     cex.main = 1)

tact <- -4.75

axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)

# Shade the critical regions using polygon():

# critical region in left tail
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0), 
        col = 'orange')

# critical region in right tail

polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0), 
        col = 'orange')

# Add arrows and texts indicating critical regions and the p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, -4.75, 0, length = 0.1)
arrows(5, 0.16, 4.75, 0, length = 0.1)

text(-3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)

text(-5, 0.18, 
     labels = expression(paste("-|",t[act],"|")), 
     cex = 0.7)
text(5, 0.18, 
     labels = expression(paste("|",t[act],"|")), 
     cex = 0.7)

# Add ticks indicating critical values at the 0.05-level, t^act and -t^act 
rug(c(-1.96, 1.96), ticksize  = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize  = -0.0451, lwd = 2, col = "darkgreen")



# CONFIDENCE INTERVAL FOR REGRESSION COEFFICIENTS -------------------------

#GENERATE A SAMPLE

set.seed(1)

Y <- rnorm(n = 100, 
           mean = 5, 
           sd = 5)


plot(Y, 
     pch = 19, 
     col = "steelblue")

#Assuming Yi= mu + ei, the confidence interval is

cbind(CIlower = mean(Y) - 1.96 * 5 / 10, CIupper = mean(Y) + 1.96 * 5 / 10) #cbind combine objects

#From the same intuition, we can have confidence interval in our previous linear model
linear_model$coefficients
confint(linear_model)

#Let's do it "by hand"

lm_summ <- summary(linear_model)
# qt gives the quantile function for the t distribution

c("lower" = lm_summ$coef[2,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2],
  "upper" = lm_summ$coef[2,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2])




# DUMMY REGRESSION --------------------------------------------------------


# Create the dummy variable 
CASchools$D <- CASchools$STR < 20

# Plot the data
plot(CASchools$D, CASchools$score,            
     pch = 20,                                
     cex = 0.6,   #set size of plot symbols to 0.6
     col = "Steelblue",                       
     xlab = expression(D[i]),                 
     ylab = "Test Score",
     main = "Dummy Regression")

#Model
dummy_model <- lm(score ~ D, data = CASchools)
summary(dummy_model)

#A
points(x = CASchools$D,  #Plot the two predicted points
       y = predict(dummy_model), 
       col = "red", 
       pch = 20,
       cex=1.2)

confint(dummy_model) #Confidence intervals



# HETEROSKEDASTICITY AND HOMOSKEDASTICITY ---------------------------------


#All inference made so far relies on the assumption that error variance does 
#not vary as regressors values changes

#Plotting an example

# load scales package for adjusting color opacities
library(scales)

# generate some heteroskedastic data:

# set seed for reproducibility
set.seed(1) 

# set up vector of x coordinates
x <- rep(c(10, 15, 20, 25), each = 25)

#Vector of errors
e <- c() # Empty Vector. We could also use numeric(100), as before

# sample 100 errors such that the variance increases with x
e[1:25] <- rnorm(25, sd = 10)
e[26:50] <- rnorm(25, sd = 20)
e[51:75] <- rnorm(25, sd = 30)
e[76:100] <- rnorm(25, sd = 40)

# set up y
y <- 720 - 3.3 * x + e

# Estimate the model 
mod <- lm(y ~ x)

# Plot the data
plot(x = x, 
     y = y, 
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score",
     cex = 0.5, 
     pch = 19, 
     xlim = c(8, 27), 
     ylim = c(600, 710))

# Add the regression line to the plot
abline(mod, col = "darkred")

# Add boxplots to the plot
boxplot(formula = y ~ x, 
        add = TRUE, 
        at = c(10, 15, 20, 25), 
        col = alpha("gray", 0.4), 
        border = "black")


## Real world example

# load package and attach data
library(AER)
data("CPSSWEducation")
attach(CPSSWEducation)

#Overview
summary(CPSSWEducation)

# estimate a simple regression model
labor_model <- lm(earnings ~ education)

# plot observations and add the regression line
plot(education, 
     earnings, 
     ylim = c(0, 150))

abline(labor_model, 
       col = "steelblue", 
       lwd = 2)

detach(CPSSWEducation)

Confint(labor_model)
summary(labor_model)

#How to test heteroskedasticity?
#Plotting the residuals
plot(mod$residuals) #Evident by the picture
plot(labor_model$residuals) #Inconclusive by the picture

#Breausch-Pagan test
library(lmtest)
bptest(labor_model) #NUll: The error variances are all equal
bptest(mod)

#How to deal with heteroskedasticity?

# compute heteroskedasticity-robust standard errors
vcov <- vcovHC(linear_model, type = "HC1") #Var-cov matrix with robust s.e.
robust_se <- sqrt(diag(vcov));robust_se #Robust s.e.
summary(linear_model)

#What are the implications of a larger s.e.?

coeftest(linear_model, vcov. = vcov) #t-test for coefficients using robust s.e.

#Another example:

#Yi= Xi +ui, ui~N(0,0.36Xi²)

set.seed(905)

# generate heteroskedastic data 
X <- 1:500
Y <- X+ rnorm(500,0,0.6*X) #Standard deviation depends on X


# estimate a simple regression model
reg <- lm(Y ~ X)

# plot the data
plot(x = X, y = Y, 
     pch = 19, 
     col = "steelblue", 
     cex = 0.8)

# add the regression line to the plot
abline(reg, 
       col = "red", 
       lwd = 2)

# Testing if B1=1
linearHypothesis(reg, hypothesis.matrix = "X = 1") #generic function to test a linear hypothesis
ls(linearHypothesis(reg, hypothesis.matrix = "X = 1")) #possible outputs
#We reject the null at 5% level


# test hypothesis using the robust standard error formula
linearHypothesis(reg, hypothesis.matrix = "X = 1", white.adjust = "hc1")
#we do not reject it.

#Without using robust standard errors we ended-up with a Type I error (false positive).


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

