
# ASSIGNMENT LESSON 5 -----------------------------------------------------
dev.new()
data(mtcars)
View(mtcars)
attach(mtcars)
#Estimate the model
model1 <- lm(mpg~cyl+wt)
summary(model1)
plot(model1$residuals)
#Test for heteroskedasticity
library(lmtest)
bptest(model1) #NUll: The error variances are all equal
#New variable
model2 <- lm(mpg~cyl+wt+gear)
summary(model2)
#Create a dummy variable
mtcars$carburetors <- ifelse(carb>2,1,0)
attach(mtcars)
model3 <- lm(mpg~cyl+wt+carburetors)
summary(model3)
#Predict a random car
Panda <- cbind('cyl'=numeric(1), 'wt'=numeric(1))
Panda[1] <-   sample(mtcars$cyl, size=1)+rnorm(1) #Sample + random error term
Panda[2] <-   sample(mtcars$wt, size=1)+rnorm(1)  
Panda <- data.frame(Panda) #Transform into data frame to use function predict.lm
predict.lm(model1, Panda)
?predict
?predict.lm



# Hypothesis Tests and Confidence Intervals in Multiple Regression --------

library(AER)
library(stargazer)
install.packages("stargazer")

data(CASchools)

#Our new variables
CASchools$size <- CASchools$students/CASchools$teachers #Size of classes       
CASchools$score <- (CASchools$read + CASchools$math)/2 #Score

model <- lm(score ~ size + english, data = CASchools)
coeftest(model, vcov. = vcovHC, type = "HC1")

# compute two-sided p-value MANUALLY
t_b1=coeftest(model, vcov. = vcovHC, type = "HC1")[2, 3];t_b1 #t_statistic
p_b1=pt(abs(t_b1),df = model$df.residual);p_b1 #Distribution functions
p_value_b1=2 * (1 - p_b1);p_value_b1



# CONFIDENCE INTERVAL -----------------------------------------------------


model <- lm(score ~ size + english, data = CASchools)
confint(model)
confint(model, level = 0.9)
#WARNING: CONFINT DOES NOT USE ROBUST STANDARD ERRORS

# compute robust standard errors
rob_se <- diag(vcovHC(model, type = "HC1"))^0.5
rob_se

# compute robust 95% confidence intervals
rbind("lower" = coef(model) - qnorm(0.975) * rob_se,
      "upper" = coef(model) + qnorm(0.975) * rob_se)

# compute robust 90% confidence intervals

rbind("lower" = coef(model) - qnorm(0.95) * rob_se,
      "upper" = coef(model) + qnorm(0.95) * rob_se)

#Let's include another variable
# scale expenditure to thousands of dollars
CASchools$expenditure <- CASchools$expenditure/1000

# estimate the model
model <- lm(score ~ size + english + expenditure, data = CASchools)
summary(model)
coeftest(model, vcov. = vcovHC, type = "HC1")

# Multicolinearity issue
cor(CASchools$size, CASchools$expenditure)


# Joint Hypothesis Testing Using the F-Statistic --------------------------

# estimate the multiple regression model
model <- lm(score ~ size + english + expenditure, data = CASchools)

# execute the function on the model object and provide both linear restrictions 
# to be tested as strings
linearHypothesis(model, c("size=0", "expenditure=0"))
#NULL: B1=B2=0

# heteroskedasticity-robust F-test
linearHypothesis(model, c("size=0", "expenditure=0"), white.adjust = "hc1")

#Overall F-test
linearHypothesis(model, c("size=0", "english=0", "expenditure=0"), white.adjust = "hc1")

summary(model)
summary(model)$fstatistic
ls(summary(model))

# Confidence Sets for Multiple Coefficients -------------------------------

# draw the 95% confidence set for coefficients on size and expenditure
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  main = "95% Confidence Set")

# draw the robust 95% confidence set for coefficients on size and expenditure 
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  main = "95% Confidence Sets",
                  vcov. = vcovHC(model, type = "HC1"),
                  col = "red")

# draw the 95% confidence set for coefficients on size and expenditure
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  add = T) #Argument add=T (TRUE)


# ANALYSIS OF OUR DATASET -------------------------------------------------

dev.new() #plot in a new window
# set up arrangement of plots
m <- rbind(c(1, 2), c(3, 0))
graphics::layout(mat = m) #matrix of positions
m

# scatterplots
plot(score ~ english, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20, 
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage of English language learners")

plot(score ~ lunch, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20,
     cex.main = 0.9,
     main = "Percentage qualifying for reduced price lunch")

plot(score ~ calworks, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20, 
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage qualifying for income assistance")


# estimate correlation between student characteristics and test scores
cor(CASchools$score, CASchools$english)
cor(CASchools$score, CASchools$lunch)
cor(CASchools$score, CASchools$calworks)
dev.off()
# COMPARATIVE TABLE -------------------------------------------------------


# load the stargazer library
library(stargazer)

# estimate different model specifications
spec1 <- lm(score ~ size, data = CASchools)
spec2 <- lm(score ~ size + english, data = CASchools)
spec3 <- lm(score ~ size + english + lunch, data = CASchools)
spec4 <- lm(score ~ size + english + calworks, data = CASchools)
spec5 <- lm(score ~ size + english + lunch + calworks, data = CASchools)

# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(spec1, type = "HC1"))),
               sqrt(diag(vcovHC(spec2, type = "HC1"))),
               sqrt(diag(vcovHC(spec3, type = "HC1"))),
               sqrt(diag(vcovHC(spec4, type = "HC1"))),
               sqrt(diag(vcovHC(spec5, type = "HC1"))))

# generate a LaTeX table using stargazer
stargazer(spec1, spec2, spec3, spec4, spec5,
          se = rob_se,
          digits = 3,
          header = F,
          font.size = "tiny",
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)"))

stargazer(spec1, spec2, spec3, spec4, spec5,
          se = rob_se,
          digits = 3,
          header = F,
          font.size = "tiny",
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)"),
          type='text') #type=text shows the table on the console
cor(CASchools$lunch, CASchools$calworks)

# HANDS ON! ---------------------------------------------------------------

#List of exercises

