
# QUICK REVIEW OF LESSON 1 ------------------------------------------------

#Basic operations

a <- 3
b <- 2
c <- a**b
c
d <- (a+3+b)/sqrt(c); d

#Vectors and sequences
v1 <- c(1:20)
v1
v <- c(1,2,v1,3,a,c)
v
v[4]
v2 <- seq(1,15,2)
v2
v3 <- rep(2,5)
v3

#Matrices
M <- matrix(1:30,5,6)
M
M2 <- matrix(v2,4,2, byrow = TRUE)
M2

#Dataframe
df <- data.frame(X=c(1:10), Y=c(5:14), X2=rep(3,10))
df
df$Z <- df$X**2 #new column
plot(df$X, df$Z) #Simple plot
plot(df$X, df$Z, main = "Plot Tile",
     type = "l", xlab = "variable X",col='red') #Options to enhance the plot

#Generate random numbers
rnorm(1, mean = 0, sd=1) #Draw 1 number from N(0,1)
rnorm(5, mean = 10, sd=1) #Draw 5 number from N(10,1)
rbinom(n=20,size=1,prob=.5) #Draw 20 numbers from a Bernoulli(0.5)

# Loop

a <- c(3:7)
b <- c(10:20)
#Create a vector (vector1) that sums a with the 5 first elements of b

vector1 <- 0 #1st, create the vector to be filled
for (i in 1:5) {
  vector1[i] <- a[i]+b[i]
}

vector1



# LESSON 2 - REVIEW OF STATISTICS USING R --------------------------------------------

dice <- c(1:6)
dice
sum(dice)
length(dice)
mean_dice <- sum(dice)/length(dice)
mean_dice
mean(1:6)
probability <- rep(1/6,6)
probability
plot(probability)
plot(probability,xlab = "Outcomes", main = 'Probability Distribution') #Plot options
cumsum(probability)


##### Plotting a Chi-squared distribution#####


curve(x^2-5) #Plot a function
curve(x^2-5, xlim = c(-5,5), col='red') #Include options
abline(h=0) #Add a horizontal line
abline(v=0)

#cHI-SQUARED

curve(dchisq(x, df=12), xlim=c(0,40), ylab = "density",
      xlab = "Hourly earnings in Euro")

# set seed for reproducibility
set.seed(1)

# Drawing a sample

sample1 <- rchisq(n = 100, df = 12)
y1 <- sample1[1] #First observation
Ybar= sum(sample1)/100

#The mean of Y1 is the mean of population (12), so Y1 is unbiased.
#The variance of Y1 is also the same of population, 2*12=24
#The variance of Ybar is variance of population/n
2*24/100
#Ybar is more efficient!



# PROPERTIES OF SAMPLE MEAN -----------------------------------------------

#To investigate the properties of sample mean, 
#we can generate multiple samples from the same population
#

pop <- rnorm(10000, 10, 1) #Population from a normal distribution, mean=10 and sd=1

#Sample from the population and estimate the mean

sample(x=pop, size=5) #Draw a sample from our population
mean(sample(x=pop, size=5)) #Mean of the sample

est1 <- replicate(expr = mean(sample(x = pop, size = 5)), n = 25000) #Get the mean 25000 times

est2 <- replicate(expr = mean(sample(x = pop, size = 25)), n = 25000) #The same, with larger sample

fo <- replicate(expr = sample(x = pop, size = 5)[1], n = 25000) #The first observation, Y1

#PLOTTING THE RESULTS

plot(density(fo)) #Plot the density estimate of Y1

plot(density(fo), 
     col = "green", 
     lwd = 2,
     ylim = c(0, 2),
     xlab = "estimates",
     main = "Sampling Distributions of Unbiased Estimators")

# add density estimate for the distribution of the sample mean with n=5 to the plot
lines(density(est1), 
      col = "steelblue", 
      lwd = 2, 
      bty = "l") #Lines, instead of plot, plots over the already existing graph

# add density estimate for the distribution of the sample mean with n=25 to the plot
lines(density(est2), 
      col = "red2", 
      lwd = 2)
# add a vertical line at the true parameter
abline(v = 10, lty = 2)
# add N(10,1) density to the plot
curve(dnorm(x, mean = 10), 
      lwd = 2,
      lty = 2,
      add = T)

# add a legend
legend("topleft",
       legend = c("N(10,1)",
                  expression(Y[1]),
                  expression(bar(Y) ~ n == 5),
                  expression(bar(Y) ~ n == 25)
       ), 
       lty = c(2, 1, 1, 1), 
       col = c("black","green", "steelblue", "red2"),
       lwd = 2)

###NOTES:
# The sampling distribution of Y1 tracks the density of N(0,1)
# As the number of observations increase, the sampling distribution gets closer the true parameter
# The probability of obtaining estimates that are close to the true value increases with n



# THE IMPORTANCE OF RANDOM SAMPLING ---------------------------------------

mean(pop)

?sample
#The argument prob denotes the probability of an element being sampled

# simulate outcomes for the sample mean when the i.i.d. assumption fails
sort(pop) #Population sorted
est3 <-  replicate(n = 25000, 
                   expr = mean(sample(x = sort(pop), 
                                      size = 25, 
                                      prob = c(rep(4, 2500), rep(1, 7500)))))


mean(est3)


# sampling distribution of sample mean, i.i.d. holds, n=25
plot(density(est2), 
     col = "steelblue",
     lwd = 2,
     xlim = c(8, 11),
     xlab = "Estimates",
     main = "When the i.i.d. Assumption Fails")

# sampling distribution of sample mean, i.i.d. fails, n=25
lines(density(est3),
      col = "red2",
      lwd = 2)
 
# add a legend
legend("topleft",
       legend = c(expression(bar(Y)[n == 25]~", i.i.d. fails"),
                  expression(bar(Y)[n == 25]~", i.i.d. holds")
       ), 
       lty = c(1, 1), 
       col = c("red2", "steelblue"),
       lwd = 2)

# Ybar is a biased estimator if i.i.d. doesn't not hold.

# HYPOTHESIS TESTING ------------------------------------------------------

###visualizing p-value
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = "Calculating a p-Value",
      yaxs = "i",
      xlab = "z",
      ylab = "",
      lwd = 2,
      axes = "F")

# add x-axis
axis(1, 
     at = c(-1.5, 0, 1.5), 
     padj = 0.75,
     labels = c(expression(-frac(bar(Y)^"act"~-~bar(mu)[Y,0], sigma[bar(Y)])),
                0,
                expression(frac(bar(Y)^"act"~-~bar(mu)[Y,0], sigma[bar(Y)]))))

# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -1.5, 0.01), -1.5),
        y = c(0, dnorm(seq(-6, -1.5, 0.01)),0), 
        col = "steelblue")

# shade p-value/2 region in right tail
polygon(x = c(1.5, seq(1.5, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.5, 6, 0.01)), 0), 
        col = "steelblue")



###Sample Variance, Sample s.d and s.e.
###The variance of population in unknown, so we estimate it, and also the s.d.

# vector of sample sizes
n <- c(10000, 5000, 2000, 1000, 500)

# sample observations, estimate using 'sd()' and plot the estimated distributions
sd(dice)
sq_y <- replicate(n = 10000, expr = sd(rnorm(n[1], 10, 3)))


plot(density(sq_y),
     main = expression("Sampling Distributions of" ~ s[Y]),
     xlab = expression(s[y]),
     lwd = 2)

for (i in 2:5) {
  sq_y <- replicate(n = 10000, expr = sd(rnorm(n[i], 10, 3)))
  lines(density(sq_y), 
        col = i, 
        lwd = 2)
  
  }

# add a legend
legend("topleft",
       legend = c(expression(n == 10000),
                  expression(n == 5000),
                  expression(n == 2000),
                  expression(n == 1000),
                  expression(n == 500)), 
       col = 1:5,
       lwd = 2)

#It gets close to the true value (3) as n increases!

######P-VALUE WHEN STANDARD DEVIATION IS UNKNOWN####

#Calculate the p-value
# sample and estimate, compute standard error

random_sample <- sample(0:1, 
         prob = c(0.9, 0.1), 
         replace = T, 
         size = 100) #sAMPLING FROM A BERNOULLI
samplemean_act <- mean(random_sample) #Mean of the sample


SE_samplemean <- sqrt(samplemean_act * (1 - samplemean_act) / 100) #Standard error

# null hypothesis
mean_h0 <- 0.1

# compute the p-value
pvalue <- 2 * pnorm(- abs(samplemean_act - mean_h0) / SE_samplemean) #pnorm gives the distribution function of Normal
pvalue

# compute a t-statistic for the sample mean
tstatistic <- (samplemean_act - mean_h0) / SE_samplemean
tstatistic

#we can see, graphically, that the t-statistic is approximately N(0,1) if n is large.

# prepare empty vector for t-statistics
tstatistics <- numeric(10000)

# set sample size
n <- 300

# simulate 10000 t-statistics
for (i in 1:10000) {
  
  s <- sample(0:1, 
              size = n,  
              prob = c(0.9, 0.1),
              replace = T)
  
  tstatistics[i] <- (mean(s)-0.1)/sqrt(var(s)/n)
  
}

# plot density and compare to N(0,1) density
plot(density(tstatistics),
     xlab = "t-statistic",
     main = "Estimated Distribution of the t-statistic when n=300",
     lwd = 2,
     xlim = c(-4, 4),
     col = "steelblue")

# N(0,1) density (dashed)
curve(dnorm(x), 
      add = T, 
      lty = 2, 
      lwd = 2)


# CONFIDENCE INTERVAL -----------------------------------------------------

# set seed
set.seed(1)

# generate some sample data
sampledata <- rnorm(100, 10, 10) #Sample from a N(10,10)

mean(sampledata)
t.test(sampledata)
ls(t.test(sampledata)) #Outcomes of t.test

t.test(sampledata)$"conf.int" #Just the desired result. 95% confidence interval
t.test(sampledata, conf.level = 0.99) #Covers the true value with a probability of 99%






# MEANS COMPARISON --------------------------------------------------------

#To compare means from two different populations, we can also perform a t-test

# set random seed
set.seed(1)

# draw data from two different populations with equal mean
sample_pop1 <- rnorm(100, 10, 10) #N(10,10)
sample_pop2 <- rnorm(100, 10, 20) #N(10,20)

mean(sample_pop1)
mean(sample_pop2)

# perform a two sample t-test
t.test(sample_pop1, sample_pop2) #Null hypothesis: the difference is zero


# HANDS ON! ---------------------------------------------------------------

#a)Draw a sample of size=100 from a Standard Normal Distribution
#b)Perform a t-test "by hand" (without the R function)
#c)Perform the t-test using the function from R and compare the values
#d)Interpret the result of the t-test
#d) Repeat step a) 1000 times, storing the mean on a vector and plot the histogram
