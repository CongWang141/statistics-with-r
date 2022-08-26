
# INTRODUCTION ------------------------------------------------------------
Sys.setenv(LANG = "en") #Set system language to English. Useful to search for error solutions.

setwd("C:/...") #Set working directory. Shortcut: Ctrl + Shift + H


##---------------------##
####BASIC OPERATIONS ####
##---------------------##


1+3
5-4
6/4
sqrt(25)
6^2
3**3
4 * 2; 5 ^ 2 # Two instructions can be placed on the same row separeted by ";"

###OBJECTS###
a <- 3
a <- 3 #Usual assignment operator in R. Shortcut: Alt + "-"
b= 5 #It also works in almost all cases
A <- 1 #R is case-sensitive


rm("a","b") #Clean specified objects
{
  c=3
d=1
y=4} #Multiple assignments in a code block

(a1=3^4) #When a variable between parenthesis, it is shown immediately
b1= a1+d ; b1 #Same result by using ;

##-------------##
#####VECTORS ####
##-------------##


v1= c(1:10)
x <- c(10.4, 5.6, 3.1, 6.4, 21.7) # Vector of numbers
#Accessing vector elements
x[1]
x[3] 
V <- c(1,4,5,6,
       7,4,3) # It can use more than one line
1/x #Operation with vectors
y <- c(x, 0, x) # New vector, using an existing one
v <- 2*x + 3*x  #vectors arithmetic
v

sum(x)/length(x) #Mean of vector x
mean(x) #Built function
sum((x-mean(x))^2)/(length(x)-1) #Variance of x
var(x)
v2= c('age', 'height', 'weight') #Vector of strings


###SEQUENCES
s1 <- seq(length=10, from=3, by=1)
S2 <- seq(5,8,0.5) #Arguments= (From, To, By)
s3 <- rep(4, times=5)
s4 = rep(x, 5) #Arguments' name can be omitted

###LOGICAL VECTORS


temp <- x > 13
temp

##Replications
x=3
replicate(n=10, expr =x+1 )
v=c(1,2,3)
replicate(n=10, expr =x+v )

###Simple loop structure

for (i in 1:5){
  print(i+1)
  
}

###MISSING VALUES

z <- c(1:3,NA); ind <- is.na(z)
ind

###Character vectors

fruit <- c(5, 10, 1, 20)
fruit
names(fruit) <- c("orange", "banana", "apple", "peach") #Function to label a vector
fruit
lunch <- fruit[c("apple","orange")] #Slicing the vector
lunch

##------------------------##
####MATRICES AND ARRAYS ####
##-----------------------##

#A matrix is a two-dimensional (m x n) object
M1 <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
M1
M2 <- matrix(c(1:9));M2
M3 <- matrix(c(1:9),3,3);M3 #specifying number of columns
M4 <- matrix(c(1:9),3,3, byrow = TRUE);M4 #Ordered by row. Default is FALSE

#An array is a three-dimensional object. All elements must be of the same type.
array1 <- array(1, dim = c(2, 2, 3)); array1


vector <- c(1:12)
multi_array <- array(vector, dim = c(2, 3, 2))
multi_array

#MATRIX OPERATIONS

M3*M4 #HADAMARD PRODUCT (element by element)
M3%*%M4 #MATRIX MULTIPLICATION
t(M3) #Transpose
diag(M3)
B <- matrix(c(1,2,3,4),2,2)
solve(B) #Inverse of B

#Equation system
A <- matrix(c(2,3,4,-2),2,2);A
y <- c(10,5); y
x <- solve(A,y)
x

##------------------##
#####DATAFRAME #######
##-----------------###

#Similar to a matrix, but each column is independent. Elements must be of the same type just in each column.


students <-   data.frame (index = 1:10,
              height=c(1.2, 1.3, 1.1, 1.35, 1.21,1.32, 1.33, 1.22, 1.40, 1.32),
              gender=c("male", "female ", "female", "male", "female","female", "male", "female","male","male"),
              bus_transport =c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE,FALSE, TRUE, TRUE))
students
names(students)
students$gender
students$weight <- c(40,38,28,50,34, 43, 38, 33 ,45, 37) #Create another variable
plot(students$height, students$weight)
plot(students$height, students$weight, pch=15, col='blue') #Several arguments can be passed to improve the plot

summary(students) #Summary of dataframe
summary(students$height) #Summary of one variable
hist(students$weight, main = "HISTOGRAM OF STUDENTS WEIGHTS")
?hist


##------------------------##
####LOAD AND SAVE DATA #####
##-----------------------###

install.packages("openxlsx") #Installing a package
library(openxlsx) #Loading the package

write.xlsx(students, file = "students.xlsx") # Save in xlsx format
write.csv(students, file = "students.csv") # Save in csv format

rm(list = ls()) # Clear the workspace. Shortcut: Ctrl + L

# Load From an online source
accidents=read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/airline-safety/airline-safety.csv")

#Load from the device
students=read.xlsx("students.xlsx") #"Import Dataset" button is highly recommended!

###DATA MANIPULATION

#Filter
no_accident=subset(accidents, accidents$fatal_accidents_85_99==0)
fatalities=subset(accidents, fatalities_00_14>100); fatalities

accidents$recent_fatalities=ifelse(accidents$fatalities_00_14>0, "yes", "no") #New column

###HANDS ON!#####

##Exercise 1-
data("airquality") #Daily air quality measurements in NY, May to September 1973.
View(airquality)

#Drop observations with one or more missing values
#Plot a line graph with temperature over time

##Exercise 2-
X1=sample(c('male', 'female'),1000, replace = TRUE) #Random sample of male and female
X2=rnorm(1000, 1.7, 0.1) #Random sample from Normal distribution. N, mean, sd
X3=X2*45 + rnorm(1000,1,5) #X3 depends on x2 plus a random error

#create a dataset
#Name columns as x1=Gender, x2=Height, x3=Weight
#Create  a new column BMI which denotes Body mass Index with the following formula: bmi= weight/height^2
#Suppose obese condition applies to male if BMI>28 or female IF BMI>30
#Filter observations with obese condition and make a scatterplot ( Weight x Height)


