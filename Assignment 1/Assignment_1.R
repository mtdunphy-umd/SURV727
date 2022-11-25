################################################################################
# Title: Assignment 1

# Name of file: assignment_1.R
# A script containing instructions for Assignment 1, FOCD 2022 UMD/UM
# Author: Michael Dunphy
# Date: Sun Sep 25, 2021
################################################################################
# Please write down the R command(s) needed after each question!
# Some questions require an interpretation of the results in addition to code!

# There is a total of 22 questions.


### Data types
# 1. Which command do you use to determine the type of an object?

typeof()

# 2. What is the type of vector A?
A <- c("2", "3", "4", "5", "6", "7", "8")

typeof(A)
# character

# 3. Convert A into an integer vector

as.integer(A)

# 4. Create an integer vector B containing the numbers one through ten

B <- c(1:10)
B


# 5. Create a new vector C from B which has the type "double"

C <- as.double(B)
C

# 6. Change the third value of B to "3.5"

B[3] <- 3.5
B

# 7. Did this affect the type of B? How?

typeof(B)
# The type of B changed to double since the third value was changed to 3.5 which is a double and all elements in a vector must be the same.

### Reading in data
# Download both the Angell.dta (Stata data format) dataset and the Angell.txt dataset from this website
# https://stats.idre.ucla.edu/stata/examples/ara/applied-regression-analysis-by-fox-data-files/

# 8. Read in the .dta version and store in an object called angell_stata

library("foreign")
angell_stata <- read.dta("angell.dta")
angell_stata

# 9. Read in the .txt version and store it in an object called angell_txt

angell_txt <- read.table("Angell.txt")
angell_txt

# 10. Drop the first five observations in the angell_txt object

angell_txt <- angell_txt[-(1:5), ]
angell_txt

# 11. Select columns 2 and 3 of the angell_stata object and store them in a new object called angell_small

angell_small <- angell_stata[, 2:3]
angell_small

# R comes also with many built-in datasets. The "MASS" package, for example, comes with the "Boston" dataset
# 12. Install the "MASS" package, load the package. Then, load the Boston dataset

library("MASS")
Boston

# 13. What is the type of the Boston object?

typeof(Boston)
# list

# 14. What is the class of the Boston object?

class(Boston)
# data.frame

### Basic data summarizing and description
# 15. How many of the suburbs in the Boston data set bound the Charles river?

# chas Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
sum(Boston$chas)
# 35 suburbs

# 16. Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates?
# Pupil-teacher ratios? Comment on the range of each variable.

# crim per capita crime rate by town.
range(Boston$crim)
# the range is 0.00632 to 88.9762
hist(Boston$crim)
summary(Boston$crim)
# the median is 0.25651 and mean is 3.6135 so with the upper range being 88.976, indicates there are suburbs in Boston with particularly high crime rates compared with the rest of the suburbs given that the data is highly positively skewed.

# tax full-value property-tax rate per $10,000.
range(Boston$tax)
# the range is 187 to 711
hist(Boston$tax)
summary(Boston$tax)
# a quarter of Boston suburbs are between $666 and $711 tax rate per $10,000. There appears to be a gap in tax rates with a group of suburbs having tax rates between $187 and less than $500 and another group having higher than $650. This group makes up a quarter of all suburbs so these suburbs wouldn't necessarily be considered outliers, but the gap in tax rates indicate that certain suburbs have particularly high tax rates compared with the rest of the suburbs.

# ptratio pupil-teacher ratio by town.
range(Boston$ptratio)
# the range is 12.6 to 22
hist(Boston$ptratio)
summary(Boston$ptratio)
# the median and mean are relatively close compared to the same measurements for crime and tax rates. This indicates the pupil-teach ratio is more normally distributed among the different suburbs than compared to the other measurements. There is a high mode of suburbs with ratios between 20 and 21, but these suburbs would not be considered outliers.

# 17. What is the median pupil-teacher ratio among the towns in this data set that
# have a per capita crime rate larger than 1 ?

median(Boston$ptratio[Boston$crim > 1])
# 20.2

### Functions
# 18. Write a function that calculates the squareroot of an integer

sq_rt <- function(x){
  x^(1/2)
}

# 19. Write a function that calculates 95% confidence intervals for a point estimate.
# The function should be called "my_CI"
# When called with "my_CI(2, 0.2)", the output of the function should read
# "The 95% CI upper bound of point estimate 2 with standard error 0.2 is 2.392. The lower bound is 1.608."
# Note: the function should take a point estimate and its standard error as arguments
# You may use the formula for 95% CI: point estimate +/- 1.96*standard error)
# Pasting text in R: paste() and paste0()

my_CI <- function(x, se){
  upper <- x + (se * 1.96)
  lower <- x - (se * 1.96)
  paste("The 95% CI upper bound of point estimate", x, "with standard error", se, "is", upper, ". The lower bound is", lower, ".")
}

# 20. Write a function that converts all negative numbers in the following dataset into NA
# Use as little code as possible and try to avoid code repetition
set.seed(1002)
df <- data.frame(replicate(10, sample(c(1:10, c(-99,-98,-5)), 6, rep = TRUE)))
names(df) <- letters[1:6]
df

neg <- function(x) {
  x[x<0] <- NA
  x
}


# 21. Use your function to convert all negative numbers in the dataset into NA without changing the class of the object

class(df)
df_neg <- neg(df)
df_neg
class(df_neg)

# 22. Change the function you wrote above such that it turns any negative number into NA!

neg <- function(x) {
  if (x < 0) {
    NA
  } else {
    x
  }
}





