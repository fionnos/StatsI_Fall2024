#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

# To calculate the Chi-squared statistic by hand, we first have to calculate the expected values, fe, for each respective observation. This can be found by dividing the row sum by the grand total and multiplying this by the column total.

# initialise a matrix for the observed data
obs <- matrix(0, 2,3) 

# add in the observed data
UC <- c(14,6,7) 
LC <- c(7,7,1)
obs[1,] <- UC
obs[2,] <- LC
obs

# initialise new matrix for the expected values
exp <- matrix(0, 2,3)

# calculate the expected value for each cell in the first row and assign to the new empty matrix, then another loop for the second row
for (i in 1:ncol(obs)) {
  exp[1,i] <- (sum(obs[1,])/sum(obs))*(sum(obs[,i]))
}
for (i in 1:ncol(obs)) {
  exp[2,i] <- (sum(obs[2,])/sum(obs))*(sum(obs[,i]))
}
print(exp)
# ideally, I would use a nested i-j loop, I have done this for question 1.c

# we can calculate the chi-squared statistic by matrix subtraction and multiplication
chi2_stat <- sum(((obs - exp)^2)/(exp))
chi2_stat


### question 1.b

# by consulting the Chi-squared PDF, we can find the associated p-value 
pchisq(chi2_stat, df = (nrow(exp)-1)*(ncol(exp)), lower.tail = FALSE)
# even with a non-stringent alpha of 0.1, we cannot reject the null hypothesis that the groups are independent 


### question 1.c: calculate residuals

# To calculate the standardised/adjusted residuals we convert the difference between to units of standard error, which for the Chi-squared distribution is the square root of the product of the each respective expected value by 1 minus the proportions for both the row and column totals

# we already have enough information to calculate the numerator (observed minus expected values), which we can store as a difference matrix
diff <- (obs - exp) ## this is where the error is occurring, can't take one matrix away from the other
diff

# next, I will compute the standard error for each expected value 
SE_mat <- matrix(0, 2,3)
gtot <-sum(obs)

# this loop attempts compute the row total and column total for each cell of the observation matrix, compute the standard error and output it to a new matrix
for (i in 1:nrow(SE_mat)) {    
  for (j in 1:ncol(SE_mat)) {   
    row_tot <- sum(obs[i,])
    col_tot <- sum(obs[,j])
    SE_mat[i,j] <- sqrt(exp[i,j] * (1-(row_tot/gtot)) * (1-(col_tot/gtot)))
  }
}
print(SE_mat)

# now finish the computation
residuals <- diff/SE_mat
round(residuals, 2)

# These results are incorrect. Trouble-shoot by breaking up loop first

for (i in 1:ncol(SE_mat)) {    
  SE_mat[1,i] <- sqrt(
    exp[1,i] * (1-(sum(obs[1,])/gtot)) * (1-(sum(obs[,i])/gtot)))
}
for (i in 1:ncol(SE_mat)) {    
  SE_mat[2,i] <- sqrt(
    exp[2,i] * (1-(sum(obs[2,])/gtot)) * (1-(sum(obs[,i])/gtot)))
}
print(SE_mat)

# these are SE values for the second row, not the first (from calculating by hand)
# 1.552648 1.43557 1.219377



## 1.d
# residuals here give the difference between observed and expected values, normalised in terms of the standard-error of each proportion


### Question 2

# 2a
# the null hypothesis here is that there is no correlation between male and female representatives (input variable) and the number of repairs to/new water facilities (response variable)
# the alternative hypothesis is that there is a correlation
# summarised as H0: B-hat = 0 and Ha: B-hat != 0


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


df_raw <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
head(df_raw) # to inspect the first few entries

# First inspect whether one third of GPs are women as stated
sum(df_raw$female)/nrow(df_raw)

# Note that here we have a binary input variable, 0 or 1, and a choice of two response variables: irrigation and water.
# We can run two regressions, one for each response variable, or combine their values. 
# I will start with the water response variable:

png(file="regression.png")
plot(water~female, 
     data = df_raw,
     xlab="Male = 0, Female = 1",
     ylab="New/repair incidences")
lm(water~female, data = df_raw)
abline(lm(water~female, data = df_raw))
dev.off()
cor.test(df_raw$female, df_raw$water)


# to look at the effect on irrigation alone
plot(irrigation~female, data = df_raw)
lm(irrigation~female, data = df_raw)
abline(lm(irrigation~female, data = df_raw))
cor.test(df_raw$female, df_raw$irrigation)


# to look at the combined effect
Y <- rowSums(df_raw[,5:6]) # combine the last two columns into a new vector
df_simple <- df_raw # create a new matrix
df_simple[,5] <- Y # overwrite what's going to be last column
df_simple <- df_simple[,-6] # remove the last column
names(df_simple)[5] = "response" # rename the last column

plot(response~female, data = df_simple)
lm(response~female, data = df_simple)
abline(lm(response~female, data = df_simple))
cor.test(df_simple$female, df_simple$response)

summary(lm(response~female, data = df_simple))



