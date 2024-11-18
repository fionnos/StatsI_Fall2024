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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

###


install.packages("car")
library(car)
data(Prestige)
help(Prestige)


### Question 1 ###

## (a) Create a new variable professional by recoding the variable type so that professionals are coded as 1, 
## and blue and white collar workers are coded as 0 (Hint: ifelse).

# for the "type" column, "prof" = 1, else = 0
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

## (b) Run a linear model with prestige as an outcome and income, professional, 
## and the interaction of the two as predictors (Note: this is a continuous × dummy interaction.)

# For curiosity, look at the additive model first
lm(prestige ~ income + professional, data = Prestige)
library(scatterplot3d)
scatterplot3d(Prestige$income, Prestige$professional, Prestige$prestige)

# As well, plot as simple 2D projection (i.e. just bivariate baseR plot of income and prestige coloured by profession)
colors <- ifelse(Prestige$profession == 1, "red", "blue")
plot(Prestige$income, Prestige$prestige,
     col = colors,
     xlab = "Income",
     ylab = "Prestige")

# Back to question 1b:
model1 <- lm(prestige ~ income + professional + income:professional, data = Prestige)


## (c) Write the prediction equation based on the result.

# hard-coding the results of the regression from 1b, we get the following equation
Y1 <- 21.142259 + (0.003171*Prestige$income) + (37.781280*Prestige$professional) - (0.002326*(Prestige$income*Prestige$professional))

## (d) Interpret the coefficient for income.
# **note there isn't a "holding things constant any more"
# when professional = 0 for so-called "non-professional" jobs, a 1 unit increase in income there is a 0.003171 increase in prestige units



## (e) Interpret the coefficient for professional.
# when income is 0/held constant and professional changes from 0 to 1, there is a 37.781280 increase in the prestige score


## (f) What is the effect of a $1,000 increase in income on prestige score for professional occupations?
## In other words, we are interested in the marginal effect of income when the variable professional takes the value of 1. 
## Calculate the change in yˆ associated with a $1,000 increase in income based on your answer for (c).

# When professional is set to 1, our full question...
Y1 <- 21.142259 + (0.003171*Prestige$income) + (37.781280*Prestige$professional) - (0.002326*(Prestige$income*Prestige$professional))

# ...simplifies to
Y2 <- 58.92354 + 0.000845*Prestige$income # again, also hard-coded

# For every $1000 increase in income, we have 0.000845*1000, or 0.845.
# Excluding the intercept value, this equates to a marginal effect of an increase in 0.845 prestige units for every $1000 increase


## (g) What is the effect of changing one’s occupations from non-professional to professional when her income is $6,000? 
## We are interested in the marginal effect of professional jobs when the variable income takes the value of 6,000. 
## Calculate the change in yˆ based on your answer for (c).

# First input 6000 for income and 1 for professional:
ans_1g1 <- 21.142259 + (0.003171*6000) + 37.781280 - (0.002326*6000*1)
# Second, calculate for non-professional jobs
ans_1g2 <- 21.142259 + 0.003171*6000 
print(ans_1g1 - ans_1g2)



### Question 2 

# a and b are here 
## from the results of the regression detailed in Question 2, we have enough information to calculate the t-statistics for both regression coefficients

0.042/0.016
0.042/0.013
## we also have the number of observations, 131 - 2 (number of variables) - 1 (number of statistics used)
p1 <- 2*pt(-abst(2.625), df=128)
p2 <- 2*pt(-abs(3.23077), df=128)

# the results are 0.00972 and 0.00157, for B1 and B2, respectively. Both are below a = 0.05 and thus we can reject a null hypothesis that the slope of B1 and B2 are 0 (i.e. that X1 = 1 and X2 = 1 do impact the vote proportion)


# Reformatting the above for the question...

# ...(a)
# From the results of the regression detailed in Question 2, we have enough information to calculate the t-statistics for both regression coefficients
# To calculate our test-statistic, divide the estimated coefficient by the standard error (code below is raw-values, not abstracted)
t_stat_assinged <- 0.042/0.016

# calculate the degrees of freedom
dfreedom <- 131 - 2 - 1
# Now take this value and calculate the p-value. Given N - k - 1, we have 131 (number of observations) - 2 (number of variables) - 1 (number of statistics used)
# Remember to use the formula for a two-sided t-test, 
p_assigned <- 2*pt(abs(t_stat_assinged), df=dfreedom, lower.tail = FALSE) # or equivalently and more compactly 2*pt(-abs(2.625), df=128) 
p_assigned < 0.05

# ...(b)
# repeat procedure from (a) for the values provided for adjacent precincts
t_stat_adjacent <- 0.042/0.013
p_adjacent <- 2*pt(abs(t_stat_adjacent), df=dfreedom, lower.tail = FALSE)
p_adjacent < 0.05

# (c) Interpret the coefficient for the constant term substantively.
# This is the vote share for candidate KC for precincts that neither had a yard sign against candidate TMcA, or were beside a precinct that did. 
# In other words, controlling for lawn signs there was a 0.302 vote share for KC


# (d) Evaluate the model fit for this regression. 
# What does this tell us about the importance of yard signs versus other factors that are not modelled?

# to evaluate the overall model fit given the statistics provided, we can use the R^2 and/or compare the full and reduced models using the F-statistic.

# We have enough information to calculate this from the R squared and overall sample size.
Fstat <- (0.094/(2-1))/((1-0.094)/(131-2))
df(Fstat, 1, 129)
## note that these are hard-coded, need to generalise




# old code for 1.f, can ignore
plot(Prestige$income, Y2)
Y3 <- 58.92354 + 0.000845*(Prestige$income + 1000)
plot(Prestige$income, Y3)

