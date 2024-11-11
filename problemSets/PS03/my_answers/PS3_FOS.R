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

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")



### Inspecting the data

str(inc.sub)

### QUESTION 1: Effect of Campaign Spending on Vote Share ("voteshare")

# modelX will be used as name convention for regressions 1 to 5
model1 <- lm(voteshare ~ difflog, data = inc.sub)

# note: the lm() function and plot() function, when specified this way, have the response and explanatory variables mirrored 
plot(inc.sub$difflog, inc.sub$voteshare)
abline(lm(voteshare ~ difflog, data = inc.sub))

png(file="model1.png")
plot(voteshare~difflog, 
     data = inc.sub,
     xlab="Incumbent spending - challenger spending",
     ylab="Share of vote")
abline(lm(voteshare ~ difflog, data = inc.sub))
dev.off()

# residuals will be stored in objects resX for this and all subsequent questions
res1 <- model1$residuals
res1

# in this an all subsequent questions, I interpret "prediction equation" as the equation of the line/plane, without the error term
Y1 <- 0.04167*inc.sub$difflog + 0.57903



### Question 2
model2 <- lm(presvote ~ difflog, data = inc.sub)

plot(inc.sub$difflog, inc.sub$presvote)
abline(lm(presvote ~ difflog, data = inc.sub))

png(file="model2.png")
plot(presvote ~ difflog, 
     data = inc.sub,
     xlab="Incumbent spending - challenger spending",
     ylab="Presidential Vote")
abline(lm(presvote ~ difflog, data = inc.sub))
dev.off()

res2 <- model2$residuals

Y2 <- 0.02384*inc.sub$difflog + 0.50758


### Question 3
model3 <- lm(voteshare ~ presvote, data = inc.sub)
plot(inc.sub$presvote, inc.sub$voteshare)
abline(lm(voteshare ~ presvote, data = inc.sub))

png(file="model3.png")
plot(voteshare ~ presvote, 
     data = inc.sub,
     xlab="Presidential Vote",
     ylab="Voteshare")
abline(lm(voteshare ~ presvote, data = inc.sub))
dev.off()

res3 <- model3$residuals

Y3 <- 0.3880*inc.sub$presvote + 0.4413

### Question 4
model4 <- lm(res1~res2)
plot(res2, res1, ylim = c(-0.5,0.5), xlim = c(-0.5,0.5))
abline(model4)

png(file="model4.png")
plot(res2, res1, 
     xlab="Model 2 Residuals",
     ylab="Model 1 Residuals",
     xlim = c(-0.5,0.5),
     ylim = c(-0.5,0.5),
     )
abline(model4)
dev.off()

Y4 <- 2.569e-01*res2 - 1.942e-18

### Question 5
model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)

install.packages("scatterplot3d") # import package for 3D scatter-plots 
library(scatterplot3d)
scatterplot3d(inc.sub$difflog, inc.sub$presvote, inc.sub$voteshare)

png(filename = "model5.png")
scatterplot3d(inc.sub$difflog, inc.sub$presvote, inc.sub$voteshare)
dev.off()

Y5 = 0.03554*inc.sub$difflog + 0.25688*inc.sub$presvote + 0.44864









# miscellaneous other tests
cor.test(inc.sub$voteshare, inc.sub$presvote)
cor.test(inc.sub$voteshare, inc.sub$difflog)
cor.test(inc.sub$presvote, inc.sub$difflog)
cor.test(res1, res2)

plot(inc.sub$difflog, Y5)
plot(inc.sub$presvote, Y5)
plot(inc.sub$difflog, Y1)

s1 = sum(res1)
s2 = sum(res2)
s5 = sum(model5$residuals)
