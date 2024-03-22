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

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
#1A
# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# Check the structure of the dataset
str(gdp_data)

# Fit the unordered multinomial logistic regression model
model <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data)

#Because It seems I have too many levels in one of my categorical variables
#I will proceed to inspect the levels of GDPwdiff
table(gdp_data$GDPWdiff)


# Define breakpoints for binning
breakpoints <- quantile(gdp_data$GDPWdiff, probs = seq(0, 1, by = 0.1)) 

# Create a new categorical variable by binning GDPWdiff
gdp_data$GDPWdiff_group <- cut(gdp_data$GDPWdiff, breaks = breakpoints, labels = FALSE)

# Fit the multinomial logistic regression model using the binned variable
model <- multinom(GDPWdiff_group ~ REG + OIL, data = gdp_data)

# Summarize the model
summary(model)

#1B)
# Load necessary library
library(MASS)

# Convert GDPWdiff_group to factor
gdp_data$GDPWdiff_group <- factor(gdp_data$GDPWdiff_group)

# Fit ordered multinomial logit model
ordered_model <- polr(GDPWdiff_group ~ REG + OIL, data = gdp_data, Hess = TRUE)

# Summarize the model
summary(ordered_model)



#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# Fit Poisson regression model
poisson_model <- glm(PAN.visits.06 ~ competitive.district, data = mexico_elections, family = poisson)

# Summary of the model
summary(poisson_model)

# 2b 

poisson_modelB <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                     data = mexico_elections, family = poisson)

# Summary of the model
summary(poisson_modelB)
