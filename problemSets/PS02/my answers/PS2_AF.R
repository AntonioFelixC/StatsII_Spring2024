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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# Structure of the dataset
str(climateSupport)

# Summary statistics of the dataset
summary(climateSupport)

# Dimensions of the dataset
dim(climateSupport)

# View first few rows of the dataset
head(climateSupport)

# Fit logistic regression model with interaction term
model_interaction <- glm(choice ~ countries * sanctions, data = climateSupport, family = binomial)

# Summary of the model
summary(model_interaction)

#The main effects of countries and sanctions are significant (p < 0.001),
#indicating that both variables have a significant effect on the likelihood
#of supporting the policy.

# Global null hypothesis and p-value
null_hypothesis <- "There is no relationship between the predictors and the likelihood of an individual supporting the policy."
p_value <- summary(model_interaction)$coefficients[1, "Pr(>|z|)"]

# Print the global null hypothesis and p-value
cat("Global Null Hypothesis:", null_hypothesis, "\n")
cat("p-value:", p_value, "\n")


#Question 2
# Extract coefficient for the interaction term between countries and sanctions
interaction_coef <- coef(model_interaction)["countries.L:sanctions.L"]

# Interpretation of the coefficient
cat("The coefficient for the interaction term between countries and sanctions is:", interaction_coef, "\n\n")
cat("Interpretation:\n")
cat("For each one-unit increase in sanctions (from 5% to 15%), the odds of supporting the policy\n")
cat("are multiplied by exp(", interaction_coef, ") = ", exp(interaction_coef), "\n\n")

#For each one-unit increase in sanctions (from 5% to 15%), 
#the odds of supporting the policy are multiplied by
#exp(-0.001754016) ≈ 0.9982475.

#This implies that increasing sanctions from 5% to 15% is associated
#with a slight decrease in the odds of supporting the policy. 
#However, the effect is minimal, indicating that the change in odds
#is not substantial.

#2B
# Define the values of countries and sanctions
countries_value <- "80 of 192"
sanctions_value <- "None"

# Predict the probability using the logistic regression model
predicted_probability <- predict(model_interaction, 
                                 newdata = data.frame(countries = countries_value, sanctions = sanctions_value),
                                 type = "response")

# Print the estimated probability
cat("The estimated probability that an individual will support the policy if there are",
    countries_value, "countries participating with no sanctions is:", predicted_probability, "\n")

#The estimated probability that an individual will support the policy
#if there are 80 of 192 countries participating with 
#no sanctions is: 0.5252101 

#2C
# Fit logistic regression model without the interaction term
model_no_interaction <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial)

# Perform likelihood ratio test
lrt <- anova(model_no_interaction, model_interaction, test = "Chisq")

# Print the results
print(lrt)

#In this case, the p-value (Pr(>Chi)) is 0.3912, 
#which is greater than the significance level of 0.05. 
#Therefore, we fail to reject the null hypothesis that the model 
#without the interaction term is sufficient. 
#This suggests that including the interaction term does not 
#significantly improve the model's fit.
