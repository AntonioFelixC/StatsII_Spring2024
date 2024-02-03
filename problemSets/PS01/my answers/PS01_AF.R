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

set.seed(123)

# Define the Kolmogorov-Smirnov test function
ks_test <- function(data){

# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)

# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))

# Calculate p-value using Kolmogorov-Smirnov CDF
n <- length(data)
p_value <- 1 - pnorm(sqrt(n) * D)

# Return the test statistic and p-value
return(list(statistic = D, p_value = p_value))}

# Generate 1,000 Cauchy random variables
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

# Perform the Kolmogorov-Smirnov test
result <- ks_test(data)
print(result)

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Define the objective function for OLS
ols_objective <- function(beta, x, y) {
  y_pred <- beta[1] + beta[2] * x
  residuals <- y - y_pred
  sum(residuals^2)}

# Use BFGS algorithm to minimize the objective function
initial_guess <- c(0, 0)  # Initial guess for coefficients
fit_bfgs <- optim(par = initial_guess, fn = ols_objective, x = data$x, y = data$y, method = "BFGS")

# Extract coefficients from BFGS result
coefficients_bfgs <- fit_bfgs$par

# Compare with lm
lm_result <- lm(y ~ x, data = data)
coefficients_lm <- coef(lm_result)

# Print results
cat("Coefficients from BFGS (Newton-Raphson):\n")
print(coefficients_bfgs)

cat("\nCoefficients from lm (Ordinary Least Squares):\n")
print(coefficients_lm)
