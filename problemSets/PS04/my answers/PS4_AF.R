#Load library and dataset

library(eha)
library(survival)
data(child)

#Explore dataset
str(child)
View(child)

#Fit the Cox Proportional Hazard model:
cox_model <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)
print(cox_model)
