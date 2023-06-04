library(QuantPsyc)
library(readxl)           
library(olsrr)                
library(car)
library(tidyverse)
library(caret)

df = read.csv("df1.csv")
df = data.frame(df)


model1 = lm(Global_Sales~ Critic_Count+ Critic_Score+ EU_Sales+ NA_Sales+ User_Count+ User_Score)

summary(model1)
confint(model1,level = 0.95)  ###### CIs for the (unstandarsised regression coefficient)
lm.beta(model1)               ###### beta values


#############Asumption checks 

par(mfrow=c(2,2))  # Change the panel layout to 2 x 2
plot(model1) #Produces the four graphs

ols_plot_resid_qq(model1)   #graph for testing normality assumption (you want residuals to sit on the line)
ols_plot_resid_fit(model1)  #residual vs fitted balues, want the residuals spread randomly around the 0 line
ols_plot_resid_hist(model1)  #residual histogram, this will show residual distribution 
ols_test_normality(model1)  #stats normality assumption (you want nonsig SW or KS tests)
vif(model1)                  #gives the VIFs for your model, want them <5


logUser_Count= log(User_Count)

model2 = glm(log(Global_Sales)~ Critic_Count + Critic_Score + log(User_Count)+User_Score, family = gaussian)

summary(model2)
confint(model2,level = 0.95)  ###### CIs for the (unstandarsised regression coefficient)
lm.beta(model2)               ###### beta values


#############Asumption checks 
par(mfrow=c(2,2))  # Change the panel layout to 2 x 2
plot(model2) #Produces the four graphs

ols_plot_resid_qq(model2)   #graph for testing normality assumption (you want residuals to sit on the line)
ols_plot_resid_fit(model2)  #residual vs fitted balues, want the residuals spread randomly around the 0 line
ols_plot_resid_hist(model2)  #residual histogram, this will show residual distribution 
vif(model2) 

