# Path Analysis
# CPA IBEX35
# Jaime Fanjul

rm(list=ls())
dev.off()


# Define working directory
#\""~
setwd("C:/Users/Jaime Fanjul/YOUR PATH/Path_Analysis")


ser=read.csv("cpa_serie.csv",sep=";")
summary(ser)

if(!require(lavaan)){install.packages("lavaan")}
if(!require(lavaanPlot)){install.packages("lavaanPlot")}
library(lavaanPlot)
library(lavaan)


# independence "~"
# covariance "~~"
# if the predictor is additive, the symbol is used "+"

# scale the values 

ser$ibe=ser$ibe/1000
ser$pet=ser$pet/10
ser$oro=ser$oro/100

summary(ser)

mediation_model ='
# Estimate the relationship between c18 interest rate  with IBEX35, gold(oro)
# Petroleum as mediation varible
c18~ibe+oro
# Estimate the relation of petroleum as mediation variable
pet~ibe
c18~pet
# Estimae the variance of exogenous variables
oro~~oro
ibe~~ibe
# Estimate endogenous variables residuals 
pet~~pet
c18~~c18'

mediation_model

adj_mm=lavaan(mediation_model, data = ser)

summary(adj_mm,fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

parameterEstimates(adj_mm)

# suggested modifications in the model schema 
# changes must be justified
# ~~ suggests a bidirectional relationship and covariance
modificationIndices(adj_mm)

# plot the model schema with all the parameters
lavaanPlot(model = adj_mm, coefs = TRUE, covs = TRUE)

## Direct Model

# now lets see what is the result removing the mediation variable petroleum and 
# using it as direct effect

direct_model ='
# Estimate the realationship (direct effect)
c18~pet+ibe+oro
# Estimate the variance of the exogenous variables
pet~~pet
ibe~~ibe
oro~~oro
# Exogenous variables residuals
c18~~c18
'
direct_model

adj_dm=lavaan(direct_model, data = ser)

summary(adj_dm,fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

parameterEstimates(adj_dm)
modificationIndices(adj_dm)

lavaanPlot(model = adj_dm, coefs = TRUE, covs = TRUE)


# comparing the models with ANOVA test 
anova(adj_mm, adj_dm)

# concluding
# The parameter with the largest standard error is the effect of the IBEX on the 18-month interest rate
# The highest value of the coefficient of determination is obtained in the model without the mediating effect of oil
# The lowest penalty values were obtained for the model with mediation
# The variable that most influences the price of the 18-month interest rate in any of the models is the IBEX35
# As a predictive model, we would choose the model without mediation because it is capable of explaining a greater variability of the data.
