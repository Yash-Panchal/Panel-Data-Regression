# Loading Libraries
library(ggplot2)
library(olsrr)
library(gvlma)
library(car)
library(gplots)
library(plm)
library(sandwich)
library(lmtest)
library(corrplot)

data <- read.csv("E:\\IIT Kanpur\\IME 2nd Sem Courses\\MBA652-SMBA\\Projects\\Panel Data Regression\\Spanish Dairy\\dairy_1.csv")

data_1 <- data
data_1$FARM <- NULL
data_1$YEAR <- NULL

# Finding Correlation Between varibales
correlation <- cor(data_1)
correlation_1 <- round(correlation, 2)
corrplot(correlation_1, method = "number")

data$YIT <- NULL

summary(data)
sd(data$MILK)
help(sd)

sum(is.na("data"))


# Scatter Plots
ggplot(data, aes(x = COWS, y = MILK)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Milk Produced vs Cows in the FARM", x = "COWS", y = "MILK")

ggplot(data, aes(x = LAND, y = MILK)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Milk Produced vs Land of the FARM", x = "LAND AREA", y = "MILK")

ggplot(data, aes(x = LABOR, y = MILK)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Milk Produced vs Labor Involved", x = "Labor", y = "MILK")

ggplot(data, aes(x = FEED, y = MILK)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Milk Produced vs Quantity of food fed to Cows", x = "Quantitiy Fed", y = "MILK")



ggplot(data= data, aes(x = FARM, y = MILK, group = YEAR, colour = YEAR))+
  geom_point() +
  geom_smooth(method= "lm", se= TRUE) +
  labs(x= "FARM Number", y= "Amount of Milk Produced", title = "Milk Production vs Farm")


ggplot(data= data, aes(x = YEAR, y = MILK, group = FARM, colour = FARM))+
  geom_point() +
  geom_smooth(method= "lm", se= TRUE) +
  labs(x= "Year", y= "Amount of Milk Produced", title = "Milk Production vs Year")

#deviation over years
plotmeans(MILK ~ YEAR, error.bars="sd", main="Deviations-Years", data=data, xlab="YEAR", ylab="Milk Production" )

#deviation over country
plotmeans(MILK ~ FARM, main="Deviations-FARMs", data=data, xlab="FARM", ylab= "Milk Production" )

#Regression Models
# Pooling Model
Pooled_Data <- pdata.frame(data, index = c("FARM","YEAR"))

Pooled_model1 <- plm(MILK ~ COWS + LAND + LABOR + FEED, data=Pooled_Data, model = 'pooling')
summary(Pooled_model1)
bptest(Pooled_model1)

Pooled_model2 <- plm(MILK ~ COWS + FEED + LABOR, data=Pooled_Data, model = 'pooling')
summary(Pooled_model2)
bptest(Pooled_model2)

#best model
Pooled_model3 <- plm(MILK ~ COWS + FEED, data=Pooled_Data, model = 'pooling')
summary(Pooled_model3)
bptest(Pooled_model3)



# Entity Fixed Models
# 1. Entity Demeaned Models
EF_Model1 <- plm(MILK ~ COWS + LAND + LABOR + FEED, data=Pooled_Data, model = 'within', effect = 'individual')
summary(EF_Model1)

EF_Model2 <- plm(MILK ~ COWS + LABOR + FEED, data=Pooled_Data, model = 'within', effect = 'individual')
summary(EF_Model2)

#best model
EF_Model3 <- plm(MILK ~ COWS + FEED, data=Pooled_Data, model = 'within', effect = 'individual')
summary(EF_Model3)




# 2. Binary Variables for FARM
BinFARM_Model1 <- lm(MILK ~ COWS + LAND + LABOR + FEED + FARM, data=Pooled_Data)
summary(BinFARM_Model1)

BinFARM_Model2 <- lm(MILK ~ COWS + LABOR + FEED + FARM, data=Pooled_Data)
summary(BinFARM_Model1)

# Best Model
BinFARM_Model3 <- lm(MILK ~ COWS + FEED + FARM, data=Pooled_Data)
summary(BinFARM_Model1)



# Time Fixed Models
# 1. Binary Variables for year
BinYEAR_Model1 <- lm(MILK ~ COWS + LAND + LABOR + FEED + YEAR, data=Pooled_Data)
summary(BinYEAR_Model1)

# best model
BinYEAR_Model2 <- lm(MILK ~ COWS + LABOR + FEED + YEAR, data=Pooled_Data)
summary(BinYEAR_Model2)




# 2. Time Demeaned Models
TF_Model1 <- plm(MILK ~ COWS + LAND + LABOR + FEED, data=Pooled_Data, model = 'within', effect = 'time')
summary(TF_Model1)

#best Model
TF_Model2 <- plm(MILK ~ COWS + LABOR + FEED, data=Pooled_Data, model = 'within', effect = 'time')
summary(TF_Model2)





# Entity and Time Fixed Models
ETF_Model1 <- plm(MILK ~ COWS + LAND + LABOR + FEED, data=Pooled_Data, model = 'within',index=c("FARM","YEAR"), effect ='twoways')
summary(ETF_Model1)

ETF_Model2 <- plm(MILK ~ COWS + LABOR + FEED, data=Pooled_Data, model = 'within',index=c("FARM","YEAR"), effect ='twoways')
summary(ETF_Model2)

#Best Model
ETF_Model3 <- plm(MILK ~ COWS + FEED, data=Pooled_Data, model = 'within',index=c("FARM","YEAR"), effect ='twoways')
summary(ETF_Model3)




#First Difference Model
# best Model
FD_Model1 <- plm(MILK ~ COWS + LAND + LABOR + FEED, data=Pooled_Data, index=c("FARM","YEAR"),model = 'fd')
summary(FD_Model1)

FD_Model2 <- plm(MILK ~ COWS + LAND + FEED, data=Pooled_Data, index=c("FARM","YEAR"),model = 'fd')
summary(FD_Model2)

FD_Model3 <- plm(MILK ~ COWS + FEED, data=Pooled_Data, index=c("FARM","YEAR"),model = 'fd')
summary(FD_Model3)
#None of the above models can be used




# Random Effect Model
RF_Model1 <- plm(MILK ~ COWS + LAND + LABOR + FEED, data = Pooled_Data, model = "random", index = c("FARM", "YEAR"))
summary(RF_Model1)

RF_Model2 <- plm(MILK ~ COWS + LABOR + FEED, data = Pooled_Data, model = "random", index = c("FARM", "YEAR"))
summary(RF_Model2)

# best Model
RF_Model3 <- plm(MILK ~ COWS + FEED, data = Pooled_Data, model = "random", index = c("FARM", "YEAR"))
summary(RF_Model3)





#Chosen Models
# Pooling Model
#best model
Pooled_model3 <- plm(MILK ~ COWS + FEED, data=Pooled_Data, model = 'pooling')
summary(Pooled_model3)
bptest(Pooled_model3)

# Entity Fixed Models
# Entity Demeaned Model 
#best model
EF_Model3 <- plm(MILK ~ COWS + FEED, data=Pooled_Data, model = 'within', effect = 'individual')
summary(EF_Model3)

# Binary variables for FARM
# Best Model
BinFARM_Model3 <- lm(MILK ~ COWS + FEED + FARM, data=Pooled_Data)
summary(BinFARM_Model3)
bptest(BinFARM_Model3)
coeftest(BinFARM_Model3, vcovHC(BinFARM_Model3, method = "arellano"))

# Time Fixed Models
# Binary Variables for YEAR
# best model
BinYEAR_Model2 <- lm(MILK ~ COWS + LABOR + FEED + YEAR, data=Pooled_Data)
summary(BinYEAR_Model2)

# Time Demeaned Model
#best Model
TF_Model2 <- plm(MILK ~ COWS + LABOR + FEED, data=Pooled_Data, model = 'within', effect = 'time')
summary(TF_Model2)
pFtest(TF_Model2, Pooled_model3)

pbgtest(TF_Model2)
plmtest(TF_Model2,c("time"), type=("bp"))

# Entity and Time Fixed Models
# Best Model
ETF_Model3 <- plm(MILK ~ COWS + FEED, data=Pooled_Data, model = 'within',index=c("FARM","YEAR"), effect ='twoways')
summary(ETF_Model3)

# Random Effect Model
# best Model
RF_Model3 <- plm(MILK ~ COWS + FEED, data = Pooled_Data, model = "random", index = c("FARM", "YEAR"))
summary(RF_Model3)



# Tests
# BP test heteroskedasticity
bptest(BinFARM_Model3)

# Breusch-Godfrey / Wooldridge Test Serail Collinearity
pbgtest(TF_Model2)

# LM TEST test for Panel Effect:
pFtest(EF_Model3, Pooled_model3)

# Hausman test
phtest(RF_Model3, BinFARM_Model3)

# Heteroskedasticity correctness measures
coeftest(BinFARM_Model3, vcovHC(BinFARM_Model3, method = "arellano"))













ETF_Model3 <- plm(ls~lp+is+il,data=Pooled_Data, model = 'within',index=c("country","time"), effect =
                    'twoways')
summary(ETF_Model3)
bptest(Pooled_model6)
pbgtest(Pooled_model6)

TF_Model3 <- plm(ls~lp+is+il,data=Pooled_Data, model = 'within', effect = 'time')
summary(TF_Model3)
bptest(TF_Model3, studentize = 'FALSE')
pbgtest(TF_Model3)
plmtest(TF_Model3,c("time"),type=("bp"))

#choice 1
BinTime_Model3 <- lm(ls~time+lp+is+il,data=Pooled_Data)
summary(BinTime_Model3)
bptest(BinTime_Model3)
pbgtest(BinTime_Model3)

#choice 2*******************taken
BinCountry_Model6 <- lm(ls~factor(country)+lp+is+il+ld+uis+uil,data=Pooled_Data)
summary(BinCountry_Model6)
bptest(BinCountry_Model6)
pbgtest(EF_Model6)
coeftest(BinCountry_Model6, vcovHC(BinCountry_Model6, method = "arellano"))

#*****************************
EF_Model9 <- plm(ls~lp+ld+uis+uil,data=Pooled_Data, model = 'within', effect = 'individual')
summary(EF_Model9)
bptest(EF_Model9)
pbgtest(EF_Model9)