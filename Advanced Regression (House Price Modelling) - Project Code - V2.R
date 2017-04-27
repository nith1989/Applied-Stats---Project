
#Install packages
install.packages("COUNT")
install.packages("caret")
install.packages("munsell")
install.packages("assertthat")
install.packages("mice")
install.packages("plyr")
install.packages("ggplot2", dep = TRUE)
install.packages("proto")
install.packages("Rcpp", dependencies = TRUE)
install.packages("acepack")
install.packages("htmlTable")
install.packages("GGally")
install.packages("labeling")
install.packages("corrplot")
install.packages("car")


#Important libraries
library(car)
library(labeling)
library(GGally)
library(corrplot)
library(htmlTable)
library(acepack)
library(ggplot2)
library(COUNT)
library(plyr)
library(caret)
library(mice)
library(lattice)
library(Hmisc)

#Data import
retail<-read.table('C:/Nithya Mahadevan/Studies/Semester - I/Applied Statistics/Project/Data/Input/train_modified.txt',header=T,sep='\t')
attach(retail)
print(str(retail))

#Input clean up and QC 
#Function definitions for 
dropMajorityNaColumns <- function(data, rate) {
  cols = checkMajorityNa(data, rate)
  drop = c()
  for (col in cols) {
    drop <- union(drop, col)
  }
  data <- data[, !(colnames(data) %in% drop)]
  return(data)
}

checkMajorityNa <- function(data, rate) {
  cols = colnames(data)
  count = 0
  result = c()
  for (s in cols){
    freq.na = count(is.na(data[s]))
    freq.not.na = freq.na["freq"][[1]][1]
    if (freq.not.na < nrow(retail)) {
      freq.is.na = freq.na["freq"][[1]][2]
      if (freq.is.na/nrow(retail) > rate) {
        count = count + 1;
        result <- union(result, s)
      }
    }
  }
  print(count)
  return(result)
}

checkMajorityCategorialValue <- function(data, rate) {
  result = c()
  cols = colnames(data)
  count = 0
  for (s in cols){
    freq.table = count(data[s])
    freq = freq.table$freq
    sum = sum(freq)
    for (i in freq) {
      if (i/sum > rate) {
        count=count+1
        result <- union(result, c(s))
      }
    }
  }
  print(count)
  return(result)
}

#Function to check the counts against each value in continuous variables
showCount <- function(data, cols) {
  for (col in cols) {
    a = count(data[col])
    print(a)
  }
}

na = checkMajorityNa(retail, 0.25)
vars.to.remove <- names(retail) %in% na
retail.new = retail[!vars.to.remove]
majority = checkMajorityCategorialValue(retail.new, 0.8)

showCount(retail.new, majority)

#Decide which variables among them to keep based on how skewed the data is
keeps <- c("LandContour","LandSlope", "Condition1", "Condition2","KitchenAbvGr",
           "BldgType", "ExterCond", "Heating", "CentralAir", 
           "Functional", "MiscVal", "SaleType","SaleCondition")

to.remove.from.majority = majority[!majority %in% keeps]
to.remove.from.data = !names(retail.new) %in% to.remove.from.majority
retail.new = retail.new[to.remove.from.data]
names(retail.new)

# Saved data dataframe for easier editting
# Removed Id column
# ExterCond (scale 0-4)
# Removed Basement variables
# Created new variable numGarage (0-Without Garage;1-With Garages)
# Keep one kitchen variable
# Keep one pool variable to YES/NO

as.data.frame(retail.new)
retail.new <- retail.new[c(-1)]
levels(retail.new$ExterCond) <- c(levels(retail.new$ExterCond),4,3,2,1,0)
retail.new$ExterCond[retail.new$ExterCond=="Ex"] <- 4
retail.new$ExterCond[retail.new$ExterCond=="Gd"] <- 3
retail.new$ExterCond[retail.new$ExterCond=="TA"] <- 2
retail.new$ExterCond[retail.new$ExterCond=="Fa"] <- 1
retail.new$ExterCond[retail.new$ExterCond=="Po"] <- 0
retail.new$ExterCond <- factor(retail.new$ExterCond)

retail.f <- retail.new
retail.f$ExterCond<-as.numeric(retail.f$ExterCond)

retail.f <- subset(retail.new, select = -c(GarageArea,GarageYrBlt,GarageType,
                                           GarageCars,BsmtExposure,BsmtFinType1,
                                           BsmtFinSF1,BsmtUnfSF,
                                           BsmtQual,BsmtFullBath))

levels(retail.f$GarageFinish) <- c(levels(retail.f$GarageFinish),1,0)
retail.f$GarageFinish[is.na(retail.f$GarageFinish)] <- 0
retail.f$GarageFinish[retail.f$GarageFinish=="Fin"] <- 1
retail.f$GarageFinish[retail.f$GarageFinish=="RFn"] <- 1
retail.f$GarageFinish[retail.f$GarageFinish=="Unf"] <- 1
retail.f$GarageFinish <- factor(retail.f$GarageFinish)
colnames(retail.f)[colnames(retail.f)=="GarageFinish"] = "numGarage"

retail.f$numGarage<-as.numeric(retail.f$numGarage)

retail.f$MoSold<-factor(retail.f$MoSold)

#Missing value treatment
checkMajorityNa(retail.f, 0)

#Replace with mean for continuous variables
retail.f$LotFrontage[is.na(retail.f$LotFrontage)] <- mean(retail.f$LotFrontage, na.rm = T)
retail.f$MasVnrArea[is.na(retail.f$MasVnrArea)] <- mean(retail.f$MasVnrArea, na.rm = T)

checkMajorityNa(retail.f, 0)

#Replace with mode for categtorical variables
getModeFromCategorical <- function(data_col) {
  return(names(sort(-table(data_col)))[1])
}

retail.f$MasVnrType[is.na(retail.f$MasVnrType)] <- getModeFromCategorical(retail.f$MasVnrType)

checkMajorityNa(retail.f, 0)

#Creating dummy variables for categorical data
dummy_cols <- dummyVars(" ~ .", data = retail.f, fullRank=T)
retail.dummy <- data.frame(predict(dummy_cols, newdata = retail.f))

#Hypothesis set
#Correlation matrix with sale price for chosen variables
x<-data.frame(retail.f$SalePrice)
z<-data.frame(retail.f$SalePrice,retail.f$YearRemodAdd,retail.f$LotArea,retail.f$GrLivArea,retail.f$YearBuilt_Dummy,
              retail.f$FullBath,retail.f$Fireplaces,retail.f$KitchenAbvGr,retail.f$TotalBsmtSF,
              retail.f$TotRmsAbvGrd,retail.f$BedroomAbvGr)
y<-data.frame(retail.f$YearRemodAdd,retail.f$LotArea,retail.f$GrLivArea,retail.f$YearBuilt_Dummy,
              retail.f$FullBath,retail.f$Fireplaces,retail.f$KitchenAbvGr,retail.f$TotalBsmtSF,
              retail.f$TotRmsAbvGrd,retail.f$BedroomAbvGr)
r<-cor(y,x)
corrplot(r,method="number",cl.pos='n')

#Scatter plots for further study
pairs(SalePrice~YearRemodd_Dummy+LotArea+GrLivArea+YearBuilt_Dummy+FullBath+Fireplaces,data=retail.new,main='Scatterplot Matrix')
pairs(SalePrice~KitchenAbvGr+TotalBsmtSF+TotRmsAbvGrd+BedroomAbvGr,data=retail.new,main='Scatterplot Matrix' )

ggpairs(y[,1:10])

#Model building
#Continuous variables alone
Model1<-lm(SalePrice~GrLivArea+FullBath+TotalBsmtSF+YearRemodd_Dummy,data=retail.f)
summary(Model1)
anova(Model1)

#Adding discrete variables too
Model2<-lm(SalePrice~GrLivArea+FullBath+TotalBsmtSF+YearRemodd_Dummy+MSSubClass_Dummy,data=retail.dummy)
summary(Model2)
anova(Model2)

#Partial F-test shows p<.05, so Model2 is better than Model1
anova(Model1,Model2)


Model3<-lm(SalePrice~GrLivArea+FullBath+TotalBsmtSF+YearRemodd_Dummy+MSSubClass_Dummy+MSZoning.FV+
                    MSZoning.RH+MSZoning.RL,data=retail.dummy)
summary(Model3)
anova(Model3)

#Partial F-test shows p<.05, so Model3 is better than Model2
anova(Model3,Model2)

Model4<-lm(SalePrice~GrLivArea+FullBath+TotalBsmtSF+YearRemodd_Dummy+MSSubClass_Dummy+MSZoning.FV+
             MSZoning.RH+MSZoning.RL+MoSold.2+MoSold.3+MoSold.4+MoSold.5+MoSold.6+MoSold.7+
             MoSold.8+MoSold.9+MoSold.10+MoSold.11+MoSold.12,data=retail.dummy)
summary(Model4)
anova(Model4)

#Partial F shows p>.05, so MoSold is not a significant term
anova(Model4,Model3)

Model5<-lm(SalePrice~GrLivArea+FullBath+TotalBsmtSF+YearRemodd_Dummy+MSSubClass_Dummy+MSZoning.FV+
             MSZoning.RH+MSZoning.RL+numGarage,data=retail.dummy)
summary(Model5)
anova(Model5)

#Partial F shows p<.05, so Model5 is better than Model3
anova(Model5,Model3)

Model6<-lm(SalePrice~GrLivArea+FullBath+TotalBsmtSF+YearRemodd_Dummy+MSSubClass_Dummy+MSZoning.FV+
             MSZoning.RH+MSZoning.RL+numGarage+OverallQual,data=retail.dummy)
summary(Model6)
anova(Model6)

#Model6 is better than Model5
anova(Model6,Model5)

Model7<-lm(SalePrice~GrLivArea+FullBath+Fireplaces+TotalBsmtSF+YearRemodd_Dummy+
             MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual,
           data=retail.dummy)
summary(Model7)
anova(Model7)

#Model7 is better than Model6
anova(Model7,Model6)

Model8<-lm(SalePrice~GrLivArea+FullBath+Fireplaces+KitchenAbvGr+TotalBsmtSF+YearRemodd_Dummy+
             MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual,
           data=retail.dummy)
summary(Model8)
anova(Model8)

#Model8 is better than Model7
anova(Model8,Model7)

Model9<-lm(SalePrice~GrLivArea+FullBath+Fireplaces+KitchenAbvGr+LotArea+TotalBsmtSF+YearRemodd_Dummy+
             MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual,
           data=retail.dummy)
summary(Model9)
anova(Model9)

#Model9 is better than Model8
anova(Model9,Model8)

#Adding quadratic terms
Model10<-lm(SalePrice~GrLivArea+FullBath+I(Fireplaces^2)+Fireplaces+KitchenAbvGr+
             LotArea+TotalBsmtSF+YearRemodd_Dummy+
             MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual,
           data=retail.dummy)
summary(Model10)
anova(Model10)

#I(Fireplaces^2) is not a significant term
anova(Model10,Model9)

Model11<-lm(SalePrice~GrLivArea+FullBath+I(KitchenAbvGr^2)+Fireplaces+KitchenAbvGr+
             LotArea+TotalBsmtSF+YearRemodd_Dummy+
             MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual,
           data=retail.dummy)
summary(Model11)
anova(Model11)

#I(KitchenAbvGr^2) is not a significant term
anova(Model11,Model9)

Model12<-lm(SalePrice~GrLivArea+FullBath+Fireplaces+KitchenAbvGr+LotArea+TotalBsmtSF+YearRemodd_Dummy+
             MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual+
              LotShape.IR2+LotShape.IR3+LotShape.Reg,data=retail.dummy)
summary(Model12)
anova(Model12)
#Model12 is better than Model9
anova(Model12,Model9)

Model13<-lm(SalePrice~GrLivArea+FullBath+Fireplaces+KitchenAbvGr+LotArea+TotalBsmtSF+YearRemodd_Dummy+
              MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual+
              LotShape.IR2+LotShape.IR3+LotShape.Reg+LandContour.HLS+LandContour.Low+LandContour.Lvl,data=retail.dummy)
summary(Model13)
anova(Model13)
#Model13 is better than Model12
anova(Model13,Model12)

Model14<-lm(SalePrice~GrLivArea+FullBath+Fireplaces+KitchenAbvGr+LotArea+TotalBsmtSF+YearRemodd_Dummy+
              MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual+
              LotShape.IR2+LotShape.IR3+LotShape.Reg+LandContour.HLS+LandContour.Low+LandContour.Lvl+
              SaleType.Con+SaleType.ConLD+SaleType.ConLI+SaleType.ConLw+SaleType.CWD+SaleType.New+SaleType.Oth+
              SaleType.WD,data=retail.dummy)
summary(Model14)
anova(Model14)
#Model14 is better than Model13
anova(Model14,Model13)

#Remove insignificant dummy variables
#Remove SaleType as it is mostly insignificant
Model15<-lm(SalePrice~GrLivArea+FullBath+Fireplaces+KitchenAbvGr+LotArea+TotalBsmtSF+YearRemodd_Dummy+
              MSSubClass_Dummy+MSZoning.FV+MSZoning.RH+MSZoning.RL+numGarage+OverallQual+
              LotShape.IR2+LotShape.IR3+LotShape.Reg+LandContour.HLS+LandContour.Low+LandContour.Lvl,data=retail.dummy)
summary(Model15)
anova(Model15)
anova(Model15,Model14)
#Final Model is Model15

#Assessing Outliers
outlierTest(Model15) # Bonferonni p-value for most extreme obs
qqPlot(Model15, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(Model15) # leverage plots

# Influential Observations
# added variable plots 
av.plots(Model15)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(Model15$coefficients)-2)) 
plot(Model15, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(Model15,	id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# Q-Q plot for studentized resid
qqPlot(Model15, main="Q-Q Plot")
# distribution of studentized residuals
sresid <- studres(Model15) 
hist(sresid, freq=FALSE, main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(Model15)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(Model15)

# Evaluate Collinearity
vif(Model15) # variance inflation factors 
sqrt(vif(Model15)) > 2 # problem?

# Test for Autocorrelated Errors
durbinWatsonTest(Model15)
