# Lauren Camero
# 1.14.2018
# read_ames.R

# Read in csv file for Ames housing data;
setwd('C:/Users/lcamero/Desktop/Predict 410')
file.name <- paste('ames_housing_data.csv',sep='');

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);

# Show the header of the data frame;
head(ames.df)

# List out the contents of the data frame;
# Use the structure function str();
str(ames.df)

# Show the distribution (and levels) of a discrete/factor type variable;
table(ames.df$LotShape)

# Note that table() will suppress the count of missing values;
# Here is how we force table() to show the missing values as a level;
table(ames.df$Fence,useNA=c('always'))
table(ames.df$SalePrice)


##########################################################################################
# Notes:
##########################################################################################

# Predictor variables first, not missing values first.Now that we have our data read into R, we need to define our sample population.
#	Let's think about our problem, and let's consult our data dictionary to find fields that would identify observations
#	that do not belong in our sample population.

##########################################################################################
# Add waterfall
##########################################################################################

# Single ifelse() statement
# ifelse(condition, value if condition is TRUE, value if the condition is FALSE)

# Nested ifelse() statement
# ifelse(condition1, value if condition1 is TRUE,
#	ifelse(condition2, value if condition2 is TRUE,
#	value if neither condition1 nor condition2 is TRUE
#	)
# )


# Create a waterfall of drop conditions;
# Work the data frame as a 'table' like you would in SAS or SQL;
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                       ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                                              ifelse(ames.df$YearBuilt <1920,'04: Built Pre-1920',
                                                     ifelse(ames.df$Functional != 'Typ','05: Not typical functionality',
                                                            ifelse(ames.df$GrLivArea <800,'06: LT 800 SqFt',
                                                                   '99: Eligible Sample')
                                                     )))));


table(ames.df$dropCondition)

# Save the table
waterfall <- table(ames.df$dropCondition);

# Format the table as a column matrix for presentation;
as.matrix(waterfall,7,1)


# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample');

# Check that all remaining observations are eligible;
#variable 1
table(ames.df$SalePrice)
summary(ames.df$SalePrice)
quantile(ames.df$SalePrice)
mean(ames.df$SalePrice)
sd(ames.df$SalePrice) 

ames.df$dropCondition1 <- ifelse(ames.df$SalePrice > 600000,'Recheck',
                                 '99: Eligible Sample')

noneligible.population <- subset(ames.df,dropCondition1 !='99: Eligible Sample');

#variable 2
table(ames.df$YearBuilt)
summary(ames.df$YearBuilt)
quantile(ames.df$YearBuilt)
mean(ames.df$YearBuilt)
sd(ames.df$YearBuilt) 
plot(ames.df$YearBuilt,ames.df$SalePrice)
boxplot(ames.df$YearBuilt)

#variable 3
table(ames.df$LotArea)
summary(ames.df$LotArea)
quantile(ames.df$LotArea)
mean(ames.df$LotArea)
sd(ames.df$LotArea)
plot(ames.df$LotArea,ames.df$SalePrice)
boxplot(ames.df$LotArea)

#variable 4
table(ames.df$OverallQual)
summary(ames.df$OverallQual)
quantile(ames.df$OverallQual)
mean(ames.df$OverallQual)
sd(ames.df$OverallQual)
plot(ames.df$OverallQual,ames.df$SalePrice)
boxplot(ames.df$OverallQual)

#variable 5
table(ames.df$OverallCond)
summary(ames.df$OverallCond)
quantile(ames.df$OverallCond)
mean(ames.df$OverallCond)
sd(ames.df$OverallCond)
plot(ames.df$OverallCond,ames.df$SalePrice)
boxplot(ames.df$OverallCond)

#variable 6
table(ames.df$YearRemodel)
summary(ames.df$YearRemodel)
quantile(ames.df$YearRemodel)
mean(ames.df$YearRemodel)
sd(ames.df$YearRemodel)
plot(ames.df$YearRemodel,ames.df$SalePrice)
boxplot(ames.df$YearRemodel)

#variable 7
table(ames.df$FullBath)
summary(ames.df$FullBath)
quantile(ames.df$FullBath)
mean(ames.df$FullBath)
sd(ames.df$FullBath)
plot(ames.df$FullBath,ames.df$SalePrice)
boxplot(ames.df$FullBath)

#variable 8
table(ames.df$HalfBath)
summary(ames.df$HalfBath)
quantile(ames.df$HalfBath)
mean(ames.df$HalfBath)
sd(ames.df$HalfBath)
plot(ames.df$HalfBath,ames.df$SalePrice)
boxplot(ames.df$HalfBath)

#variable 9
table(ames.df$BedroomAbvGr)
summary(ames.df$BedroomAbvGr)
quantile(ames.df$BedroomAbvGr)
mean(ames.df$BedroomAbvGr)
sd(ames.df$BedroomAbvGr)
plot(ames.df$BedroomAbvGr,ames.df$SalePrice)
boxplot(ames.df$BedroomAbvGr)

#variable 10
table(ames.df$TotRmsAbvGrd)
summary(ames.df$TotRmsAbvGrd)
quantile(ames.df$TotRmsAbvGrd)
mean(ames.df$TotRmsAbvGrd)
sd(ames.df$TotRmsAbvGrd)
plot(ames.df$TotRmsAbvGrd,ames.df$SalePrice)
boxplot(ames.df$TotRmsAbvGrd)

#variable 11
table(ames.df$Fireplaces)
summary(ames.df$Fireplaces)
quantile(ames.df$Fireplaces)
mean(ames.df$Fireplaces)
sd(ames.df$Fireplaces)
plot(ames.df$Fireplaces,ames.df$SalePrice)
boxplot(ames.df$Fireplaces)

#variable 12
table(ames.df$PoolArea)
summary(ames.df$PoolArea)
quantile(ames.df$PoolArea)
mean(ames.df$PoolArea)
sd(ames.df$PoolArea)
plot(ames.df$PoolArea,ames.df$SalePrice)
boxplot(ames.df$PoolArea)

#variable 13
table(ames.df$MoSold)
summary(ames.df$MoSold)
quantile(ames.df$MoSold)
mean(ames.df$MoSold)
sd(ames.df$MoSold)
plot(ames.df$MoSold,ames.df$SalePrice)
boxplot(ames.df$MoSold)

#variable 14
table(ames.df$YrSold)
summary(ames.df$YrSold)
quantile(ames.df$YrSold)
mean(ames.df$YrSold)
sd(ames.df$YrSold)
plot(ames.df$YrSold,ames.df$SalePrice)
boxplot(ames.df$YrSold)

#variable 15
table(ames.df$LotFrontage)
summary(ames.df$LotFrontage)
mean(ames.df$LotFrontage)
sd(ames.df$LotFrontage)
plot(ames.df$LotFrontage,ames.df$SalePrice)
boxplot(ames.df$LotFrontage)

#variable 16
table(ames.df$BsmtFinSF1)
summary(ames.df$BsmtFinSF1)
mean(ames.df$BsmtFinSF1)
sd(ames.df$BsmtFinSF1)
plot(ames.df$BsmtFinSF1,ames.df$SalePrice)
boxplot(ames.df$BsmtFinSF1)

#variable 17
table(ames.df$BsmtUnfSF)
summary(ames.df$BsmtUnfSF)
mean(ames.df$BsmtUnfSF)
sd(ames.df$BsmtUnfSF)
plot(ames.df$BsmtUnfSF,ames.df$SalePrice)
boxplot(ames.df$BsmtUnfSF)

#variable 18
table(ames.df$FirstFlrSF)
summary(ames.df$FirstFlrSF)
mean(ames.df$FirstFlrSF)
sd(ames.df$FirstFlrSF)
plot(ames.df$FirstFlrSF,ames.df$SalePrice)
boxplot(ames.df$FirstFlrSF)

#variable 19
table(ames.df$SecondFlrSF)
summary(ames.df$SecondFlrSF)
mean(ames.df$SecondFlrSF)
sd(ames.df$SecondFlrSF)
plot(ames.df$SecondFlrSF,ames.df$SalePrice)
boxplot(ames.df$SecondFlrSF)

#variable 20
table(ames.df$WoodDeckSF)
summary(ames.df$WoodDeckSF)
mean(ames.df$WoodDeckSF)
sd(ames.df$WoodDeckSF)
plot(ames.df$WoodDeckSF,ames.df$SalePrice)
boxplot(ames.df$WoodDeckSF)

par(mfrow=c(1,3))
boxplot(ames.df$LotArea, xlab = 'Lot Area')
boxplot(ames.df$WoodDeckSF, xlab = 'Wood Deck SF')
boxplot(ames.df$BsmtFinSF1, xlab = 'Finished Basement SF')

##Check if log(saleprice) is better

#variable 2
par(mfrow=c(1,2))
plot(ames.df$YearBuilt,ames.df$SalePrice)
plot(ames.df$YearBuilt,log(ames.df$SalePrice))

#variable 3
par(mfrow=c(1,2))
plot(ames.df$LotArea,ames.df$SalePrice)
plot(ames.df$LotArea,log(ames.df$SalePrice))

#variable 4
par(mfrow=c(1,2))
plot(ames.df$OverallQual,ames.df$SalePrice, xlab = 'Overall Quality', ylab = 'Sale Price')
plot(ames.df$OverallQual,log(ames.df$SalePrice), xlab = 'Overall Quality', ylab = 'Natural Log of Sale Price')


##########################################################################################
# Waterfall Notes:
##########################################################################################

#We have not defined our actual data sample yet.  We have defined the set of eligible
#cbservations. Due to the manner in which R handles missing values we will need to take
#one more step to define the sample population.  THAT STEP - decide which predictor
#variables that we want to use, par the data frame down to those variables, and then use 
#na.omit() to remove the observations with missing values.

#We still need to define our predictor variables.  Some predictor variables are defined
#on the data frame.  Some predictor variables will be defined using fields on the data
#frame.




# Lauren Camero
# 1.20.2018
# assignment#2.R

# Read in csv file for Ames housing data;

# Note that back slash is an escape character in R so we use \\ when we want \;
setwd('C:/Users/lcamero/Desktop/Predict 410')
file.name <- paste('ames_housing_data.csv',sep='');

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);


# Create a waterfall of drop conditions;
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                       ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                                              ifelse(ames.df$YearBuilt <1950,'04: Built Pre-1950',
                                                     ifelse(ames.df$TotalBsmtSF <1,'05: No Basement',
                                                            ifelse(ames.df$GrLivArea <800,'06: LT 800 SqFt',
                                                                   '99: Eligible Sample')
                                                     )))));

# Save the table
waterfall <- table(ames.df$dropCondition);

# Format the table as a column matrix for presentation;
as.matrix(waterfall,7,1)


# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(eligible.population$dropCondition);


##########################################################################################
# Create a list of interesting predictor variables
##########################################################################################

str(ames.df)

# Predictor variables that I like:
LotFrontage
LotArea
LotConfig
Neighborhood
HouseStyle
OverallQual
OverallCond
YearBuilt
YearRemodel
Exterior1
BsmtFinSF1
BsmtFinSF2
CentralAir
GrLivArea
BsmtFullBath
BsmtHalfBath
FullBath
HalfBath
BedroomAbvGr
TotRmsAbvGrd
Fireplaces
GarageCars
GarageArea
WoodDeckSF
OpenPorchSF
EnclosedPorch
ThreeSsnPorch
ScreenPorch
PoolArea
MoSold
YrSold
SaleCondition
SalePrice

# Make a vector of the names;

keep.vars <- c('SID','PID','LotFrontage','LotArea','LotConfig','Neighborhood',
               'HouseStyle','OverallQual','OverallCond','YearBuilt','YearRemodel','Exterior1',
               'BsmtFinSF1','BsmtFinSF2','CentralAir','GrLivArea','BsmtFullBath','BsmtHalfBath',
               'FullBath','HalfBath','BedroomAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars',
               'GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ThreeSsnPorch',
               'ScreenPorch','PoolArea','MoSold','YrSold','SaleCondition','SalePrice'
);


# Note that the R data frame is a (rectangular) list object which means that it can be 
# accessed in two ways - as a matrix or as a list;
# Note that the keep.vars are the COLUMNS that we want to keep, not the rows;

skinny.df <- eligible.population[,keep.vars];

# Use the structure command to view the contents of the data frame;
str(skinny.df)




##########################################################################################
# Delete observations with missing values 
##########################################################################################
sample.df <- na.omit(skinny.df);

# Check the change in dimension;
dim(skinny.df)
dim(sample.df)

dim(skinny.df)-dim(sample.df)



##########################################################################################
# Define some discrete variables and indicator variables 
##########################################################################################

# Define total square footage
sample.df$TotalSqftCalc <- sample.df$BsmtFinSF1+sample.df$BsmtFinSF2+sample.df$GrLivArea;

# Define total bathrooms
sample.df$TotalBathCalc <- sample.df$BsmtFullBath + 0.5*sample.df$BsmtHalfBath ++
  + sample.df$FullBath + 0.5*sample.df$HalfBath;


# Corner lot indicator
# ifelse(condition,valueTrue,valueFalse)
sample.df$CornerLotInd <- ifelse(sample.df$LotConfig=='Corner',1,0);

# Check how the indicator is assigned
table(sample.df$CornerLotInd,sample.df$LotConfig)

# Define two indicators for fire places
table(sample.df$Fireplaces)

# Intercept Adjustment for a single fireplace
sample.df$FireplaceInd1 <- ifelse((sample.df$Fireplaces>0)&(sample.df$Fireplaces<2),1,0);
table(sample.df$FireplaceInd1,sample.df$Fireplaces)

# Intercept Adjustment for 2 or more fireplaces
sample.df$FireplaceInd2 <- ifelse((sample.df$Fireplaces>1),1,0);
table(sample.df$FireplaceInd2,sample.df$Fireplaces)

# Additive Intercept Adjustment for a single fireplace
sample.df$FireplaceAdder1 <- ifelse((sample.df$Fireplaces>0),1,0);

# Additive Intercept Adjustment for 2 or more fireplaces
sample.df$FireplaceAdder2 <- ifelse((sample.df$Fireplaces>1),1,0);

table(sample.df$FireplaceAdder1,sample.df$Fireplaces)
table(sample.df$FireplaceAdder2,sample.df$Fireplaces)



# Central Air Indicator
sample.df$CentralAirInd <- ifelse(sample.df$CentralAir=='Y',1,0);
table(sample.df$CentralAirInd) 
# Looks like this is not useful since almost all homes have central air


# Exterior Siding Type
sample.df$BrickInd <- ifelse(sample.df$Exterior1=='BrkFace',1,0);
sample.df$VinylSidingInd <- ifelse(sample.df$Exterior1=='VinylSd',1,0);

# Pool Indicator
sample.df$PoolInd <- ifelse(sample.df$PoolArea>0,1,0);

# Wood Deck Indicator
sample.df$WoodDeckInd <- ifelse(sample.df$WoodDeckSF>0,1,0);

# Porch Indicator - Open Porch OR Screen Porch
sample.df$PorchInd <- ifelse((sample.df$OpenPorchSF>0)||(sample.df$ScreenPorch>0),1,0);

# Quality Index
sample.df$QualityIndex <- sample.df$OverallQual*sample.df$OverallCond;

table(sample.df$QualityIndex)


# Year Sold Indicators
sample.df$I2006 <- ifelse(sample.df$YrSold==2006,1,0);
sample.df$I2007 <- ifelse(sample.df$YrSold==2007,1,0);
sample.df$I2008 <- ifelse(sample.df$YrSold==2008,1,0);
sample.df$I2009 <- ifelse(sample.df$YrSold==2009,1,0);
sample.df$I2010 <- ifelse(sample.df$YrSold==2010,1,0);

table(sample.df$YrSold)
table(sample.df$I2006)
table(sample.df$I2007)
table(sample.df$I2008)
table(sample.df$I2009)
table(sample.df$I2010)


# List out sample.df
str(sample.df)



##########################################################################################
# Add a train/test flag to split the sample 
##########################################################################################
sample.df$u <- runif(n=dim(sample.df)[1],min=0,max=1);
sample.df$train <- ifelse(sample.df$u<0.70,1,0);

# Check the counts on the train/test split
table(sample.df$train)

# Check the train/test split as a percentage of whole
table(sample.df$train)/dim(sample.df)[1]



##########################################################################################
# Save data frame as an .RData data object 
##########################################################################################

# Save the R data frame as an .RData object
saveRDS(sample.df,file='ames_housing_data.csv');

# Read (or reload) the .RData object as an R data frame
a <- readRDS('ames_housing_data.csv');

# Check it
str(a)

########################################################################################
#Begin the EDA with Variable 1: total square feet
########################################################################################


# Check it
str(sample.df)

# Technically we should perform our EDA on the training data set
train.df <- subset(sample.df,train==1);



##################################################################################
# BASE R Scatterplot
##################################################################################

# Let's control the R plot to make it pretty
plot(train.df$TotalSqftCalc,train.df$SalePrice/1000,xlab='Total SQFT',ylab='Sale Price (000)',
     main='SQFT and Sale Price')

# In low dimensions scatterplots can be useful for visualizing the relationship
# between the response and a predictor variable


##################################################################################
# BASE R Box Plot
##################################################################################

boxplot(train.df$SalePrice/1000 ~ train.df$Neighborhood, las=2)
title('Sale Price By Neighborhood')

# Boxplots are the tool of choice for visualizing factor variables

##################################################################################
# Treating a discrete variable as continuous 
##################################################################################

plot(train.df$TotalSqftCalc,train.df$SalePrice)

########################################################################################
#Begin the EDA with Variable 2: Lot Area
########################################################################################

par(mfrow = c(1, 3))
plot(eligible.population$LotArea, eligible.population$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Lot Size (square feet)",
     main = 'Lot Area vs. Sales Price')
abline(lm(train.df$SalePrice ~ train.df$LotArea), col = "red")
lines(lowess(train.df$LotArea, train.df$SalePrice),
      col = "blue")
hist(train.df$LotArea, main='Lot Area', xlab = '')
boxplot(train.df$LotArea, main='Lot Area')

par(mfrow = c(1, 3))
plot(train.df$QualityIndex, train.df$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Lot Size (square feet)",
     main = 'Lot Area vs. Sales Price')
abline(lm(train.df$SalePrice ~ train.df$QualityIndex), col = "red")
lines(lowess(train.df$QualityIndex, train.df$SalePrice),
      col = "blue")
hist(train.df$QualityIndex, main='Quality Index', xlab = '')
boxplot(train.df$QualityIndex, main='Quality Index')

par(mfrow = c(1, 3))
plot(train.df$TotalSqftCalc, train.df$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Total Square Feet",
     main = 'Total Square Feet vs. Sales Price')
abline(lm(train.df$SalePrice ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc, train.df$SalePrice),
      col = "blue")
hist(train.df$TotalSqftCalc, main='Total Square Feet', xlab = '')
boxplot(train.df$TotalSqftCalc, main='Total Square Feet')


#title(main = "Residential Properties

# Sometime we will choose to treat a discrete variable as a continuous variable.
# In these cases the discrete variable needs to have 'enough' values, and a 'nice' 
# relationship with the response variable.

###################################################################################
#fit a regular model on Lot Area - model 2
###################################################################################

# Fit a linear regression model with R
model.2 <- lm(SalePrice ~ LotArea, data=train.df)

# Display model summary
summary(model.2)

# List out components of lm object
names(model.2)

# Access a component of lm object
model.2$coefficients

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.2 <- mean(model.2$residuals^2)
mae.2 <- mean(abs(model.2$residuals))

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.2)

# Hint:  We need to check the assumptions of normality and homoscedasticity;
# (1) QQ Plot
# (2) Scatterplot of residuals versus predictor


# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.2$residuals)
qqline(model.2$residuals)

# Make a scatterplot
plot(train.df$LotArea,model.2$residuals)
title('Residual vs Predictor')

# this ended up with a very low r squared, so I am looking at another variable
###################################################################################
#fit a regular model on Quality Index model - 3
###################################################################################

# Fit a linear regression model with R
model.3 <- lm(SalePrice ~ QualityIndex, data=train.df)

# Display model summary
summary(model.3)

# List out components of lm object
names(model.3)

# Access a component of lm object
model.3$coefficients

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.3 <- mean(model.3$residuals^2)
mae.3 <- mean(abs(model.3$residuals))

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.2)

# Hint:  We need to check the assumptions of normality and homoscedasticity;
# (1) QQ Plot
# (2) Scatterplot of residuals versus predictor

par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.3$residuals)
qqline(model.3$residuals)

# Make a scatterplot
#plot(train.df$QualityIndex,model.3$residuals, xlab = 'Quality Index', ylab = 'Resdiuals')
#title('Quality Index: Residual vs Predictor')
plot(train.df$QualityIndex, model.3$residuals,
     ylab = "Resdiuals",
     xlab = "Quality Index",
     main = 'Quality Index vs. Resdiuals')
abline(lm(model.3$residuals ~ train.df$QualityIndex), col = "red")
lines(lowess(train.df$QualityIndex,model.3$residuals),
      col = "blue")


# Note that these plots are sufficient to assess the GOF of a regression model in Predict 410.

###################################################################################
#fit a regular model on total Sqaure Feet - model 1
###################################################################################

# Fit a linear regression model with R
model.1 <- lm(SalePrice ~ TotalSqftCalc, data=train.df)

# Display model summary
summary(model.1)

# List out components of lm object
names(model.1)

# Access a component of lm object
model.1$coefficients

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.1 <- mean(model.1$residuals^2)
mae.1 <- mean(abs(model.1$residuals))


# BASE R diagnostic plot for lm object
plot(model.1)

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.1)

# Hint:  We need to check the assumptions of normality and homoscedasticity;
# (1) QQ Plot
# (2) Scatterplot of residuals versus predictor


# Use the Base R functon qqplot() to assess the normality of the residuals
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
qqnorm(model.1$residuals)
qqline(model.1$residuals)

# Make a scatterplot
#plot(train.df$TotalSqftCalc,model.1$residuals)
#title('Residual vs Predictor')
plot(train.df$TotalSqftCalc, model.3$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Total Square Feet vs. Resdiuals')
abline(lm(model.3$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.3$residuals),
      col = "blue")

# Note that these plots are sufficient to assess the GOF of a regression model in Predict 410.

###################################################################################
#fit a regular model on total Sqaure Feet & Quality Index - model 4
###################################################################################

# Fit a linear regression model with R
model.4 <- lm(SalePrice ~ TotalSqftCalc + QualityIndex, data=train.df)

# Display model summary
summary(model.4)

# List out components of lm object
names(model.4)

# Access a component of lm object
model.4$coefficients

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.4 <- mean(model.1$residuals^2)
mae.4 <- mean(abs(model.1$residuals))


# BASE R diagnostic plot for lm object
plot(model.4)
# Not toouseful for writing a report

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.4)

# Hint:  We need to check the assumptions of normality and homoscedasticity;
# (1) QQ Plot
# (2) Scatterplot of residuals versus predictor


# Use the Base R functon qqplot() to assess the normality of the residuals
par(mfrow = c(1, ), oma = c(0, 0, 2, 0))
qqnorm(model.4$residuals)
qqline(model.4$residuals)

# Make a scatterplot
plot(train.df$TotalSqftCalc+train.df$QualityIndex,model.4$residuals)
title('Residual vs Predictor 1')

# Make a scatterplot
plot(train.df$QualityIndex,model.4$residuals)
title('Residual vs Predictor 2')
# Note that these plots are sufficient to assess the GOF of a regression model in Predict 410.



########################################################################################
# Diagnostic plots using Weisberg's car package
########################################################################################
# First we need to install the car package
# See Section 1.2.4 p.31 of CAR (Companion to Applied Regression)

# Install package and all other needed packages
#install.packages('car', dependencies=TRUE)

# Load library into your active R session
#library(car)

# Use the function qqPlot() in the car package to assess the Studentized residuals
qqPlot(model.1)

# Note that this is not the typical QQ plot.  Also note that the Studentized residuals
# have a different distribution than the standard residuals.


# Cook's Distance Plot
influenceIndexPlot(model.1,vars=c('Cook','hat'))

# Note that the x index is not correct.  They are the row labels from the original
# data frame.

rownames(train.df) <- seq(1,length(model.1$residuals),1)
model.1 <- lm(SalePrice ~ TotalSqftCalc, data=train.df)
influenceIndexPlot(model.1,vars=c('Cook','hat'))

# If we want the labels correct, then we have to go back to the original data frame
# and fix the row names, then refit the model, and then call the plot.


#################################################################################
#add loess smoother to the scatterplots
#################################################################################

loessMod50 <- loess(model.1$residuals~ train.df$TotalSqftCalc,  span=0.50) # 50% smoothing span

smoothed50 <- predict(loessMod50) 

par(mfrow = c(1, 1))
plot(y = model.1$residuals, x = train.df$TotalSqftCalc, type="l"
     , main="Loess Smoothing and Prediction", xlab="Total Square Feet", ylab="Residuals")
lines(smoothed50, train.df$TotalSqftCalc, col="blue")

#definitely need to fix the loess model


###################################################################################
#Switch Quality Index model - 5 to log(SalesPrice)
###################################################################################

# Fit a linear regression model with R
model.5 <- lm(log(SalePrice) ~ QualityIndex, data=train.df)

# Display model summary
summary(model.5)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.5 <- mean(model.5$residuals^2)
mae.5 <- mean(abs(model.5$residuals))

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.5)

# Use the Base R functon qqplot() to assess the normality of the residuals
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# Make a scatterplot
plot(train.df$QualityIndex, model.3$residuals,
     ylab = "Resdiuals",
     xlab = "Quality Index",
     main = 'Quality Index vs. Resdiuals')
abline(lm(model.3$residuals ~ train.df$QualityIndex), col = "red")
lines(lowess(train.df$QualityIndex,model.3$residuals),
      col = "blue")

plot(train.df$QualityIndex, model.5$residuals,
     ylab = "Resdiuals",
     xlab = "Quality Index",
     main = 'Natural Log: Quality Index vs. Resdiuals')
abline(lm(model.5$residuals ~ train.df$QualityIndex), col = "red")
lines(lowess(train.df$QualityIndex,model.5$residuals),
      col = "blue")

qqnorm(model.3$residuals)
qqline(model.3$residuals)

qqnorm(model.5$residuals)
qqline(model.5$residuals)




###################################################################################
#Switch TotalSqftCalc model - 6 to log(SalesPrice)
###################################################################################

# Fit a linear regression model with R
model.6 <- lm(log(SalePrice) ~ TotalSqftCalc, data=train.df)

# Display model summary
summary(model.6)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.6 <- mean(model.6$residuals^2)
mae.6 <- mean(abs(model.6$residuals))

# BASE R diagnostic plot for lm object
plot(model.6)

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

plot(train.df$TotalSqftCalc, model.1$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Total Square Feet vs. Resdiuals')
abline(lm(model.1$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.1$residuals),
      col = "blue")

plot(train.df$TotalSqftCalc, model.6$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Natural Log: Total Square Feet vs. Resdiuals')
abline(lm(model.6$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.6$residuals),
      col = "blue")

qqnorm(model.1$residuals)
qqline(model.1$residuals)

qqnorm(model.6$residuals)
qqline(model.6$residuals)



###################################################################################
#Switch multiple regression model - 7 to log(SalesPrice)
###################################################################################

# Fit a linear regression model with R
model.7 <- lm(log(SalePrice) ~ TotalSqftCalc + QualityIndex, data=train.df)

# Display model summary
summary(model.7)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.7 <- mean(model.7$residuals^2)
mae.7 <- mean(abs(model.7$residuals))

# BASE R diagnostic plot for lm object
plot(model.7)

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.7)


# Make a scatterplot
plot(train.df$TotalSqftCalc,model.7$residuals)
title('Residual vs Predictor 1')

# Make a scatterplot
plot(train.df$QualityIndex,model.7$residuals)
title('Residual vs Predictor 2')

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

plot(train.df$TotalSqftCalc, model.1$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Total Square Feet vs. Resdiuals')
abline(lm(model.1$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.1$residuals),
      col = "blue")

plot(train.df$TotalSqftCalc, model.6$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Natural Log: Total Square Feet vs. Resdiuals')
abline(lm(model.6$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.6$residuals),
      col = "blue")
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
qqnorm(model.4$residuals, main = 'Untransformed')
qqline(model.4$residuals)

qqnorm(model.7$residuals, main = 'Transformed')
qqline(model.7$residuals)


# Lauren Camero
# 1.20.2018
# assignment#2.R

# Read in csv file for Ames housing data;

# Note that back slash is an escape character in R so we use \\ when we want \;
setwd('C:/Users/lcamero/Desktop/Predict 410')
file.name <- paste('ames_housing_data.csv',sep='');

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);


# Create a waterfall of drop conditions;
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                       ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                                              ifelse(ames.df$YearBuilt <1950,'04: Built Pre-1950',
                                                     ifelse(ames.df$TotalBsmtSF <1,'05: No Basement',
                                                            ifelse(ames.df$GrLivArea <800,'06: LT 800 SqFt',
                                                                   ifelse(ames.df$LotArea >40000,'07: LT under 40000 SqFt',
                                                                          '99: Eligible Sample')
                                                            ))))));

# Save the table
waterfall <- table(ames.df$dropCondition);

# Format the table as a column matrix for presentation;
as.matrix(waterfall,7,1)


# Eliminate all observations that are not part of the eligible sample population;
eligible.population <- subset(ames.df,dropCondition=='99: Eligible Sample');

# Check that all remaining observations are eligible;
table(eligible.population$dropCondition);


##########################################################################################
# Create a list of interesting predictor variables
##########################################################################################

str(ames.df)

# Predictor variables that I like:
LotFrontage
LotArea
LotConfig
Neighborhood
HouseStyle
OverallQual
OverallCond
YearBuilt
YearRemodel
Exterior1
BsmtFinSF1
BsmtFinSF2
CentralAir
GrLivArea
BsmtFullBath
BsmtHalfBath
FullBath
HalfBath
BedroomAbvGr
TotRmsAbvGrd
Fireplaces
GarageCars
GarageArea
WoodDeckSF
OpenPorchSF
EnclosedPorch
ThreeSsnPorch
ScreenPorch
PoolArea
MoSold
YrSold
SaleCondition
SalePrice

# Make a vector of the names;

keep.vars <- c('SID','PID','LotFrontage','LotArea','LotConfig','Neighborhood',
               'HouseStyle','OverallQual','OverallCond','YearBuilt','YearRemodel','Exterior1',
               'BsmtFinSF1','BsmtFinSF2','CentralAir','GrLivArea','BsmtFullBath','BsmtHalfBath',
               'FullBath','HalfBath','BedroomAbvGr','TotRmsAbvGrd','Fireplaces','GarageCars',
               'GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ThreeSsnPorch',
               'ScreenPorch','PoolArea','MoSold','YrSold','SaleCondition','SalePrice'
);


# Note that the R data frame is a (rectangular) list object which means that it can be 
# accessed in two ways - as a matrix or as a list;
# Note that the keep.vars are the COLUMNS that we want to keep, not the rows;

skinny.df <- eligible.population[,keep.vars];

# Use the structure command to view the contents of the data frame;
str(skinny.df)




##########################################################################################
# Delete observations with missing values 
##########################################################################################
sample.df <- na.omit(skinny.df);

# Check the change in dimension;
dim(skinny.df)
dim(sample.df)

dim(skinny.df)-dim(sample.df)



##########################################################################################
# Define some discrete variables and indicator variables 
##########################################################################################

# Define total square footage
sample.df$TotalSqftCalc <- sample.df$BsmtFinSF1+sample.df$BsmtFinSF2+sample.df$GrLivArea;

# Define total bathrooms
sample.df$TotalBathCalc <- sample.df$BsmtFullBath + 0.5*sample.df$BsmtHalfBath ++
  + sample.df$FullBath + 0.5*sample.df$HalfBath;


# Corner lot indicator
# ifelse(condition,valueTrue,valueFalse)
sample.df$CornerLotInd <- ifelse(sample.df$LotConfig=='Corner',1,0);

# Check how the indicator is assigned
table(sample.df$CornerLotInd,sample.df$LotConfig)

# Define two indicators for fire places
table(sample.df$Fireplaces)

# Intercept Adjustment for a single fireplace
sample.df$FireplaceInd1 <- ifelse((sample.df$Fireplaces>0)&(sample.df$Fireplaces<2),1,0);
table(sample.df$FireplaceInd1,sample.df$Fireplaces)

# Intercept Adjustment for 2 or more fireplaces
sample.df$FireplaceInd2 <- ifelse((sample.df$Fireplaces>1),1,0);
table(sample.df$FireplaceInd2,sample.df$Fireplaces)

# Additive Intercept Adjustment for a single fireplace
sample.df$FireplaceAdder1 <- ifelse((sample.df$Fireplaces>0),1,0);

# Additive Intercept Adjustment for 2 or more fireplaces
sample.df$FireplaceAdder2 <- ifelse((sample.df$Fireplaces>1),1,0);

table(sample.df$FireplaceAdder1,sample.df$Fireplaces)
table(sample.df$FireplaceAdder2,sample.df$Fireplaces)



# Central Air Indicator
sample.df$CentralAirInd <- ifelse(sample.df$CentralAir=='Y',1,0);
table(sample.df$CentralAirInd) 
# Looks like this is not useful since almost all homes have central air


# Exterior Siding Type
sample.df$BrickInd <- ifelse(sample.df$Exterior1=='BrkFace',1,0);
sample.df$VinylSidingInd <- ifelse(sample.df$Exterior1=='VinylSd',1,0);

# Pool Indicator
sample.df$PoolInd <- ifelse(sample.df$PoolArea>0,1,0);

# Wood Deck Indicator
sample.df$WoodDeckInd <- ifelse(sample.df$WoodDeckSF>0,1,0);

# Porch Indicator - Open Porch OR Screen Porch
sample.df$PorchInd <- ifelse((sample.df$OpenPorchSF>0)||(sample.df$ScreenPorch>0),1,0);

# Quality Index
sample.df$QualityIndex <- sample.df$OverallQual*sample.df$OverallCond;

table(sample.df$QualityIndex)


# Year Sold Indicators
sample.df$I2006 <- ifelse(sample.df$YrSold==2006,1,0);
sample.df$I2007 <- ifelse(sample.df$YrSold==2007,1,0);
sample.df$I2008 <- ifelse(sample.df$YrSold==2008,1,0);
sample.df$I2009 <- ifelse(sample.df$YrSold==2009,1,0);
sample.df$I2010 <- ifelse(sample.df$YrSold==2010,1,0);

table(sample.df$YrSold)
table(sample.df$I2006)
table(sample.df$I2007)
table(sample.df$I2008)
table(sample.df$I2009)
table(sample.df$I2010)


# List out sample.df
str(sample.df)



##########################################################################################
# Add a train/test flag to split the sample 
##########################################################################################
sample.df$u <- runif(n=dim(sample.df)[1],min=0,max=1);
sample.df$train <- ifelse(sample.df$u<0.70,1,0);

# Check the counts on the train/test split
table(sample.df$train)

# Check the train/test split as a percentage of whole
table(sample.df$train)/dim(sample.df)[1]



##########################################################################################
# Save data frame as an .RData data object 
##########################################################################################

# Save the R data frame as an .RData object
saveRDS(sample.df,file='ames_housing_data.csv');

# Read (or reload) the .RData object as an R data frame
a <- readRDS('ames_housing_data.csv');

# Check it
str(a)

########################################################################################
#Begin the EDA with Variable 1: total square feet
########################################################################################


# Check it
str(sample.df)

# Technically we should perform our EDA on the training data set
train.df <- subset(sample.df,train==1);



##################################################################################
# BASE R Scatterplot
##################################################################################

# Let's control the R plot to make it pretty
plot(train.df$TotalSqftCalc,train.df$SalePrice/1000,xlab='Total SQFT',ylab='Sale Price (000)',
     main='SQFT and Sale Price')

# In low dimensions scatterplots can be useful for visualizing the relationship
# between the response and a predictor variable


##################################################################################
# BASE R Box Plot
##################################################################################
par(mfrow = c(1, 1))
boxplot(train.df$SalePrice/1000 ~ train.df$Neighborhood, las=2)
title('Sale Price By Neighborhood')

# Boxplots are the tool of choice for visualizing factor variables

##################################################################################
# Treating a discrete variable as continuous 
##################################################################################

plot(train.df$TotalSqftCalc,train.df$SalePrice)

########################################################################################
#Begin the EDA with Variable 2: Lot Area
########################################################################################

par(mfrow = c(1, 3))
plot(train.df$LotArea, train.df$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Lot Size (square feet)",
     main = 'Lot Area vs. Sales Price')
abline(lm(train.df$SalePrice ~ train.df$LotArea), col = "red")
lines(lowess(train.df$LotArea, train.df$SalePrice),
      col = "blue")
hist(train.df$LotArea, main='Lot Area', xlab = '')
boxplot(train.df$LotArea, main='Lot Area')

par(mfrow = c(1, 3))
plot(train.df$QualityIndex, train.df$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Quality Index",
     main = 'Quality Index vs. Sales Price')
abline(lm(train.df$SalePrice ~ train.df$QualityIndex), col = "red")
lines(lowess(train.df$QualityIndex, train.df$SalePrice),
      col = "blue")
hist(train.df$QualityIndex, main='Quality Index', xlab = '')
boxplot(train.df$QualityIndex, main='Quality Index')

par(mfrow = c(1, 3))
plot(train.df$TotalSqftCalc, train.df$SalePrice,
     ylab = "Sale Price ($$)",
     xlab = "Total Square Feet",
     main = 'Total Square Feet vs. Sales Price')
abline(lm(train.df$SalePrice ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc, train.df$SalePrice),
      col = "blue")
hist(train.df$TotalSqftCalc, main='Total Square Feet', xlab = '')
boxplot(train.df$TotalSqftCalc, main='Total Square Feet')


cor(train.df$TotalSqftCalc,train.df$SalePrice)
cor(train.df$LotArea,train.df$SalePrice)
cor(train.df$QualityIndex,train.df$SalePrice)
cor(train.df$BsmtFullBath,train.df$SalePrice)
cor(train.df$BedroomAbvGr,train.df$SalePrice)
cor(train.df$Fireplaces,train.df$SalePrice)

table(sample.df$BldgType)


#title(main = "Residential Properties

# Sometime we will choose to treat a discrete variable as a continuous variable.
# In these cases the discrete variable needs to have 'enough' values, and a 'nice' 
# relationship with the response variable.

###################################################################################
#fit a regular model on Lot Area - model 2
###################################################################################

# Fit a linear regression model with R
model.2 <- lm(SalePrice ~ LotArea, data=train.df)

# Display model summary
summary(model.2)

# List out components of lm object
names(model.2)

# Access a component of lm object
model.2$coefficients

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.2 <- mean(model.2$residuals^2)
mae.2 <- mean(abs(model.2$residuals))

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.2)

# Hint:  We need to check the assumptions of normality and homoscedasticity;
# (1) QQ Plot
# (2) Scatterplot of residuals versus predictor


# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.2$residuals)
qqline(model.2$residuals)

# Make a scatterplot
plot(train.df$LotArea,model.2$residuals)
title('Residual vs Predictor')

# this ended up with a very low r squared, so I am looking at another variable
###################################################################################
#fit a regular model on Quality Index model - 3
###################################################################################

# Fit a linear regression model with R
model.3 <- lm(SalePrice ~ QualityIndex, data=train.df)

# Display model summary
summary(model.3)

# List out components of lm object
names(model.3)

# Access a component of lm object
model.3$coefficients

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.3 <- mean(model.3$residuals^2)
mae.3 <- mean(abs(model.3$residuals))

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.2)

# Hint:  We need to check the assumptions of normality and homoscedasticity;
# (1) QQ Plot
# (2) Scatterplot of residuals versus predictor

par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
# Use the Base R functon qqplot() to assess the normality of the residuals
qqnorm(model.3$residuals)
qqline(model.3$residuals)

# Make a scatterplot
#plot(train.df$QualityIndex,model.3$residuals, xlab = 'Quality Index', ylab = 'Resdiuals')
#title('Quality Index: Residual vs Predictor')
plot(train.df$QualityIndex, model.3$residuals,
     ylab = "Resdiuals",
     xlab = "Quality Index",
     main = 'Quality Index vs. Resdiuals')
abline(lm(model.3$residuals ~ train.df$QualityIndex), col = "red")
lines(lowess(train.df$QualityIndex,model.3$residuals),
      col = "blue")


# Note that these plots are sufficient to assess the GOF of a regression model in Predict 410.

###################################################################################
#fit a regular model on total Sqaure Feet - model 1
###################################################################################

# Fit a linear regression model with R
model.1 <- lm(SalePrice ~ TotalSqftCalc, data=train.df)

# Display model summary
summary(model.1)

# List out components of lm object
names(model.1)

# Access a component of lm object
model.1$coefficients

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.1 <- mean(model.1$residuals^2)
mae.1 <- mean(abs(model.1$residuals))


# BASE R diagnostic plot for lm object
plot(model.1)

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.1)

# Hint:  We need to check the assumptions of normality and homoscedasticity;
# (1) QQ Plot
# (2) Scatterplot of residuals versus predictor


# Use the Base R functon qqplot() to assess the normality of the residuals
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
qqnorm(model.1$residuals)
qqline(model.1$residuals)

# Make a scatterplot
#plot(train.df$TotalSqftCalc,model.1$residuals)
#title('Residual vs Predictor')
plot(train.df$TotalSqftCalc, model.3$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Total Square Feet vs. Resdiuals')
abline(lm(model.3$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.3$residuals),
      col = "blue")

# Note that these plots are sufficient to assess the GOF of a regression model in Predict 410.

###################################################################################
#fit a regular model on total Sqaure Feet & Quality Index - model 4
###################################################################################

# Fit a linear regression model with R
model.4 <- lm(SalePrice ~ TotalSqftCalc + QualityIndex, data=train.df)

# Display model summary
summary(model.4)

# List out components of lm object
names(model.4)

# Access a component of lm object
model.4$coefficients

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.4 <- mean(model.4$residuals^2)
mae.4 <- mean(abs(model.4$residuals))


# BASE R diagnostic plot for lm object
plot(model.4)
# Not toouseful for writing a report

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.4)

# Hint:  We need to check the assumptions of normality and homoscedasticity;
# (1) QQ Plot
# (2) Scatterplot of residuals versus predictor


# Use the Base R functon qqplot() to assess the normality of the residuals
par(mfrow = c(1, ), oma = c(0, 0, 2, 0))
qqnorm(model.4$residuals)
qqline(model.4$residuals)

# Make a scatterplot
plot(train.df$TotalSqftCalc+train.df$QualityIndex,model.4$residuals)
title('Residual vs Predictor 1')

# Make a scatterplot
plot(train.df$QualityIndex,model.4$residuals)
title('Residual vs Predictor 2')
# Note that these plots are sufficient to assess the GOF of a regression model in Predict 410.

par(mfrow = c(1, 1))
boxplot(model.4$residuals ~ train.df$Neighborhood, las=2)

meanSalePrice <- aggregate(train.df$SalePrice, by=list(Neighborhood=train.df$Neighborhood), FUN=mean)
colnames(meanSalePrice) <- c('Neighborhood','meanSalePrice')

mae.neighborhood <- aggregate(abs(model.4$residuals), by=list(Neighborhood=train.df$Neighborhood), FUN=mean)
mae.4 <- mean(abs(model.4$residuals))
train.df$price.per.sqft <- train.df$SalePrice/train.df$TotalSqftCalc
mean.price.per.neighborhood <- aggregate(train.df$price.per.sqft, by=list(Neighborhood=train.df$Neighborhood), FUN=mean)  

plot(mean.price.per.neighborhood$x, mae.neighborhood$x, 
     xlab = 'Mean Price per Square Foot by Neighborhood', 
     ylab = 'Mean MAE by Neighborhood')

##########################################################################################
#Code a family of indicator variables to include in your multiple regression model
# Table GarageCars
table(sample.df$GarageCars)

# Let's create a family of indicator variables;
# We will take the baseline category to be 0;
# What does this mean?  Why am I creating three indicator variables?
# Should I be creating an indicator variable for 0?
sample.df$garage1 <- ifelse(sample.df$GarageCars==1,1,0);
sample.df$garage2 <- ifelse(sample.df$GarageCars==2,1,0);
sample.df$garage3 <- ifelse(sample.df$GarageCars>=3,1,0);


# Check the indicator assignment against the original table results;
table(sample.df$garage1)
table(sample.df$garage2)
table(sample.df$garage3)



# Fit a linear regression model with R
model.10 <- lm(SalePrice ~ TotalSqftCalc + garage1 + garage2 + garage3, data=sample.df)

# Display model summary
summary(model.10)


# Fit a second model;
model.20 <- lm(SalePrice ~ TotalSqftCalc + garage2 + garage3, data=sample.df)

# Display model summary
summary(model.20)
#our base category is 0
mae.20 <- mean(abs(model.20$residuals))

mae.20
mae.4
#the MAE of the multiple regression using size of the garage in car capacity is much less than the original MLR

######################################################################################
#Difference in sales price vs log of sales price
######################################################################################
model.30 <- lm(SalePrice ~ TotalSqftCalc + QualityIndex + TotRmsAbvGrd  + LotArea + garage2 + garage3, data=train.df)
model.40 <- lm(log(SalePrice) ~ TotalSqftCalc + QualityIndex + TotRmsAbvGrd  + LotArea + garage2 + garage3, data=train.df)

summary(model.30)
summary(model.40)

mse.30 <- mean(model.30$residuals^2);
mae.30 <- mean(abs(model.30$residuals));

mse.40 <- mean((train.df$SalePrice-exp(model.40$fitted.values))^2);
mae.40 <- mean(abs(train.df$SalePrice-exp(model.40$fitted.values)));

mse.30
mae.30
mse.40
mae.40


# For the orig model
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
qqnorm(model.30$residuals)
qqline(model.30$residuals)

# Make a scatterplot
#plot(train.df$TotalSqftCalc,model.1$residuals)
#title('Residual vs Predictor')
plot(train.df$TotalSqftCalc + train.df$QualityIndex + train.df$TotRmsAbvGrd  + train.df$LotArea 
     + train.df$garage2 + train.df$garage3, model.30$residuals,
     ylab = "Resdiuals",
     xlab = "Predictors",
     main = 'Predictors vs. Resdiuals')

#for the log model
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
qqnorm(model.40$residuals)
qqline(model.40$residuals)

# Make a scatterplot
#plot(train.df$TotalSqftCalc,model.1$residuals)
#title('Residual vs Predictor')
plot(train.df$TotalSqftCalc + train.df$QualityIndex + train.df$TotRmsAbvGrd  + train.df$LotArea 
     + train.df$garage2 + train.df$garage3, model.40$residuals,
     ylab = "Resdiuals",
     xlab = "Predictors",
     main = 'Predictors vs. Resdiuals')


########################################################################################
# Diagnostic plots using Weisberg's car package
########################################################################################
# First we need to install the car package
# See Section 1.2.4 p.31 of CAR (Companion to Applied Regression)

# Install package and all other needed packages
#install.packages('car', dependencies=TRUE)

# Load library into your active R session
#library(car)

# Use the function qqPlot() in the car package to assess the Studentized residuals
qqPlot(model.1)

# Note that this is not the typical QQ plot.  Also note that the Studentized residuals
# have a different distribution than the standard residuals.


# Cook's Distance Plot
influenceIndexPlot(model.1,vars=c('Cook','hat'))

# Note that the x index is not correct.  They are the row labels from the original
# data frame.

rownames(train.df) <- seq(1,length(model.1$residuals),1)
model.1 <- lm(SalePrice ~ TotalSqftCalc, data=train.df)
influenceIndexPlot(model.1,vars=c('Cook','hat'))

# If we want the labels correct, then we have to go back to the original data frame
# and fix the row names, then refit the model, and then call the plot.


#################################################################################
#add loess smoother to the scatterplots
#################################################################################

loessMod50 <- loess(model.1$residuals~ train.df$TotalSqftCalc,  span=0.50) # 50% smoothing span

smoothed50 <- predict(loessMod50) 

par(mfrow = c(1, 1))
plot(y = model.1$residuals, x = train.df$TotalSqftCalc, type="l"
     , main="Loess Smoothing and Prediction", xlab="Total Square Feet", ylab="Residuals")
lines(smoothed50, train.df$TotalSqftCalc, col="blue")

#definitely need to fix the loess model


###################################################################################
#Switch Quality Index model - 5 to log(SalesPrice)
###################################################################################

# Fit a linear regression model with R
model.5 <- lm(log(SalePrice) ~ QualityIndex, data=train.df)

# Display model summary
summary(model.5)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.5 <- mean(model.5$residuals^2)
mae.5 <- mean(abs(model.5$residuals))

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.5)

# Use the Base R functon qqplot() to assess the normality of the residuals
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
# Make a scatterplot
plot(train.df$QualityIndex, model.3$residuals,
     ylab = "Resdiuals",
     xlab = "Quality Index",
     main = 'Quality Index vs. Resdiuals')
abline(lm(model.3$residuals ~ train.df$QualityIndex), col = "red")
lines(lowess(train.df$QualityIndex,model.3$residuals),
      col = "blue")

plot(train.df$QualityIndex, model.5$residuals,
     ylab = "Resdiuals",
     xlab = "Quality Index",
     main = 'Natural Log: Quality Index vs. Resdiuals')
abline(lm(model.5$residuals ~ train.df$QualityIndex), col = "red")
lines(lowess(train.df$QualityIndex,model.5$residuals),
      col = "blue")

qqnorm(model.3$residuals)
qqline(model.3$residuals)

qqnorm(model.5$residuals)
qqline(model.5$residuals)




###################################################################################
#Switch TotalSqftCalc model - 6 to log(SalesPrice)
###################################################################################

# Fit a linear regression model with R
model.6 <- lm(log(SalePrice) ~ TotalSqftCalc, data=train.df)

# Display model summary
summary(model.6)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.6 <- mean(model.6$residuals^2)
mae.6 <- mean(abs(model.6$residuals))

# BASE R diagnostic plot for lm object
plot(model.6)

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

plot(train.df$TotalSqftCalc, model.1$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Total Square Feet vs. Resdiuals')
abline(lm(model.1$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.1$residuals),
      col = "blue")

plot(train.df$TotalSqftCalc, model.6$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Natural Log: Total Square Feet vs. Resdiuals')
abline(lm(model.6$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.6$residuals),
      col = "blue")

qqnorm(model.1$residuals)
qqline(model.1$residuals)

qqnorm(model.6$residuals)
qqline(model.6$residuals)



###################################################################################
#Switch multiple regression model - 7 to log(SalesPrice)
###################################################################################

# Fit a linear regression model with R
model.7 <- lm(log(SalePrice) ~ TotalSqftCalc + QualityIndex, data=train.df)

# Display model summary
summary(model.7)

# Access residuals to compute Mean Square Error (MSE) and Mean Absolute Error (MAE)
mse.7 <- mean(model.7$residuals^2)
mae.7 <- mean(abs(model.7$residuals))

# BASE R diagnostic plot for lm object
plot(model.7)

# Panel the plots
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.7)


# Make a scatterplot
plot(train.df$TotalSqftCalc,model.7$residuals)
title('Residual vs Predictor 1')

# Make a scatterplot
plot(train.df$QualityIndex,model.7$residuals)
title('Residual vs Predictor 2')

par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))

plot(train.df$TotalSqftCalc, model.1$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Total Square Feet vs. Resdiuals')
abline(lm(model.1$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.1$residuals),
      col = "blue")

plot(train.df$TotalSqftCalc, model.6$residuals,
     ylab = "Resdiuals",
     xlab = "Total Square Feet",
     main = 'Natural Log: Total Square Feet vs. Resdiuals')
abline(lm(model.6$residuals ~ train.df$TotalSqftCalc), col = "red")
lines(lowess(train.df$TotalSqftCalc,model.6$residuals),
      col = "blue")
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
qqnorm(model.4$residuals, main = 'Untransformed')
qqline(model.4$residuals)

qqnorm(model.7$residuals, main = 'Transformed')
qqline(model.7$residuals)


# Lauren Camero
# 2.11.2018
# assignment#5.R

# Read in csv file for Ames housing data;

# Note that back slash is an escape character in R so we use \\ when we want \;
setwd('C:/Users/lcamero/Desktop/Predict 410')
file.name <- paste('ames_housing_data.csv',sep='');

# Read in the csv file into an R data frame;
ames.df <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);
data<-na.omit(ames.df)

# Create a waterfall of drop conditions;
ames.df$dropCondition <- ifelse(ames.df$BldgType!='1Fam','01: Not SFR',
                                ifelse(ames.df$SaleCondition!='Normal','02: Non-Normal Sale',
                                       ifelse(ames.df$Street!='Pave','03: Street Not Paved',
                                              ifelse(ames.df$YearBuilt <1950,'04: Built Pre-1950',
                                                     ifelse(ames.df$TotalBsmtSF <1,'05: No Basement',
                                                            ifelse(ames.df$GrLivArea <800,'06: LT 800 SqFt',
                                                                   ifelse(ames.df$LotArea >40000,'07: LT under 40000 SqFt',
                                                                          '99: Eligible Sample')
                                                            ))))));

# Save the table
waterfall <- table(ames.df$dropCondition);

# Format the table as a column matrix for presentation;
as.matrix(waterfall,7,1)


# Eliminate all observations that are not part of the eligible sample population;
my.data <- subset(ames.df,dropCondition=='99: Eligible Sample');
my.data$TotalSqftCalc <- my.data$BsmtFinSF1+my.data$BsmtFinSF2+my.data$GrLivArea;

# Porch Indicator - Open Porch OR Screen Porch
my.data$PorchInd <- ifelse((my.data$OpenPorchSF>0)||(my.data$ScreenPorch>0),1,0);

# Quality Index
my.data$QualityIndex <- my.data$OverallQual*my.data$OverallCond;

# Year Sold Indicators
my.data$I2006 <- ifelse(my.data$YrSold==2006,1,0);
my.data$I2007 <- ifelse(my.data$YrSold==2007,1,0);
my.data$I2008 <- ifelse(my.data$YrSold==2008,1,0);
my.data$I2009 <- ifelse(my.data$YrSold==2009,1,0);
my.data$I2010 <- ifelse(my.data$YrSold==2010,1,0);

#######################################################################################
#use uniform random sampling to split the sample 70/30
#######################################################################################

# Set the seed on the random number generator so you get the same split every time that
# you run the code.
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1);

# Create train/test split;
train.df <- subset(my.data, u<0.70);
test.df <- subset(my.data, u>=0.70);

# Check your data split. 
dim(my.data)
dim(train.df)
dim(test.df)
dim(train.df)+dim(test.df)
str(train.clean)

#########################################################################################
#we create a dataset containing only the predictor variables we want to consider
#########################################################################################

drop.list <- c('SID','PID','LotConfig','Neighborhood','HouseStyle','YearBuilt','YearRemodel',
               'Exterior1','BsmtFinSF1','BsmtFinSF2','CentralAir','YrSold','MoSold','SaleCondition',
               'u','train','I2010','BsmtFullBath','BsmtHalfBath','FullBath','HalfBath',
               'FireplaceInd1','FireplaceInd2','OverallQual','OverallCond','PoolArea','GrLivArea',
               'BldgType','LotFrontage','Fence','SubClass','Zoning','Street','LotShape','LandCountour',
               'Street','I2010','I2009','I2008','SaleCondition','SaleType','YrSold','MoSold','MiscVal',
               'PoolQC', 'LandSlope','Condition1','Condition2','HouseStyle','YearRemodel','RoofStyle','RoofMat',
               'MiscFeature','WoodDeckSF','OpenPorchSF','EnclosedPorch','ThreeSsnPorch','ScreenPorch','PoolArea',
               'dropCondition','PoolQC','LandCountour','Exterior2','MasVnrType','MasVnrArea','ExterQual','ExterCond',
               'FireplaceQu','BsmtQual','BsmtCond','BsmtExposure','BsmtFinType1','BsmtFinType1','BsmtUnfSF','Heating',
               'Alley','HeatingQC','Electrical','FirstFlrSF','SecondFlrSF','LowQualFinSF','KitchenQual','Functional',
               'Utilities','GarageType','GarageYrBlt',
               'RoofStyleGambrel','BsmtCondTA','GarageCond');
train.cleaned <-train.df[,!(names(my.data) %in% drop.list)];
train.clean <- na.omit(train.cleaned)
test.cleaned <-test.df[,!(names(my.data) %in% drop.list)];
test.clean <- na.omit(test.cleaned)

#########################################################################################
#specify the upper model and lower models
#########################################################################################

# Define the upper model as the FULL model
summary(train.clean)
upper.lm <- lm(SalePrice ~ .,data=train.clean);
summary(upper.lm)
# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean);
# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean);
summary(sqft.lm)

#########################################################################################
#step AIC
#########################################################################################

# Note: There is only one function for classical model selection in R - stepAIC();
# stepAIC() is part of the MASS library.
# The MASS library comes with the BASE R distribution, but you still need to load it;
library(MASS)
# Call stepAIC() for variable selection
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1),
                      direction=c('forward'));
summary(forward.lm)
str(forward.lm)

backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=train.df)
summary(junk.lm)


ld.vars <- attributes(alias(backward.lm)$Complete)$dimnames[[1]]

# Compute the VIF values
library(car)
str(train.clean)
summary(forward.lm)
sort(vif(forward.lm),decreasing=TRUE)
sort(vif(backward.lm),decreasing=TRUE)
sort(vif(stepwise.lm),decreasing=TRUE)

vif(forward.lm)
vif(backward.lm)
vif(stepwise.lm)
vif(junk.lm)
as.matrix(vif(junk.lm),7,1)

AIC(forward.lm)
AIC(backward.lm)
AIC(stepwise.lm)
AIC(junk.lm)

BIC(forward.lm)
BIC(backward.lm)
BIC(stepwise.lm)
BIC(junk.lm)

mean(forward.lm$residuals^2)
mean(backward.lm$residuals^2)
mean(stepwise.lm$residuals^2)
mean(junk.lm$residuals^2)

mean(abs(forward.lm$residuals))
mean(abs(backward.lm$residuals))
mean(abs(stepwise.lm$residuals))
mean(abs(junk.lm$residuals))

# Define the upper model as the FULL model
upper.test <- lm(SalePrice ~ .,data=test.clean);
summary(upper.test)
# Define the lower model as the Intercept model
lower.test <- lm(SalePrice ~ 1,data=test.clean);
# Need a SLR to initialize stepwise selection
sqft.test <- lm(SalePrice ~ TotalSqftCalc,data=test.clean);
summary(sqft.test)

forward.test <- stepAIC(object=lower.test,scope=list(upper=formula(upper.test),lower=~1),
                        direction=c('forward'));
summary(forward.test)

backward.test <- stepAIC(object=upper.test,direction=c('backward'));
summary(backward.test)

stepwise.test <- stepAIC(object=sqft.test,scope=list(upper=formula(upper.test),lower=~1),
                         direction=c('both'));
summary(stepwise.test)

junk.test <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + TotalSqftCalc, data=test.df)
summary(junk.test)

summary(forward.test)
summary(backward.test)
summary(stepwise.test)
summary(junk.test)

AIC(forward.test)
AIC(backward.test)
AIC(stepwise.test)
AIC(junk.test)

BIC(forward.test)
BIC(backward.test)
BIC(stepwise.test)
BIC(junk.test)

mean(forward.test$residuals^2)
mean(backward.test$residuals^2)
mean(stepwise.test$residuals^2)
mean(junk.test$residuals^2)

mean(abs(forward.test$residuals))
mean(abs(backward.test$residuals))
mean(abs(stepwise.test$residuals))
mean(abs(junk.test$residuals))

forward.test <- predict(forward.test,newdata=test.clean);
backward.test <- predict(backward.test,newdata=test.clean);
stepwise.test <- predict(stepwise.test,newdata=test.clean);
junk.test <- predict(junk.test,newdata=test.df);

# Training Data
# Abs Pct Error
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice;
# Assign Prediction Grades;
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )
)
forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)
as.matrix(forward.trainTable,4,3)
help(hist)
# Test Data
# Abs Pct Error
forward.testPCT <- abs(test.clean$SalePrice-forward.test)/test.clean$SalePrice
backward.testPCT <- abs(test.clean$SalePrice-backward.test)/test.clean$SalePrice
stepwise.testPCT <- abs(test.clean$SalePrice-stepwise.test)/test.clean$SalePrice
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice

# Assign Prediction Grades;
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )
)
forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)
as.matrix(forward.testTable,4,3)


###########################################################################################
#Junk

junk.pct <- abs(junk.lm$residuals)/train.df$SalePrice
# Assign Prediction Grades;
junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]',
                               ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                      ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                             'Grade 4: (0.25+]')
                               )
)
junk.trainTable <- table(junk.PredictionGrade)
junk.trainTable/sum(junk.trainTable)
as.matrix(junk.trainTable,4,3)

junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice

# Assign Prediction Grades;
junk.testPredictionGrade <- ifelse(junk.testPCT<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(junk.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(junk.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )
)
junk.testTable <-table(junk.testPredictionGrade)
junk.testTable/sum(junk.testTable)
as.matrix(junk.testTable,4,3)
setwd('C:/Users/lcamero/Desktop/Predict 410')
file.name <- paste('stock_portfolio.csv',sep='');
my.data <- read.csv(file.name,header=TRUE,stringsAsFactors=FALSE);
head(my.data)
str(my.data)
# Note Date is a string of dd-Mon-yy in R this is '%d-%B-%y';
my.data$RDate <- as.Date(my.data$Date,'%d-%B-%y');
sorted.df <- my.data[order(my.data$RDate),];
head(sorted.df)
AA <- log(sorted.df$AA[-1]/sorted.df$AA[-dim(sorted.df)[1]]);
# Manually check the first entry: log(9.45/9.23)
# Type cast the array as a data frame;
returns.df <- as.data.frame(AA);
returns.df$BAC <- log(sorted.df$BAC[-1]/sorted.df$BAC[-dim(sorted.df)[1]]);
returns.df$BHI <- log(sorted.df$BHI[-1]/sorted.df$BHI[-dim(sorted.df)[1]]);
returns.df$CVX <- log(sorted.df$CVX[-1]/sorted.df$CVX[-dim(sorted.df)[1]]);
returns.df$DD <- log(sorted.df$DD[-1]/sorted.df$DD[-dim(sorted.df)[1]]);
returns.df$DOW <- log(sorted.df$DOW[-1]/sorted.df$DOW[-dim(sorted.df)[1]]);
returns.df$DPS <- log(sorted.df$DPS[-1]/sorted.df$DPS[-dim(sorted.df)[1]]);
returns.df$GS <- log(sorted.df$GS[-1]/sorted.df$GS[-dim(sorted.df)[1]]);
returns.df$HAL <- log(sorted.df$HAL[-1]/sorted.df$HAL[-dim(sorted.df)[1]]);
returns.df$HES <- log(sorted.df$HES[-1]/sorted.df$HES[-dim(sorted.df)[1]]);
returns.df$HON <- log(sorted.df$HON[-1]/sorted.df$HON[-dim(sorted.df)[1]]);
returns.df$HUN <- log(sorted.df$HUN[-1]/sorted.df$HUN[-dim(sorted.df)[1]]);
returns.df$JPM <- log(sorted.df$JPM[-1]/sorted.df$JPM[-dim(sorted.df)[1]]);
returns.df$KO <- log(sorted.df$KO[-1]/sorted.df$KO[-dim(sorted.df)[1]]);
returns.df$MMM <- log(sorted.df$MMM[-1]/sorted.df$MMM[-dim(sorted.df)[1]]);
returns.df$MPC <- log(sorted.df$MPC[-1]/sorted.df$MPC[-dim(sorted.df)[1]]);
returns.df$PEP <- log(sorted.df$PEP[-1]/sorted.df$PEP[-dim(sorted.df)[1]]);
returns.df$SLB <- log(sorted.df$SLB[-1]/sorted.df$SLB[-dim(sorted.df)[1]]);
returns.df$WFC <- log(sorted.df$WFC[-1]/sorted.df$WFC[-dim(sorted.df)[1]]);
returns.df$XOM <- log(sorted.df$XOM[-1]/sorted.df$XOM[-dim(sorted.df)[1]]);
returns.df$VV <- log(sorted.df$VV[-1]/sorted.df$VV[-dim(sorted.df)[1]]);

# Compute correlation matrix for returns;
returns.cor <- cor(returns.df)
returns.cor[,c('VV')]
# Barplot the last column to visualize magnitude of correlations;
barplot(returns.cor[1:20,c('VV')],las=2,ylim=c(0,1.0))
title('Correlations with VV')

# Make correlation plot for returns;
# If you need to install corrplot package; Note how many dependencies this package has;
#install.packages('corrplot', dependencies=TRUE)
library(corrplot)
corrplot(returns.cor)

# load car package
library(car)
# Fit some model
model.1 <- lm(VV ~ GS+DD+DOW+HON+HUN+JPM+KO+MMM+XOM, data=returns.df)
summary(model.1)
vif(model.1)
# Fit the full model
model.2 <- lm(VV ~ AA+BAC+GS+JPM+WFC+BHI+CVX+DD+DOW+DPS+HAL+HES+HON+HUN+KO+MMM+MPC+PEP+SLB+XOM,
              data=returns.df)
summary(model.2)
vif(model.2)

returns.pca <- princomp(x=returns.df[,-21],cor=TRUE)
help("princomp")
# See the output components returned by princomp();
names(returns.pca)
pc.1 <- returns.pca$loadings[,1];
pc.2 <- returns.pca$loadings[,2];
names(pc.1)
help(names)
col.list <- c("green", "blue", "red", "gold","darkorchid","darkblue","gray","darkorange")
plot(-10,10,type='p',xlim=c(-0.27,-0.12),ylim=c(-0.27,0.6),xlab='PC 1',ylab='PC 2')
text(pc.1,pc.2,labels=names(pc.1),cex=0.75,
     #col = c("green", "blue", "red", "gold","darkorchid","darkblue","gray","darkorange"))
     col = col.list)



# Plot the default scree plot;
plot(returns.pca)

# Make Scree Plot
scree.values <- (returns.pca$sdev^2)/sum(returns.pca$sdev^2);
plot(scree.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(scree.values,lwd=2,cex=1.5)
title('Scree Plot')
# Make Proportion of Variance Explained
variance.values <- cumsum(returns.pca$sdev^2)/sum(returns.pca$sdev^2);
plot(variance.values,xlab='Number of Components',ylab='',type='l',lwd=2)
points(variance.values,lwd=2,cex=1.5)
abline(h=0.8,lwd=1.5,col='red')
abline(v=8,lwd=1.5,col='red')
text(13,0.5,'Keep 8 Principal Components',col='red')
title('Total Variance Explained Plot')

# Create the data frame of PCA predictor variables;
return.scores <- as.data.frame(returns.pca$scores);
return.scores$VV <- returns.df$VV;
return.scores$u <- runif(n=dim(return.scores)[1],min=0,max=1);
head(return.scores)
# Split the data set into train and test data sets;
train.scores <- subset(return.scores,u<0.70);
test.scores <- subset(return.scores,u>=0.70);
dim(train.scores)
dim(test.scores)
dim(train.scores)+dim(test.scores)
dim(return.scores)
# Fit a linear regression model using the first 8 principal components;
pca1.lm <- lm(VV ~ Comp.1+Comp.2+Comp.3+Comp.4+Comp.5+Comp.6+Comp.7+Comp.8, data=train.scores);
summary(pca1.lm)
# Compute the Mean Absolute Error on the training sample;
pca1.mae.train <- mean(abs(train.scores$VV-pca1.lm$fitted.values));
vif(pca1.lm)
# Score the model out-of-sample and compute MAE;
pca1.test <- predict(pca1.lm,newdata=test.scores);
pca1.mae.test <- mean(abs(test.scores$VV-pca1.test));

# Let's compare the PCA regression model with a 'raw' regression model;
# Create a train/test split of the returns data set to match the scores data set;
returns.df$u <- return.scores$u;
train.returns <- subset(returns.df,u<0.70);
test.returns <- subset(returns.df,u>=0.70);
dim(train.returns)
dim(test.returns)
dim(train.returns)+dim(test.returns)
dim(returns.df)
# Fit model.1 on train data set and score on test data;
model.1 <- lm(VV ~ GS+DD+DOW+HON+HUN+JPM+KO+MMM+XOM, data=train.returns)
model1.mae.train <- mean(abs(train.returns$VV-model.1$fitted.values));
model1.test <- predict(model.1,newdata=test.returns);
model1.mae.test <- mean(abs(test.returns$VV-model1.test));
summary(model.1)
# Fit model.2 on train data set and score on test data;
model.2 <- lm(VV ~ BAC+GS+JPM+WFC+BHI+CVX+DD+DOW+DPS+HAL+HES+HON+HUN+KO+MMM+MPC+PEP+SLB+XOM, data=train.returns)
model2.mae.train <- mean(abs(train.returns$VV-model.2$fitted.values));
model2.test <- predict(model.2,newdata=test.returns);
model2.mae.test <- mean(abs(test.returns$VV-model2.test));
summary(model.2)

full.lm <- lm(VV ~ ., data=train.scores);
summary(full.lm)
library(MASS)
backward.lm <- stepAIC(full.lm,direction=c('backward'))
summary(backward.lm)
backward.mae.train <- mean(abs(train.scores$VV-backward.lm$fitted.values));
vif(backward.lm)
backward.test <- predict(backward.lm,newdata=test.scores);
backward.mae.test <- mean(abs(test.scores$VV-backward.test));

pca1.mae.train 
pca1.mae.test
model1.mae.train 
model1.mae.test
model2.mae.train 
model2.mae.test
backward.mae.train
backward.mae.test
cor.values <- c(1.000,0.210,0.370,-0.32,0.000,-0.31,-0.26,0.090,-0.38,
                0.210,1.000,0.090,-0.29,0.120,-0.30,-0.14,0.010,-0.39,
                0.370,0.090,1.000,-0.31,-0.04,-0.30,-0.11,0.120,-0.39,
                -0.32,-0.29,-0.31,1.00,-0.16,0.25,-0.13,-0.14,0.900,
                0.00,0.120,-0.04,-0.16,1.000,-0.20,-0.03,-0.08,-0.38,
                -0.31,-0.30,-0.30,0.25,-0.20,1.000,-0.24,-0.16,0.180,
                -0.26,-0.14,-0.11,-0.13,-0.03,-0.24,1.000,-0.20,0.040,
                0.090,0.010,0.120,-0.14,-0.08,-0.16,-0.20,1.000,-0.24,
                -0.38,-0.39,-0.39,0.900,-0.38,0.180,0.040,-0.24,1.000
);
# How do we put these correlation values into a correlation matrix?;
help(matrix)
cor.matrix <- matrix(cor.values,nrow=9,ncol=9,byrow=TRUE);
# Check that object is a matrix object;
is.matrix(cor.matrix)
# Check that matrix is symmetric;
# This check helps check for typos;
isSymmetric(cor.matrix)

f.1 <- factanal(covmat=cor.matrix, n.obs=1442, factors=3, rotation='varimax');
names(f.1)

g.1 <- factanal(covmat=cor.matrix, n.obs=1442, factors=3, rotation='promax');

gamma.f1 <- f.1$loadings;
approx.f1 <- gamma.f1%*%t(gamma.f1) + diag(f.1$uniqueness);
mae.f1 <- mean(abs(approx.f1-cor.matrix))
gamma.g1 <- g.1$loadings;
approx.g1 <- gamma.g1%*%t(gamma.g1) + diag(g.1$uniqueness);
mae.g1 <- mean(abs(approx.g1-cor.matrix))
# Change my.path to point to your file;
setwd('C:/Users/lcamero/Desktop/Predict 410')
my.file <- paste('European_Employment.csv',sep='');
my.data <- read.csv(my.file,header=TRUE);
str(my.data)
head(my.data)

print(my.data)

# Pairwise scatterplot
pairs(my.data[,-c(2)])

eu.df <- subset(my.data,Group=='EU');
efta.df <- subset(my.data,Group=='EFTA');
eastern.df <- subset(my.data,Group=='Eastern');
other.df <- subset(my.data,Group=='Other');
# Plot of FIN versus SER;
plot(my.data$SER,my.data$FIN,xlab='Services',ylab='Finance',xlim=c(0,27),ylim=c(0,17))
text(eu.df$SER,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$SER,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$SER,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$SER,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')

help(par)

# Plot MAN versus SER;
plot(my.data$MAN,my.data$FIN,xlab='Manufacturing',ylab='Finance',xlim=c(0,32),ylim=c(0,17))
text(eu.df$MAN,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$MAN,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$MAN,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$MAN,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')

apply(my.data[,-c(1,2)],MARGIN=1,FUN=sum)
pca.out <- princomp(x=my.data[,-c(1,2)],cor=FALSE);
names(pca.out)
pc.1 <- pca.out$scores[,1];
pc.2 <- pca.out$scores[,2];
my.pca <- as.data.frame(list(Country=my.data$Country,Group=my.data$Group,pc1=pc.1,pc2=pc.2));
# Do we know why I used list() instead of cbind()?;
eu.pca <- subset(my.pca,Group=='EU');
efta.pca <- subset(my.pca,Group=='EFTA');
eastern.pca <- subset(my.pca,Group=='Eastern');
other.pca <- subset(my.pca,Group=='Other');
plot(eu.pca$pc1,eu.pca$pc2,xlab='Principal Component 1',ylab='Principal Component 2',
     xlim=c(-60,25),ylim=c(-25,30))points(efta.pca$pc1,efta.pca$pc2)
points(eastern.pca$pc1,eastern.pca$pc2)
points(other.pca$pc1,other.pca$pc2)
text(eu.pca$pc1,eu.pca$pc2,labels=eu.pca$Country,cex=0.75,col='green',pos=4)
text(efta.pca$pc1,efta.pca$pc2,labels=efta.pca$Country,cex=0.75,col='blue',pos=1)
text(eastern.pca$pc1,eastern.pca$pc2,labels=eastern.pca$Country,cex=0.75,col='red',pos=1)
text(other.pca$pc1,other.pca$pc2,labels=other.pca$Country,cex=0.75,col='grey',pos=3)

# Drop the 'Other' Category;
label.data <- subset(my.data,Group != 'Other');
# Cluster in FIN*SER view;
fin.ser <- hclust(d=dist(label.data[,c('FIN','SER')]),method='complete');
plot(fin.ser,labels=label.data[,1],xlab='Hierarchical Clustering FIN vs SER 2D View',sub='');

fin.ser.3 <- cutree(fin.ser,k=3);
finser.3df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=fin.ser.3))
finser.t3 <- table(finser.3df$Group,finser.3df$Cluster)
finser.comp3 <- t(finser.t3[1:3,])*(1/apply(finser.t3[1:3,],FUN=sum,MARGIN=2))
finser.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));
fin.ser.6 <- cutree(fin.ser,k=6);
finser.6df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=fin.ser.6))
finser.t6 <- table(finser.6df$Group,finser.6df$Cluster)
finser.comp6 <- t(finser.t6[1:3,])*(1/apply(finser.t6[1:3,],FUN=sum,MARGIN=2))
finser.accuracy6 <- sum(apply(finser.t6[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t6[1:3,],FUN=sum,MARGIN=2));

# Cluster in PC1*PC2 view;
pca.out <- princomp(x=label.data[,-c(1,2)],cor=FALSE);
my.pca <- as.data.frame(list(Country=label.data$Country,Group=label.data$Group,
                             pc1=pca.out$scores[,1],pc2=pca.out$scores[,2]) );
eu.pca <- subset(my.pca,Group=='EU');
efta.pca <- subset(my.pca,Group=='EFTA');
eastern.pca <- subset(my.pca,Group=='Eastern');
other.pca <- subset(my.pca,Group=='Other');
pc1.pc2 <- hclust(d=dist(my.pca[,c('pc1','pc2')]),method='complete');
plot(pc1.pc2,labels=my.pca[,1],xlab='Hierarchical Clustering PC1 vs PC2 2D View',sub='');
pca.3 <- cutree(pc1.pc2,k=3);
pca.3df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.3))
pca.t3 <- table(pca.3df$Group,pca.3df$Cluster)
pca.comp3 <- t(pca.t3[1:3,])*(1/apply(pca.t3[1:3,],FUN=sum,MARGIN=2))
pca.accuracy3 <- sum(apply(pca.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.t3[1:3,],FUN=sum,MARGIN=2));
pca.6 <- cutree(pc1.pc2,k=6);
pca.6df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.6))
pca.t6 <- table(pca.6df$Group,pca.6df$Cluster)
pca.comp6 <- t(pca.t6[1:3,])*(1/apply(pca.t6[1:3,],FUN=sum,MARGIN=2))
pca.accuracy6 <- sum(apply(pca.t6[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.t6[1:3,],FUN=sum,MARGIN=2));

finser.accuracy3
finser.accuracy6
pca.accuracy3
pca.accuracy6

# Cluster in FIN*SER view;
# Specify 3 Clusters;
finser.k3 <- kmeans(x=label.data[,c('FIN','SER')],centers=3);
names(finser.k3)
finser.k3df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=finser.k3$cluster,
                                  FIN=label.data$FIN,SER=label.data$SER));
finser.k3tab <- table(finser.k3df$Group,finser.k3df$Cluster);
finser.k3ac <- sum(apply(finser.k3tab[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.k3tab[1:3,],FUN=sum,MARGIN=2));
# Plot the cluster centers;
plot(label.data$SER,label.data$FIN,xlab='Services',ylab='Finance',xlim=c(0,27),ylim=c(0,17),col='white')
text(eu.df$SER,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$SER,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$SER,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$SER,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(finser.k3$centers[,2],finser.k3$centers[,1],labels=seq(1,3,1),col='black',cex=1)
points(finser.k3$centers[,2],finser.k3$centers[,1],col='black',cex=2.5)
text(finser.k3df$SER,finser.k3df$FIN,labels=finser.k3df$Cluster,col='grey',cex=0.75);
title('k-Means with 3 Clusters')

# Specify 6 Clusters;
finser.k6 <- kmeans(x=label.data[,c('FIN','SER')],centers=6);
names(finser.k6)
finser.k6df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=finser.k6$cluster,
                                  FIN=label.data$FIN,SER=label.data$SER));
finser.k6tab <- table(finser.k6df$Group,finser.k6df$Cluster);
finser.k6ac <- sum(apply(finser.k6tab[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.k6tab[1:3,],FUN=sum,MARGIN=2));
# Plot the cluster centers;
plot(label.data$SER,label.data$FIN,xlab='Services',ylab='Finance',xlim=c(0,27),ylim=c(0,17),col='white')
text(eu.df$SER,eu.df$FIN,labels=eu.df$Country,cex=0.75,pos=4,col='green')
text(efta.df$SER,efta.df$FIN,labels=efta.df$Country,cex=0.75,pos=4,col='blue')
text(eastern.df$SER,eastern.df$FIN,labels=eastern.df$Country,cex=0.75,pos=4,col='red')
text(other.df$SER,other.df$FIN,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(finser.k6$centers[,2],finser.k6$centers[,1],labels=seq(1,6,1),col='black',cex=1)
points(finser.k6$centers[,2],finser.k6$centers[,1],col='black',cex=2.5)
text(finser.k6df$SER,finser.k6df$FIN,labels=finser.k6df$Cluster,col='grey',cex=0.75);
title('k-Means with 6 Clusters')

# Cluster in PC1*PC2 view;
# Specify 3 Clusters;
pca.k3 <- kmeans(x=my.pca[,-c(1,2)],centers=3);
names(pca.k3)
pca.k3df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.k3$cluster,pc1=my.pca$pc1,pc2=my.pca$pc2));
pca.k3tab <- table(pca.k3df$Group,pca.k3df$Cluster);
pca.k3ac <- sum(apply(pca.k3tab[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.k3tab[1:3,],FUN=sum,MARGIN=2));
# Plot the cluster centers;
plot(my.pca$pc1,my.pca$pc2,xlab='Principal Component 1',ylab='Principal Component 2',
     xlim=c(-60,20),ylim=c(-22,25),col='white')
text(eu.pca$pc1,eu.pca$pc2,labels=eu.pca$Country,cex=0.75,pos=4,col='green')
text(efta.pca$pc1,efta.pca$pc2,labels=efta.pca$Country,cex=0.75,pos=4,col='blue')
text(eastern.pca$pc1,eastern.pca$pc2,labels=eastern.pca$Country,cex=0.75,pos=4,col='red')
#text(other.pca$pc1,other.pca$pc2,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(pca.k3$centers[,1],pca.k3$centers[,2],labels=seq(1,3,1),col='black',cex=1)
points(pca.k3$centers[,1],pca.k3$centers[,2],col='black',cex=2.5)
text(pca.k3df$pc1,pca.k3df$pc2,labels=pca.k3df$Cluster,col='grey',cex=0.75);
title('k-Means with 3 Clusters')
# Specify 6 Clusters;
pca.k6 <- kmeans(x=my.pca[,-c(1,2)],centers=6);
names(pca.k6)
pca.k6df <- as.data.frame(list(Country=my.pca[,1],Group=my.pca[,2],Cluster=pca.k6$cluster,pc1=my.pca$pc1,pc2=my.pca$pc2));
pca.k6tab <- table(pca.k6df$Group,pca.k6df$Cluster);
pca.k6ac <- sum(apply(pca.k6tab[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.k6tab[1:3,],FUN=sum,MARGIN=2));
# Plot the cluster centers;
plot(my.pca$pc1,my.pca$pc2,xlab='Principal Component 1',ylab='Principal Component 2',
     xlim=c(-60,25),ylim=c(-25,30),col='white')
text(eu.pca$pc1,eu.pca$pc2,labels=eu.pca$Country,cex=0.75,pos=4,col='green')
text(efta.pca$pc1,efta.pca$pc2,labels=efta.pca$Country,cex=0.75,pos=4,col='blue')
text(eastern.pca$pc1,eastern.pca$pc2,labels=eastern.pca$Country,cex=0.75,pos=4,col='red')
#text(other.pca$pc1,other.pca$pc2,labels=other.df$Country,cex=0.75,pos=4,col='grey')
text(pca.k6$centers[,1],pca.k6$centers[,2],labels=seq(1,6,1),col='black',cex=1)
points(pca.k6$centers[,1],pca.k6$centers[,2],col='black',cex=2.5)
text(pca.k6df$pc1,pca.k6df$pc2,labels=pca.k6df$Cluster,col='grey',cex=0.75);
title('k-Means with 6 Clusters')

kmeans.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));
kmeans.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));
kmeans.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));
kmeans.accuracy3 <- sum(apply(finser.t3[1:3,],FUN=max,MARGIN=2))/sum(apply(finser.t3[1:3,],FUN=sum,MARGIN=2));

finser.accuracy3
finser.accuracy6
pca.accuracy3
pca.accuracy6
pca.t6 <- table(pca.6df$Group,pca.6df$Cluster)

pca.accuracyk3 <- sum(apply(pca.k3tab[1:3,],FUN=max,MARGIN=2))/sum(apply(pca.k3tab[1:3,],FUN=sum,MARGIN=2));

# Loop through 1-20 clusters using all dimensions;
# Compute the accuracy for each cluster, store, and plot;
# Set the maximum number of clusters to consider;
k.max <- 20;
# Initialize the accuracy arrays for storage;
accuracy.hier <- rep(NA,k.max);
accuracy.kmeans <- rep(NA,k.max);
# Fit the hierarchical clustering model outside of the loop for efficiency;
all.h <- hclust(d=dist(label.data[,-c(1,2)]),method='complete');
# Loop through different cluster sizes and compute classification accuracy;
for (j in 1:k.max){
  # Fit hierarchical cluster model of size j;
  hier.j <- cutree(all.h,k=j);
  hier.df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=hier.j));
  hier.table <- table(hier.df$Group,hier.df$Cluster);
  # Cannot use apply() on a vector;
  if (j==1){
    accuracy.hier[j] <- max(hier.table[1:3,])/sum(hier.table[1:3,]);
  }else{
    accuracy.hier[j] <- sum(apply(hier.table[1:3,],FUN=max,MARGIN=2))/sum(apply(hier.table[1:3,],FUN=sum,MARGIN=2));
  }#end if-else;
  # Fit k-means clustering model of size j;
  kmeans.j <- kmeans(x=label.data[,-c(1,2)],centers=j);
  kmeans.df <- as.data.frame(list(Country=label.data[,1],Group=label.data[,2],Cluster=kmeans.j$cluster));
  kmeans.table <- table(kmeans.df$Group,kmeans.df$Cluster);
  # Cannot use apply() on a vector;
  if (j==1){
    accuracy.kmeans[j] <- max(kmeans.table[1:3,])/sum(kmeans.table[1:3,]);
  }else{
    accuracy.kmeans[j] <- sum(apply(kmeans.table[1:3,],FUN=max,MARGIN=2))/sum(apply(kmeans.table[1:3,],FUN=sum,MARGIN=2));
  }#end if-else;
} #end j loop;

plot(seq(1,k.max,1),accuracy.hier,ylim=c(0,1),xlab='# Clusters',ylab='Accuracy',cex.axis=1,type='l',lwd=2,col='red')
points(seq(1,k.max,1),accuracy.hier,ylim=c(0,1),cex=1.5,type='p',col='red',pch=19)
points(seq(1,k.max,1),accuracy.kmeans,ylim=c(0,1),type='l',lwd=2,col='blue')
points(seq(1,k.max,1),accuracy.kmeans,ylim=c(0,1),cex=1.5,type='p',col='blue')
title('Classification Accuracy')
legend(1,0.2,legend=c('Hierarchical','k-Means'),col=c('red','blue'),lwd=2)

finser.k3ac
finser.k6ac
pca.k3ac
pca.k6ac
