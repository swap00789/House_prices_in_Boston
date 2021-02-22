### The analysis begins by understanding the 

CRIM - per capita crime rate by town
ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
INDUS - proportion of non-retail business acres per town.
CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
NOX - nitric oxides concentration (parts per 10 million)
RM - average number of rooms per dwelling
AGE - proportion of owner-occupied units built prior to 1940
DIS - weighted distances to five Boston employment centres
RAD - index of accessibility to radial highways
TAX - full-value property-tax rate per $10,000
PTRATIO - pupil-teacher ratio by town
B where Bk is the proportion of blacks by town
LSTAT - % lower status of the population
MEDV - Median value of owner-occupied homes in $1000's

#Install package 'caret'
install.packages("caret")
install.packages("formattable")

#Load Library 'caret' and 'formattable'
library(formattable)
library(caret)

# Read the dataset from housing.data file using read.table
housing.df <- read.csv(file.choose(), header = T)

#Displays the housing dataframe
head(housing.df)

#View Data
View(housing.df)

# Display the structure of the housing.df data frame
str(housing.df)

#Displays the summary statistics like minimum value, maximum value, median, mean, and the 1st and 3rd 
#quartile values for each column in our dataset.
summary(housing.df)

#Cleaning the data my omitting the NA values 
housing.df <- na.omit(housing.df)

# create new dataset without missing data
housing.df

# Visualize the distribution and density of the outcome, MEDV. The black curve represents the density
#This calculates the mean of housing MEDV
m <- mean(housing.df$MEDV)
#This calculates the standard deviation of housing MEDV
sd <-sd(housing.df$MEDV)
#This creates a histogram of the MEDV
h <-hist(housing.df$MEDV,
     main="Histogram for House Pricing in Boston", 
     xlab="Median Value Houses (In Thousands)", 
     border="blue", 
     col="lightgray", 
     breaks=12 
    )
#This adds a curve to the histogram while maintaining the count of data instead of density
xfit<-seq(min(housing.df$MEDV),max(housing.df$MEDV),length=40) 
#This gives the density of MEDV,
yfit<-dnorm(xfit,mean=m,sd=sd)
#This calculates the size of the bins
yfit <- yfit*diff(h$mids[1:2])*length(housing.df$MEDV)
#this adds a curve to the hsitogram to show normal distribution of data.
lines(xfit, yfit, col="red", lwd=2)

#the boxplot is also plotted to bring an additional perspective of MEDV 
boxplot(housing.df$MEDV,data=housing.df, 
        main="Owner-Occupied Homes in Boston", 
        xlab="Median Value Of Houses (in Thousands)",
        horizontal = T,
        col = "Orange",
        notch = TRUE)

#scatterplot of some of the important variables (based on intuition) with the outcome variable MEDV.
plot(housing.df[,c(3,5,6,11,13,14)],
     pch=3, 
     main = ("MEDV Vs Other Variables"))

# Correlation of each independent variable with the dependent variable
cor(housing.df,housing.df$MEDV)

#We partition the data on a 7/3 ratio as training/test datasets.
set.seed(12345)

housing.df <- cbind(scale(housing.df[1:13]), housing.df[14])
#Do data partitioning
inTrain <- createDataPartition(y = housing.df$MEDV, p = 0.70, list = FALSE)
#Parition data into training data
training <- housing.df[inTrain,]
#Parition data into testing data
testing <- housing.df[-inTrain,]


#Perform  linear regression model with MEDV as the dependent variable 
#and all the remaining variables as independent variables. 
set.seed(12345)
#Try linear model using all features
fit.lm <- lm(log(MEDV)~.,data = training)

#This prints the coefficients of the x variables of boston housing dataset
data.frame(coef = round(fit.lm$coefficients,2))

set.seed(12345)
#predict on test set
pred.lm <- predict(fit.lm, newdata = testing)

# Root-mean squared error
rmse.lm <- sqrt(sum((exp(pred.lm) - testing$MEDV)^2)/length(testing$MEDV))

#This prints the RMSE, R2 and P-value of the predicted test data
c(RMSE = rmse.lm, R2 = summary(fit.lm)$r.squared, P_value = summary(fit.lm)$coefficients[1,4])

percent(summary(fit.lm)$r.squared)
#82.17%

# Plot of predicted price vs actual price
plot(exp(pred.lm),testing$MEDV, 
     main = "Predicted And Actual Values Of Boston Housing",
     xlab = "Predicted Values", 
     ylab = "Actual Values",
     abline(a = 0, b = 1))

#summary of the regression model
summary(fit.lm)
