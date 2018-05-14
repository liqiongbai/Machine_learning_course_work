### Machine Learning HW Assignment 4: 
### Statistical Regression and Classification, CRC Press, 2017: 
### pg. 61, do 1.22 Exercises 1.22: 1-4, <br>；pg. 120: 2.14: 1-4.

## 1.22 
## 1.22.1.In Section 1.12.1.2, the reader was reminded that the results of a cross-validation are random, 
# due to the random partitioning into training and test sets. 
# Try doing several runs of the linear and k-NN code in that section, comparing results. 

# install.packages("freqparcoord")
library(freqparcoord)
data(mlb)

# Check for linear model
xvalpart <- function(data, p){
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE)
  list(train=data[trainidxs ,],valid=data[-trainidxs,])
}

xvallm <- function(data, ycol, predvars, p, meanabs=TRUE){
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  trainy <- train[, ycol]
  trainpreds <- train[, predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[, predvars])
  predy <- cbind(1, validpreds) %*% coef(lmout)
  realy <- valid[, ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list(predy = predy, realy = realy)
}

round(xvallm(mlb, 5, c(4,6), 0.8),2)
round(xvallm(mlb, 5, c(4,6), 0.7),2)
round(xvallm(mlb, 5, c(4,6), 0.6),2)
round(xvallm(mlb, 5, c(4,6), 0.5),2)

# Check for KNN Model
xvalknn <- function(data,ycol ,predvars ,k,p,meanabs=TRUE){
  data <- data[, c(predvars, ycol)] 
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol],k)
  kout <- knnest(train[,ycol],xd,k)
  predy <- predict(kout, valid[, -ycol], TRUE)
  realy <- valid[, ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list (predy = predy , realy = realy)
}

# install.packages("regtools")
library(regtools)
round(xvalknn(mlb, 5, c(4,6), 25, 0.8),2)


## 1.22.2.Extend (1.28) to include interaction terms for age and gender, and age^2 and gender. 
#Run the new model, and find the estimated effect of being female, for a 32-year-old person with a Master’s degree.

library(freqparcoord)
data(prgeng)

prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
pe$agefem <- pe$age * pe$fem
pe$age2fem <- pe$age2 * pe$fem
model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + agefem + age2fem, data = pe)
summary(model)

age <- 32
age2 <- 32^2
wkswrkd <- 52
ms <- 1
phd <- 0
fem <- 1
agefem <- age*fem
age2fem <- age2*fem
input <- data.frame(age,age2,wkswrkd,ms,phd,fem,agefem,age2fem)
predict(model, input, interval = "prediction", level = 0.95)


## 1.22.3.Consider the bodyfat data mentioned in Section 1.2. 
# Use lm() to form a prediction equation for density from the other variables (skipping the first three),
# and comment on whether use of indirect methods in this way seems feasible.

# install.packages("mfp")
library(mfp)
data(bodyfat)
model = lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data = bodyfat)
summary(model)
## prediction for desity from bodyfat from indirect methods in this way is fairly feasible. 


## 1.22.4.4.In Section 1.19.5.2, we gave this intuitive explanation:
# In other words, the national mean height is a weighted average of the state means, with the weight for each state being its proportion of the national population. Replace state by gender in the following.
# (a) Write English prose that relates the overall mean height of people and the gender-specific mean heights.
# (b) Write English prose that relates the overall proportion of people taller than 70 inches to the gender-specific proportions.
  
# Answers:
# (a)the overall mean height of people is a weight average of the gender-specific mean height, with the weight for each gender being its proportion of the gender population. 
# (b)the overall proportion of people taller than 70 inches is a weight average of the gender-specific proportion of people taller than 70 inches, with the weight for each gender being its proportion of the gender population.


## 2.14
# 2.14.1.Consider the census data in Section 1.16.1.
# (a) Form an approximate 95% confidence interval for β6 in the model (1.28).
# (b) Form an approximate 95% confidence interval for the gender effect for Master’s degree holders, β6 + β7, in the model (1.28).

library(freqparcoord)
data(prgeng)

prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex-1
tmp <- prgeng[edu >= 13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]

# (a)
model = lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem, data = prgeng)
model_summary <- summary(model)
beta6 <- model_summary$coefficients['fem',]
t_value <- qt(0.975, nrow(prgeng)-1)
lcl_beta6 <- beta6[1] - t_value*beta6[2]
ucl_beta6 <- beta6[1] + t_value*beta6[2]
lcl_beta6   #-12866.93
ucl_beta6   #-10102.05

# (b)
prgeng$msfem <- prgeng$ms * prgeng$fem
prgeng$phdfem <- prgeng$phd * prgeng$fem
model2 <- lm(wageinc ~ age + age2 + wkswrkd + ms + phd + fem + msfem + phdfem, data = prgeng )
model2_summary <- summary(model2)
beta7 <- model2_summary$coefficients['msfem',]
lcl_beta6_7 <- lcl_beta6 + (beta7[1] - t_value*beta7[2])
ucl_beta6_7 <- ucl_beta6 + (beta7[1] + t_value*beta7[2])
lcl_beta6_7   #-20411.85
ucl_beta6_7   #-10871.63


## 2.14.2.The full bikeshare dataset spans 3 years’ time. Our analyses here have only used the first year. Extend the analysis in Section 2.8.5 to the full data set, adding dummy variables indicating the second and third year. 
# Form an approximate 95% confidence interval for the difference between the coefficients of these two dummies.

day <- read.csv('day.csv')
day$temp2 <- day$temp^2
day$clearday <- as.integer(day$weathersit == 1)
model <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
model_summary <- summary(model)
t_value <- qt(0.975, nrow(day)-1)
yr <- model_summary$coefficients['yr',]
lcl_yr <- yr[1] - t_value * yr[2]
ucl_yr <- yr[1] + t_value * yr[2]
lcl_yr    #1604.973
ucl_yr    #1827.537


## 2.14.3.Suppose we are studying growth patterns in children, at k particular ages. 
# Denote the height of the ith child in our sample data at age j by Hij, with Hi = (Hi1,...,Hik)′ denoting the data for child i. 
# Suppose the population distribution of each Hi is k-variate normal with mean vector μ and covariance matrix Σ. 
# Say we are interested in successive differences in heights,Dij =Hi,j+1−Hij, j=1,2,...,k−1. DefineDi =(Di1,...,Dik)′. Explain why each Di is (k−1)-variate normal, and derive matrix expressions for the mean vector and covariance matrices.


## 2.14.4.In the simulation in Section 2.9.3, it is claimed that ρ2 = 0.50. Confirm this through derivation.
