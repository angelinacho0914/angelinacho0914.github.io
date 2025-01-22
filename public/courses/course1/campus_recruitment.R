# QTM2000 Exam 1
# Professor Mathaisel
# Campus Recruitment Case
# Read the case before using this script

# Script written by Professor Mathaisel based on the following references:
# https://www.kaggle.com/benroshan/factors-affecting-campus-placement
# https://www.kaggle.com/benroshan/part-4-data-visualization-with-r
# https://www.kaggle.com/kaustabbrahmachari/linear-regression
# https://www.kaggle.com/pedromv/knn-to-predict-campus-placement

#####################################################################
# Installing the required packages on your computer's C Drive.
# Once installed, there is no need to repeat the installation.
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("GGally")
install.packages("corrgram")
install.packages("corrplot")
install.packages("class")
install.packages("fastDummies")

library(tidyverse)
library(Hmisc)
library(GGally)
library(corrgram) 
library(corrplot)
library(class)
library(fastDummies)
#####################################################################
# Read the dataframe
cr <- read.csv("C:/Docs/Babson/QTM2000/Data/Campus Recruitment.csv")
str(cr)
# We do not need column 1 (serial number)
cr <- cr[ -c(1)]
str(cr)

# Are there any missing values? In the following, a zero infers that there are no missing values,
# otherwise, the script indicates how many missing values.
cr[cr==""] <- NA
sapply(cr,function(x) sum(is.na(x)))

#####################################################################
# Data visualizations

# Bar plots
cr %>% ggplot(aes(x = gender, fill = gender)) +  geom_bar(aes(fill = status))
cr %>% ggplot(aes(x = workex)) +  geom_bar(aes(fill = status))
ggplot(cr, aes(degree_t)) + geom_bar(aes(fill=status))

# Box plots
ggplot(cr, aes(y=degree_p)) + geom_boxplot(aes(x=status), fill = "chocolate")
ggplot(cr, aes(y=degree_p)) + geom_boxplot(aes(x=gender), fill = "orchid")

# Density plots
ggplot(cr) + geom_density(aes(degree_p, fill=status), alpha=0.5)

# Violin plot
ggplot(cr, aes(y = hsc_p)) + geom_violin(aes(x = status), fill = "violet")
ggplot(cr, aes(y = hsc_p)) + geom_violin(aes(x = gender), fill = "wheat")

# Is the data normally distributed (Normality)
ggplot(cr) + geom_density(aes(degree_p), fill="pink", color="blue", size=1) +
  theme_classic() + ggtitle("Density Plot")

#####################################################################
# Correlation 

# Correlation as a color chart with only the attributes that are numeric
num.cols = sapply(cr,is.numeric)
cor.data = cor(cr[,num.cols]) 
print(corrplot(cor.data, method = 'color'))

# Multiple scatter plots and correlations in one visualization. Be patient - 
# watch the console. This visual takes a long time to generate.
ggpairs(cr[,num.cols], aes(colour = cr$status, alpha = 0.8), title="Pairs Plot for Campus Recruitment") + 
  theme_grey(base_size = 8)

#####################################################################
# Predictive Analytics: Linear Regression

# Scatter plot with a regression line for just two attributes
#ggplot(cr, mapping=aes(degree_p,hsc_p))+geom_point(mapping = aes(x=hsc_p,y=etest_p))+geom_smooth(method = "lm",se=FALSE)
ggplot(cr, mapping=aes(degree_p,hsc_p))+geom_point(mapping = NULL)+geom_smooth(method = "lm",se=FALSE)
# Simple (two attribute) regression
simple_linear_model <- lm(data=cr,etest_p~hsc_p)
summary(simple_linear_model)

# Multiple regression
multiple_linear_model <- lm(salary ~ ssc_p+hsc_p+degree_p+etest_p+mba_p, cr)
summary(multiple_linear_model)


#####################################################################
# k-NN

# Deleting the salary column because it has too many missing values, but keeping the original cr dataframe.
data = cr
data = data[,!names(data) %in% 'salary']
data[data==''] <- NA # To make sure missing data is NA

# Correlation matrix as a visualization.
categorical_s = c('gender','hsc_s','ssc_b','hsc_b','ssc_s','degree_t','workex','specialisation','status')#List of all categorical variables
results_s <- fastDummies::dummy_cols(data, select_columns = categorical_s) # Creating the dummy columns
res_s <- results_s[, !(names(results_s) %in% categorical_s)]               # Deleting initial columns
corrplot(cor(res_s), type = 'upper', method = 'circle',  tl.col = 'black',  p.mat = res_s$P, sig.leve = 0.05)

# Transform all our categorical variables (apart from our target variable: status) 
# into numerical by creating dummy boolean variables.
categorical = c('gender','hsc_s','ssc_b','hsc_b','ssc_s','degree_t','workex','specialisation')
results <- fastDummies::dummy_cols(data, select_columns = categorical) # Creating the dummy columns
res <- results[, !(names(results) %in% categorical)]                   # Deleting initial columns

# Next, we need to standardize/normalize ( 0 mean 1 std) the quantitative variables. Do you know why?
# Creating a function to do this. Then we'll call the function in the next step.
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
# Normalize our placement dataset
plac_norm <- as.data.frame(lapply(res[,c(1:5,7:22)], normalize)) #all columns apart from status (column 6)

# Then we randomly split the data set into a training set(2/3 rule) and a test set(1/3 rule).
# The first is used to train the model, while the second is used to evaluate the trained model.
set.seed(1234)
#Training set will be 2/3 of original data set
ind <- sample(2, nrow(plac_norm), replace=TRUE, prob=c(0.666, 0.334))

plac.training <- plac_norm[ind == 1,]
plac.test <- plac_norm[ind == 2,]
str(plac.training)
str(plac.test)

plac.trainLabels <- res[ind==1,6]   # Column 6 is our target.
plac.testLabels <- res[ind==2,6]

# To graphically show which values of k give us the best classification, 
# we can plot "Accuracy vs k number of Neighbors"
# Creating the variables and defining the accuracy and precision of our model
KnnTestPrediction <- list()
accuracy <- numeric()
precision <- numeric()
# Next, we need to find the best value of k, so we'll run through the knn model
# for values of k from 1 to 100
# Simulating from k=1 to k=100...
for(k in 1:100){
  
  # KnnTestPrediction for each k
  KnnTestPrediction[[k]] <- knn(plac.training, plac.test, plac.trainLabels, k, prob=TRUE)
  
  # Accuracy for each k   
  accuracy[k] <- sum(KnnTestPrediction[[k]]==plac.testLabels)/length(plac.testLabels)*100
  
  #Precision for each k
  precision[k] <- sum(KnnTestPrediction[[k]]=="Placed" & plac.testLabels=="Placed")/(sum((plac.testLabels=="Placed")))
}

# To find the best value of k, let's plot accuracy vs number of k
plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")

# Visually locate the best value of k with a line with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# Visually locate the maximum accuracy
abline(h=max(accuracy), col="grey", lty=2)

# Visually locate the minimum accuracy
abline(h=min(accuracy), col="grey", lty=2)

# Another visual to help determine the best k for accuracy
plot(precision, type="b", col="darkgreen", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification precision", 
     main="Precision vs Neighbors")

# Using the above visualizations, let's choose k=6 and re-running the knn model with k=6
# Execution of k-NN with k = 6
knn <- knn(plac.training, plac.test, plac.trainLabels, k=6, prob = TRUE)

# Accuracy of knn for k = 6
round(sum(plac.testLabels==knn)/length(plac.testLabels)*100,2)

# Cross Classification / Confusion matrix of knn for k = 6
table(plac.testLabels, knn)

#####################################################################
