########## Libraries ##########
library("reshape2")
library("ggplot2")
library("dplyr")
library("MASS")
require("miscTools")

########## Question 1. ##########
redwinedata = read.csv("D:/Data Science Course/DSCT-Logistic-Regression-master/winequality-red.csv", sep=";", header=TRUE, row.names=NULL)
whitewinedata = read.csv("D:/Data Science Course/DSCT-Logistic-Regression-master/winequality-white.csv", sep=";", header=TRUE, row.names=NULL)
redwinedata$ID = seq.int(nrow(redwinedata))
whitewinedata$ID = seq.int(nrow(whitewinedata))


########## Question 2. ##########
averagequality_red = mean(redwinedata$quality)
averagequality_white = mean(whitewinedata$quality)

# Average quality of red wine is 5.636.
# Average quality of white wine is 5.878.



########## Question 3. ##########
set.seed(5) # Set a seed to ensure that this data can be reproduced in the future.

# # Random selection of training and test data
# train_red = data.frame(sample_frac(redwinedata, 0.8))     # Select 80% of the red wine data as training data.
# train = as.numeric(rownames(train_red))                   # Mark the rows of the training data.
# test_red = data.frame(-redwinedata[-train,])              # Select the unmarked rows as the red wine test data.
# 
# train_white = data.frame(sample_frac(whitewinedata, 0.8)) # Select 80% of the white wine data as training data.
# train = as.numeric(rownames(train_white))                 # Mark the rows of the training data.
# test_white = data.frame(-whitewinedata[-train,])          # Select the unmarked rows as the white wine test data.

# Selection of data based on the different qualities
train_red = rbind(
  data.frame(sample_frac(filter(redwinedata, redwinedata$quality == 3), 0.8)),
  data.frame(sample_frac(filter(redwinedata, redwinedata$quality == 4), 0.8)),
  data.frame(sample_frac(filter(redwinedata, redwinedata$quality == 5), 0.8)),
  data.frame(sample_frac(filter(redwinedata, redwinedata$quality == 6), 0.8)),
  data.frame(sample_frac(filter(redwinedata, redwinedata$quality == 7), 0.8)),
  data.frame(sample_frac(filter(redwinedata, redwinedata$quality == 8), 0.8))
)
test_red = subset(redwinedata, !(redwinedata$ID %in% train_red$ID))

train_white = rbind(
  data.frame(sample_frac(filter(whitewinedata, whitewinedata$quality == 3), 0.8)),
  data.frame(sample_frac(filter(whitewinedata, whitewinedata$quality == 4), 0.8)),
  data.frame(sample_frac(filter(whitewinedata, whitewinedata$quality == 5), 0.8)),
  data.frame(sample_frac(filter(whitewinedata, whitewinedata$quality == 6), 0.8)),
  data.frame(sample_frac(filter(whitewinedata, whitewinedata$quality == 7), 0.8)),
  data.frame(sample_frac(filter(whitewinedata, whitewinedata$quality == 8), 0.8)),
  data.frame(sample_frac(filter(whitewinedata, whitewinedata$quality == 9), 0.8))
)
test_white = subset(whitewinedata, !(whitewinedata$ID %in% train_white$ID))



########## Question 4. ##########
regression_red = lm(quality ~ . -ID, data = train_red)
regression_white = lm(quality ~ . -ID, data = train_white)
summary(regression_red)
summary(regression_white)

# The adjusted R-squared for quality against other variables for red wine is 0.3525.
# The adjusted R-squared for quality against other variables for white wine is 0.2784.



########## Question 5. ##########
col_red = cor(data.matrix(train_red))
col_white = cor(data.matrix(train_white))
print(col_red)
print(col_white)

# No, multicollinearity does not exist as non of the predictor variables have a correlation close to 1 or -1 with each other.

# regression_red2 = lm(quality ~ volatile.acidity +chlorides +total.sulfur.dioxide +sulphates +alcohol, data = train_red)
# regression_white2 = lm(quality ~ . -fixed.acidity -citric.acid -chlorides -total.sulfur.dioxide -ID, data = train_white)
# summary(regression_red2)
# summary(regression_white2)

regression_red2 = stepAIC(regression_red, direction="both")
regression_red2$anova # display results
summary(regression_red2)
regression_white2 = stepAIC(regression_white, direction="both")
regression_white2$anova # display results
summary(regression_white2)



########## Question 6. ##########
# The p-value for models is 2.2e-16 which is <0.05 making bothe models significant.
# Both models have volatile.acidity, free.sulfur.dioxide, pH, sulfates, and alcohol as part of their regression model.
# The R-squared value and the adjusted R-squared value are higher for red wine than white wine.



########## Question 7. ##########
predict_red = predict(regression_red2, newdata = test_red)
predict_white = predict(regression_white2, test_white)

print(predict_red)
print(predict_white)



########## Question 8. ##########
RSS_red = c(crossprod(regression_red2$residuals))
MSE_red = RSS_red/length(regression_red2$residuals)
RMSE_red = sqrt(MSE_red)

RSS_white = c(crossprod(regression_white2$residuals))
MSE_white = RSS_white/length(regression_white2$residuals)
RMSE_white = sqrt(MSE_white)

print(RMSE_red)
print(RMSE_white)

# The RMSE value for red wine is 0.6462717
# The RMSE value for white wine is 0.7511892.
# The smaller the RMSE value, the closer is the predicted value to the actual value.
# Therefore, in this case the model for red wine is more accurate than the model for white wine.



########## Question 9. ##########
# regression_red3 = lm(quality ~ . -ID, data = test_red)
# summary(regression_red3)$r.square

sse = sum((fitted(regression_red3) - mean(test_red$quality))^2)
ssr = sum((fitted(regression_red3) - test_red$quality)^2)
r2red = 1-(ssr/(sse + ssr))
print(r2red)

# regression_white3 = lm(quality ~ . -ID, data = test_white)
# summary(regression_white3)$r.square

sse = sum((fitted(regression_white3) - mean(test_white$quality))^2)
ssr = sum((fitted(regression_white3) - test_white$quality)^2)
r2white = 1-(ssr/(sse + ssr))
print(r2white)

# The red wine model is more accurate as it has a higher R square value.
# The accuracy can be improved by selecting the training and test differently over the same set.


