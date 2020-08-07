# Coder: Anh Tran
# Objective: Build logistic regression model

set.seed(1)
# Build the logistic regression model
LogitModel <- glm(qnsuicide~weight+stratum+PSU+sex+grade+race4+sexid2
                            +qn17+qn24+qn19
                            +qn13+qn18+qn23+qn15+qn16
                            +qn25
                            +qn42+qn48+qn52+qn57
                            +qn60+qn62
                            +qnobese+qn68
                            +qn80+qn81
                            +qn79+qn83+qn88+qn89, data=trainData, family=binomial(link="logit"))

summary(LogitModel)


#confint.default(LogitModel)
#exp(coef(LogitModel))

# Make predictions on the test data
predicted_prop <- plogis(predict(LogitModel, testData))

# Decide on optimal prediction probability cutoff for the model
optCutOff <- optimalCutoff(testData$qnsuicide, predicted_prop)[1] 
optCutOff
# 0.4791532

predicted <- ifelse(predicted_prop > optCutOff, 1, 0)

# Misclassification and AUC
misClassError(testData$qnsuicide, predicted_prop, threshold = optCutOff)
#19.27
plotROC(testData$qnsuicide, predicted_prop)
#AUC = 0.8102

# Construct confusion matrix
confusionMatrix(table(testData$qnsuicide, predicted))

#sensitivity(table(testData$qnsuicide, predicted))
#specificity(table(testData$qnsuicide, predicted))


