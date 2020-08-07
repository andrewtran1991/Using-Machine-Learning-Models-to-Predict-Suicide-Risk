# Coder: Anh Tran
# Objective: Build knn model


set.seed(2) # set a new seed for part b so that the same samples can be reproduced

# Run leave-one-out cross-validation knn model with different kernels

knn_cv_model <- train.kknn(qnsuicide~weight+stratum+PSU+sex+grade+race4+sexid2
                           +qn17+qn24+qn19
                           +qn13+qn18+qn23+qn15+qn16
                           +qn25
                           +qn42+qn48+qn52+qn57
                           +qn60+qn62
                           +qnobese+qn68
                           +qn80+qn81
                           +qn79+qn83+qn88+qn89,trainData,  kmax = 100, kernel = c("optimal","rectangular", "inv", "gaussian", "triangular"), scale = TRUE)
knn_cv_model
plot(knn_cv_model)

# Make predictions on the test data
pred_knn <-predict(knn_cv_model, testData)

# Round off the values to 0 and 1
pred_bin<-round(pred_knn)

# Calculate AUC
plotROC(testData$qnsuicide, pred_knn)

# Construct confusion matrix
confusionMatrix(table(testData$qnsuicide, pred_bin))
