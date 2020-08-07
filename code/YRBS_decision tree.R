# Coder: Anh Tran
# Objective: Build decision tree

set.seed(3)

# For decision tree visualization, convert 0 and 1 of the health-risk behavior to No and Yes
# and qnsuicide to low and high risk
## Recode other dichotomous variables 1 = 1 and 2 = 0 
trainData2 <- trainData %>% 
  mutate_at(c("qn17","qn24", "qn19", 
              "qn13", "qn18", "qn23", "qn15", "qn16", 
              "qn25", 
              "qn42", "qn48", "qn52", "qn57", 
              "qn60", "qn62",
              "qnobese", "qn68", 
              "qn80", "qn81",
              "qn79", "qn83","qn88", "qn89"), funs(recode(., `1`="Yes", `0`="No")))

trainData2 <- trainData2 %>% 
  mutate_at(c("qnsuicide"), funs(recode(., `1`="High_risk", `0`="Low_risk")))

trainData2 <- trainData2 %>% 
  mutate_at(c("grade"), funs(recode(., "9th grade"="Ninth", "10th grade"="Tenth",
                                    "11th grade"="Eleventh", "12th grade"="Twelfth")))

trainData2 <- trainData2 %>% 
  mutate_at(c("race4"), funs(recode(., "01.White"="White", "02.Black/African American"="Black_African American",
                                    "03.Latino/Hispanic"="Latino_Hispanic", "04.All Other Races"="All_Other_Races")))

trainData2 <- trainData2 %>% 
  mutate_at(c("sexid2"), funs(recode(., "01.Heterosexual"="Heterosexual", "02.Sexual Minority"="Sexual_Minority",
                                     "03.Unsure"="Unsure")))

testData2 <- testData %>% 
  mutate_at(c("qn17","qn24", "qn19", 
              "qn13", "qn18", "qn23", "qn15", "qn16", 
              "qn25", 
              "qn42", "qn48", "qn52", "qn57", 
              "qn60", "qn62",
              "qnobese", "qn68", 
              "qn80", "qn81",
              "qn79", "qn83","qn88", "qn89"), funs(recode(., `1`="Yes", `0`="No")))

testData2 <- testData2 %>% 
  mutate_at(c("qnsuicide"), funs(recode(., `1`="High_risk", `0`="Low_risk")))

testData2 <- testData2 %>% 
  mutate_at(c("grade"), funs(recode(., "9th grade"="Ninth", "10th grade"="Tenth",
                                    "11th grade"="Eleventh", "12th grade"="Twelfth")))

testData2 <- testData2 %>% 
  mutate_at(c("race4"), funs(recode(., "01.White"="White", "02.Black/African American"="Black_African American",
                                    "03.Latino/Hispanic"="Latino_Hispanic", "04.All Other Races"="All_Other_Races")))

testData2 <- testData2 %>% 
  mutate_at(c("sexid2"), funs(recode(., "01.Heterosexual"="Heterosexual", "02.Sexual Minority"="Sexual_Minority",
                                     "03.Unsure"="Unsure")))

# Create a decision tree model

CART <- rpart(qnsuicide~weight+stratum+PSU+sex+grade+race4+sexid2
              +qn17+qn24+qn19
              +qn13+qn18+qn23+qn15+qn16
              +qn25
              +qn42+qn48+qn52+qn57
              +qn60+qn62
              +qnobese+qn68
              +qn80+qn81
              +qn79+qn83+qn88+qn89, data = trainData2, method = 'class')
summary(CART)

rpart.plot(CART, type=2, extra=101,
box.palette="RdBu", shadow.col="gray", nn=TRUE)

plotcp(CART)
# size tree 4 is the optimal number

# Examine which predictors are important
vip(CART, num_features = 29, bar = FALSE)

# Tune the parameters by 10-fold cross validation
trctrl <- trainControl(method = "cv", n = 10, classProbs = TRUE)
CART_tuned <- train(
  qnsuicide~weight+stratum+PSU+sex+grade+race4+sexid2
  +qn17+qn24+qn19
  +qn13+qn18+qn23+qn15+qn16
  +qn25
  +qn42+qn48+qn52+qn57
  +qn60+qn62
  +qnobese+qn68
  +qn80+qn81
  +qn79+qn83+qn88+qn89,
  data = trainData2,
  method = "rpart",
  parms = list(split="gini"),
  trControl = trctrl
)

plot(CART_tuned)
# best tune 0.0147929

# Decision rules in the model
CART_tuned$finalModel

rpart.plot(CART_tuned$finalModel, type=2, extra=104,
           box.palette="RdBu", tweak=1.2, varlen =20, nn=TRUE)

# Make predictions on the test data
pred_CART <- predict(CART_tuned, testData2, type="prob")

# pred_CART <- as.data.frame(pred_CART)
# pred_CART <- pred_CART %>%
#   dplyr::mutate(Label = ifelse(`Low_risk` > `High_risk`, 0, 1)) %>%
#   dplyr::mutate(MaxProb = pmax(`Low_risk`, `High_risk`))
# 
# # Calculate AUC
# plotROC(testData$qnsuicide, pred_CART$Label)
# 
# # confusion matrix on the label classfication
# confusionMatrix(table(pred_CART$Label,testData$qnsuicide))


# Take the cut-off probability 50%
Pred <- ifelse(pred_CART$High_risk> 0.50, "High_risk", "Low_risk")
Pred <- as.factor(Pred)
# Ordering the vectors
Predicted <- ordered(Pred, levels = c("High_risk", "Low_risk"))
Actual <- ordered(testData2$qnsuicide,levels = c("High_risk", "Low_risk"))
# Making confusion matrix
cm <-confusionMatrix(table(Predicted,Actual))
cm

# Take the cut-off probability 40%
Pred <- ifelse(pred_CART$High_risk> 0.40, "High_risk", "Low_risk")
Pred <- as.factor(Pred)
# Ordering the vectors
Predicted <- ordered(Pred, levels = c("High_risk", "Low_risk"))
Actual <- ordered(testData2$qnsuicide,levels = c("High_risk", "Low_risk"))
# Making confusion matrix
cm <-confusionMatrix(table(Predicted,Actual))
cm

# Create a confusion matrix function
CmFn <- function(cutoff) {
  Pred <- ifelse(pred_CART$High_risk> cutoff, "High_risk", "Low_risk")
  Pred <- as.factor(Pred)
  
  Predicted <- ordered(Pred, levels = c("High_risk", "Low_risk"))
  Actual <- ordered(testData2$qnsuicide,levels = c("High_risk", "Low_risk"))

  cm <-confusionMatrix(table(Predicted,Actual))
  cm$overall[1]
  
  # extracting accuracy
  Accuracy <- cm$overall[1]
  # extracting sensitivity
  Sensitivity <- cm$byClass[1]
  # extracting specificity
  Specificity <- cm$byClass[2]

  Indicators <- cbind(cutoff,Accuracy,Sensitivity,Specificity)
  return(Indicators)}


# Find all indicator values with a range of cutoff values 
cutoff1 <- seq( 0, 1, by = .05 )
Indicators_list <- lapply(cutoff1, CmFn)

# Create indicator table
Indicators_table <- as.data.frame(Indicators_list)
Indicators_table2 <- transpose(Indicators_table)
Indicators_table2 <- as.data.frame(split(Indicators_table2, 1:4))
names(Indicators_table2)[1] <- "cutoff"
names(Indicators_table2)[2] <- "accuracy"
names(Indicators_table2)[3] <- "sensitivity"
names(Indicators_table2)[4] <- "specificity"


