# Coder: Anh Tran
# Objective: Prepare the YRBS data for analyses and modeling


# Necessary libraries
library(tidyverse)
library(data.table)
library(zoo)
library(ggplot2)
library(survey)
library(Hmisc)
library(mice)
library(InformationValue)
library(caret)
library(corrplot)
library(kknn)
library(rpart)
library(rpart.plot)
library(vip) 
library(ROCR)
# Load the data
yrbs_data_raw <- read.csv("data/YRBS_CA.csv",   
                         stringsAsFactors = FALSE, header = TRUE) 



# Select variables
yrbs_data <- yrbs_data_raw %>%
  select("record", "year", "weight", "stratum", "PSU", "age", "sex", "grade", "race4", "sexid2",
         "qn26", "qn27", # Suicide thought
         "qn28", "qn29",  # attempted suicide variables
         "qn17","qn24", "qn19", # violence (community-related)
         "qn13", "qn18", "qn23", "qn15", "qn16", # violence (school-related)
         "qn25", # mental health issue
         "qn42", "qn48", "qn52", "qn57", #substance use
         "qn60", "qn62", # sexual health
         "qnobese", "qn68", # weight-related 
         "qn80", "qn81", "qn79", "qn83", "qn88", # sedentary activities
         "qn89") # low academic performance 

# qn26 During the past 12 months, did you ever seriously consider attempting suicide?
# qn27 During the past 12 months, did you make a plan about how you would attempt suicide?
# qn28 During the past 12 months, how many times did you actually attempt suicide? (1 if more than 0)
# qn29 If you attempted suicide during the past 12 months, did any attempt result in an injury, poisoning, or overdose that had to be treated by a doctor or nurse?
# qn17 During the past 12 months, how many times were you in a physical fight? (1 if more than 0)
# qn24 During the past 12 months, have you ever been electronically bullied?
# qn19 Have you ever been physically forced to have sexual intercourse when you did not want to?
# qn13 During the past 30 days, on how many days did you carry a weapon such as a gun, knife, or club on school property? (1 if more than 0)
# qn18 During the past 12 months, how many times were you in a physical fight on school property? (1 if more than 0)
# qn23 During the past 12 months, have you ever been bullied on school property?
# qn15 During the past 30 days, on how many days did you not go to school because you felt you would be unsafe at school or on your way to or from school? (1 if more than 0)
# qn16 During the past 12 months, how many times has someone threatened or injured you with a weapon such as a gun, knife, or club on school property? (1 if more than 0)
# qn25 During the past 12 months, did you ever feel so sad or hopeless almost every day for two weeks or more in a row that you stopped doing some usual activities?
# qn42 During the past 30 days, on how many days did you have at least one drink of alcohol? (1 if more than 0)
# qn48 During the past 30 days, how many times did you use marijuana? (1 if more than 0)
# qn52 During your life, how many times have you used methamphetamines (also called speed, crystal, crank, or ice)? (1 if more than 0)
# qn57 During your life, how many times have you used a needle to inject any illegal drug into your body?
# qn60 How old were you when you had sexual intercourse for the first time? (1 if sex before 13 yrs old)
# qn62 During the past 3 months, with how many people did you have sexual intercourse? (1 if more than 0)
# qnobese Whethere the student is obese or not
# qn68 How do you describe your weight? (1 if describing as overweight)
# qn80 On an average school day, how many hours do you watch TV? (1 if more than 3 hours/day)
# qn81 On an average school day, how many hours do you play video or computer games or use a computer for something that is not school work? (1 if more than 3 hours/day)
# qn79 During the past 7 days, on how many days were you physically active for a total of at least 60 minutes per day? (reverse coding 1 if < 5 days)
# qn83 During the past 12 months, on how many sports teams did you play? (reverse coding 1 if equal 0)
# qn88 On an average school night, how many hours of sleep do you get? (reverse coding 1 if less than 8)
# qn89 During the past 12 months, how would you describe your grades in school? (reverse coding 1 if less B grade)


# yrbs_data_2017 <-yrbs_data %>%
#   filter(year==2017)
# summary(yrbs_data_2017)
# 
# yrbs_data_2015 <-yrbs_data %>%
#   filter(year==2015)
# summary(yrbs_data_2015)


# Recode variables
## Demographics
yrbs_data <- yrbs_data %>%
  dplyr::mutate(age = recode(age, `1`= "12 yrs old",
                                  `2`= "13 yrs old",
                                  `3`= "14 yrs old",
                                  `4`= "15 yrs old",
                                  `5`= "16 yrs old",
                                  `6`= "17 yrs old",
                                  `7`= "18 yrs old+"))
yrbs_data$age = as.factor(yrbs_data$age)

yrbs_data <- yrbs_data %>%
  dplyr::mutate(sex = recode(sex, `1`= "Female",
                             `2`= "Male"))
yrbs_data$sex = as.factor(yrbs_data$sex)


yrbs_data <- yrbs_data %>%
  dplyr::mutate(grade = recode(grade, `1`= "9th grade",
                             `2`= "10th grade",
                             `3`= "11th grade",
                             `4`= "12th grade",
                             `5`= "ungraded/other"))
yrbs_data$grade = as.factor(yrbs_data$grade)


yrbs_data <- yrbs_data %>%
  dplyr::mutate(race4 = recode(race4, `1`= "01.White",
                               `2`= "02.Black/African American",
                               `3`= "03.Latino/Hispanic",
                               `4`= "04.All Other Races"))
yrbs_data$race4 = as.factor(yrbs_data$race4)


yrbs_data <- yrbs_data %>%
  dplyr::mutate(sexid2 = recode(sexid2, `1`= "01.Heterosexual",
                               `2`= "02.Sexual Minority",
                               `3`= "03.Unsure"))
yrbs_data$sexid2 = as.factor(yrbs_data$sexid2)


## Recode other dichotomous variables 1 = 1 and 2 = 0 
yrbs_data <- yrbs_data %>% 
  mutate_at(c("qn26", "qn27", 
              "qn28", "qn29",  
              "qn17","qn24", "qn19", 
              "qn13", "qn18", "qn23", "qn15", "qn16", 
              "qn25", 
              "qn42", "qn48", "qn52", "qn57", 
              "qn60", "qn62",
              "qnobese", "qn68", 
              "qn80", "qn81",
              "qn79", "qn83","qn88", "qn89"), funs(recode(., `1`=1, `2`=0, .default = NaN)))

# Reverse coding qn79, qn83, qn89, qn88
yrbs_data <- yrbs_data %>% 
  mutate_at(c("qn79", "qn83", "qn88", "qn89"), funs(recode(., `1`=0, `0`=1, .default = NaN)))

## Combine the suicide thoughts and suicide attempt variables (qn26,qn27,qn28 and qn29) to create a binary non-fatal suicide risk variable
yrbs_data <- yrbs_data %>% 
  rowwise() %>% 
  mutate(qnsuicide = sum(qn26, qn27, qn28, qn29, na.rm=TRUE)) %>%
  dplyr::mutate(qnsuicide = ifelse(qnsuicide >= 1, 1, qnsuicide)) %>%
  dplyr::mutate(qnsuicide = ifelse(is.na(qn26) & is.na(qn27) & is.na(qn28) & is.na(qn29), NA, qnsuicide)) 
  
yrbs_data <- yrbs_data %>% 
  filter(!is.na(qnsuicide))

yrbs_data <- yrbs_data %>%
  select(-qn26,-qn27,-qn28,-qn29, -age)

# Convert binary variables to factor type
yrbs_data[,10:33] <- lapply(yrbs_data[,10:33] , factor)

###################################################
# Imputing missing data
# Reference procedure: https://uvastatlab.github.io/2019/05/01/getting-started-with-multiple-imputation-in-r/
set.seed(1)  

sum(apply(yrbs_data, MARGIN = 1, function(x) sum(is.na(x)))>1)
# 990 rows with at least one missing data
sapply(yrbs_data, function(x) sum(is.na(x)))
p_missing <- unlist(lapply(yrbs_data, function(x) sum(is.na(x))))/nrow(yrbs_data)
sort(p_missing[p_missing > 0], decreasing = TRUE)
# -> The maximum missing data is about 14%

yrbs_imp <- yrbs_data %>% 
  dplyr::select(-record, -year)

# Create a correlation matrix
res <- cor(as.data.frame(lapply(yrbs_imp[,-c(4:7)], as.integer)), use = "complete.obs")
corrplot(res, method="circle")
colnames(res)[which(abs(res) > 0.07)]
# -> weight, stratum, qn42, qn48, qn62 have high correlation values

# Run the mice code with 0 iterations 
imp <- mice(yrbs_imp, maxit=0)

# Extract predictorMatrix and methods of imputation 
# predM = imp$predictorMatrix

# adjusting the predictor matrix to automatically generate a predictor matrix based on 
# the bivariate correlations of the variables in the data
# where .10 is the default cutoff. 
new.pred <- quickpred(yrbs_imp)
meth = imp$method

# Unordered categorical variable 
poly <- c("sex","grade","race4","sexid2")

# Dichotomous variable
log <- c( "qn17","qn24", "qn19", 
        "qn13", "qn18", "qn23", "qn15", "qn16", 
        "qn25", 
        "qn42", "qn48", "qn52", "qn57", 
        "qn60", "qn62",
        "qnobese", "qn68", 
        "qn80", "qn81",
        "qn79", "qn83", "qn89", "qn88")


meth[log] = "logreg"
meth[poly] = "polyreg"

# impute the data by creating 5 datasets, use predM as the predictor matrix 

imp2 <- mice(yrbs_imp, maxit = 5, 
             predictorMatrix = new.pred, 
             method = meth, print =  FALSE)


# First, turn the datasets into long format
yrbs_data2 <- mice::complete(imp2, action="long", include = TRUE)

# Replace the NA in original dataset with the mode of all imputation of each record

cols_to_mutate <-colnames(yrbs_data2)
cols_to_mutate <- cols_to_mutate[-c(1,2,3,4,5,33)]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

yrbs_data3 <- yrbs_data2 %>%
  dplyr::group_by(.id) %>%
  dplyr::mutate_at(cols_to_mutate, function(x){x = na.aggregate(x, FUN=Mode)})

yrbs_data3 <- yrbs_data3 %>%
  filter(.imp==0)

any(is.na(yrbs_data3))
#yrbs_data2 <- complete(imp2,1)
 
# Convert binary variables back to integer type
yrbs_data3[,10:33] <- lapply(yrbs_data3[,10:33] , as.integer)
yrbs_data3[,10:33]<- yrbs_data3[,10:33] -1

summary(yrbs_data3)
###################################################################
# check class bias
table(yrbs_data3$qnsuicide)
# unbalanced class

set.seed(100)  
# Create traing (80%) and testing (20%) samples

# Create a train data set
input_ones_imp <- yrbs_data3[which(yrbs_data3$qnsuicide == 1), ]  
input_zeros_imp <- yrbs_data3[which(yrbs_data3$qnsuicide == 0), ]  

input_ones_training_rows <- sample(1:nrow(input_ones_imp), 0.8*nrow(input_ones_imp))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros_imp), 0.8*nrow(input_zeros_imp))  # 0's for training

training_ones_imp <- input_ones_imp[input_ones_training_rows, ]  
training_zeros_imp <- input_zeros_imp[input_zeros_training_rows, ]

trainData <- rbind(training_ones_imp, training_zeros_imp)  


# Create a test data set
test_ones_imp <- input_ones_imp[-input_ones_training_rows, ]
test_zeros_imp <- input_zeros_imp[-input_zeros_training_rows, ]

testData <- rbind(test_ones_imp, test_zeros_imp) 

