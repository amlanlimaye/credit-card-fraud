library(utils)
library(dplyr)
library(chron)
library(lubridate)
library(stats)
library(speedglm)
library(neuralnet)
library(randomForest)

original_dataset <- read.csv("corporate_cards_with_labels.csv", colClasses = 
                               c("integer", "factor", "Date", "factor",
                                 "character", "factor", "factor", 
                                 "factor", "numeric", "factor"))

# Weekend boolean variable
original_dataset$weekend_bool <- as.numeric(is.weekend(original_dataset$DATE))

# Day of month variable
original_dataset$day_of_month <- mday(original_dataset$DATE)

# Day of week variable
original_dataset$day_of_week <- wday(original_dataset$DATE)

# Month variable
original_dataset$month <- month(original_dataset$DATE)

# Creating heavy new variables

original_dataset=original_dataset %>%
    mutate(Past1Day=DATE) %>%
    mutate(Past2Day=DATE-days(1)) %>%
    mutate(Past3Day=DATE-days(2)) %>%
    mutate(Past7Day=DATE-days(6)) %>%
    mutate(Past90Day=DATE-days(89))

# Using text progress bar to track progress of for loop
pb = txtProgressBar(min = 0, max = nrow(original_dataset), 
                    initial = 0, style = 3)
setTxtProgressBar(pb,i)

## number of transactions by Card number
for (i in 1:dim(original_dataset)[1]){
    original_dataset$Card1Day[i]=nrow(filter(original_dataset,CARDNUM==CARDNUM[i] & 
                                          DATE<=DATE[i] & DATE>=Past1Day[i]))
    original_dataset$Card2Day[i]=nrow(filter(original_dataset,CARDNUM==CARDNUM[i] & 
                                          DATE<=DATE[i] & DATE>=Past2Day[i]))
    original_dataset$Card3Day[i]=nrow(filter(original_dataset,CARDNUM==CARDNUM[i] & 
                                          DATE<=DATE[i] & DATE>=Past3Day[i]))
    original_dataset$Card7Day[i]=nrow(filter(original_dataset,CARDNUM==CARDNUM[i] & 
                                          DATE<=DATE[i] & DATE>=Past7Day[i]))
    original_dataset$Card90Day[i]=nrow(filter(original_dataset,CARDNUM==CARDNUM[i] & 
                                           DATE<=DATE[i] & DATE>=Past90Day[i]))
    setTxtProgressBar(pb,i)
}

pb = txtProgressBar(min = 0, max = nrow(original_dataset), 
                    initial = 0, style = 3)
setTxtProgressBar(pb,i)

## amount of transactions by Card number
for (i in 1:dim(original_dataset)[1]){
    original_dataset$Card1Amount[i]=original_dataset %>%
        filter(CARDNUM==CARDNUM[i] & DATE <=DATE[i] & DATE>=Past1Day[i]) %>%
        dplyr::summarise(Card1Amount=sum(AMOUNT))
    original_dataset$Card2Amount[i]=original_dataset %>%
        filter(CARDNUM==CARDNUM[i] & DATE <=DATE[i] & DATE>=Past2Day[i]) %>%
        dplyr::summarise(Card2Amount=sum(AMOUNT))
    original_dataset$Card3Amount[i]=original_dataset %>%
        filter(CARDNUM==CARDNUM[i] & DATE <=DATE[i] & DATE>=Past3Day[i]) %>%
        dplyr::summarise(Card3Amount=sum(AMOUNT))
    original_dataset$Card7Amount[i]=original_dataset %>%
        filter(CARDNUM==CARDNUM[i] & DATE <=DATE[i] & DATE>=Past7Day[i]) %>%
        dplyr::summarise(Card7Amount=sum(AMOUNT))
    original_dataset$Card90Amount[i]=original_dataset %>%
        filter(CARDNUM==CARDNUM[i] & DATE <=DATE[i] & DATE>=Past90Day[i]) %>%
        dplyr::summarise(Card90Amount=sum(AMOUNT))
    setTxtProgressBar(pb,i)
}

original_dataset$Card_Trans_1=90/1*original_dataset$Card1Day/original_dataset$Card90Day
original_dataset$Card_Trans_2=90/2*original_dataset$Card2Day/original_dataset$Card90Day
original_dataset$Card_Trans_3=90/3*original_dataset$Card3Day/original_dataset$Card90Day
original_dataset$Card_Trans_7=90/7*original_dataset$Card7Day/original_dataset$Card90Day

original_dataset$Card1Amount=as.numeric(original_dataset$Card1Amount)
original_dataset$Card2Amount=as.numeric(original_dataset$Card2Amount)
original_dataset$Card3Amount=as.numeric(original_dataset$Card3Amount)
original_dataset$Card7Amount=as.numeric(original_dataset$Card7Amount)
original_dataset$Card90Amount=as.numeric(original_dataset$Card90Amount)

original_dataset$Card_Amount_1=90/1*original_dataset$Card1Amount/original_dataset$Card90Amount
original_dataset$Card_Amount_2=90/2*original_dataset$Card2Amount/original_dataset$Card90Amount
original_dataset$Card_Amount_3=90/3*original_dataset$Card3Amount/original_dataset$Card90Amount
original_dataset$Card_Amount_7=90/7*original_dataset$Card7Amount/original_dataset$Card90Amount

pb = txtProgressBar(min = 0, max = nrow(original_dataset), 
                    initial = 0, style = 3)
setTxtProgressBar(pb,i)

## number of transactions by Merchant number
for (i in 1:dim(original_dataset)[1]){
    original_dataset$Merch1Day[i]=nrow(filter(original_dataset,MERCHNUM==MERCHNUM[i] & 
                                           DATE<=DATE[i] & DATE>=Past1Day[i]))
    original_dataset$Merch2Day[i]=nrow(filter(original_dataset,MERCHNUM==MERCHNUM[i] & 
                                           DATE<=DATE[i] & DATE>=Past2Day[i]))
    original_dataset$Merch3Day[i]=nrow(filter(original_dataset,MERCHNUM==MERCHNUM[i] & 
                                           DATE<=DATE[i] & DATE>=Past3Day[i]))
    original_dataset$Merch7Day[i]=nrow(filter(original_dataset,MERCHNUM==MERCHNUM[i] & 
                                           DATE<=DATE[i] & DATE>=Past7Day[i]))
    original_dataset$Merch90Day[i]=nrow(filter(original_dataset,MERCHNUM==MERCHNUM[i] & 
                                            DATE<=DATE[i] & DATE>=Past90Day[i]))
    setTxtProgressBar(pb,i)
}

pb = txtProgressBar(min = 0, max = nrow(original_dataset), 
                    initial = 0, style = 3)
setTxtProgressBar(pb,i)

## amount of transactions by Merchant number
for (i in 1:dim(original_dataset)[1]){
    original_dataset$Merch1Amount[i]=original_dataset %>%
        filter(MERCHNUM==MERCHNUM[i] & DATE <=DATE[i] & DATE>=Past1Day[i]) %>%
        dplyr::summarise(Card1Amount=sum(AMOUNT))
    original_dataset$Merch2Amount[i]=original_dataset %>%
        filter(MERCHNUM==MERCHNUM[i] & DATE <=DATE[i] & DATE>=Past2Day[i]) %>%
        dplyr::summarise(Card2Amount=sum(AMOUNT))
    original_dataset$Merch3Amount[i]=original_dataset %>%
        filter(MERCHNUM==MERCHNUM[i] & DATE <=DATE[i] & DATE>=Past3Day[i]) %>%
        dplyr::summarise(Card3Amount=sum(AMOUNT))
    original_dataset$Merch7Amount[i]=original_dataset %>%
        filter(MERCHNUM==MERCHNUM[i] & DATE <=DATE[i] & DATE>=Past7Day[i]) %>%
        dplyr::summarise(Card7Amount=sum(AMOUNT))
    original_dataset$Merch90Amount[i]=original_dataset %>%
        filter(MERCHNUM==MERCHNUM[i] & DATE <=DATE[i] & DATE>=Past90Day[i]) %>%
        dplyr::summarise(Card90Amount=sum(AMOUNT))
    setTxtProgressBar(pb,i)
}

original_dataset$Merch_Trans_1=90/1*original_dataset$Merch1Day/original_dataset$Merch90Day
original_dataset$Merch_Trans_2=90/2*original_dataset$Merch2Day/original_dataset$Merch90Day
original_dataset$Merch_Trans_3=90/3*original_dataset$Merch3Day/original_dataset$Merch90Day
original_dataset$Merch_Trans_7=90/7*original_dataset$Merch7Day/original_dataset$Merch90Day

original_dataset$Merch1Amount=as.numeric(original_dataset$Merch1Amount)
original_dataset$Merch2Amount=as.numeric(original_dataset$Merch2Amount)
original_dataset$Merch3Amount=as.numeric(original_dataset$Merch3Amount)
original_dataset$Merch7Amount=as.numeric(original_dataset$Merch7Amount)
original_dataset$Merch90Amount=as.numeric(original_dataset$Merch90Amount)

original_dataset$Merch_Amount_1=90/1*original_dataset$Merch1Amount/original_dataset$Merch90Amount
original_dataset$Merch_Amount_2=90/2*original_dataset$Merch2Amount/original_dataset$Merch90Amount
original_dataset$Merch_Amount_3=90/3*original_dataset$Merch3Amount/original_dataset$Merch90Amount
original_dataset$Merch_Amount_7=90/7*original_dataset$Merch7Amount/original_dataset$Merch90Amount

# Separating data before/after 2010-09-01
modeling_data <- original_dataset[original_dataset$DATE < "2010-09-01",]
out_of_time_data <- original_dataset[original_dataset$DATE >= "2010-09-01",]

# Removing unnecessary columns from original dataset for modeling
modeling_data <- modeling_data[,-c(1:8, 15:19)]
out_of_time_data <- out_of_time_data[,-c(1:8, 15:19)]

set.seed(1)
model_train_ind <- sample(seq_len(nrow(modeling_data)), 
                          size = (0.8 * nrow(modeling_data))) 

model_train <- modeling_data[model_train_ind, ]
model_test <- modeling_data[-model_train_ind, ]

# "Good" rows in modeling data
good_rows <- model_train[model_train$Fraud.label == 0,]

# "Bad" rows in modeling data
bad_rows <- model_train[model_train$Fraud.label == 1,]

# Sampling good rows to create 10/1 down-sample
set.seed(2)
good_ind <- sample(seq_len(nrow(good_rows)), size = 10*nrow(bad_rows)) # Down-sample control

# 10/1 down-sampled training data
train_data <- rbind(good_rows[good_ind,], bad_rows)
train_data <- na.omit(train_data)

# Logistic Regression
logit_model <- glm(data = train_data, 
                   family = binomial("logit"), maxit = 100,
                   formula = Fraud.label~.)

summary(logit_model)

train_data_preds <- train_data

train_data_preds$logit_score <- 
  predict(logit_model, newdata = train_data, 
          type='response')

model_test$logit_score <- 
    predict(logit_model, newdata = model_test, 
            type='response')

out_of_time_data$logit_score <- predict(logit_model, newdata = out_of_time_data, 
                                        type='response')

# Sorting the above 3 data frames by their logit scores
train_data_preds <- arrange(train_data_preds, desc(logit_score))
model_test <- arrange(model_test, desc(logit_score))
out_of_time_data <- arrange(out_of_time_data, desc(logit_score))

sum(as.numeric(train_data_preds[1:430,]$Fraud.label)-1)/(sum(as.numeric(train_data_preds$Fraud.label)-1))
sum(as.numeric(model_test[1:637,]$Fraud.label))/(sum(as.numeric(model_test$Fraud.label)))
sum(as.numeric(out_of_time_data[1:810,]$Fraud.label)-1)/(sum(as.numeric(out_of_time_data$Fraud.label)-1))

# Neural Networks
set.seed(3)
train_data$weekend_bool <- as.integer(train_data$weekend_bool)
train_data$day_of_month <- as.integer(train_data$day_of_month)
train_data$day_of_week <- as.integer(train_data$day_of_week)
train_data$month <- as.integer(train_data$month)
train_data$Fraud.label <- as.integer(train_data$Fraud.label) - 1

n <- names(train_data)
f <- as.formula(paste("Fraud.label ~", paste(n[!n %in% "Fraud.label"], collapse = " + ")))
f

model_nn <- neuralnet(formula = f, data = train_data,  
                      hidden = 4, lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1)
plot(model_nn, rep = "best")

model_test$Fraud.label <- as.integer(model_test$Fraud.label) - 1
model_test$weekend_bool <- as.integer(model_test$weekend_bool)
model_test$day_of_month <- as.integer(model_test$day_of_month)
model_test$day_of_week <- as.integer(model_test$day_of_week)
model_test$month <- as.integer(model_test$month)

train_data_preds$nn_score <- as.numeric(compute(model_nn,train_data_preds[,-c(2,43)])$net.result)
model_test$nn_score <- as.numeric(compute(model_nn, model_test[,-c(2,43)])$net.result)
out_of_time_data$nn_score <- as.numeric(compute(model_nn,out_of_time_data[,-c(2,43)])$net.result)

train_data_preds$nn_score <- as.numeric(train_data_preds$nn_score)
model_test$nn_score <- as.numeric(model_test$nn_score)
out_of_time_data$nn_score <- as.numeric(out_of_time_data$nn_score)

# Sorting the above 3 data frames by their nn scores
train_data_preds <- arrange(train_data_preds, desc(nn_score))
model_test <- arrange(model_test, desc(nn_score))
out_of_time_data <- arrange(out_of_time_data, desc(nn_score))

sum(as.numeric(train_data_preds[1:430,]$Fraud.label)-1)/(sum(as.numeric(train_data_preds$Fraud.label)-1))
sum(as.numeric(model_test[1:1366,]$Fraud.label))/(sum(as.numeric(model_test$Fraud.label)))
sum(as.numeric(out_of_time_data[1:2700,]$Fraud.label)-1)/(sum(as.numeric(out_of_time_data$Fraud.label)-1))

# Random Forests
set.seed(4)
model_rf <- randomForest(as.factor(Fraud.label) ~ ., 
                         data = train_data,
                         importance = T,
                         ntree = 50)

rf_preds <- predict(model_rf, model_test[,-c(2, 43, 44)], 
                    OOB=TRUE, type = "prob")
colnames(rf_preds) <- c("zero", "one")
rf_preds <- data.frame(rf_preds)

model_test$rf_score <- rf_preds$one

rf_preds <- predict(model_rf, train_data_preds[,-c(2, 43, 44)], 
                    OOB=TRUE, type = "prob")
colnames(rf_preds) <- c("zero", "one")
rf_preds <- data.frame(rf_preds)

train_data_preds$rf_score <- rf_preds$one

rf_preds <- predict(model_rf, out_of_time_data[,-c(2, 43, 44)], 
                    OOB=TRUE, type = "prob")
colnames(rf_preds) <- c("zero", "one")
rf_preds <- data.frame(rf_preds)

out_of_time_data$rf_score <- rf_preds$one

varImpPlot(model_rf)

# Sorting the above 3 data frames by their nn scores
train_data_preds <- arrange(train_data_preds, desc(rf_score))
model_test <- arrange(model_test, desc(rf_score))
out_of_time_data <- arrange(out_of_time_data, desc(rf_score))

sum(as.numeric(train_data_preds[1:430,]$Fraud.label)-1)/(sum(as.numeric(train_data_preds$Fraud.label)-1))
sum(as.numeric(model_test[1:1366,]$Fraud.label))/(sum(as.numeric(model_test$Fraud.label)))
sum(as.numeric(out_of_time_data[1:2700,]$Fraud.label)-1)/(sum(as.numeric(out_of_time_data$Fraud.label)-1))

# sort test data frame by ensemble score

train_data_preds <- arrange(train_data_preds, desc(ensemble_score))
model_test <- arrange(model_test, desc(ensemble_score))
out_of_time_data <- arrange(out_of_time_data, desc(ensemble_score))

sum(as.numeric(train_data_preds[1:430,]$Fraud.label)-1)/sum(as.numeric(train_data_preds$Fraud.label)-1)
sum(as.numeric(model_test[1:1366,]$Fraud.label))/sum(as.numeric(model_test$Fraud.label))
sum(as.numeric(out_of_time_data[1:2700,]$Fraud.label)-1)/(sum(as.numeric(out_of_time_data$Fraud.label)-1))
