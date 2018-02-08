## SVM

## Loading train data which contains msno and is_churn
train_v2_data = read.csv(file.choose(), header = T)

## loading transaction data which contains payment method, plan, etc.
transaction_v2_data = read.csv(file.choose(), header = T)

## merging both data inorder to have is_churn column with transaction data
train_transaction_v2 <- merge(train_v2_data, transaction_v2_data, by="msno")

## Taking out sample 10%
set.seed(511)
ind <- sample(2, nrow(train_transaction_v2), replace = T, prob = c(0.1, 0.9))
#ind
data_sample <- train_transaction_v2[ind == 1,]
#data_sample <-merged_v2_data


#dividing the sample further
ind2 <- sample(2, nrow(data_sample), replace = T, prob = c(0.7, 0.3))
train <- data_sample[ind2==1,]
test <- data_sample[ind2==2,]

library(e1071)
model_svm <- svm(is_churn ~ + payment_method_id + payment_plan_days + 
                   plan_list_price + actual_amount_paid + is_auto_renew +
                   transaction_date + membership_expire_date + is_cancel , train)
summary(model_svm)

# Prediction
p1 <- predict(model_svm, train)
# Confusion matrix - training data
tab1 <- table(predicted = p1, Actual = train$is_churn)
tab1
# Miss classification error 
(1 - sum(diag(tab1))/sum(tab1))
# Accuracy
(sum(diag(tab1))/sum(tab1))

# Confusion matrix - test
p2 <- predict(model_svm, test)
# Confusion matrix - test data
(tab2 <- table(predicted = p2, Actual = test$is_churn))
# Miss classification error
(1 - sum(diag(tab2))/sum(tab2))
#Accuracy
(sum(diag(tab2))/sum(tab2))

