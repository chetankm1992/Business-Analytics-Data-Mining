## Loading train data which contains msno and is_churn
train_v2_data = read.csv(file.choose(), header = T)

## loading transaction data which contains payment method, plan, etc.
transaction_v2_data = read.csv(file.choose(), header = T)

## merging both data inorder to have is_churn column with transaction data
train_transaction_v2 <- merge(train_v2_data, transaction_v2_data, by="msno")


# Factor variables
train_transaction_v2$is_churn <- as.factor(train_transaction_v2$is_churn)

# Partition data - train 70%, test 30%
set.seed(511)
ind <-sample(2,nrow(train_transaction_v2), replace = T, prob = c(0.7, 0.3))
train <- train_transaction_v2[ind ==1,]
test <- train_transaction_v2[ind ==2,]

#library(caret)
lrmodel <- glm(is_churn ~ + payment_method_id + payment_plan_days + 
                 plan_list_price + actual_amount_paid + is_auto_renew +
                 transaction_date + membership_expire_date + is_cancel,
                data = train, family = "binomial")
summary(lrmodel)


# Confustion matrix for training data
p3 <- predict(lrmodel, train, type = "response")
p3 <- ifelse(p3>0.5,1,0)
tab3 <- table(predicted = p3, Actual = train$is_churn)
tab3
# Miss classification error for training data
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100

# Confustion matrix for test data
p4 <- predict(lrmodel, test, type = "response")
p4 <- ifelse(p4>0.5,1,0)
tab4 <- table(predicted = p4, Actual = test$is_churn)
tab4
# Miss classification error for test data
(1-sum(diag(tab4))/sum(tab4)) * 100
# Accuracy
(sum(diag(tab4))/sum(tab4)) * 100

