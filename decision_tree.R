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

## Implenting tree model
tree_model <- tree(is_churn ~ + payment_method_id + payment_plan_days + plan_list_price + actual_amount_paid + is_auto_renew+ transaction_date + membership_expire_date + is_cance, data = train, method = 'class') plot(tree_model) text(tree_model, pretty = 0)
# Confustion matrix for training data
p3 <- predict(tree_model, train, type = "response")
tab3 <- table(predicted = p3, Actual = train$is_churn)
tab3
# Miss classification error for training data
(1-sum(diag(tab3))/sum(tab3)) * 100
# Accuracy
(sum(diag(tab3))/sum(tab3)) * 100
# Confusion matrix - test
p2 <- predict(tree_model, test, type = 'response')
# Confusion matrix - test data
(tab2 <- table(predicted = p2, Actual = test$is_churn))
# Miss classification error
(1 - sum(diag(tab2))/sum(tab2)) * 100
#Accuracy
(sum(diag(tab2))/sum(tab2)) * 100