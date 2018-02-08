library(ROCR)

##################### ROC curve for logistic regression ######################

prob <- predict(lrmodel, newdata=test, type="response")
pred <- prediction(prob, test$is_churn)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, ylab='Sensitivity (TPR)', xlab='Specificity (FPR)', col="red")

auc <- performance(perf, measure = "auc")
auc <- auc@y.values[[1]]
auc