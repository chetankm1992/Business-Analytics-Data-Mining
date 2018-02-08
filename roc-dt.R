library(pROC)
prob1 <- predict(tree_model1, newdata=test, type="prob")
auc1 <- auc(test$is_churn, prob1[,2])
auc1

plot(roc(test$is_churn, prob1[,2]), ylab='Sensitivity (TPR)', xlab='Specificity (FPR)', col="red", xlim=c(1,0))

