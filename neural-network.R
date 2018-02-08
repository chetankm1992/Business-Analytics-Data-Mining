## Loading train data which contains msno and is_churn
train_v2_data = read.csv(file.choose(), header = T)

## loading transaction data which contains payment method, plan, etc.
transaction_v2_data = read.csv(file.choose(), header = T)

## merging both data inorder to have is_churn column with transaction data
train_transaction_v2 <- merge(train_v2_data, transaction_v2_data, by="msno")

data_sample_v2 <- train_transaction_v2

data_sample_v2$is_churn <- as.numeric(data_sample_v2$is_churn)
data_sample_v2$msno <- as.numeric(data_sample_v2$msno)

## Neural Network
#Normalize
data <- (data_sample_v2 - min(data_sample_v2, na.rm=TRUE))/(max(data_sample_v2,na.rm=TRUE) - 
                                          min(data_sample_v2, na.rm=TRUE))
# Partition
set.seed(511)
ind <- sample(2, nrow(data), replace =T, prob = c(0.7,0.3))
train1 <-data[ind==1,]
test1 <- data[ind ==2,]

# Neural network model
library(neuralnet)
n <- names(train1)
f <- as.formula(paste("is_churn ~", paste(n[!n %in% "is_churn"], collapse = " + ")))
#nmodel <- neuralnet(f, data = train, hidden = , stepmax=1e6)
nmodel <- neuralnet(f, data=train1, hidden=3,
                    linear.output = FALSE)
#nmodel
plot(nmodel)
p <- nmodel$net.result[[1]]


# Prediction train
pred <- compute(nmodel,train1[,-1])
#  Confusion matrix - train data
p5 <- pred$net.result
p5 <- ifelse(p5>0.5,1,0)
tab5 <- table(predicted=p5, Actual = train1$is_churn)
tab5
# Miss clacification Error - train data
(1- sum(diag(tab5)/sum(tab5))) * 100
# Accuracy - train data
(sum(diag(tab5)/sum(tab5))) * 100

# Prediction test
pred <- compute(nmodel,test1[,-1])
# Confusion matrix - test data
p6 <- pred$net.result
p6 <- ifelse(p6>0.5,1,0)
tab6 <- table(predicted=p6, Actual = test1$is_churn)
tab6
# Miss clacification Error - train data
(1- sum(diag(tab6)/sum(tab6))) * 100
# Accuracy - train data
(sum(diag(tab6)/sum(tab6))) * 100
  
