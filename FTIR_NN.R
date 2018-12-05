library(mxnet)
setwd("O:/PhD/Data/Portugal 2018/paper")
load(file = "data.rda")

data.fn <- function()

ind<-c(1:20,71:90)#,451:470)

#Working Data on normal NN no deriv and scale
train.x <- train$FTIR
train.x <- scale(train.x)
train.y <- as.numeric(train$Sub_Spec)-1
train.x.arr <- t(train.x)
dim(train.x.arr) <- c(ncol(train.x),1,nrow(train.x))

test.x <- test$FTIR
test.x <- scale(test.x)
test.y <- as.numeric(test$Sub_Spec)-1
test.x.arr <- t(test.x)
dim(test.x.arr) <- c(ncol(test.x),1,nrow(test.x))

# Neural Network Classifier MLP
mx.set.seed(0)
model <- mx.mlp(train.x, train.y, activation = "tanh", optimizer = "adam",
                hidden_node=c(700), array.layout = "rowmajor",
                out_node=6, out_activation="softmax",
                num.round=100, array.batch.size=100,
                learning.rate=0.05,# momentum=0.9,
                eval.metric=mx.metric.accuracy)
conNN <- pred.fn(model, test.x, test.y)


mx.model.save(model, "NN_model", 60)
model<-mx.model.load("NN_model", 60)

graph.viz(model$symbol)


preds = predict(model, test.x)

pred.label = max.col(t(preds))-1
pred.label <- factor(pred.label)
levels(pred.label) <- c("br","co","es","rb","ro","sb")
test.y <- factor(test.y)
levels(test.y) <- c("br","co","es","rb","ro","sb")
confusionMatrix(pred.label, test.y)

pred.fn <- function(model, data, truth, levels = c("br","co","es","rb","ro","sb")){
  preds <- predict(model, data)
  pred.label <- max.col(t(preds))-1
  pred.label <- factor(pred.label)
  levels(pred.label) <- levels
  truth <- factor(truth)
  levels(truth) <- levels
  confM <- confusionMatrix(pred.label, truth)
  return(confM)
}

# 1D convolution Lenet

data <- mx.symbol.Variable('data')

# first conv
conv1 <- mx.symbol.Convolution(data=data, kernel=c(5), num_filter=20)
tanh1 <- mx.symbol.Activation(data=conv1, act_type="relu")
pool1 <- mx.symbol.Pooling(data=tanh1, pool_type="max",
                           kernel=c(2), stride=c(2))
# second conv
conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(5), num_filter=50)
tanh2 <- mx.symbol.Activation(data=conv2, act_type="relu")
pool2 <- mx.symbol.Pooling(data=tanh2, pool_type="max",
                           kernel=c(2), stride=c(2))
# first fullc
fc1 <- mx.symbol.FullyConnected(data=pool2, num_hidden=500)
tanh3 <- mx.symbol.Activation(data=fc1, act_type="tanh")
# second fullc
fc2 <- mx.symbol.FullyConnected(data=tanh3, num_hidden=6)
# loss
lenet <- mx.symbol.SoftmaxOutput(data=fc2)

device.cpu <- mx.cpu()

mx.set.seed(0)

model <- mx.model.FeedForward.create(lenet, X=train.x.arr, y=as.numeric(train.y), optimizer = "adam",
                                     ctx=device.cpu, num.round=100, array.batch.size=400,
                                     learning.rate=0.1, wd=0.00001, #momentum = .9
                                     eval.metric=mx.metric.accuracy,
                                     epoch.end.callback=mx.callback.log.train.metric(100))
conM <- pred.fn(model, test.x.arr, test.y)
conM