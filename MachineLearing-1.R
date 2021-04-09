setwd('C:\\Users\\lenovo\\Desktop\\Final project')
data <- read.csv("train1.csv")
test0 <- read.csv("test1.csv") # 读入数据
data <- data[,-1] # 去掉id列
test0 <- test0[,-(1:2)]

# 预处理
sum(is.na(data)) # 查看缺失值
# 显示没有缺失值

#str(data) # 构造查看数据结构函数
getinf <- function(data){
  data_inf <- list();var_length <- c()
  data_table <- list();zeronum <- c()
  for (i in 1:length(data)){
    data_inf[[i]] <- unique(data[,i])
    var_length[i] <- length(data_inf[[i]])
    data_table[[i]] <- table(data[,i])
    zeronum[i] <- sum(data[,i]==0)
  } # 这里查看每个变量的取值结构 即试图分辨分类变量与数值变量
#data_infm <- do.call(cbind,lapply(data_inf,`length<-`,max(length(data_inf))))
  return(list("data_inf"=data_inf,"var_length"=var_length,
              "data_table"=data_table,"Zero"=zeronum))
}

data_inf <- getinf(data)
View(data_inf$Zero)

# 突然发现并非没有缺失值，而是数据里把缺失值都换成了0，于是修改前面的函数
# 再观察每个变量中0的个数，发现有一些观测有大量缺失，
# 这里认为那些本身取值很大的变量中只要有取0的即可认为是缺失值，且取0的个数为个位数的大致也认为是缺失
# 记录X8:X13,X32:X37,X44:X49,X68:X79,X86:X91,X207:X208,X225:X229,X236:X241,X272:X273这些变量
v <- c(8:13,32:37,44:49,68:79,86:91,207:208,225:229,236:241,272:273);v<-v+1;data1 <- data
for(i in v){
  data1 <- data1[which(data1[,i]!=0),] # 删掉这些变量中取0的观测 一共删掉了44条观测
}

data_inf1 <- getinf(data1) # 再运行一遍函数
#View(data_inf1$Zero)

var_length1 <- data_inf1$var_length # 查看每个变量都有什么取值
which.min(data_inf1$var_length)
#View(data_inf1$var_length)
# 这里发现有很多变量只有一个取值 认为这些变量没有意义 因此删掉
data1 <- data1[,-which(var_length1==1)] # 去掉了32个变量

sum(data$X26!=data$X62)  # 发现这两个变量是一样的 于是去掉62
data1 <- data1[,-which(colnames(data1)%in%"X62")]

data_inf2 <- getinf(data1) # 再提取一遍信息

# 认为取值在10个以下的变量为分类变量
#for(i in which(data_inf2$var_length<=10)){
#  data1[,i] <- as.factor(data1[,i])
#}
#data_inf2 <- getinf(data1) # 最终整理好的数据再提取一遍信息

library(caret)
# 划分训练集测试集
colnames(data1)[1] <- "y"
data1$y <- factor(data1$y)
levels(data1$y) <- c('1','2')
set.seed(1) # 划分
setindex <- createDataPartition(data1$y,p=0.7,list = F,times = 1)
train <- data1[setindex,]
test <- data1[-setindex,]

# 删除方差近似为0的变量 也就是变量取值过于单一的
preProcess1 <- preProcess(train,method = "nzv")
train1 <- predict(preProcess1,train) # 这样又删除了83个变量
test1 <- predict(preProcess1,test)
traininf <- getinf(train1) # 再次查看数据结构
# 删掉与Y相关度较低的变量
#library(gam)
#set.seed(2019)
#cont <- sbfControl(functions = rfSBF,method = "cv",number = 10)
#sbf <- sbf(train1[,-1],train1[,1],sbfControl = cont)
#sbf$optVariables
#train2 <- train1[,c(1,which(colnames(train1)%in%sbf$optVariables))]


# 检验离群值
# 第1,2,15:18,43:46,70:75,141,147,154:183列为分类变量
library(DMwR)
out <- lofactor(train1[,-1],k=10)
plot(density(out))
sum(out>2) # 认为lof值大于2的为离群值，去掉
train2 <- train1[-which(out>2),]

traininf1 <- getinf(train2) # 再次查看数据结构


# 使用RandomForest
library(randomForest)
# 使用包中函数选择最优参数
tuneRF(train2[,-1],train2[,1],ntreeTry = 500,stepFactor = 1.2,improve = 0.05)#,mtryStart = 3)
#使用循环最优参数mtry和ntree
err <- c();B <- c()
for (i in 1:50){
  set.seed(2019)
  rfmodel <- randomForest(y~.,data=train1,ntree=500,mtry=i)
  err[i] <- min(rfmodel$err.rate[,1])
  B[i] <- which.min(rfmodel$err.rate[,1])
}
(mtry <- which.min(err))
(ntree <- B[which.min(err)]) # 这个玩意弄出来感觉不太靠谱
# 因为随机森林会受随机种子影响，且改变这几个参数最终结果也接近，在反复运行后选择取出现次数最多的参数
# 最后选择参数 mtry=12, ntree=500

mo.rf <- randomForest(y~.,data = train2,ntree=500,mtry=12,importance=T,localImp=T,proximity=T)
pre.rf <- predict(mo.rf,test1[,-1],type="prob")
pre.rf0 <- predict(mo.rf,test1[,-1])

pre.rf.f <- c();acc <- c();j=1
for(i in seq(0.1,0.9,by=0.05)){
  pre.rf.f <- ifelse(pre.rf[,1]>i,1,2)
  pre.rf.f <- factor(pre.rf.f)
  acc[j] <- sum(diag(table(pre.rf.f,test1$y)))/nrow(test1)
  j=j+1
}
acc
max(acc)
seq(0.1,0.9,by=0.05)[which.max(acc)]

# 针对test0
# 将个别变量装换成分类变量
#for(i in which(colnames(test0)%in%colnames(train1[c(1,2,15:18,43:46,70:75,141,147,154:183)]))){
#  test0[,i] <- as.factor(test0[,i])
#}
testinf <- getinf(test0)
pre.rf.fin <- predict(mo.rf,test0,type="prob")


# 使用Xgboost
library(xgboost)
#xgboost仅接受数值型变量，故对因子型变量要先做one hot编码
trainx<- model.matrix(~.,data=train2[,-1])[,-1] 
trainy <- as.numeric(as.factor(train2[,1]))-1 #将因变量转化为numeric,且取值为0和1
testx <- model.matrix(~.,data=test1[,-1])[,-1]
param <- list(seed=1,objective="binary:logistic",max_depth=10,min_child_weight=0.1,eval_metric ="auc")
#建模
model.xgb <- xgboost(data=trainx,label=trainy,params=param,nrounds=50)

#预测
pred.xgb <- predict(model.xgb, testx)
pred.xgboost <- ifelse(pred.xgb<.5,0,1)
pred.xgboost <- factor(pred.xgboost);levels(pred.xgboost) <- c('1','2')
conM1 <- confusionMatrix(pred.xgboost,test1$y,positive = '2')
conM1

for(i in seq(0.1,0.9,by=0.01)){
  pred.xgboost <- ifelse(pred.xgb<i,0,1)
  pre.xg.f <- factor(pred.xgboost)
  acc[j] <- sum(diag(table(pre.xg.f,test1$y)))/nrow(test1)
  j=j+1
}
acc

test00 <- test0[,which(colnames(test0)%in%colnames(train2))]
testx0 <- model.matrix(~.,data=test00)[,-1]

pred.xgb <- predict(model.xgb, testx0)
pred.xgboost <- ifelse(pred.xgb<.5,0,1)
pred.xgboost2 <- factor(pred.xgboost);levels(pred.xgboost2) <- c('1','2')



# 再换方法
# 标准化
preprocess1 <- preProcess(train2,method=c("center","scale"))
train2 <- predict(preprocess1,train2) #对训练集标准化
test1 <- predict(preprocess1,test1) #对测试集标准化

# 主成分
#PCA，使用caret包中的preProcess函数实现
preprocess1 <- preProcess(train2,method="pca",thresh=0.85) #PCA
#thresh表示PCA中累积方差比例的阈值,也可以通过pcaComp来设置主成分个数
train3 <- predict(preprocess1,train2) 
test2 <- predict(preprocess1,test1) #按基于训练集的PCA方法获得了测试集上相应的主成分特征
test2x <- test2[,-1]
# KNN
library(kknn)
#留一交叉验证确定最优参数 留一交叉 -> train.kknn,欧氏距离distance = 2
model.best.kknn <- train.kknn(y ~ .,train3,kmax = 50,
                              kernel = c("rectangular","triangular","epanechnikov","biweight","triweight",
                                         "cos", "inv", "gaussian","rank","optimal"),distance=2,scale=T)
# 设定kmax为50，即从1到50进行挑选，kernel选取了帮助文档中所有出现的权重函数
plot(model.best.kknn)
#model.best.kknn$MISCLASS # 显示错误率
model.best.kknn # 输出最优参数情况
# 显示最优的kernel为"triangular"，k=20
# 用最优参数对测试集进行预测
model.test.kknn <- kknn(y ~ .,train3,test2x,k=model.best.kknn$best.parameters$k,
                        scale=T,distance=2,kernel=model.best.kknn$best.parameters$kernel)
table(test2$y,model.test.kknn$fitted.values) # 混淆矩阵
conM.test <- confusionMatrix(model.test.kknn$fitted.values,test2$y,positive = '1') # 基于混淆矩阵评价模型
conM.test
#1 - as.numeric(conM.test$overall[1]) # 输出错误率


#library(caret)
# 划分训练集测试集
#set.seed(1)
#setindex <- createDataPartition(data1$y,p=0.7,list = F,times = 1)
#data1 <- data1[setindex,]
#test0 <- data1[-setindex,-1]
#test0y <- data1[-setindex,1]
#######################################
# 最终针对test0  ######################
colnames(data1)[1] <- "y"
data1$y <- factor(data1$y)
levels(data1$y) <- c('1','2')
# 删除方差近似为0的变量 也就是变量取值过于单一的
preProcess1 <- preProcess(data1,method = "nzv")
data2 <- predict(preProcess1,data1)
data_inf3 <- getinf(data2)
# 提取分类变量名字
varname <- colnames(data2)[which(data_inf3$var_length<=10)]
# 分离分类变量和定量变量
data2.f <- data2[,which(colnames(data2)%in%varname)]
data2.n <- data2[,-which(colnames(data2)%in%varname)]

# 对定量变量pca
#PCA，使用caret包中的preProcess函数实现
preprocess2 <- preProcess(data2.n,method="pca",thresh=0.85) #PCA
#thresh表示PCA中累积方差比例的阈值,也可以通过pcaComp来设置主成分个数
data2.n.2 <- predict(preprocess2,data2.n) 
#test2 <- predict(preprocess1,test1) #按基于训练集的PCA方法获得了测试集上相应的主成分特征

# 对分类变量处理 再分箱 因为发现测试集有不同的level但数量很小
for(i in 3:ncol(data2.f)){
  data2.f[which(data2.f[,i]>4),i] <- 4
}
data_inf4 <- getinf(data2.f)

# 整理出最终的训练集
train.fin <- cbind(data2.f,data2.n.2) # 这个对RandomForest，knn用
for(i in which(train_inf$var_length<=10)){
  train.fin[,i] <- as.factor(train.fin[,i])
}
str(train.fin)
train_inf <- getinf(train.fin)
#trainx.fin <- model.matrix(~.,data=train.fin[,-1])[,-1]
#trainy.fin <- as.numeric(as.factor(train.fin[,1]))-1 #将因变量转化为numeric,且取值为0和1

# 对test0处理
test0.f <- test0[,which(colnames(test0)%in%varname)]
# 对分类变量处理 再分箱 因为发现测试集有不同的level但数量很小
for(i in 2:ncol(test0.f)){
  test0.f[which(test0.f[,i]>4),i] <- 4
}
# 对定量变量处理
test0.n <- test0[,which(colnames(test0)%in%colnames(data2.n))]
test0.n.2 <- predict(preprocess2,test0.n) 
# 将定量定性合并
test0.fin <- cbind(test0.f,test0.n.2)
for(i in which(test0_inf$var_length<=10)){
  test0.fin[,i] <- as.factor(test0.fin[,i])
}
str(test0.fin)
test0_inf <- getinf(test0.fin)
#testx <- model.matrix(~.,data=test0.fin)[,-1]

# 建模
# randomforest
mo.rf <- randomForest(y~.,data = train.fin,ntree=500)
pre.rf <- predict(mo.rf,test0.fin,type="prob")
pre.rf.f <- predict(mo.rf,test0.fin)
table(pre.rf.f)


# Xgboost
library(xgboost)
#xgboost仅接受数值型变量，故对因子型变量要先做one hot编码
trainx<- model.matrix(~.,data=train.fin[,-1])[,-1] 
trainy <- as.numeric(as.factor(train.fin[,1]))-1 #将因变量转化为numeric,且取值为0和1
testx <- model.matrix(~.,data=test0.fin)[,-1]
param <- list(seed=1,objective="binary:logistic",max_depth=10,min_child_weight=0.1,eval_metric ="auc")
#建模
model.xgb <- xgboost(data=trainx,label=trainy,params=param,nrounds=50)
#预测
pred.xgb <- predict(model.xgb, testx)
pred.xgboost <- ifelse(pred.xgb<.5,0,1)
pred.xgboost <- factor(pred.xgboost);levels(pred.xgboost) <- c('1','2')
table(pred.xgboost)
#conM.xg <- confusionMatrix(pred.xgboost,test1$y,positive = '1')
#conM.xg


# knn
library(kknn)
#留一交叉验证确定最优参数 留一交叉 -> train.kknn,欧氏距离distance = 2
model.best.kknn <- train.kknn(y ~ .,train.fin,kmax = 50,
                              kernel = c("rectangular","triangular","epanechnikov","biweight","triweight",
                                         "cos", "inv", "gaussian","rank","optimal"),distance=2,scale=T)
# 设定kmax为50，即从1到50进行挑选，kernel选取了帮助文档中所有出现的权重函数
plot(model.best.kknn)
#model.best.kknn$MISCLASS # 显示错误率
model.best.kknn # 输出最优参数情况
# 显示最优的kernel为"inv"，k=17
# 用最优参数对测试集进行预测
model.test.kknn <- kknn(y ~ .,train.fin,test0.fin,k=model.best.kknn$best.parameters$k,
                        scale=T,distance=2,kernel=model.best.kknn$best.parameters$kernel)
#table(test2$y,model.test.kknn$fitted.values)
#conM.test <- confusionMatrix(model.test.kknn$fitted.values,test2$y,positive = '1')
#conM.test
pred.knn <- model.test.kknn$fitted.values
table(pred.knn)




write.csv(pred.xgboost,"result1.csv")











