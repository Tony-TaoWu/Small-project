setwd('C:\\Users\\lenovo\\Desktop\\Final project')
library(readxl)
data <- read_excel("train2.xlsx")

data <- data[,-1] # 去掉id列
data <- as.data.frame(data)

# 转换因变量类型
colnames(data)[1] <- "y"
data$y <- factor(data$y)
levels(data$y) <- c('1','2')

# 发现缺失值为字符NA而非空白
#data1 <- data
data[data=="NA"] <- NA

# 计算缺失值
sum(is.na(data))
sum(!complete.cases(data))
str(data)
# 将gender,income,dependent转换变量类型
data$gender <- as.factor(data$gender)
levels(data$gender) <- c('1','2')
data$income <- as.numeric(data$income)
data$dependent <- as.numeric(data$dependent)
str(data)

# 构造查看数据结构函数
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

# 查看缺失情况
library(VIM)
aggr(data,prop=F,numbders=T)
library(mice)
md.pattern(data)

# 认定gender,dependent,del_30,del_60,del_90为分类变量，观察table之后发现取极值情况极少
data1 <- data
# 进行分箱
data1$dependent[which(data1$dependent>3)] <- 3 # dependent分为四类
table(data1$dependent)
data1$del_30[which(data1$del_30>1)] <- 1 # 后三个变量分为两类
table(data1$del_30)
data1$del_60[which(data1$del_60>1)] <- 1
table(data1$del_60)
data1$del_90[which(data1$del_90>1)] <- 1
table(data1$del_90)
# 类型变换
data1$dependent <- as.factor(data1$dependent)
data1$del_30 <- as.factor(data1$del_30)
data1$del_60 <- as.factor(data1$del_60)
data1$del_90 <- as.factor(data1$del_90)
str(data1)

# 直接去掉没有性别的观测
data1 <- data1[-which(is.na(data1$gender)),]
# 观察缺失dependent的观测 发现没有dependent的也没有income
# 于是直接删除没有dependent的观测
#View(data1[which(is.na(data1$dependent)),])
data1 <- data1[-which(is.na(data1$dependent)),]

aggr(data1,prop=F,numbders=T)
md.pattern(data1)

# 划分
library(caret)
set.seed(1) # 划分
setindex <- createDataPartition(data1$y,p=0.7,list = F,times = 1)
train <- data1[setindex,]
test <- data1[-setindex,]

# 查看训练集缺失
aggr(train,prop=F,numbders=T)
md.pattern(train)
sum(is.na(train$gender))
sum(is.na(train$dependent))

train1 <- train
sum(is.na(train1$income))
# 有10000多观测没有income

# 采用knn插补
preprocess1 <- preProcess(train1,method="medianImpute") # 默认k=5
train2 <- predict(preprocess1,train1) #进行插补
sum(is.na(train2))
#View(train2[!complete.cases(train1),]) #查看插补的值
test1 <- predict(preprocess1,test)
sum(is.na(test1))
#View(test1[!complete.cases(test),]) #查看插补的值

#levels(train2$gender) <- c('1','2')
#levels(test1$gender) <- c('1','2')

# 采用随机森林
# 使用包中函数选择最优参数
library(randomForest)
tuneRF(train2[,-1],train2[,1],ntreeTry = 500,stepFactor = 1.2,improve = 0.05)#,mtryStart = 3)
mo.rf <- randomForest(y~.,data = train2,ntree=500)
pre.rf <- predict(mo.rf,test1[,-1],type="prob")
pre.rf0 <- predict(mo.rf,test1[,-1])

conM.rf <- confusionMatrix(pre.rf0,test1$y,positive = '1')
conM.rf

# Xgboost
library(xgboost)
#train2$y <- as.numeric(train2$y)
#train2$y <- factor(train2$y);levels(train2$y) <- c('1','2')
#xgboost仅接受数值型变量，故对因子型变量要先做one hot编码
trainx<- model.matrix(~.,data=train2[,-1])[,-1]
trainy <- as.numeric(as.factor(train2[,1]))-1 #将因变量转化为numeric,且取值为0和1
testx<- model.matrix(~.,data=test1[,-1])[,-1]
#trainy
param <- list(seed=1,objective="binary:logistic",max_depth=10,min_child_weight=0.1,eval_metric ="auc")
#建模
model.xgb <- xgboost(data=trainx,label=trainy,params=param,nrounds=50)
# 预测
pred.xgb <- predict(model.xgb,testx)
pred.xgboost <- ifelse(pred.xgb<.5,0,1)
pred.xgboost <- factor(pred.xgboost);levels(pred.xgboost) <- c('1','2')

conM.xg <- confusionMatrix(pred.xgboost,test1$y,positive = '1')
conM.xg



# 对测试集处理
test0 <- read_excel("test2.xlsx")
test0 <- test0[,-(1:2)]
test0 <- as.data.frame(test0)
test0[test0=="NA"] <- NA

# 将gender,income,dependent转换变量类型
test0$gender <- ifelse(test0$gender=="Male",1,2)
test0$income <- as.numeric(test0$income)
test0$dependent <- as.numeric(test0$dependent)
str(test0)

# 插补
preprocess1 <- preProcess(data1,method="medianImpute") # 默认k=5
train0 <- predict(preprocess1,data1) #进行插补
sum(is.na(train0))

preprocess2 <- preProcess(test0,method="medianImpute") # 默认k=5
test0 <- predict(preprocess2,test0)
sum(is.na(test0))

test0$gender <- as.factor(test0$gender)
levels(test0$gender) <- c('1','2')
str(test0)

# 进行分箱
test0$dependent[which(test0$dependent>3)] <- 3 # dependent分为四类
test0$del_30[which(test0$del_30>1)] <- 1 # 后三个变量分为两类
test0$del_60[which(test0$del_60>1)] <- 1
test0$del_90[which(test0$del_90>1)] <- 1
# 类型变换
test0$dependent <- as.factor(test0$dependent)
test0$del_30 <- as.factor(test0$del_30)
test0$del_60 <- as.factor(test0$del_60)
test0$del_90 <- as.factor(test0$del_90)
str(test0)



# 建模
mo.rf <- randomForest(y~.,data = train0,ntree=500)
# 预测
pre.rf <- predict(mo.rf,test0,type="prob")
pre.rf0 <- predict(mo.rf,test0)
sum(pre.rf>0.5)

# 建模
trainx<- model.matrix(~.,data=train0[,-1])[,-1]
trainy <- as.numeric(as.factor(train0[,1]))-1 #将因变量转化为numeric,且取值为0和1
testx<- model.matrix(~.,data=test0)[,-1]
#trainy
param <- list(seed=1,objective="binary:logistic",max_depth=10,min_child_weight=0.1,eval_metric ="auc")
#建模
model.xgb <- xgboost(data=trainx,label=trainy,params=param,nrounds=50)
# 预测
pred.xgb <- predict(model.xgb,testx)

#pred.xgboost <- ifelse(pred.xgb<.5,0,1)
#pred.xgboost <- factor(pred.xgboost);levels(pred.xgboost) <- c('1','2')
#conM.xg <- confusionMatrix(pred.xgboost,train0$y,positive = '1')
#conM.xg


# 这都什么破结果。。
#View(cbind(pred.xgb,pre.rf2))
pre.rf2 <- as.numeric(pre.rf[,2])
pred.fin <- pred.xgb*.8+pre.rf2*.2
sum(pred.xgb>0.5)
sum(pred.fin>0.5)

write.csv(pred.fin,file="results2.csv")
















