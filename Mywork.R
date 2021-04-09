library(readxl);library(mice);library(VIM);library(caret)
library(kknn);library(rpart);library(klaR);library(xgboost)
library(rpart.plot);library(ROCR);library(randomForest);library(pROC)
library(grDevices)

setwd('C:\\Users\\lenovo\\Desktop\\lt-vehicle-loan-default-prediction')
setwd('C:\\Users\\lenovo\\Desktop\\datasets-for-churn-telecom') # TNND 上面那个做不下去了 换这个
setwd('C:\\Users\\lenovo\\Desktop') # TNND 上面那个又做不下去了 换这个

data <- read.table('adult.dat',header = F,sep = ',')

####################################################################################################
#  这些都是第一个数据集的处理代码
data <- read.csv('train.csv',header = T)
sum(is.na(data))
str(data)
# 一共23万条数据 41个变量
# 但第一变量UniqueID 没用 删掉
data <- data[,-which(colnames(data)%in%c('UniqueID'))]
# 根据Date.of.Birth 可以算出年龄Age 因此在Excel里算出了Age（以2018年为止）故删掉这个变量
data <- data[,-which(colnames(data)%in%c('Date.of.Birth'))]
# 提取各变量名称
var.name <- names(data)
var.name
# 在str()数据之后发现有几个变量应该转换类型
# 这些应该转换成因子型
# branch_id, supplier_id, manufacturer_id, Current_pincode_ID, Employment.Type, State_ID,
# Employee_code_ID, MobileNo_Avl_Flag, Aadhar_flag,PAN_flag, VoterID_flag, Driving_flag, Passport_flag
data$branch_id <- as.factor(data$branch_id)
data$supplier_id <- as.factor(data$supplier_id)
data$manufacturer_id <- as.factor(data$manufacturer_id)
data$Current_pincode_ID <- as.factor(data$Current_pincode_ID)
data$Employment.Type <- as.factor(data$Employment.Type)
data$State_ID  <- as.factor(data$State_ID)
data$Employee_code_ID <- as.factor(data$Employee_code_ID)
data$MobileNo_Avl_Flag <- as.factor(data$MobileNo_Avl_Flag)
data$Aadhar_flag <- as.factor(data$Aadhar_flag)
data$PAN_flag <- as.factor(data$PAN_flag)
data$VoterID_flag <- as.factor(data$VoterID_flag)
data$Driving_flag <- as.factor(data$Driving_flag)
data$Passport_flag <- as.factor(data$Passport_flag)
str(data)
# 可以看到MobileNo_Avl_Flag 只有一个level 删掉
data <- data[,-which(colnames(data)%in%c('MobileNo_Avl_Flag'))]
# supplier_id   Current_pincode_ID   Employee_code_ID   DisbursalDate  level 数太多 去掉
data <- data[,-which(colnames(data)%in%c('supplier_id','Current_pincode_ID','Employee_code_ID','DisbursalDate'))]
str(data)
# table every variate
#data.table <- list()
#for(i in 1:ncol(data)){
#  data.table[[i]] <- table(data[,i])
#}
# 关心变量的构成  0 为没有违约  1 为违约
table(data$loan_default)
# 随便看看个别变量的构成
table(data$NO.OF_INQUIRIES)
table(data$CREDIT.HISTORY.LENGTH)
table(data$AVERAGE.ACCT.AGE)
table(data$DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS)
table(data$NEW.ACCTS.IN.LAST.SIX.MONTHS)
table(data$SEC.INSTAL.AMT)
table(data$SEC.DISBURSED.AMOUNT)
table(data$PRI.NO.OF.ACCTS)
table(data$PRI.ACTIVE.ACCTS)
table(data$SEC.NO.OF.ACCTS)
table(data$SEC.ACTIVE.ACCTS)
table(data$SEC.CURRENT.BALANCE)
# 我们关心的变量是 loan_default 将其转换为因子型变量
coly <- which(colnames(data)%in%c('loan_default'))
data$loan_default <- factor(data$loan_default)
levels(data$loan_default) <- c('1','2')
names(data)[coly] <- 'y'
str(data$y)
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
# 下面进行第二个数据集的处理
data <- read.csv('cell2celltrain.csv',header = T)
# 第一列是ID 没用 去掉
data <- data[,-1]
str(data)
# 我们关心的变量叫 Churn
coly <- which(colnames(data)%in%c('Churn'))
levels(data$Churn) <- c('1','2')
names(data)[coly] <- 'y'
str(data$y)
table(data$y)
# 查看缺失比例
n <- nrow(data);p <- ncol(data[,-1]) # n 为数据量; p 为自变量数
# 查看每各变量缺失的比例
proportion <- c()
for(i in 1:ncol(data)){
  proportion[i] <- sum(is.na(data[,i]))/n*100
}
round(proportion,8) # 查看每个变量空缺值的比例
# 去掉所有含缺失值的变量
data <- data[,-which(proportion>0)]
# 去掉含缺失值的数据
data <- data[complete.cases(data),]
# 检查缺失数据
sum(is.na(data))
# 没有缺失值了 万岁！
# 转换变量类型  并删除level数过多的ServiceArea
data$UniqueSubs <- as.factor(data$UniqueSubs)
data$ActiveSubs <- as.factor(data$ActiveSubs)
data <- data[,-which(colnames(data)%in%c('ServiceArea'))]
data <- data[,-which(colnames(data)%in%c('HandsetPrice'))]
data <- data[,-c(18:28)]
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
# 下面进行第三个数据集的处理
data <- read.csv('income_evaluation.csv',header = T)
dim(data)
str(data)
summary(data)
# 数值型变量 描述性统计
library(psych)
describe(data,quant = c(.25,.75))
#attributes(data)
# 相关性可视化
library(corrplot)
# 数值型变量为1,3,5，11,12,13
data.corr <- cor(data[,c(1,3,5,11,12,13)]);data.corr
corrplot(data.corr,type = 'upper',method = 'circle',tl.col = 'black',order = 'hclust',tl.pos = 'lt')
corrplot(data.corr,type = 'lower',method = 'number',add = T,order = 'hclust',tl.pos = "n", cl.pos = "n",diag = FALSE)
# 因子型变量 2,4,6,7,8,9,10,14,15
library(qicharts)
library(qicharts2)
paretochart(data$workclass,title = 'Pareto chart',subtitle = 'For workclass',print.data = F)
paretochart(data$education,title = 'Pareto chart',subtitle = 'For education',print.data = F)
paretochart(data$marital.status,title = 'Pareto chart',subtitle = 'For marital.status',print.data = F)
paretochart(data$occupation,title = 'Pareto chart',subtitle = 'For occupation',print.data = F)
paretochart(data$relationship,title = 'Pareto chart',subtitle = 'For relationship',print.data = F)
paretochart(data$race,title = 'Pareto chart',subtitle = 'For race',print.data = F)
paretochart(data$sex,title = 'Pareto chart',subtitle = 'For sex',print.data = F)
paretochart(data$native.country,title = 'Pareto chart',subtitle = 'native.country',print.data = F)
paretochart(data$income,title = 'Pareto chart',subtitle = 'For income',print.data = F)
library(fdth)
plot(fdt_cat(data$workclass),type='pa',col=c('skyblue','red')) 
# 只有两个level的变量sex 和 income 画简单的柱形图
barplot(summary(data$sex))
library(ggplot2)
ggplot(data=data, mapping=aes(x=sex,fill=sex))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  labs(title = 'The barplot of variable sex')+
  theme(plot.title = element_text(size = 14, hjust = .5, color = "gray30"))

ggplot(data=data, mapping=aes(x=income,fill=income))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  labs(title = 'The barplot of variable income')+
  theme(plot.title = element_text(size = 14, hjust = .5, color = "gray30"))




# 我们关心的变量叫 income   <=50k的是level 1 ， >50k 的是level 2 
coly <- which(colnames(data)%in%c('income'))
levels(data$income) <- c('1','2')
names(data)[coly] <- 'y'
str(data$y)
table(data$y)
# 删除 两个变量 fnlwgt education.num 具体原因看网页代码
data <- data[,-which(colnames(data)%in%c('fnlwgt'))]
data <- data[,-which(colnames(data)%in%c('education.num'))]
data <- data[,-which(colnames(data)%in%c('native.country'))]

# 查看缺失值
sum(is.na(data))


# 划分 取80% 为训练集   20% 为测试集
#set.seed(1)
#trainIndex <- createDataPartition(data$y , p=.99 ,list=F,times=1)
#data1 <- data[trainIndex,]
# 考虑是否二阶抽样
data1 <- data
set.seed(1)
trainIndex1 <- createDataPartition(data1$y , p=.8 ,list=F,times=1)
train <- data1[trainIndex1,]
test <- data1[-trainIndex1,]
table(train$y)
table(test$y)

# 删除方差几乎为零的变量:即删除频率比（频率最高的两个值的频数之比）超过95/5，且取值百分比（变量取值个数除以样本量再乘以100）小于等于10的变量
# 删除相关性高的变量
# 这两个操作就合并在一起了
#nzv.pre <- preProcess(train,method = 'nzv')
#train <- predict(nzv.pre,train)
#cor.pre <- preProcess(train,method = "corr",cutoff = .75)
#train <- predict(cor.pre,train)

#test <- predict(nzv.pre,test)
#test <- predict(cor.pre,test)

str(train)
#dim(train)


#################################################
################下面是建模时间！！###############
#################################################
coly <- which(colnames(test)%in%c('y'))
#  Naive Bayes
#留一交叉验证寻找最优参数usekernel和fL
ptm <- proc.time() # 开始计算时间
pred.NB3 <- factor(rep("2",nrow(train)*4),levels=c("1","2"))
ntrain <- nrow(train)
istart <- -1
for (ifL in 0:1) {
  for (iusekernel in 0:1) {
    istart <- istart+1
    for (i in 1:nrow(train)){
      NB.model <- NaiveBayes(y ~ ., train[-i,],usekernel=iusekernel,fL=ifL)  
      pred.NB <- predict(NB.model,train[i,-coly])
      pred.NB3[istart*ntrain+i] <- pred.NB$class
    }
  }
}
NB.accuracy <- c(rep(0,4))
for (i in 1:4) {
  pred.NB4 <- pred.NB3[((i-1)*nrow(train)+1):(i*nrow(train))]
  NB.measure <- confusionMatrix(pred.NB4,train[,coly],positive="2")
  NB.accuracy[i] <- NB.measure$overall[1]
}
NB.accuracy
proc.time() - ptm   # 结束计算时间
#把上述结果整理为一个表格
para.table <- matrix(rep(NA,4),nrow=2,ncol=2)
colnames(para.table)=c("usekernel=F","usekernel=T")
rownames(para.table)=c("fL=0","fL=1")
para.table[1,1]=1-NB.accuracy[1]
para.table[1,2]=1-NB.accuracy[2]
para.table[2,1]=1-NB.accuracy[3]
para.table[2,2]=1-NB.accuracy[4]
para.table #结果显示usekernel=T，fL=0或1为最优
# 使用最优参数建模
ptm <- proc.time() # 开始计算时间
#NB.model <- NaiveBayes(y ~ ., train, usekernel=T,fL=1) # Accuracy : 0.696    Kappa : 0.392  
NB.model <- NaiveBayes(y ~ ., train, usekernel=T,fL=0) # Accuracy : 0.6851   Kappa : 0.3703    
#NB.model <- NaiveBayes(y ~ ., train, usekernel=F,fL=1) # Accuracy : 0.6869   Kappa : 0.3739  
#NB.model <- NaiveBayes(y ~ ., train, usekernel=F,fL=0) # Accuracy : 0.6791   Kappa : 0.3583 
pred.NB.test <- predict(NB.model,test[,-coly])
proc.time() - ptm     # 结束计算时间
con.NB <- confusionMatrix(pred.NB.test$class,test[,coly],positive="2")
con.NB
con.NB$byClass
# ROC AUC
NB.roc <- roc(test$y,pred.NB.test$posterior[,1],levels = c('1','2'))
NB.roc$auc
plot(NB.roc, print.auc=T, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=T)




# KNN
#留一交叉验证确定最优参数 留一交叉 -> train.kknn,欧氏距离distance = 2
ptm <- proc.time()    # 开始计算时间
model.best.kknn <- train.kknn(y ~ .,train,kmax = 50,
                              kernel = c("rectangular","triangular","epanechnikov","biweight","triweight",
                                         "cos", "inv", "gaussian","rank","optimal"),distance=2,scale=T)
# 设定kmax为50，即从1到50进行挑选，kernel选取了帮助文档中所有出现的权重函数
plot(model.best.kknn)
head(model.best.kknn$MISCLASS) # 显示错误率
model.best.kknn # 输出最优参数情况
# 用最优参数对测试集进行预测
model.test.kknn <- kknn(y ~ .,train,test[,-coly],k=model.best.kknn$best.parameters$k,
                        scale=T,distance=2,kernel=model.best.kknn$best.parameters$kernel)
proc.time() - ptm   # 结束计算时间
#table(testy,model.test.kknn$fitted.values) # 混淆矩阵
con.KNN <- confusionMatrix(model.test.kknn$fitted.values,test[,coly],positive = '2') # 基于混淆矩阵评价模型
con.KNN
con.KNN$byClass
# ROC AUC
KNN.roc <- roc(test$y,model.test.kknn$prob[,1],levels = c('1','2'))
KNN.roc$auc



# Tree 
ptm <- proc.time() # 开始计算时间
rp_rpart <- rpart(y ~ .,train,minsplit=1,maxdepth=12,method="class") #参数的默认值为：minsplit=20,cp=0.01,maxdepth=30，最好自行设定
print(rp_rpart)
plotcp(rp_rpart)
cpmatrix <- printcp(rp_rpart)
mincpindex <- which.min(cpmatrix[,4])
cponeSE <- cpmatrix[mincpindex,4]+cpmatrix[mincpindex,5]
cpindex <- min(which(cpmatrix[,4]<=cponeSE))
cpmatrix[cpindex,1]#所确定的cp值
#剪枝
rp_rpart2 <- prune(rp_rpart,cp=cpmatrix[cpindex,1]) #设置cp
rp_rpart2$variable.importance  #查看变量相对重要性
summary(rp_rpart2)
print(rp_rpart2)
#画出模型
rpart.plot(rp_rpart2,type=4) #每类判定条件更明确
post(rp_rpart2,file="",title.="post: CART") 
pre.cart <- data.frame(predict(rp_rpart2,test[,-coly]))#已剪枝的模型预测值
pre.cart.class <- predict(rp_rpart2,test[,-coly],type='class')
proc.time() - ptm  # 结束计算时间

pred.cart <- prediction(pre.cart[,2], test[,coly])
con.cart <- confusionMatrix(pre.cart.class,factor(test[,coly]),positive="2")
con.cart
con.cart$byClass

recall.cart <- con.cart$byClass[[1]]
precision.cart <- con.cart$byClass[[3]]
#F-measure
Fmeasure.cart <- 2*precision.cart*recall.cart/(recall.cart+precision.cart)
Fmeasure.cart
#AUC
perf.auc.cart <- performance(pred.cart,measure="auc")#所得的是一个list对象
unlist(perf.auc.cart@y.values)#AUC值
# ROC AUC
CART.roc <- roc(test$y,pre.cart[,2],levels = c('1','2'))
CART.roc$auc


#  Random forest
#train1 <- train[,-which(colnames(train)%in%c('native.country'))]
ptm <- proc.time() # 开始计算时间
# 确定随机森林的最优参数
p <- ncol(train)-1
err <- rep(0,p)
B <- rep(0,p)
for (i in 1:p){
  set.seed(1)
  rfmodel <- randomForest(y ~ .,data=train,ntree=500,mtry=i,nodesize=1,maxnodes=2^10)
  err[i] <- min(rfmodel$err.rate[,1])
  B[i] <- which.min(rfmodel$err.rate[,1])
}
err
B
mtry.optimal <- which.min(err)
ntree.optimal <- B[which.min(err)]
c(mtry.optimal,ntree.optimal)
#用最优参数建模
set.seed(1)
model.rf <- randomForest(y ~ .,data=train,ntree=500,mtry=mtry.optimal,nodesize=1,maxnodes=2^12,importance=T)#,proximity=T)
c(min(model.rf$err.rate[,1]),which.min(model.rf$err.rate[,1]))#发现加了importance=T后err.rate结果有所变化啦！
#MDSplot(model.rf,fac = train$y)
plot(1:12,err,xlab = 'mtry',type = 'b',ylab = 'OOB')
plot(model.rf,col=c("red","blue","green"))#看树的个数与OOB错误率的关系,红色表示整体的OOB错误率，蓝色和绿色分别表示是“no”和"yes"两类的OOB错误率
legend("topright",c("Total","1","2"),lty=c(1,2,2),col=c("red","blue","green"))

model.rf$importance#各变量的重要程度
varImpPlot(model.rf, main="Variable Importance Random Forest audit")#作图
# 预测
pred.rf <- predict(model.rf,test)
proc.time() - ptm   # 结束计算时间

con.RF <- confusionMatrix(pred.rf,test[,coly],positive="2")
con.RF
con.RF$byClass
# AUC
pred.rf1 <- predict(model.rf,test,type = 'prob')
pred <- prediction(pred.rf1[,2],test$y)
pred <- performance(pred,'auc')
unlist(pred@y.values)#AUC值
# ROC AUC
RF.roc <- roc(test$y,pred.rf1[,1],levels = c('1','2'))
RF.roc$auc


# Xgboost
ptm <- proc.time() # 开始计算时间
# xgboost仅接受数值型变量，故对因子型变量要先做one hot编码
trainx <- model.matrix(~.,data=train[,-coly])[,-1]
trainy <- as.numeric(as.factor(train[,coly]))-1 #将因变量转化为numeric,且取值为0和1
testx<- model.matrix(~.,data=test[,-coly])[,-1]

param <- list(seed=1,objective="binary:logistic",subsample = 1,max_depth=10,
              colsample_bytree = .3,min_child_weight=.1,gamma = 5,eval_metric ="auc")
#建模
model.xgb <- xgboost(data=trainx,label=trainy,params=param,nrounds=100)
#也可交叉验证建模
#modelcv.xgb <- xgb.cv(data=trainx,label=trainy,params=param,nrounds=10,nfold=2)
#modelcv.xgb 
#查看各子学习器
submodel <- xgb.dump(model.xgb, with_stats = T)
#submodel[1:20] #显示子学习器详细情况的前20行
#获得特征的真实名称
names <- dimnames(data.matrix(trainx))[[2]]
#names
#计算特征重要性矩阵
importance_matrix <- xgb.importance(names, model = model.xgb)
#变量重要性作图
xgb.plot.importance(importance_matrix[1:25,])
#预测
pred.xgb <- predict(model.xgb,testx)
pred.xgboost <- ifelse(pred.xgb<.5,0,1)
proc.time() - ptm   # 结束计算时间

pred.xgboost <- factor(pred.xgboost);levels(pred.xgboost) <- c('1','2')
con.XB <- confusionMatrix(pred.xgboost,test$y,positive = '2')
con.XB
con.XB$byClass
# ROC AUC
Xg.roc <- roc(test$y,pred.xgb,levels = c('1','2'))
Xg.roc$auc
# AUC
pred.xg <- prediction(pred.xgb,test$y)
pred.xg <- performance(pred.xg,'auc')
unlist(pred.xg@y.values)#AUC值


###################################################################################################
#########         模型效果比较          ###########################################################
###################################################################################################
pr.NB <- prediction(pred.NB.test$posterior[,2],test$y)
pr.KNN <- prediction(model.test.kknn$prob[,2],test$y)
pr.CART <- prediction(pre.cart[,2], test[,coly])
pr.RF <- prediction(pred.rf1[,2],test$y)
pr.XG <- prediction(pred.xgb,test$y)
#pred11 <- performance(pr.XG,'auc')
#unlist(pred11@y.values)#AUC值

#敏感度-特异度曲线
perf.NB1 <- performance(pr.NB,"sens","spec")
perf.KNN1 <- performance(pr.KNN,"sens","spec")
perf.CART1 <- performance(pr.CART,"sens","spec")
perf.RF1 <- performance(pr.RF,"sens","spec")
perf.XG1 <- performance(pr.XG,"sens","spec")
par(bg='grey92')
plot(perf.NB1,col=1)
plot(perf.KNN1,col=2,add=T)
plot(perf.CART1,col=3,add=T)
plot(perf.RF1,col=4,add=T)
plot(perf.XG1,col=5,add=T)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
legend("bottomleft",c("NB","KNN","CART","RandomForest","Xgboost"),col=1:5,lty=1)


#查全率-查准率曲线
perf.NB12 <- performance(pr.NB,"prec","rec")
perf.KNN12 <- performance(pr.KNN,"prec","rec")
perf.CART12 <- performance(pr.CART,"prec","rec")
perf.RF12 <- performance(pr.RF,"prec","rec")
perf.XG12 <- performance(pr.XG,"prec","rec")
par(bg='grey92')
plot(perf.NB12,col=1)
plot(perf.KNN12,col=2,add=T)
plot(perf.CART12,col=3,add=T)
plot(perf.RF12,col=4,add=T)
plot(perf.XG12,col=5,add=T)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
legend("bottomleft",c("NB","KNN","CART","RandomForest","Xgboost"),col=1:5,lty=1)


#ROC
perf.NB121 <- performance(pr.NB,"tpr","fpr")
perf.KNN121 <- performance(pr.KNN,"tpr","fpr")
perf.CART121 <- performance(pr.CART,"tpr","fpr")
perf.RF121 <- performance(pr.RF,"tpr","fpr")
perf.XG121 <- performance(pr.XG,"tpr","fpr")
par(bg='grey92')
plot(perf.NB121,col=1)
plot(perf.KNN121,col=2,add=T)
plot(perf.CART121,col=3,add=T)
plot(perf.RF121,col=4,add=T)
plot(perf.XG121,col=5,add=T)
abline(0,1,lty = 4)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
legend("bottomright",c("NB","KNN","CART","RandomForest","Xgboost"),col=1:5,lty=1)


#提升图
perf.NB1212 <- performance(pr.NB,"lift","rpp")
perf.KNN1212 <- performance(pr.KNN,"lift","rpp")
perf.CART1212 <- performance(pr.CART,"lift","rpp")
perf.RF1212 <- performance(pr.RF,"lift","rpp")
perf.XG1212 <- performance(pr.XG,"lift","rpp")
par(bg='grey92')
plot(perf.NB1212,col=1)
plot(perf.KNN1212,col=2,add=T)
plot(perf.CART1212,col=3,add=T)
plot(perf.RF1212,col=4,add=T)
plot(perf.XG1212,col=5,add=T)
abline(h = seq(0,4,.5),v = seq(0,1,.2),col = 'white')
legend("topright",c("NB","KNN","CART","RandomForest","Xgboost"),col=1:5,lty=1)



# ggplot2 画ROC曲线
rocplot<- function(pred, truth, ...){
  predob<- prediction(pred, truth)
  #打印AUc
  perf.auc<- performance(predob, measure = 'auc', x.measure = 'cutoff')
  #
  perf<- performance(predob, 'tpr','fpr')
  df<- data.frame(x = attributes(perf)$x.values[[1]],y = attributes(perf)$y.values[[1]])  
  p    <- ggplot(data = df)
  p + geom_line(aes(x,y),colour = "yellowgreen",size = 1) + 
    geom_ribbon(aes(x,ymin = 0,ymax = y),fill = alpha("yellowgreen",0.5)) +
    labs(title = paste("ROC Curve & AUC:",(perf.auc@y.values))) + 
    xlab("Specificity") +
    ylab("Sensitivity") +
    theme(plot.title = element_text(size = 17)) 
}

rocplot(pred.NB.test$posterior[,2],test$y)










