library(mice);library(VIM);library(klaR);library(ROCR);library(pROC)
library(randomForest);library(rpart);library(rpart.plot)

setwd('C:\\Users\\lenovo\\Desktop')
data <- read.csv('framingham_heart_disease.csv',header = T)
colnames(data)[16] <- 'y'
str(data)
table(data$y)

#  调整变量类型
data$male <- factor(data$male)
data$education <- factor(data$education)
data$currentSmoker <- factor(data$currentSmoker)
data$prevalentStroke <- factor(data$prevalentStroke)
data$BPMeds <- factor(data$BPMeds)
data$prevalentHyp <- factor(data$prevalentHyp)
data$diabetes <- factor(data$diabetes)
#data$TenYearCHD <- factor(data$TenYearCHD)
data$y <- factor(data$y)
str(data)
summary(data)
# 数值型变量 描述性统计
library(psych)
describe(data,quant = c(.25,.75))

# 相关性可视化
library(corrplot)
# 要去掉所有缺失值
data1.1 <- data[,c(2,5,10,11,12,13,14,15)]
str(data1.1)
data1.1 <- data1.1[complete.cases(data),]
colnames(data1.1) <- c('age','CPD','tot','sys','dia','BMI','HR','glu')
# 数值型变量为 2,5,10,11,12,13,14,15
data.corr <- cor(data1.1);data.corr
corrplot(data.corr,type = 'upper',method = 'circle',tl.col = 'grey',order = 'hclust',
         tl.pos = 'lt',diag = T)
corrplot(data.corr,type = 'lower',method="color",addCoef.col="black",add = T,
         order = 'hclust',tl.pos = "n",tl.col = 'grey',cl.pos = "n",diag = T)

#   描述性统计
library(ggplot2)
ggplot(data=data, mapping=aes(x=male,fill=male))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+
  labs(title = 'The barplot of variable male')+
  theme(plot.title = element_text(size = 14, hjust = .5, color = "gray30"))
ggplot(data=data, mapping=aes(x=education,fill=education))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=4)+
  theme_minimal()+
  labs(title = 'The barplot of variable education')+
  theme(plot.title = element_text(size = 18, hjust = .5, color = "gray30"))
ggplot(data=data, mapping=aes(x=currentSmoker,fill=currentSmoker))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=4)+
  theme_minimal()+
  labs(title = 'The barplot of variable currentSmoker')+
  theme(plot.title = element_text(size = 18, hjust = .5, color = "gray30"))
ggplot(data=data, mapping=aes(x=BPMeds,fill=BPMeds))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=4)+
  theme_minimal()+
  labs(title = 'The barplot of variable BPMeds')+
  theme(plot.title = element_text(size = 18, hjust = .5, color = "gray30"))
ggplot(data=data, mapping=aes(x=prevalentStroke,fill=prevalentStroke))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=4)+
  theme_minimal()+
  labs(title = 'The barplot of variable prevalentStroke')+
  theme(plot.title = element_text(size = 18, hjust = .5, color = "gray30"))
ggplot(data=data, mapping=aes(x=prevalentHyp,fill=prevalentHyp))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=4)+
  theme_minimal()+
  labs(title = 'The barplot of variable prevalentHyp')+
  theme(plot.title = element_text(size = 18, hjust = .5, color = "gray30"))
ggplot(data=data, mapping=aes(x=diabetes,fill=diabetes))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=4)+
  theme_minimal()+
  labs(title = 'The barplot of variable diabetes')+
  theme(plot.title = element_text(size = 18, hjust = .5, color = "gray30"))
ggplot(data=data, mapping=aes(x=TenYearCHD,fill=TenYearCHD))+
  geom_bar(stat="count",width=0.6)+
  scale_color_manual(values=c("#999999", "#E69F00"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=4)+
  theme_minimal()+
  labs(title = 'The barplot of variable TenYearCHD')+
  theme(plot.title = element_text(size = 18, hjust = .5, color = "gray30"))

#   数据预处理
sum(is.na(data))
sum(!complete.cases(data)) # 查看空缺行
md.pattern(data,rotate.names = T)  # 查看缺失模式
aggr(data,prop=T,numbders=T)

# 或者直接用随机森林插补全部缺失
mf1 <- missForest(data, maxiter = 5)  #用随机森林迭代弥补缺失值
data2 <- mf1$ximp # 得到插补后的数据
write.csv(data2,'data2.csv')
sum(is.na(data2))
#######################################################################
data2 <- read.csv('C:\\Users\\lenovo\\Desktop\\data2.csv',header = T)
colnames(data2)[16] <- 'y'
data2$male <- factor(data2$male)
data2$education <- factor(data2$education)
data2$currentSmoker <- factor(data2$currentSmoker)
data2$prevalentStroke <- factor(data2$prevalentStroke)
data2$BPMeds <- factor(data2$BPMeds)
data2$prevalentHyp <- factor(data2$prevalentHyp)
data2$diabetes <- factor(data2$diabetes)
data2$y <- factor(data2$y)
sum(is.na(data2))
str(data2)
#######################################################################
#cor.pre <- preProcess(data2,method = "corr",cutoff = .75)
#data5 <- predict(cor.pre,data2)


#  观察分析每个变量
#  cigsPerDay 与前面是否吸烟是相关的
table(data$cigsPerDay)
cigs <- data$cigsPerDay[which(data$cigsPerDay!=0)]
sum(cigs<20)
summary(cigs)
# 考虑两种办法 一个是无视此变量 只保留是否抽烟一个变量
# 另一种方法是 将这个变量按照是否大于20根分箱 大于等于20的在是否抽烟变量中改为2 这样是否抽烟变量就变成了三个level
# 暂时采用第一种方法
data2 <- data2[,-which(colnames(data2)%in%c('cigsPerDay'))]

#多个变量下的LOF
#library(DMwR)
#outlier.score2 <- lofactor(data5[,c(2,9,10,11,12,13)],k=5) #剔除定性变量
#outlier.score2[which(outlier.score2>1)]


#  age
#table(data$age)
#summary(data$age)
#age <- data$age
#sum(age<=49)
#sum(age>49)
#  考虑将年龄按照49分箱？
#data$age[which(data$age<=49)] <- 0
#data$age[which(data$age>49)] <- 1
#table(data$age)
# 看看age和y的table
#table(data$age,data$y)

#  0 1 变量
#table(data$male)
#table(data$age)
#table(data$education)
#table(data$currentSmoker)
#table(data$BPMeds)
#table(data$prevalentStroke)# 这个变量两个level之间相差太多 考虑去掉
#data <- data[,-which(colnames(data)%in%c('prevalentStroke'))]

#table(data$prevalentHyp)
#table(data$diabetes)

#  删除缺失值
#data <- data[complete.cases(data),] # 删除含缺失数据的行
#data1 <- data[which(data$y==1),]


#  将其中几个变量转为因子型变量
#data$male <- factor(data$male)
#data$age <- factor(data$age)
#data$education <- factor(data$education)
#data$currentSmoker <- factor(data$currentSmoker)
#ata$BPMeds <- factor(data$BPMeds)
#data$prevalentHyp <- factor(data$prevalentHyp)
#data$diabetes <- factor(data$diabetes)
#data$y <- factor(data$y)
#str(data)
#  子抽样
library(ROSE)
set.seed(1)
data3 <- ROSE(y~., data=data2)$data
#head(data) 
table(data3$y)

# 划分
coly <- which(colnames(data3)%in%c('y'))
set.seed(1)
trainIndex <- createDataPartition(data3$y , p=2/3 ,list=F,times=1)
train <- data3[trainIndex,]
test <- data3[-trainIndex,]



#  XGboost
library(xgboost)
# xgboost仅接受数值型变量，故对因子型变量要先做one hot编码
trainx <- model.matrix(~.,data=train[,-coly])[,-1]
trainy <- as.numeric(as.factor(train[,coly]))-1 #将因变量转化为numeric,且取值为0和1
testx<- model.matrix(~.,data=test[,-coly])[,-1]

param <- list(seed=1,objective="binary:logistic",subsample = 1,max_depth=5,
              colsample_bytree = 1,min_child_weight=.1,eval_metric ="auc")#,gamma = 5)
#建模
model.xgb <- xgboost(data=trainx,label=trainy,params=param,nrounds=200)
#也可交叉验证建模
#modelcv.xgb <- xgb.cv(data=trainx,label=trainy,params=param,nrounds=100,nfold=2)
#modelcv.xgb
#获得特征的真实名称
names <- dimnames(data.matrix(trainx))[[2]]
#names
#计算特征重要性矩阵
importance_matrix <- xgb.importance(names, model = model.xgb)
#变量重要性作图
xgb.plot.importance(importance_matrix)
#预测
pred.xgb <- predict(model.xgb,testx)
pred.xgboost <- ifelse(pred.xgb<.5,0,1)
pred.xgboost <- factor(pred.xgboost);levels(pred.xgboost) <- c('0','1')
con.XB <- confusionMatrix(pred.xgboost,test$y,positive = '1')
con.XB
con.XB$byClass


#  Naive Bayes
#留一交叉验证寻找最优参数usekernel和fL
#ptm <- proc.time() # 开始计算时间
pred.NB3 <- factor(rep("2",nrow(train)*4),levels=c("0","1"))
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
  NB.measure <- confusionMatrix(pred.NB4,train[,coly],positive="1")
  NB.accuracy[i] <- NB.measure$overall[1]
}
NB.accuracy
#proc.time() - ptm   # 结束计算时间
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
#ptm <- proc.time() # 开始计算时间
#NB.model <- NaiveBayes(y ~ ., train, usekernel=T,fL=1) # Accuracy : 0.696    Kappa : 0.392  
NB.model <- NaiveBayes(y ~ ., train, usekernel=T,fL=0) # Accuracy : 0.6851   Kappa : 0.3703    
#NB.model <- NaiveBayes(y ~ ., train, usekernel=F,fL=1) # Accuracy : 0.6869   Kappa : 0.3739  
#NB.model <- NaiveBayes(y ~ ., train, usekernel=F,fL=0) # Accuracy : 0.6791   Kappa : 0.3583 
pred.NB.test <- predict(NB.model,test[,-coly])
#proc.time() - ptm     # 结束计算时间
con.NB <- confusionMatrix(pred.NB.test$class,test[,coly],positive="1")
con.NB
con.NB$byClass
# ROC AUC
NB.roc <- roc(test$y,pred.NB.test$posterior[,1],levels = c('0','1'))
NB.roc$auc
plot(NB.roc, print.auc=T, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=T)


#  Random forest
#train1 <- train[,-which(colnames(train)%in%c('native.country'))]
#ptm <- proc.time() # 开始计算时间
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
model.rf <- randomForest(y ~ .,data=train,ntree=500,mtry=mtry.optimal,nodesize=1,maxnodes=2^10,importance=T)#,proximity=T)
c(min(model.rf$err.rate[,1]),which.min(model.rf$err.rate[,1]))#发现加了importance=T后err.rate结果有所变化啦！
#MDSplot(model.rf,fac = train$y)
plot(1:14,err,xlab = 'mtry',type = 'b',ylab = 'OOB',main = 'OOB for different mtry')
plot(model.rf,col=c("red","blue","green"),main = 'OOB for different ntree')#看树的个数与OOB错误率的关系,红色表示整体的OOB错误率，蓝色和绿色分别表示是“no”和"yes"两类的OOB错误率
legend("topright",c("Total","0","1"),lty=c(1,2,2),col=c("red","blue","green"))

model.rf$importance#各变量的重要程度
varImpPlot(model.rf, main="Variable Importance Random Forest audit")#作图
# 预测
pred.rf <- predict(model.rf,test)
#proc.time() - ptm   # 结束计算时间

con.RF <- confusionMatrix(pred.rf,test[,coly],positive="1")
con.RF
con.RF$byClass
# AUC
pred.rf1 <- predict(model.rf,test,type = 'prob')
pred <- prediction(pred.rf1[,2],test$y)
pred <- performance(pred,'auc')
unlist(pred@y.values)#AUC值
# ROC AUC
RF.roc <- roc(test$y,pred.rf1[,1],levels = c('0','1'))
RF.roc$auc




# Tree 
#ptm <- proc.time() # 开始计算时间
rp_rpart <- rpart(y ~ .,train,minsplit=1,maxdepth=10,method="class") #参数的默认值为：minsplit=20,cp=0.01,maxdepth=30，最好自行设定
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
#proc.time() - ptm  # 结束计算时间

pred.cart <- prediction(pre.cart[,2], test[,coly])
con.cart <- confusionMatrix(pre.cart.class,factor(test[,coly]),positive="1")
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



data <- train
#   根据上面的结果 我们选择的变量为glucose，age，sysBP，BMI，diaBP
data.fin <- data[,which(colnames(data)%in%c('glucose','age','sysBP','BMI','diaBP','y'))]
summary(data.fin)
#  将这几个变量分箱
# 对变量sysBP  -- 收缩压  正常：90mmHg<收缩压<140mmHg
sysbp <- data.fin$sysBP
sum(sysbp<140&sysbp>90)
sum(sysbp>=140|sysbp<=90)
# 对变量diaBP  -- 舒张压  正常：60mmHg<舒张压<90mmHg
diabp <- data.fin$diaBP
sum(diabp>60&diabp<90)
sum(diabp<=60|diabp>=90)
# 对变量BMI --  正常：18.5-23.9
bmi <- data.fin$BMI
sum(bmi<23.9&bmi>18.5)
sum(bmi>=23.9|bmi<=18.5)
# 对变量glucose -- 正常：70–120
glu <- data.fin$glucose
sum(glu>70&glu<120)
sum(glu<=70|glu>=120)
# 对变量age --  50
age <- data.fin$age
sum(age<=50)
sum(age>50)

table(data.fin$y)

# 尝试分箱
data.b <- data.fin
data.b$age <- factor(ifelse(data.b$age<=50,'0','1'))
data.b$sysBP <- factor(ifelse(data.b$sysBP<140&data.b$sysBP>90,'0','1'))
data.b$diaBP <- factor(ifelse(data.b$diaBP<90&data.b$diaBP>60,'0','1'))
data.b$BMI <- factor(ifelse(data.b$BMI<23.9&data.b$BMI>18.5,'0','1'))
data.b$glucose <- factor(ifelse(data.b$glucose<120&data.b$glucose>70,'0','1'))
str(data.b)

write.csv(data.b,'data.b.csv')
N <- c(); fin <- matrix(rep(0,7*32),ncol = 7)
obj <- c();n <- 1;pro <- c()
lel <- c('0','1')
for(i in 1:2){
  for(j in 1:2){
    for(k in 1:2){
      for(l in 1:2){
        for(m in 1:2){
          N[n] <- sum(data.b$age==lel[i]&data.b$sysBP==lel[j]&data.b$diaBP==lel[k]&
                        data.b$BMI==lel[l]&data.b$glucose==lel[m])
          pro[n] <- sum(data.b$age==lel[i]&data.b$sysBP==lel[j]&data.b$diaBP==lel[k]&
                          data.b$BMI==lel[l]&data.b$glucose==lel[m]&data.b$y=='1')
          names(pro)[n] <- paste('(',i-1,',',j-1,',',k-1,',',l-1,',',m-1,')',sep = '')
          names(N)[n] <- paste('(',i-1,',',j-1,',',k-1,',',l-1,',',m-1,')',sep = '')
          fin[n,1]<-i-1;fin[n,2]<-j-1;fin[n,3]<-k-1;fin[n,4]<-l-1;fin[n,5]<-m-1;fin[n,6]<-pro[n];fin[n,7]<-N[n]
          n <- n+1
        }
      }
    }
  }
}
pro
N
sum(N)
sum(pro)
fin <- as.data.frame(fin)
#fin <- fin[which(fin$V6!=0),]
colnames(fin)[1:5] <- c('age','sysBP','diaBP','BMI','glucose')
as.numeric(t(fin$age))
as.numeric(t(fin$sysBP))
as.numeric(t(fin$diaBP))
as.numeric(t(fin$BMI))
as.numeric(t(fin$glucose))
as.numeric(t(fin$V6))
as.numeric(t(fin$V7))

#dummies <- dummyVars(y~.,data=data.b)
#a <- predict(dummies,newdata=data.b)

# 我们得到参数估计
# 先将测试集提取处理
data <- test
#   根据上面的结果 我们选择的变量为glucose，age，sysBP，BMI，diaBP
data.fin1 <- data[,which(colnames(data)%in%c('glucose','age','sysBP','BMI','diaBP','y'))]
summary(data.fin1)
# 尝试分箱
data.b1 <- data.fin1
data.b1$age <- as.integer(ifelse(data.b1$age<=50,0,1))
data.b1$sysBP <- as.integer(ifelse(data.b1$sysBP<140&data.b1$sysBP>90,0,1))
data.b1$diaBP <- as.integer(ifelse(data.b1$diaBP<90&data.b1$diaBP>60,0,1))
data.b1$BMI <- as.integer(ifelse(data.b1$BMI<23.9&data.b1$BMI>18.5,0,1))
data.b1$glucose <- as.integer(ifelse(data.b1$glucose<120&data.b1$glucose>70,0,1))
#str(data.b1)
# 计算概率
pro <- c()
#data.b1 <- as.matrix(data.b1)
for(i in 1:nrow(data.b1)){
  logit <- -1.125+0.8821*data.b1[i,1]+0.5285*data.b1[i,2]+
    0.4181*data.b1[i,3]+0.009286*data.b1[i,4]+0.6427*data.b1[i,5]
  pro[i] <- exp(logit)/(1+exp(logit))
}
cla <- factor(ifelse(pro<0.5,0,1))
con.MC <- confusionMatrix(cla,data.b1$y,positive = '1')
con.MC
con.MC$byClass

###################################################################################################
#########         模型效果比较          ###########################################################
###################################################################################################
pr.NB <- prediction(pred.NB.test$posterior[,2],test$y)
pr.CART <- prediction(pro, test[,coly])
pr.RF <- prediction(pred.rf1[,2],test$y)
pr.XG <- prediction(pred.xgb,test$y)

pred11 <- performance(pr.NB,'auc')
pred12 <- performance(pr.CART,'auc')
pred13 <- performance(pr.RF,'auc')
pred14 <- performance(pr.XG,'auc')
unlist(pred11@y.values)#AUC值
unlist(pred12@y.values)#AUC值
unlist(pred13@y.values)#AUC值
unlist(pred14@y.values)#AUC值


#敏感度-特异度曲线
perf.NB1 <- performance(pr.NB,"sens","spec")
perf.CART1 <- performance(pr.CART,"sens","spec")
perf.RF1 <- performance(pr.RF,"sens","spec")
perf.XG1 <- performance(pr.XG,"sens","spec")
par(bg='grey92')
plot(perf.NB1,col=1)
plot(perf.CART1,col=3,add=T)
plot(perf.RF1,col=4,add=T)
plot(perf.XG1,col=5,add=T)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
legend("bottomleft",c("NB","MCMC","RandomForest","Xgboost"),col=c(1,3,4,5),lty=1)


#查全率-查准率曲线
perf.NB12 <- performance(pr.NB,"prec","rec")
perf.CART12 <- performance(pr.CART,"prec","rec")
perf.RF12 <- performance(pr.RF,"prec","rec")
perf.XG12 <- performance(pr.XG,"prec","rec")
par(bg='grey92')
plot(perf.NB12,col=1)
plot(perf.CART12,col=3,add=T)
plot(perf.RF12,col=4,add=T)
plot(perf.XG12,col=5,add=T)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
legend("bottomleft",c("NB","MCMC","RandomForest","Xgboost"),col=c(1,3,4,5),lty=1)


#ROC
perf.NB121 <- performance(pr.NB,"tpr","fpr")
perf.CART121 <- performance(pr.CART,"tpr","fpr")
perf.RF121 <- performance(pr.RF,"tpr","fpr")
perf.XG121 <- performance(pr.XG,"tpr","fpr")
par(bg='grey92')
plot(perf.NB121,col=1)
plot(perf.CART121,col=3,add=T)
plot(perf.RF121,col=4,add=T)
plot(perf.XG121,col=5,add=T)
abline(0,1,lty = 4)
abline(h = seq(0,1,.2),v = seq(0,1,.2),col = 'white')
legend("bottomright",c("NB","MCMC","RandomForest","Xgboost"),col=c(1,3,4,5),lty=1)


#提升图
perf.NB1212 <- performance(pr.NB,"lift","rpp")
perf.CART1212 <- performance(pr.CART,"lift","rpp")
perf.RF1212 <- performance(pr.RF,"lift","rpp")
perf.XG1212 <- performance(pr.XG,"lift","rpp")
par(bg='grey92')
plot(perf.NB1212,col=1)
plot(perf.CART1212,col=3,add=T)
plot(perf.RF1212,col=4,add=T)
plot(perf.XG1212,col=5,add=T)
abline(h = seq(0,4,.5),v = seq(0,1,.2),col = 'white')
legend("topright",c("NB","MCMC","RandomForest","Xgboost"),col=c(1,3,4,5),lty=1)


con.NB
con.NB$byClass
con.MC
con.MC$byClass
con.RF
con.RF$byClass
con.XB
con.XB$byClass




