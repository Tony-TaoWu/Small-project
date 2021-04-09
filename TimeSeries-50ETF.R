library(quantmod);library(tseries);library(fGarch);library(forecast);library(FinTS)
library(ggplot2);library(DescTools) # 载包
 
# 获取数据
getSymbols("",from="2009-03-01",to="2019-02-28")
# 提取收盘价
data<-Cl(``)
tail(data) # 查看最后六条数据

# 检查缺失值
sum(is.na(data))
data <- na.omit(data) # 去掉一个缺失值
sum(is.na(data))
ts.plot(data) # 画时序图  可以看到是比较明显的非平稳
summary(ts(data))  # 数据的信息

# 单位根检验  -----   序列平稳性
adf.test(data)            ## null hypothesis is that x has a unit root
pp.test(data)              ## null hypothesis is that x has a unit root
kpss.test(data)           ## Null hypothesis is stationarity
# 结果是非平稳
ts.plot(log(data))
log.return <- na.omit(as.vector(diff(log(data))))  # 取对数再查分 得到对数收益率
log.return <- ts(log.return)
log.return <- log.return*100  # 转化为百分比

par(mfrow = c(2,1))
ts.plot(data)
ts.plot(log.return) # 处理前后时序图对比时序图
par(mfrow = c(1,1))
length(log.return) # 序列长度T
mean(log.return) # 均值
sd(log.return) # 标准差

# 再进行一次平稳性检验
adf.test(log.return)           
pp.test(log.return)            
kpss.test(log.return)  
# 结果显示为平稳

# 查看周期图
spectrum(log.return)

# 序列的密度
density(log.return)
plot(density(log.return),main = '对数差分后密度')

# 对序列进行白噪声检验 取自由度为ln(T) 近似为8  其他阶数也相应做一下
Box.test(log.return,lag = 6,type = 'Ljung-Box')
Box.test(log.return,lag = 8,type = 'Ljung-Box')
Box.test(log.return,lag = 10,type = 'Ljung-Box')
# 结果显示为拒绝 也就说并非白噪声

lr <- log.return - mean(log.return) # 去掉均值
# 序列的acf pacf图
par(mfrow = c(2,1))
acfPlot(lr)
pacfPlot(lr)
par(mfrow = c(1,1))

# 检验是否为非线性 原假设为模型有AR部分 也就是说是线性
library(TSA)
Tsay.test(lr)
tlrt(lr)

# 下面使用了三种方法进行定阶
# 1. eacf 方法定阶 
library(TSA)
eacf <- eacf(lr) #   可以看到在(2,2)后构成由o构成的三角形 

# 2. auto.arima自动定阶 建立ARMA模型
mean.model <- auto.arima(lr,trace = FALSE,seasonal = F,method = 'CSS-ML') # 估计方法为 CSS估计AR，ML估计MA
mean.model1 <- auto.arima(lr,trace = FALSE,seasonal = F,method = 'CSS') # 估计方法为minimize conditional sum-of-squares
mean.model2 <- auto.arima(lr,trace = FALSE,seasonal = F,method = 'ML')  # 估计方法为maximum likelihood

mean.model   # 结果为ARMA(2,2)
mean.model1  # 结果为ARMA(2,5)
mean.model2  # 结果为ARMA(2,2)

# 3. 循环计算ARMA(1,1) 到  ARMA(4,4) 的AIC和BIC
aic <- c(); k <- 1; bic <- c()
for(i in 1:4){
  for(j in 1:4){
    fit <- arima(lr,order=c(i,0,j),include.mean=F,method = 'CSS-ML')# method = 'CSS' or 'ML
    aic[k] <- fit$aic
    bic[k] <- BIC(fit)
    names(aic)[k] <- paste('(',i,',',j,')',sep = '')
    names(bic)[k] <- paste('(',i,',',j,')',sep = '')
    k <- k+1
  }
}
aic
bic
which.min(aic)
which.min(bic)
# 结果均显示ARMA(2,2)的效果最好

# 于是我们用ARMA(2,2)拟合 并得到残差
lr.arma=arima(lr,order=c(2,0,2),include.mean=F,method = 'CSS-ML')
lr.arma

# 检验参数显著性
coef <- lr.arma$coef
se <- sqrt(diag(lr.arma$var.coef))
t <- coef/se
p.value <- c()
for(i in 1:4){
  if(t[i]>0){
    p.value[i] <- pt(t[i],2428,lower.tail = F)
  }
  else{
    p.value[i] <- pt(t[i],2428,lower.tail = T)
  }
}
names(p.value) <- c('ar1','ar2','ma1','ma2');p.value
result <- ifelse(p.value < .05,'显著','不显著');result  # 结果显示参数都显著

# 提取残差
res <- residuals(lr.arma)
summary(lr.arma)

# 平稳性判别：模拟ARMA(2,2)的时序图(只需要看自回归部分)
mean.res <- mean(res);sd.res <- sd(res)
x <- c();ep <- c()
x[1] <- lr[1];x[2] <- lr[2];ep[1] <- 0; ep[2] <- 0
for(i in 3:200){
  ep[i] <- rged(1,mean = mean.res,sd = sd.res,nu = 1.07)
  x[i] <- 0.1279*x[i-1] - 0.9616*x[i-2] + ep[i] #- 0.1474*ep[i-1] + 0.9313*ep[i-2]
}
ts.plot(x,main = '平稳性判别：ARMA(2,2)的时序图')

# 检验残差是否为白噪声
tsdiag(lr.arma) # 查看残差时序图 acf图 及Box.test()的p值
Box.test(res,lag = 4,type = 'Ljung-Box')
Box.test(res,lag = 6,type = 'Ljung-Box') # Box.test 检验统计量自由度应该取ln(T)-p-q 为了保险我们都做一遍
Box.test(res,lag = 8,type = 'Ljung-Box')
Box.test(res,lag = 10,type = 'Ljung-Box')
# 都不拒绝 可认为是白噪声

# 查看残差均值 方差
mean(res);sd(res)
# 查看残差分布是什么 是不是正态
par(mfrow = c(1,1))
qqnorm(res);qqline(res) # 不是正态
plot(density(res),main = '概率密度图')
ynorm=dnorm(density(res)$x, mean=mean(res),sd=sd(res))
matplot(density(res)$x,cbind(density(res)$y,ynorm),xlab = 'x',ylab = 'p',type="l",main = '残差的密度与相应的正态密度比较')
legend('topleft',legend = c('残差密度','正态密度'),col = 1:2,lty = 1:2,lwd = 1)
jarque.bera.test(res)   #  拒绝 ---> 非正态
# 后发现应该是Ged分布
plot(density(res),main = '概率密度图',xlab = 'x')
curve(dged(x,mean = 0,sd = sd(res),nu = 1.065),col = 2,add = T,lty = 2)
legend('topleft',legend = c('残差密度','Ged分布概率密度'),col = c(1,2),lty = 1:2,lwd = 1)
arrows(2,.1,3,.15,col = 2,lwd = 1)
text(4.5,.17,labels = 'Ged(mean,sd,nu=1.065)',col = 2)
# 残差分布与Ged分布的QQ-plot 可以看到QQ-plot效果不错
set.seed(4399)
ged <- rged(2432,mean = 0,sd = sd(res),nu = 1.065)
qqplot(res,ged,main = '残差分布与Ged分布qqplot',xlab = '残差',ylab = 'Ged分位数');qqline(res)
# Kolmogorov-Smirnov检验 检验两个分布是否同分布 
ks.test(res,ged) # 原假设为两分布同分布 备择假设为不同分布
# 结果为二者同分布


# 检验是否存在arch效应
res2 = res^2 # 残差平方
acfPlot(res2) # 平方残差的acf 显然不是白噪声
Box.test(res2,type = 'Ljung-Box',lag = 8)  # 拒绝 说明非白噪声
ArchTest(x =res)  #  拒绝 存在arch效应

# 查看残差和残差平方的acf和pacf图
par(mfrow=c(2,2))
acfPlot(res, lag=24)
pacfPlot(res, lag=24)
acfPlot(res2, lag=24)
pacfPlot(res2, lag=24)
par(mfrow=c(1,1))

# 对残差拟合Garch模型 由于实际中用到的也就是低阶模型 我们只尝试(1,1)到(2,2)
res.arch11 <- garchFit(~garch(1,1),data = res,cond.dist = 'ged',trace = F)
res.arch12 <- garchFit(~garch(1,2),data = res,cond.dist = 'ged',trace = F)
res.arch21 <- garchFit(~garch(2,1),data = res,cond.dist = 'ged',trace = F)
res.arch22 <- garchFit(~garch(2,2),data = res,cond.dist = 'ged',trace = F)
# 查看四个模型的结果
res.arch11
res.arch12
res.arch21
res.arch22
# 结果显示garch(1,1)的效果相对最好

# 但是观察GARCH(1,1)拟合的参数发现 alpha+beta近似于1，于是考虑使用IGARCH模型
library(rugarch)
spec1=ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0,0,0), include.mean=T),  
                 distribution.model="ged")
igarch=ugarchfit(data=res,spec=spec1)#,out.sample = 20)
igarch # 查看模型
plot(igarch)
0 # 输入0停止画图

# 查看IGARCH模型的残差
re11 <- residuals(igarch,standardize = T) # 标准化残差
Box.test(re11,type = 'Ljung-Box',lag = 6)  # 不拒绝 说明是白噪声
Box.test(re11^2,type = 'Ljung-Box',lag = 6)  # 残差平方白噪声检验 不拒绝  说明是白噪声
ArchTest(re11)  # 不拒绝 说明无Arch效应 模型拟合是合理的
# 查看残差及残差平方的acf和pacf
par(mfrow=c(2,2))
acfPlot(re11, lag=24)
pacfPlot(re11, lag=24)
acfPlot(re11^2, lag=24)
pacfPlot(re11^2, lag=24)
par(mfrow=c(1,1))

# 使用rugarch包的plot功能
plot(igarch) # 可画11种图
0  # 输入0可让画图功能停止


# 最终模型为ARMA(2,2) + IGARCH(1,1)
spec = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
                 mean.model = list(armaOrder = c(2,2), include.mean = F),  
                 distribution.model = "ged")
igarch.fin = ugarchfit(data=lr,spec=spec,out.sample = 0)
igarch.fin # 查看模型
# 查看参数
coef(igarch.fin)
# 提取残差
res.fin <- residuals(igarch.fin,standardize = T)
# 对残差进行Ged分布的拟合
plot(density(res.fin),main = '概率密度图',xlab = 'x')
curve(dged(x,mean = 0,sd = sd(res.fin),nu = 1.22),col = 2,add = T,lty = 2)
legend('topleft',legend = c('残差密度','Ged分布概率密度'),col = c(1,2),lty = 1:2,lwd = 1)
arrows(2,.1,3,.15,col = 2,lwd = 1)
text(4.5,.17,labels = 'Ged(mean,sd,nu=1.22)',col = 2)
# 残差分布与Ged分布的QQ-plot 可以看到QQ-plot效果不错
set.seed(4399)
ged1 <- rged(2432,mean = 0,sd = sd(res.fin),nu = 1.22)
qqplot(as.numeric(res.fin),ged1,main = '残差分布与Ged分布qqplot',xlab = '残差',ylab = 'Ged分位数')#;qqline(ged1)
abline(a = 0,b = 1)
# Kolmogorov-Smirnov检验 检验两个分布是否同分布 
ks.test(as.numeric(res.fin),ged1) # 原假设为两分布同分布 备择假设为不同分布
# 结果为不拒绝原假设 即二者同分布


#######################################################################   预测
# 样本内静态预测
igarch.fin.s <- ugarchfit(data=lr,spec=spec,out.sample = 20)
fore.s <- ugarchforecast(igarch.fin.s,n.ahead = 20,n.roll = 0)
fore.s
# 画图比较
#par(mfrow = c(2,1))
plot(fore.s)
1
0
#plot(fore.roll,which = 'all')  #  下面这个图是我仿照上面的画的
mean.pred <- ts(fore.s@forecast$seriesFor,start = 2413,end = 2432)
se.pred <- ts(fore.s@forecast$sigmaFor,start = 2413,end = 2432)
plot(window(lr,start=2400,end=2432),col="chocolate3",ylim = c(-5,5),lwd = 1,main = '样本内预测')
lines(mean.pred,col="red",lwd = 2)
lines(mean.pred+se.pred,col="blue",lwd = 2)
lines(mean.pred-se.pred,col="blue",lwd = 2)
abline(h = seq(from = -4,to = 4,by = 2),lty = 2,col = 'grey')
#par(mfrow = c(1,1))


# 样本内动态滚动预测
igarch.fin.roll <- ugarchfit(data=lr,spec=spec,out.sample = 20)
fore.roll <- ugarchforecast(igarch.fin.roll,n.ahead = 1,n.roll = 20)
fore.roll
# 画图比较
#par(mfrow = c(2,1))
plot(fore.roll)
2
0
#plot(fore.roll,which = 'all')  #  下面这个图是我仿照上面的画的
mean.pred.roll <- ts(t(fore.roll@forecast$seriesFor),start = 2413,end = 2432)
se.pred.roll <- ts(t(fore.roll@forecast$sigmaFor),start = 2413,end = 2432)
plot(window(lr,start=2400,end=2432),col="chocolate3",ylim = c(-6,12),lwd = 1,main = '样本内预测')
lines(mean.pred.roll,col="red",lwd = 1)
lines(mean.pred.roll+2*se.pred.roll,col="blue",lwd = 1)
lines(mean.pred.roll-2*se.pred.roll,col="blue",lwd = 1)
abline(h = seq(from = -5,to = 10,by = 5),lty = 2,col = 'grey')
#par(mfrow = c(1,1))

##################################  样本外预测
#  ##   提取样本外的真实数据
getSymbols("",from="2009-03-01",to="2019-04-28")
data1 <- Cl(``);data1 <- na.omit(data1)
lr.pr <- ts(na.omit(as.vector(diff(log(data1)))))*100
lr.pr <- lr.pr - mean(log.return) # 去掉均值
lr.pr1 <- lr.pr

# 样本外静态预测
fore <- ugarchforecast(igarch.fin,n.ahead = 20,n.roll = 0)
fore
# 画图比较
#par(mfrow = c(2,1))
plot(fore)
1
0
#plot(fore,which = 'all')   #  下面这个图是我仿照上面的画的
mean.pred.out <- ts(fore@forecast$seriesFor,start = 2433,end = 2452)
se.pred.out <- ts(fore@forecast$sigmaFor,start = 2433,end = 2452)
plot(window(lr,start=2400,end=2432),col="chocolate3",ylim = c(-5,8),xlim = c(2400,2452),lwd = 1,main = '样本外预测')
lines(mean.pred.out,col="red",lwd = 1)
lines(mean.pred.out+se.pred.out,col="blue",lwd = 1)
lines(mean.pred.out-se.pred.out,col="blue",lwd = 1)
abline(h = seq(from = -4,to = 8,by = 2),lty = 2,col = 'grey')
lines(window(lr.pr1,start = 2432,end = 2452),col = 'forestgreen',lwd = 1)
#par(mfrow = c(1,1))



#  样本外动态预测
mean.pred.out.roll <- ts(start = 2433,end = 2452)
se.pred.out.roll <- ts(start = 2433,end = 2452)
for(i in 2433:2452){
  igarch.fin.out = ugarchfit(data=window(lr.pr,start=1,end=i-1),spec=spec,out.sample = 0)
  fore.roll.out <- ugarchforecast(igarch.fin.out,n.ahead = 1,n.roll = 0)
  mean.pred.out.roll[i-2432] <- fore.roll.out@forecast$seriesFor
  se.pred.out.roll[i-2432] <- fore.roll.out@forecast$sigmaFor
  lr.pr[i] <- fore.roll.out@forecast$seriesFor + rged(1,mean(res.fin),sd(res.fin),nu = 1.22)
}
plot(window(lr,start=2400,end=2432),col="chocolate3",ylim = c(-5,8),xlim = c(2400,2452),lwd = 1,main = '样本外预测')
lines(mean.pred.out.roll,col="red",lwd = 1)
lines(mean.pred.out.roll+2*se.pred.out.roll,col="blue",lwd = 1)
lines(mean.pred.out.roll-2*se.pred.out.roll,col="blue",lwd = 1)
abline(h = seq(from = -4,to = 8,by = 2),lty = 2,col = 'grey')
lines(window(lr.pr1,start = 2432,end = 2452),col = 'forestgreen',lwd = 1)

#   这里我们发现不论是样本内还是样本外的静态动态预测 其结果都是差别极小的，也就是说预测值的差别都在10的-3次方左右
#  在图像上很难看到波动，而且在理论意义上对价格进行预测准确性也是极低的，但我们转换了思路，不具体确定其未来的价格，
#  根据预测的正负来判定其未来的涨跌，这对我们研究ETF也是很有意义的?
c <- mean.pred.out.roll;c
d <- window(lr.pr1,start = 2433,end = 2452);d
c.1 <- ifelse(c<0,0,1)
d.1 <- ifelse(d<0,0,1)
t <- table(c.1,d.1);t

#  原始数据预测
getSymbols("",from="2009-03-01",to="2019-04-28")
data2<-Cl(``)
data0<-ts(data2)
data0[2434]#原始数据的0时刻值
mean.pred.real<-ts(data0[2434]*exp(cumsum(mean.pred.out.roll)/100),start = 2434,end = 2454)#收益率变换到原始数据
set.seed(1)
q<-quantile(exp(rged(10000,mean(res.fin),sd(res.fin),1.22)),0.975)#收益率的标准差变换到原始数据的分位数
upper<-ts(mean.pred.real+q*se.pred.out.roll/100,start = 2434,end = 2454)#上界
lower<-ts(mean.pred.real-q*se.pred.out.roll/100,start = 2434,end = 2454)#下界
plot(window(data0,start=2352,end=2434),col="chocolate3",ylim = c(2,3),xlim = c(2352,2455),lwd = 2,main = '原始数据预测')
lines(mean.pred.real,col="red1",lwd = 2)
lines(upper,col="blue",lwd = 2)
lines(lower,col="blue",lwd = 2)
lines(window(data0,start = 2434,end = 2454),col = 'forestgreen',lwd = 2)#原始数据的真实值
abline(h = seq(2,3,by = .2),col = 'grey',lty = 2)
legend('topleft',legend = c('收盘价','预测曲线','区间','真实数据')
       ,col = c('chocolate3','red','blue','forestgreen'),lty = 1,lwd = 2)


a <- mean.pred.real
b <- ts(window(data0,start = 2434,end = 2454))
b <- as.numeric(b)
b <- ts(b,start = 2434,end = 2454)
a.1 <- diff(a)
b.1 <- diff(b)
a.2 <- ifelse(a.1<0,0,1)
b.2 <- ifelse(b.1<0,0,1)
table(a.2,b.2)


#应用  期权定价
getSymbols("",from="2009-03-01",to="2019-04-28")
data<-Cl(``)
data0<-ts(data)
data0[2434]#原始数据的0时刻值
#欧式看涨期权定价公式
black.schores<-function(S0,K,r,sigma,T){
  d1<-(log(S0/K)+(r+sigma^2/2)*T)/(sigma*sqrt(T))
  d2<-(log(S0/K)+(r-sigma^2/2)*T)/(sigma*sqrt(T))
  c<-S0*pnorm(d1)-K*exp(-r*T)*pnorm(d2)
  return(c)
}
#用garch波动率模型的扩展B-S定价公式(用ged分布)
black.schores.garch<-function(S0,K,r,sigma.T,T){
  sigma<-sqrt(sum(sigma.T^2))
  d1<-(log(S0/K)+r*T+0.5*sigma^2)/sigma
  d2<-(log(S0/K)+r*T-0.5*sigma^2)/sigma
  c<-S0*pged(d1,0,1,1.125)-K*exp(-r*T)*pged(d2,0,1,1.125)
  return(c)
}

K<-c(2.10,2.20,2.30,2.4,2.5,2.6,2.7,2.8,2.9,3) #z执行价格
C<-c(0.6537,0.5543,0.4550,0.3561,0.2630,0.1842,0.1210,0.0750,0.0451,0.0277)  #2/28日收盘3月认购期权费
pred.C<-black.schores(data0[2434],K,0.03,sd(log.return)/100,20/252)#调用B-S函数
pred.Garch<-black.schores.garch(data0[2434],K,0.03,se.pred.out.roll/100,20/252)#调用garch模型B-S函数
plot(K,C,type="o",ylim=c(-0.1,0.7),main ="上证50ETF3月认购期权价格",xlab = "行权价",ylab = "期权费",lwd = 2)
lines(K,pred.Garch,type="o",col="red",lwd=2)
lines(K,pred.C,type="o",col="blue",lwd = 2)

sd(C-pred.C)
sd(C-pred.Garch)
legend("topright",legend = c("市场价","garch模型B-S","传统B-S"),col = c("black","red","blue"),lwd = c(2,2,2))

tail(``)
#研究的序列到2019/4/26
volume<-Vo(``)
volume[which(volume>4000000000)]
plot(ts(data),ylim=c(0,4),col="chocolate3",lwd=2,ylab="50ETF")#50ETF
lines(ts(volume/2000000000),col="blue",lwd=2)#图像变形处理
legend("topleft",legend=c("50ETF价格","50ETF成交量"),col= c("chocolate3","blue"),lwd=c(2,2))
return=na.omit(diff(data))
diff.volume=na.omit(diff(volume))
adf.test(return)#上证50一阶差分序列平稳
pp.test(return)
kpss.test(return)

adf.test(diff.volume)#成交量一阶差分平稳
pp.test(diff.volume)
kpss.test(diff.volume)

#两个序列都是一阶单整的
m1=lm(ts(data)~ts(volume))#回归模型建立
m1
summary(m1)
m1$residuals
res.multi=ts(m1$residuals)
ts.plot(res.multi,main="residuals")
adf.test(res.multi)#残差非平稳，但p值比较小
pp.test(res.multi)#残差平稳 
kpss.test(res.multi)#残差非平稳
#协整关系不显著
library(lmtest)
dwtest(m1)#DW值很低，可能存在伪回归关系

#格兰杰因果检验
#格兰杰检验的对象需是两个平稳序列
grangertest(return~diff.volume,order = 2)#成交量不是收益率变化的原因
grangertest(diff.volume~return,order = 2)#收益率是成交量变化的原因

#针对非平稳的原始序列做格兰杰因果检验是不正确的
#grangertest(ts(data)~ts(volume),order = 3)#成交量不是价格变化的原因
#grangertest(ts(volume)~ts(data),order = 3)#价格是成交量变化的原因
