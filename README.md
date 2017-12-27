# -时序为主
# 毕业论文代码----sarima模型建模过程
by<- read.table("clipboard",T)
TO<- ts(by$total,start=c(2008,1),end=c(2016,12),frequency=12)#建立序列
plot(TO,type="o",col="red")

#自相关图

TO.zxg<- as.vector(by$total)

(TO.acf<- acf(TO.zxg,lag.max=25))
###################################差分过程
##一次差分
TO.diff<- diff(TO,diff=1)
acf(TO.diff)
plot(TO.diff,type="o",col="red")
TO.diff.acf<- as.vector(TO.diff)
TO.diff.acf<- acf(TO.diff.acf,lag.max=25)

TO.diff12<- diff(TO.diff,diff=1,lag=12)
TO.diff2 <- as.vector(TO.diff12)
(TO.acf<- acf(TO.diff2,lag.max=25))
(TO.pacf<- pacf(TO.diff2,lag.max=25))
plot(TO.diff12,type="o",col="red")
library(tseries)
adf.test(TO.diff2)
##白噪声检验
for(i in 1:30){
print(Box.test(TO.diff12, type="Ljung-Box",lag=i))
}
##自相关图定Q
##########模型定阶
(TO.acf<- acf(TO.diff2,lag.max=25))
(TO.pacf<- pacf(TO.diff2,lag.max=25))
TO.jm<- TO.diff12
###aic,bic

P=NULL		
Q=NULL	
AIC=NULL
BIC=NULL
for(p in 0:4){
	for(q in 0:4){
		fit=Arima(TO.jm,order=c(p,0,q),seasonal=list(order=c(0,0,1),period=12));
		aic=AIC(fit);
            bic=BIC(fit);
		P=c(P,p);
		Q=c(Q,q);
		AIC=c(AIC,aic);
            BIC=c(BIC,bic);
				  }
			  }

L=cbind(P,Q,AIC,BIC)
ORDER=order(AIC,BIC)
k=ORDER[1]
head(L)
L[k,]
L
#######################jianmo
library(forecast)
library(timeDate)
jianmo<-Arima(TO,order=c(1,1,2),seasonal=list(order=c(0,1,1),period=12))
jianmo
#######白噪声检验
for(i in 1:30){
print(Box.test(jianmo$residual, type="Ljung-Box",lag=i))
}

jianmo.cc<- jianmo$residual
library(car)
qqnorm(jianmo.cc)
(acf(jianmo.cc))
#####预测
fore<- forecast(jianmo,h=6)
plot(fore)
###另一种预测

TO.fore=forecast(jianmo,h=6)  #预测未来12期#
L1=TO.fore$fitted-1.96*sqrt(jianmo$sigma2)   #构造拟合期数据置信区间下限#
U1=TO.fore$fitted+1.96*sqrt(jianmo$sigma2)  #构造拟合期数据置信区间上限#
L2=ts(TO.fore$lower[,2],start=c(2008,1),frequency=12)  #构造预测期数据置信区间下限#
U2=ts(TO.fore$upper[,2],start=c(2008,1),frequency=12)  #构造预测期数据置信区间上限#
a1=min(TO,L1,L2)   #确定plot图中y轴最小值#
a2=max(TO,U1,U2)  #确定plot图中y轴最大值#
plot(TO,pch=8,xlim=c(2008,2017),ylim=c(a1,a2))  #绘制时序散点图#
lines(TO.fore$fitted,col="red",lwd=2)    #在散点图基础上绘制线#
lines(TO.fore$mean,col="pink",lwd=2)  #在原图上绘制预测期的均值线#
lines(L1,col="blue",lty=2)   #绘制拟合期数据置信区间下限#
lines(U1,col="blue",lty=2)   #绘制拟合期数据置信区间上限#
lines(L2,col="blue",lty=2)   #绘制预测期数据置信区间下限#
lines(U2,col="blue",lty=2)  #绘制预测期数据置信区间上限#
abline(v=c(2016,12))      #在1996年10月处绘制参考线#
