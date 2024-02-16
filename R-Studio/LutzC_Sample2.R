#Chad Lutz
rm(list=ls())
library(rio)
library(car)
beer=import("6304 Module 7 Assignment Data.xlsx")
names(beer)
colnames(beer)=c("index","date","production")
str(beer)
beer$year=as.numeric(format(beer$date,'%Y'))
beer$month=as.numeric(format(beer$date,'%m'))
str(beer)
attach(beer)
plot(index,production,pch=19,type="l",
     main= "Australia Beer Sales -- Raw Data")
beer.out=lm(production~index,data=beer)
summary(beer.out)
cor(beer$production,beer.out$fitted.values)

plot(index,production,pch=19,type="o",
     main=paste("Australia Beer Sales -- Simple Regression
     r=",round(cor(beer$production,beer.out$fitted.values),4)))
points(index,beer.out$fitted.values,type="l",col="red",lwd=3)

durbinWatsonTest(beer.out)

indices=data.frame(month=1:12,average=0,index=0)
for(i in 1:12) {
  count=0
  for(j in 1:nrow(beer)) {
    if(i==beer$month[j]) {
      indices$average[i]=indices$average[i]+beer$production[j]
      count=count+1
    }
  }
  indices$average[i]=indices$average[i]/count
  indices$index[i]=indices$average[i]/mean(beer$production)}

for(i in 1:12) {
  for(j in 1:nrow(beer)) {
    if(i==beer$month[j]) {
      beer$deseason[j]=beer$production[j]/indices$index[i]
    }
  }
}

desbeer.out=lm(deseason~index,data=beer)
summary(desbeer.out)

beer$index2=beer$index^2
sqbeer.out=lm(deseason~index+index2,data=beer)
summary(sqbeer.out)

for(j in 1:nrow(beer)) {
  xx=beer$month[j]
  beer$reseason.y.hat[j]=desbeer.out$fitted.values[j]*indices$index[xx]
  beer$reseason.error[j]=beer$production[j]-beer$reseason.y.hat[j]
}


for(j in 1:nrow(beer)) {
  xx=beer$month[j]
  beer$sq.reseason.y.hat[j]=sqbeer.out$fitted.values[j]*indices$index[xx]
  beer$sq.reseason.error[j]=beer$production[j]-beer$sq.reseason.y.hat[j]
}

plot(beer$index,beer$production,type="o",lwd=3,
     main=paste("Australia Beer Sales -- Season Adjusted
     r=",round(cor(beer$production,beer$reseason.y.hat),4)))
points(beer$index,beer$reseason.y.hat,type="l",col="red",lwd=2)

plot(beer$index,beer$production,type="o",lwd=3,
     main=paste("Australia Beer Sales -- Season Adjusted^2
     r=",round(cor(beer$production,beer$sq.reseason.y.hat),4)))
points(beer$index,beer$sq.reseason.y.hat,type="l",col="red",lwd=2)
