#Chad Lutz
rm(list=ls())
library(rio)
craigs.list=import("6304 Module 5 Assignment Data.xlsx")
colnames(craigs.list)=tolower(make.names(colnames(craigs.list)))

cars.subset=subset(craigs.list, make == "cadillac" & 
                     year >= 2006 & year <= 2011 & 
                     condition %in% c("excellent","good") &
                     cylinders %in% c("6","8") &
                     paint.color != "black" &
                     paint.color !="custom")

set.seed(13)
my.cars=cars.subset[sample(1:nrow(cars.subset),90),]

str(my.cars)

#my.cars$cylinders=as.factor(my.cars$cylinders)
#my.cars$year=as.factor(my.cars$year)
#my.cars$condition=as.factor(my.cars$condition)
#my.cars$paint.color=as.factor(my.cars$paint.color)
#OR
my.factors=c("cylinders","year","condition",
             "paint.color")
my.cars[my.factors]=lapply(my.cars[my.factors],factor)
cars.out=lm(price~year+condition+cylinders+odometer+
              paint.color,data=my.cars)
summary(cars.out)
confint(cars.out)

par(mfrow=c(2,2))
plot(my.cars$price,cars.out$fitted.values,
     pch=19,main="Actuals v. Fitteds, Cadillac Price")
abline(0,1,col="red",lwd=3)
qqnorm(cars.out$residuals,pch=19,
       main="Normality Plot, Cadillac Price")
qqline(cars.out$residuals,lwd=3,col="red")
hist(cars.out$residuals,col="red",
     main="Residuals, Cadillac Price",
     probability=TRUE)
curve(dnorm(x,mean(cars.out$residuals),
            sd(cars.out$residuals)),
      from=min(cars.out$residuals),
      to=max(cars.out$residuals),
      lwd=3,col="blue",add=TRUE)
plot(cars.out$fitted.values,rstandard(cars.out),
     pch=19,ylim=c(-4,4),main="Equality of Variances, Cadillac Price")
abline(0,0,lwd=3,col="red")
par(mfrow=c(1,1))

maryann=data.frame(matrix(nrow=1,ncol=5))
colnames(maryann)=c("condition","odometer","cylinders",
                    "year","paint.color")
maryann[1,1]="excellent"
maryann[1,2]=215354
maryann[1,3]=8
maryann[1,4]=2011
maryann[1,5]="red"

maryann[my.factors]=lapply(maryann[my.factors],factor)
predict(cars.out,maryann,interval = "predict")
