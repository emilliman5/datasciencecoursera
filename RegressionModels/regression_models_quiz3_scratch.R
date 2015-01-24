data(mtcars)

fit<-lm(mpg~factor(cyl)+wt, data=mtcars)
summary(fit)

plot(mtcars$wt, mtcars$mpg, col=mtcars$cyl, pch=19)
legend("topright", legend=levels(factor(mtcars$cyl)), col=c(4,6,8), pch=19)
abline(fit)

fit2<-lm(mpg~factor(cyl), data=mtcars)
summary(fit2)

fitInt<-lm(mpg~wt+factor(cyl)+factor(cyl)*wt, data=mtcars)
summary(fitInt)

anova(fit, fitInt)
x<-c(0.586,0.166,-0.042,-0.614,11.72)
y<-c(0.549,-0.026,-0.127,-0.751,1.344)

xyfit<-lm(y~x)
influence.measures(xyfit)
hatvalues(xyfit)
dfbetas(xyfit)
plot(x,y)
