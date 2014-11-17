###Q1

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)


###Q2
n=length(x)
beta1<-cor(y,x)*sd(y)/sd(x)
beta0<-mean(y)-beta1*mean(x)
e<-y-beta0-beta1*x
sigma<-sqrt(sum(e^2)/(n-2))


###Q3

data(mtcars)
head(mtcars)

x3<-mtcars$wt
y3<-mtcars$mpg

q3fit<-lm(x3~y3)
summary(q3fit)
sumCoef<-summary(q3fit)$coefficients
sumCoef[1,1]+c(-1,1)*qt(.975,df=q3fit$df)*sumCoef[1,2]
sumCoef[2,1]+c(-1,1)*qt(.975,df=q3fit$df)*sumCoef[2,2]

n=length(x3)
beta1<-cor(y3,x3)*sd(y3)/sd(x3)
beta0<-mean(y3)-beta1*mean(x3)
ssx<-sum((3-mean(x3))^2)
yVals<-beta0+beta1*3
se2<-sigma*sqrt(1+1/n+(3-mean(x3))^2/ssx)

yVals-c(-1,1)*qt(.975,df=q3fit$df)*se2
