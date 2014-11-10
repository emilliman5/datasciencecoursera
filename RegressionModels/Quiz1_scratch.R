library(UsingR)
library(ggplot2)
library(manipulate)
data(galton)

ggplot(galton, aes(x=parent, y=child))+geom_point()

lm(I(x = child-mean(child)) ~ I(parent-mean(parent))-1, data=galton)

x<-c(0.18,-1.54,.42,.95); w<-c(2,1,3,1)

mean(x)
mean(x*w)

x2<- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y2 <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

qplot(x2,y2)

lm(y2~x2)

data(mtcars)

lm(mpg~wt, data=mtcars)

head(mtcars)

x3 <- c(8.58, 10.46, 9.01, 9.64, 8.86)

(x3-mean(x3))/sd(x3)

x9 <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x9)
