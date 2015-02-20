training<-training[,c(-1,-3,-4,-5)]
levels(training$user_name)

adelmo<-training[training$user_name=="adelmo",]
PCA1<-prcomp(adelmo[,c(-1,-40,-41,-42,-53)], scale=TRUE, center=TRUE)

scores<-as.data.frame(PCA1$x)

ggplot(data = scores, aes(x = PC1, y = PC2,colour=adelmo$classe, label = rownames(scores))) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(alpha = 0.8, size = 4) +
  ggtitle("PCA plot for Adelmo of TimePoints - Sensor Data")

corcir<-circle(c(0,0), npoints=100)

correlations<-as.data.frame(cor(adelmo[,c(-1,-40,-41,-42,-53)],PCA1$x))
arrows = data.frame(x1 = rep(0,dim(correlations)[1]), y1 = rep(0,dim(correlations)[1]), x2 = correlations$PC1, y2 = correlations$PC2)

ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") + 
  geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0, 
  colour = "gray65") + xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 aixs", 
  y = "pc2 axis") + ggtitle("Circle of correlations")


foreach(i=2:(length(colnames(training))-1)) %do%  hist(as.numeric(training[,i]), 
                                                     main=c("Index:",i,colnames(training)[i]))
hist(log10(training[,47]), main=colnames(training)[47])
