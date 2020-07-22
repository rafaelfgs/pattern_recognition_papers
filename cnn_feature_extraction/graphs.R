# x<-array(base$train$x[,,,which(base$train$y=="0")[1]],c(dim(base$train$x)[-4],1))
# cnnFF(x)
# 
# for (i in 1:length(net$layers)){
#   for (j in 1:length(net$layers[[i]]$a)){
#     if (i<length(net$layers)) {a<-net$layers[[i]]$a[[j]][,,1]} else{a<-net$layers[[i]]$a}
#     png(paste0("layer",i,"_",j,".png"))
#     plot(c(0,dim(a)[1]),c(0,dim(a)[2]),'n',xlab=NA,ylab=NA,xaxt="n",yaxt="n")
#     rasterImage(a,0,0,dim(a)[1],dim(a)[2])
#     dev.off()
#   }
# }

# png("L_MNIST.png")
# plot(net$L,type='l',xlab=NA,ylab=NA,xlim=c(0,7200),ylim=c(20,55),col='blue')
# dev.off()

# png("L_CIFAR10.png")
# plot(net$L,type='l',xlab=NA,ylab=NA,xlim=c(0,4000),ylim=c(50,100),col='blue')
# dev.off()

# plot(c(0,10),c(0,10),'n',xlab=NA,ylab=NA,xaxt="n",yaxt="n")
# rasterImage(edec,0,0,10,10)

acc<-array(0,c(10,10))
for (i in 1:length(base$test$c)){
  ind <- sample(which(base$test$y==base$test$c[i]))
  cnnFF(array(base$test$x[,,,ind],c(dim(base$test$x)[-4],length(ind))))
  xsvm <- as.data.frame(t(net$layers[[length(net$layers)]]$a))
  pred <- predict( classifier , xsvm )
  for (j in 1:length(base$test$c)){
    acc[i,j] = sum(pred==base$test$c[j])*100/length(ind)
  }
  cat(i)
}
colnames(acc)=base$test$c
rownames(acc)=base$test$c