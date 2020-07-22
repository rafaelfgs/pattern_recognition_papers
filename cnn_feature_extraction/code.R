require(OpenImageR)
require(e1071)

setwd( "C:/Users/rafae/Dropbox/UFMG/2019-1/EEE928/artigo/final" )

rm(list=ls())
source( "func.R" )

assign( "net",  list(), envir = .GlobalEnv )
assign( "classifier",  list(), envir = .GlobalEnv )

# base <- loadMnist( classvalues = 0:9 )

base <- loadCifar10( classvalues = 0:9 )

# createNet( input = dim(base$train$x)[-4] , name = "minst_0-9" )

# load("net_mnist_0-9_9861.RData")

load("net_cifar10_0-9_4550.RData")

# trainNet( x = base$train$x , y = base$train$y , numdata = 1000 , alpha = 0.2 , batchsize = 1000 , numepochs = 1 )

# trainSVM( x = base$train$x , y = base$train$y , numdata = 60000 , kernel = "radial" , cost = 10 )

# predictAcc( x = base$test$x , y = base$test$y , numdata = 10000 )

# saveAll( x = base$test$x , y = base$test$y , numdata = 10000 )