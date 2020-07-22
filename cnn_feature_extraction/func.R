
# FUNCOES INICIAIS

loadMnist <- function ( classvalues ) {
  
  if ( file.exists("mnist.RData") ) {
    load("mnist.RData")
    cat("File \"mnist.RData\" loaded!\n")
  } else {
    stop("File \"mnist.RData\" not found!\n\n")
  }
  
  classnames <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  
  numtrain <- rep(0,length(classvalues))
  for ( n in 1:length(classvalues) ) { numtrain[n] <- sum(mnist$train$y==classvalues[n]) }
  
  numtest <- rep(0,length(classvalues))
  for ( n in 1:length(classvalues) ) { numtest[n] <- sum(mnist$test$y==classvalues[n]) }
  
  if ( sum(numtrain) == 0 || sum(numtest) == 0 ) { stop("Class value not found on MNIST!\n\n") }
  
  indtrain <- rep(0,length(mnist$train$y))
  for ( n in 1:length(classvalues) ) { indtrain[mnist$train$y==classvalues[n]] <- 1 }
  indtrain <- sample(which(indtrain==1))
  
  indtest <- rep(0,length(mnist$test$y))
  for ( n in 1:length(classvalues) ) { indtest[mnist$test$y==classvalues[n]] <- 1 }
  indtest <- sample(which(indtest==1))
  
  base <- list( train = list() , test = list() )
  
  base$train$c <- classnames
  
  base$test$c <- classnames
  
  base$train$x <- aperm( mnist$train$x[indtrain,,] / 255 , c(2,3,1) )
  base$train$x <- array( base$train$x , c( dim(mnist$train$x)[2:3] , 1 , length(indtrain) ) )
  
  base$test$x <- aperm( mnist$test$x[indtest,,] / 255 , c(2,3,1) )
  base$test$x <- array( base$test$x , c( dim(mnist$test$x)[2:3] , 1 , length(indtest) ) )
  
  base$train$y <- rep("",length(mnist$train$y))
  for ( n in 1:length(classvalues) ) { base$train$y[mnist$train$y[indtrain]==classvalues[n]] <- classnames[n] }
  base$train$y <- base$train$y[base$train$y!=""]
  
  base$test$y <- rep("",length(mnist$test$y))
  for ( n in 1:length(classvalues) ) { base$test$y[mnist$test$y[indtest]==classvalues[n]] <- classnames[n] }
  base$test$y <- base$test$y[base$test$y!=""]
  
  base$train$yd <- array( 0 , c( length(classvalues) , length(indtrain) ) )
  for ( n in 1:length(classvalues) ) { base$train$yd[n,mnist$train$y[indtrain]==classvalues[n]] <- 1 }
  
  base$test$yd <- array( 0 , c( length(classvalues) , length(indtest) ) )
  for ( n in 1:length(classvalues) ) { base$test$yd[n,mnist$test$y[indtest]==classvalues[n]] <- 1 }
  
  cat("Database \"mnist\" created!\n\n")
  
  return(base)
  
}

loadCifar10 <- function( classvalues ) {
  
  if ( file.exists("cifar10.RData") ) {
    load("cifar10.RData")
    cat("File \"cifar10.RData\" loaded!\n")
  } else {
    stop("File \"cifar10.RData\" not found!\n\n")
  }
  
  classnames <- c("airplane", "automobile", "bird", "cat", "deer", "dog", "frog", "horse", "ship", "truck")
  
  numtrain <- rep(0,length(classvalues))
  for ( n in 1:length(classvalues) ) { numtrain[n] <- sum(cifar10$train$y==classvalues[n]) }
  
  numtest <- rep(0,length(classvalues))
  for ( n in 1:length(classvalues) ) { numtest[n] <- sum(cifar10$test$y==classvalues[n]) }
  
  if ( sum(numtrain) == 0 || sum(numtest) == 0 ) { stop("Class value not found on CIFAR10!\n\n") }
  
  indtrain <- rep(0,length(cifar10$train$y))
  for ( n in 1:length(classvalues) ) { indtrain[cifar10$train$y==classvalues[n]] <- 1 }
  indtrain <- sample(which(indtrain==1))
  
  indtest <- rep(0,length(cifar10$test$y))
  for ( n in 1:length(classvalues) ) { indtest[cifar10$test$y==classvalues[n]] <- 1 }
  indtest <- sample(which(indtest==1))
  
  base <- list( train = list() , test = list() )
  
  base$train$c <- classnames
  
  base$test$c <- classnames
  
  base$train$x <- aperm( cifar10$train$x[indtrain,,,] / 255 , c(2,3,4,1) )
  
  base$test$x <- aperm( cifar10$test$x[indtest,,,] / 255 , c(2,3,4,1) )
  
  base$train$y <- rep("",length(cifar10$train$y))
  for ( n in 1:length(classvalues) ) { base$train$y[cifar10$train$y[indtrain]==classvalues[n]] <- classnames[n] }
  base$train$y <- base$train$y[base$train$y!=""]
  
  base$test$y <- rep("",length(cifar10$test$y))
  for ( n in 1:length(classvalues) ) { base$test$y[cifar10$test$y[indtest]==classvalues[n]] <- classnames[n] }
  base$test$y <- base$test$y[base$test$y!=""]
  
  base$train$yd <- array( 0 , c( length(classvalues) , length(indtrain) ) )
  for ( n in 1:length(classvalues) ) { base$train$yd[n,cifar10$train$y[indtrain]==classvalues[n]] <- 1 }
  
  base$test$yd <- array( 0 , c( length(classvalues) , length(indtest) ) )
  for ( n in 1:length(classvalues) ) { base$test$yd[n,cifar10$test$y[indtest]==classvalues[n]] <- 1 }
  
  cat("Database \"cifar10\" created!\n\n")
  
  return(base)
  
}

createNet <- function ( inputsize , name ) {
  
  net$name <<- name
  
  net$layers[[1]] <<- list( type = "i" )
  net$layers[[2]] <<- list( type = "c" , outputmaps = 6 , kernelsize = 5 )
  net$layers[[3]] <<- list( type = "s" , scale = 2 )
  net$layers[[4]] <<- list( type = "c" , outputmaps = 12 , kernelsize = 5 )
  net$layers[[5]] <<- list( type = "s" , scale = 2 )
  net$layers[[6]] <<- list( type = "o" )
  
  for ( n in 1:length(net$layers) ) {
    
    if ( net$layers[[n]]$type == "i" ) {
      
      net$layers[[n]]$mapsize <<- inputsize
      
    } else {
      
      if ( net$layers[[n]]$type == "s" ) {
        
        input <- net$layers[[n-1]]$mapsize
        k <- net$layers[[n]]$scale
        
        net$layers[[n]]$mapsize <<- c( floor( input[1:2] / k ) , input[3] )
        
      } else {
        
        if ( net$layers[[n]]$type == "c" ) {
          
          input <- net$layers[[n-1]]$mapsize
          outputmaps <- net$layers[[n]]$outputmaps
          k <- net$layers[[n]]$kernelsize
          
          net$layers[[n]]$b <<- array( 0 , outputmaps )
          net$layers[[n]]$w <<- array( 0 , c( input[3] , outputmaps , k , k ) )
          
          for ( j in 1:outputmaps ) {
            for ( i in 1:input[3] ) {
              mat <- ( runif(k**2) - 0.5 ) * 2 * sqrt( 6 / ( (input[3]+outputmaps) * k**2 ) )
              net$layers[[n]]$w[i,j,,] <<- array( mat , c(k,k) )
            }
          }
          
          net$layers[[n]]$mapsize <<- c( input[1:2]-k+1 , outputmaps )
          
        } else {
          
          if ( net$layers[[n]]$type == "o" ) {
            
            input <- net$layers[[n-1]]$mapsize
            net$layers[[n]]$mapsize <<- c( prod(input) , 1 )
            
          }
          
        }
        
      }
      
    }
    
  }
  
  cat("Random Net created with following Mapsizes!\n")
  
  for ( n in 1:length(net$layers) ) {
    
    if ( net$layers[[n]]$type == "i" ) {
      m <- net$layers[[n]]$mapsize
      cat( "Input: " , m[1] , "x" , m[2] , "x" , m[3] , "\n" )
      
    } else {
      
      if ( net$layers[[n]]$type == "s" ) {
        m <- net$layers[[n]]$mapsize
        cat( "Pool:  " , m[1] , "x" , m[2] , "x" , m[3] , "\n" )
        
      } else {
        
        if ( net$layers[[n]]$type == "c" ) {
          m <- net$layers[[n]]$mapsize
          cat( "Conv:  " , m[1] , "x" , m[2] , "x" , m[3] , "\n" )
          
        } else {
          
          if ( net$layers[[n]]$type == "o" ) {
            m <- net$layers[[n]]$mapsize
            cat( "Output:" , m[1] , "x" , m[2] , "\n\n" )
            
          }
          
        }
        
      }
      
    }
    
  }
  
}

trainNet <- function ( x , y , numdata , alpha , batchsize , numepochs ) {
  
  # x <- base$train$x; y <- base$train$y; numdata <- 100; alpha <- 0.5; batchsize <- 100; numepochs <- 1 ; i <- 1L ; j <- 1L
  
  t <- Sys.time()
  
  yclass <- unique(y)
  numbatches <- floor( numdata / batchsize )
  
  for ( i in 1:numepochs ) {  
    
    ind <- sample( dim(x)[4] , numdata )
    
    for ( j in 1:numbatches ) {
      
      batch_x <- x[ , , , ind[ ((j-1)*batchsize+1) : (j*batchsize) ] ]
      batch_x <- array( batch_x , c( dim(x)[1:3] , batchsize ) )
      batch_y <- y[ ind[ ((j-1)*batchsize+1) : (j*batchsize) ] ]
      
      cnnFF( batch_x )
      cnnFV( batch_y , yclass )
      cnnError( batch_y , yclass )
      cnnBP()
      cnnGrad( alpha )
      
      cat( sprintf( "Epoch: %i / %i   -   Data: %i / %i   -   Separable: %s   -   Loss: %6.2f / %6.2f\r" , i , numepochs ,
                    j*batchsize , numdata , net$fv$separated[length(net$fv$separated)] , net$L[length(net$L)] , net$M[length(net$M)] ) )
      
    }
    
    cat( sprintf( "Epoch %i   -   Data: %i   -   Separable: ( Ok=%i , Sp=%i , No=%i )   -   Loss: %6.2f / %6.2f\n" ,
                  i , numdata , sum(net$fv$separated[((i-1)*numbatches+1):(i*numbatches)]=="Ok") , 
                  sum(net$fv$separated[((i-1)*numbatches+1):(i*numbatches)]=="Sp") , 
                  sum(net$fv$separated[((i-1)*numbatches+1):(i*numbatches)]=="No") , 
                  net$L[length(net$L)] , mean(net$L[((i-1)*numbatches+1):(i*numbatches)]) ) )
    
  }
  
  cat("\n")
  
  net$timer <<- Sys.time() - t
  
}

trainSVM <- function ( x , y , numdata , kernel , degree , cost ) {
  
  # x = base$train$x; y = base$train$y; numdata = 1000; kernel = "radial"; degree = 3; cost = 10
  
  t <- Sys.time()
  
  ind <- sample( dim(x)[4] , numdata )
  x <- array( x[ , , , ind ] , c( dim(x)[-4] , numdata ) )
  y <- y[ ind ]
  
  cnnFF(x)
  fv <- net$layers[[length(net$layers)]]$a
  
  xsvm <- as.data.frame(t(fv))
  ysvm <- as.factor(y)
  
  classifier <<- svm( xsvm , ysvm , kernel = kernel , degree = degree , cost = cost )
  
  classifier$timer <<- Sys.time() - t
  
}

predictAcc <- function ( x , y , numdata ) {
  
  # x = base$test$x; y = base$test$y; numdata = 1000
  
  cat( "Predicting Accuracy\r" )
  
  ind <- sample( dim(x)[4] , numdata )
  x <- array( x[ , , , ind ] , c( dim(x)[-4] , numdata ) )
  y <- y[ ind ]
  
  cnnFF(x)
  fv <- net$layers[[length(net$layers)]]$a
  
  xsvm <- as.data.frame(t(fv))
  ysvm <- as.factor(y)
  
  pred <- predict( classifier , xsvm )
  
  acc <- mean( pred == ysvm )
  
  precision <- paste0( floor(log10(numdata))+1 , "." , max(c(floor(log10(numdata))-2,0)) )
  
  cat( sprintf( paste0("Test Accuracy: %",precision,"f%%\n\n") , 100*acc ) )
  
}

saveAll <- function ( x , y , numdata ) {
  
  # x = base$test$x; y = base$test$y; numdata = 100;
  
  cat( "Predicting Accuracy\r" )
  
  ind <- sample( dim(x)[4] , numdata )
  x <- array( x[ , , , ind ] , c( dim(x)[-4] , numdata ) )
  y <- y[ ind ]
  
  cnnFF(x)
  fv <- net$layers[[length(net$layers)]]$a
  
  xsvm <- as.data.frame(t(fv))
  
  pred <- predict( classifier , xsvm )
  
  acc <- mean( pred == y )
  
  cnnFF( array( x[,,,1] , c(dim(x)[-4],1) ) )
    
  precision <- 10**floor(log10(numdata))
  
  filename <- paste0( "net_" , net$name , "_" ,  round(precision*mean(acc)), ".RData" )
  
  save( net , classifier , file = filename )
  
  cat( paste0( "File \"" , filename , "\" saved!\n\n" ) )
  
}


# FUNCOES DE TREINO

cnnFF <- function ( x ) {
  
  for ( n in 1:length(net$layers) ) {
    
    if ( net$layers[[n]]$type == "i" ) {
      
      for ( i in 1:net$layers[[1]]$mapsize[3] ) {
        net$layers[[1]]$a[[i]] <<- array( x[,,i,] , dim(x)[c(1,2,4)] )
      }
      
    }
    
    if ( net$layers[[n]]$type == "c" ) {
      
      k <- net$layers[[n]]$kernelsize
      
      for ( j in 1:net$layers[[n]]$mapsize[3] ) {
        
        dimA <- dim( net$layers[[n-1]]$a[[1]] )
        z <- array( 0 , c(dimA - c(k,k,0) + c(1,1,0)) )
        
        for ( i in 1:net$layers[[n-1]]$mapsize[3] ) {
          mat <- net$layers[[n-1]]$a[[i]]
          mask <- net$layers[[n]]$w[i,j,,]
          z <- z + convValid( mat , mask )
        }
        
        net$layers[[n]]$a[[j]] <<- sigm( z + net$layers[[n]]$b[j] )
        
      }
      
    }
    
    if ( net$layers[[n]]$type == "s" ) {
      
      for ( i in 1:net$layers[[n]]$mapsize[3] ) {
        mat <- net$layers[[n-1]]$a[[i]]
        scal <- net$layers[[n]]$scale
        net$layers[[n]]$a[[i]] <<- avgPool( mat , c(scal,scal) )
      }
      
    }
    
    if ( net$layers[[n]]$type == "o" ) {
      
      mat <- array( 0 , c( net$layers[[n-1]]$mapsize , dim(x)[4] ) )
      for ( i in 1:net$layers[[n-1]]$mapsize[3] ) {
        mat[,,i,] <- net$layers[[n-1]]$a[[i]]
      }
      net$layers[[n]]$a <<- array( as.vector(mat) , c( net$layers[[n]]$mapsize[1] , dim(x)[4] ) )
      
    }
    
  }
  
}

cnnFV <- function ( y , yclass ) {
  
  fvHat <- net$layers[[length(net$layers)]]$a;
  
  validcols <- rep( 0 , length(unique(y)) )
  for ( n in 1:length(unique(y)) ) {
    validcols[n] <- which(unique(y)[n]==yclass)
  }
  
  fvMean <- array( 0 , c( nrow(fvHat) , length(yclass) ) )
  colnames(fvMean) = yclass
  for ( n in validcols ) {
    fvMean[,n] <- rowMeans(array(fvHat[,y==yclass[n]],c(nrow(fvHat),max(c(1,sum(y==yclass[n]))))))
  }
  
  fvCurr <- array( 0 , c( nrow(fvHat) , length(yclass) ) )
  colnames(fvCurr) = yclass
  
  if ( "final" %in% names(net$fv) ) {
    for ( n in validcols ) {
      if ( !( yclass[n] %in% colnames(net$fv$final) ) ) {
        stop( sprintf( "Class value \"%s\" not found on \"net$fv$final!\"!\n\n" , yclass[n] ) )
      }
      fvCurr[,n] <- net$fv$final[ , colnames(net$fv$final) == yclass[n] ]
    }
  }
  
  for ( n in validcols ) {
    fvCurr[ fvMean[,n] >= apply(fvMean[,validcols],1,mean) , n ] <- 1
    fvCurr[ fvMean[,n] <  apply(fvMean[,validcols],1,mean) , n ] <- 0
  }
  
  if ( "memory" %in% names(net$fv) ) {
    fvMemory <- net$fv$memory
  } else {
    fvMemory <- array( 0 , dim(fvCurr) )
  }
  colnames(fvMemory) = yclass
  
  memMax <- apply(fvMemory[,validcols],1,mean) + apply(fvMemory[,validcols],1,sd)/2
  memMin <- apply(fvMemory[,validcols],1,mean) - apply(fvMemory[,validcols],1,sd)/2
  fvFinal <- fvCurr
  fvFinal[ fvMemory > array(memMax,dim(fvFinal)) ] <- 1
  fvFinal[ fvMemory < array(memMin,dim(fvFinal)) ] <- 0

  fvMemory[ fvCurr == 1 ] <- fvMemory[ fvCurr == 1 ] + 1
  fvMemory[ fvCurr == 0 ] <- fvMemory[ fvCurr == 0 ] - 1
  
  edec <- array( 0 , c( length(yclass) , length(yclass) ) )
  colnames(edec) = yclass
  rownames(edec) = yclass
  for ( n in validcols ) {
    currcol <- array( fvFinal[,n] , dim(fvFinal) )
    edec[n,validcols] <- 1 - apply( ( currcol[,validcols] - fvFinal[,validcols] ) ** 2 , 2 , mean )
  }
  
  indsepar <- 0.7 # 1 - 0.06 * sqrt( nrow(fvHat) / length(yclass) )
  if ( indsepar > 0.8 ) { warning("Many Classes for few Features! Consider a Network Increasing!") }
  if ( indsepar < 0.5 ) { warning("Many Features for few Classes! Consider a Network Decreasing!") }
  
  ebin <- edec > indsepar
  for ( n in 1:nrow(ebin) ) {
    for ( i in 1:nrow(ebin) ) {
      for ( j in which(ebin[i,]|ebin[,i]) ) {
        ebin[i,] <- ebin[i,]|ebin[,i]|ebin[j,]|ebin[,j]
      }
    }
  }
  ebin <- ebin[ c( -which(apply(ebin,1,sum)<=1) , -which(duplicated(ebin)) ) , ]
  ebin <- array( ebin , c( length(ebin)/length(validcols) , length(yclass) ) )
  colnames(ebin) = yclass
  
  fvSeparated <- "Ok"
  ntries <- 0
  
  while ( length(ebin) > 0 ) {
    
    fvSeparated <- "Sp"
    
    tryCatch ( {
      
      for ( i in 1:nrow(ebin) ) {
        
        colbin <- ( rowSums(fvFinal[,which(ebin[i,])]) == 0 ) | ( rowSums(fvFinal[,which(ebin[i,])]) == sum(ebin[i,]) )
        colmax <- max( edec[which(ebin[i,])[1] , (1:ncol(ebin))[-which(ebin[i,])[1]]] ) - indsepar
        colchange <- ceiling( nrow(fvFinal) * colmax / sum(ebin[i,]) )
        
        for ( j in 1:sum(ebin[i,]) ) {
          
          colother <- apply( array( fvMean[colbin,which(ebin[i,])[-j]] , c(sum(colbin),sum(ebin[i,])-1) ) , 1 , mean )
          colvar <- order( colother - fvMean[colbin,which(ebin[i,])[j]] )
          colvar <- which(colbin)[ colvar[ sample( ceiling(length(colvar)/8) , min( c( colchange , ceiling(length(colvar)/8) ) ) ) ] ]
          
          fvMean[ colvar , which(ebin[i,])[j] ] <- fvMean[ colvar , which(ebin[i,])[j] ] + 0.001
          fvFinal[ colvar , which(ebin[i,])[j] ] <- 1
          fvFinal[ colvar , which(ebin[i,])[-j] ] <- 0
          
        }
        
      }
      
    }, error = function(e) ntries <- 4 )
    
    for ( n in validcols ) {
      currcol <- array( fvFinal[,n] , dim(fvFinal) )
      edec[n,validcols] <- 1 - apply( ( currcol[,validcols] - fvFinal[,validcols] ) ** 2 , 2 , mean )
    }
    
    ebin <- edec > indsepar
    for ( n in 1:nrow(ebin) ) {
      for ( i in 1:nrow(ebin) ) {
        for ( j in which(ebin[i,]|ebin[,i]) ) {
          ebin[i,] <- ebin[i,]|ebin[,i]|ebin[j,]|ebin[,j]
        }
      }
    }
    ebin <- ebin[ c( -which(apply(ebin,1,sum)<=1) , -which(duplicated(ebin)) ) , ]
    ebin <- array( ebin , c( length(ebin)/length(validcols) , length(yclass) ) )
    colnames(ebin) = yclass
    
    ntries <- ntries + 1
    if ( ntries > 4 ) {
      fvSeparated <- "No"
      break
    }
    
  }
  
  fvGoal <- array( 0 , c( nrow(fvHat) , length(y) ) )
  for ( n in 1:length(yclass) ) {
    fvGoal[ , y==yclass[n] ] <- fvFinal[ , colnames(fvFinal) == yclass[n] ]
  }
  
  net$fv$class <<- y
  net$fv$hat <<- fvHat
  net$fv$mean <<- fvMean
  net$fv$curr <<- fvCurr
  net$fv$error <<- edec
  net$fv$final <<- fvFinal
  net$fv$memory <<- fvMemory
  net$fv$goal <<- fvGoal
  net$fv$separated <<- c( net$fv$separated , fvSeparated )
  
}

cnnError <- function ( y , yclass ) {
  
  fvHat <- net$fv$hat;
  fvGoal <- net$fv$goal;
  
  net$E <<- fvHat - fvGoal
  
  if ( "L" %in% names(net) ) { 
    net$L <<- c( net$L , mean( colSums(net$E**2) ) )
  } else {
    net$L <<- mean( colSums(net$E**2) )
  }
  
  if ( "M" %in% names(net) ) { 
    net$M <<- c( net$M , 0.9*net$M[length(net$M)] + 0.1*net$L[length(net$L)] )
  } else {
    net$M <<- net$L
  }
  
}

cnnBP <- function() {
  
  for ( n in length(net$layers):1 ) {
    
    numel <- length( net$layers[[n]]$a )
    
    if ( n == length(net$layers) ) {
      net$layers[[n]]$da  <<- net$E * ( net$layers[[n]]$a * (1-net$layers[[n]]$a) )
    }
    
    if ( n == length(net$layers)-1 ) {
      
      sa <- dim( net$layers[[n]]$a[[1]] )
      fvnum <- sa[1] * sa[2]
      
      for ( i in 1:numel ) {
        p1 <- net$layers[[6]]$da[ ((i-1)*fvnum+1) : (i*fvnum) , ]
        net$layers[[n]]$da[[i]] <<- array( p1 , sa[1:3] )
      }
      
    }
    
    if ( n < length(net$layers)-1 && net$layers[[n]]$type == "c" ) {
      
      for ( i in 1:numel ) {
        p <- cbind( net$layers[[n+1]]$scale , net$layers[[n+1]]$scale , 1 )
        a <- net$layers[[n]]$a[[i]]
        xp <- expand( net$layers[[n+1]]$da[[i]] , p )
        net$layers[[n]]$da[[i]] <<- a * (1-a) * ( xp / net$layers[[n+1]]$scale**2 )
      }
      
    }
    
    if ( n < length(net$layers)-1 && net$layers[[n]]$type == "s" ) {
      
      for( i in 1:numel ) {
        z <- array( 0 , dim(net$layers[[n]]$a[[1]]) )
        for( j in 1:length(net$layers[[n+1]]$a) ) {
          k <- rot180(net$layers[[n+1]]$w[i,j,,])
          z <- z + convFull( net$layers[[n+1]]$da[[j]] , k )
        }
        net$layers[[n]]$da[[i]] <<- z
      }
      
    }
    
  }
  
  for ( n in 1:length(net$layers) ) {
    
    if ( net$layers[[n]]$type == "c" ) {
      
      la <- length(net$layers[[n]]$a)
      laAnt <- length(net$layers[[n-1]]$a)
      k <- net$layers[[n]]$kernelsize
      net$layers[[n]]$dw <<- array( 0 , c(laAnt,la,k,k) )
      
      for ( j in 1:la ) {
        for ( i in 1:laAnt ) {
          imgFlip <- flipall(net$layers[[n-1]]$a[[i]])
          con <- convn3D( imgFlip , net$layers[[n]]$da[[j]] )
          net$layers[[n]]$dw[i,j,,] <<- con / dim(net$layers[[n]]$da[[j]])[3]
        }
        net$layers[[n]]$db[[j]] <<- sum(net$layers[[n]]$da[[j]]) / dim(net$layers[[n]]$da[[j]])[3]
      }
      
    }
    
  }
  
}

cnnGrad <- function ( alpha ) {
  
  for ( n in 2:length(net$layers)-1 ) {
    if ( net$layers[[n]]$type == "c" ) {
      
      for ( j in 1:length(net$layers[[n]]$a) ) {
        for ( i in 1:length(net$layers[[n-1]]$a) ) {
          net$layers[[n]]$w[i,j,,] <<- net$layers[[n]]$w[i,j,,] - alpha * net$layers[[n]]$dw[i,j,,]
        }
        net$layers[[n]]$b[[j]] <<- net$layers[[n]]$b[[j]] - alpha * net$layers[[n]]$db[[j]]
      }
      
    }
  }
  
}



# FUNCOES BASICAS

convValid <- function ( inputmat , mask ) {
  
  di <- dim(inputmat)
  dm <- dim(mask)
  dm2 <- floor(dm/2)
  
  outputmat <- array( 0 , di - 2*c(dm2,0) )
  
  for ( n in 1:di[3] ) {
    ini <- dm2 + 1
    fim <- di[1:2] - dm2
    fullmat <- convolution( inputmat[,,n] , mask , "same" )
    outputmat[,,n] <- fullmat[ ini[1]:fim[1] , ini[2]:fim[2] ]
  }
  
  return (outputmat)
  
}

convFull <- function ( inputmat , mask ) {
  
  di <- dim(inputmat)
  dm <- dim(mask)
  dm2 <- floor(dm/2)
  
  outputmat <- array( 0, di + c(dm[1:2],0) - c(1,1,0) )
  
  for ( n in 1:di[3] ) {
    outputmat[,,n] <- convolution( inputmat[,,n] , mask , "full" )
  }
  
  return (outputmat) 
  
}

convn3D <- function ( inputmat , mask ) {
  
  di <- dim(inputmat)
  dm <- dim(mask)
  dm2 <- floor(dm/2)
  
  outputmat <- array( 0 , di[1:2] - dm[1:2]  + 1 )
  
  for ( n in 1:di[3] ) {
    ini <- dm2
    fim <- di-dm2
    fullmat <- convolution( inputmat[,,n] , mask[,,di[3]-n+1] , "same" )
    outputmat <- outputmat + fullmat[ ini[1]:fim[1] , ini[2]:fim[2] ]
  }
  
  return (outputmat)
  
}

avgPool <- function ( inputmat , s ) {
  
  outputmat <- array( 0 , dim(inputmat) / c(s,1) )
  
  for ( n in 1:dim(outputmat)[3] ) {
    for ( j in 1:dim(outputmat)[2] ) {
      for ( i in 1:dim(outputmat)[1] ) {
        outputmat[i,j,n] <- mean( inputmat[ ((i-1)*s[1]+1):(i*s[1]), ((j-1)*s[2]+1):(j*s[2]), n ] )
      }
    }
  }
  
  return (outputmat) 
  
}

expand <- function ( inputmat , s ) {
  
  outputmat <- array( 0, s*dim(inputmat) )
  
  for ( n in 1:dim(inputmat)[3]) {
    for ( j in 1:dim(inputmat)[2] ) {
      for ( i in 1:dim(inputmat)[1] ) {
        lin <- ( (i-1)*s[1]+1 ):( i*s[1] )
        col <- ( (j-1)*s[2]+1 ):( j*s[2] )
        outputmat[lin,col,n] <- inputmat[i,j,n]
      }
    }
  }
  
  return (outputmat)
  
}

rot180 <- function ( inputmat ) {
  
  outputmat <- t( apply(inputmat , 2 , rev) )
  outputmat <- t( apply(outputmat , 2 , rev) )
  
  return (outputmat)
  
}

flipall <- function ( inputmat ) {
  
  d <- dim(inputmat)
  
  outputmat <- array( inputmat[ d[1]:1, d[2]:1, d[3]:1 ] , d )
  
  return (outputmat)
  
}

sigm <- function ( inputvar ) {
  
  outputvar <- 1 / ( 1 + exp(-inputvar) )
  
  return (outputvar)
  
}
