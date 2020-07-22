rm(list=ls())
library(evclust)

setwd("D:/Dropbox/UFMG/2019-1/EEE928/final/codigo")

# ENTRADAS DO ALGORITMO ----

# Base de dados
base <- read.csv('SyntheticDatasetRodrigo.csv',FALSE)
# base <- read.table('sea.data',sep=',')[,c(1,2,4)]
# base <- read.table('sea.data',sep=',')[,c(1,3,4)]
# base <- read.table('sea.data',sep=',')[,c(2,3,4)]
# base <- force(fourclass)

# Constantes globais
assign("forg_factor", 2/3, envir = .GlobalEnv)
assign("batch_size", 1700, envir = .GlobalEnv)
assign("num_parts", 16, envir = .GlobalEnv)
assign("apply_filt", FALSE, envir = .GlobalEnv)



# FUNCOES NECESSARIAS ----

# Transforma os dados em um grid para cada classe
createGrid <- function(x1, x2, y, cl, mat, g1, g2) {
  
  # x1<-x1tr; x2<-x2tr; y<-ytr; cl<-cl; mat<-gmat; g1<-grid1; g2<-grid2
  
  # Determina o tamanho do grid
  grid_size <- c(length(g1)-1,length(g2)-1)
  
  # Transforma a matriz de grid antiga em uma imagem
  num_old <- array(0,c(grid_size,length(cl)))
  for (j in 1:grid_size[2]) {
    for (i in 1:grid_size[1]) {
      num_old[i,j,] <- mat[(j-1)*grid_size[1]+i,4+(1:length(cl))]
    }
  }
  
  # Transforma os novos pontos em uma matriz de imagem
  num_new <- array(0,c(grid_size,length(cl)))
  for (k in 1:length(cl)) {
    cut1 <- cut(x1[y==cl[k]],grid1)
    cut2 <- cut(x2[y==cl[k]],grid2)
    num_new[,,k] <- table(cut1,cut2) / length(y)
  }
  
  # Determina o filtro a ser utilizado
  if (apply_filt) {
    num_mask <- matrix(c(1,2,1,2,4,2,1,2,1)/16,3,3)
  } else{
    num_mask <- matrix(c(0,0,0,0,1,0,0,0,0),3,3)
  }
  
  # Determina as imagens filtrada e resultante como nula
  num_filt <- array(0,c(grid_size,length(cl)))
  num_res <- array(0,c(grid_size,length(cl)))
  
  # Percorre o numero de classes e o tamanho da imagem
  for (k in 1:length(cl)) {
    for (j in 1:grid_size[1]) {
      for (i in 1:grid_size[2]) {
        
        # Determina os indices para a filtragem
        ind1_filt <- max(c(1,i-1)):min(c(grid_size[1],i+1))
        ind2_filt <- max(c(1,j-1)):min(c(grid_size[2],j+1))
        ind1_mask <- ceiling(2/i):(nrow(num_mask)-floor(i/grid_size[1]))
        ind2_mask <- ceiling(2/j):(ncol(num_mask)-floor(j/grid_size[2]))
        
        # Realiza a filtragem do ponto [i,j,k]
        num_filt[i,j,k] <- sum(num_new[ind1_filt,ind2_filt,k]*num_mask[ind1_mask,ind2_mask])
        
      }
    }
  }
  
  
  # Determina a imagem final como combinacao da antiga com a nova
  for (k in 1:length(cl)) {
    if (all(mat[,4+(1:length(cl))]==0)) {
      num_res[,,k] <- num_filt[,,k]
    } else {
      num_res[,,k] <- forg_factor*num_old[,,k] + (1-forg_factor)*num_filt[,,k]
    }
  }
  
  # Transforma imagem final em uma nova matriz de grid
  mat <- matrix(0,prod(grid_size),4+length(cl))
  for (j in 1:grid_size[2]) {
    for (i in 1:grid_size[1]) {
      mat[(j-1)*grid_size[1]+i,] <- c(grid1[i],grid1[i+1],grid2[j],grid2[j+1],num_res[i,j,])
    }
  }
  
  # Retorna a nova matriz de grid
  return(mat)
  
}

# Combina duas quadtrees
combineTree <- function(mat_old, mat_new, cl) {
  
  # mat_old<-mat; mat_new<-mat_new; cl<-cl
  
  # Inicia a matriz resultante
  mat_res <- NULL
  
  # Inicia as indices das linhas de cada matriz
  i <- 1L
  j <- 1L
  
  while (i <= nrow(mat_old) && j <= nrow(mat_new)) {
    
    # Determina se os intervalos sao semelhantes
    samerow <- mat_old[i,2] == mat_new[j,2] && mat_old[i,4] == mat_new[j,4]
    
    # Combina as matrizes antiga e nova para a dada linha
    if (samerow) {
      
      if (all(mat_old[,4+(1:length(cl))]==0)) {
        num <- mat_new[j,4+(1:length(cl))]
      } else {
        num <- forg_factor*mat_old[i,4+(1:length(cl))] +
               (1-forg_factor)*mat_new[j,4+(1:length(cl))]
      }
      
      mat_res <- cbind(mat_res,c(mat_old[i,1:4],num))
      
      i <- i + 1L
      j <- j + 1L
      
    } else {
      
      # Incrementa o indice a linha da matriz antiga e combina seus resultados
      if (mat_old[i,2] < mat_new[j,2]) {
        
        while (!samerow) {
          
          rate <- 1/((mat_new[j,2]-mat_new[j,1]) / (mat_old[i,2] - mat_old[i,1]))**2
          
          if (all(mat_old[,4+(1:length(cl))]==0)) {
            num <- mat_new[j,4+(1:length(cl))]
          } else {
            num <- forg_factor*mat_old[i,4+(1:length(cl))] +
                   (1-forg_factor)*rate*mat_new[j,4+(1:length(cl))]
          }
          
          mat_res <- cbind(mat_res,c(mat_old[i,1:4],num))
          
          samerow <- mat_old[i,2] == mat_new[j,2] && mat_old[i,4] == mat_new[j,4]
          
          i <- i + 1L
          
        }
        
        j <- j + 1L
        
      # Incrementa o indice a linha da matriz nova e combina seus resultados
      } else {
        
        while (!samerow) {
          
          rate <- ((mat_new[j,2]-mat_new[j,1]) / (mat_old[i,2] - mat_old[i,1]))**2
          
          if (all(mat_old[,4+(1:length(cl))]==0)) {
            num <- mat_new[j,4+(1:length(cl))]
          } else {
            num <- forg_factor*rate*mat_old[i,4+(1:length(cl))] +
                   (1-forg_factor)*mat_new[j,4+(1:length(cl))]
          }
          
          mat_res <- cbind(mat_res,c(mat_new[j,1:4],num))
          
          samerow <- mat_old[i,2] == mat_new[j,2] && mat_old[i,4] == mat_new[j,4]
          
          j <- j + 1L
          
        }
        
        i <- i + 1L
        
      }
      
    }
    
  }
  
  # Reordena a nova matriz resultante
  mat_res <- t(mat_res)
  
  # Retorna a matriz resultante
  return(mat_res)
  
}

# Transforma os dados em uma matriz quadtree para cada classe
createTree <- function(x1, x2, y, cl, mat, t1, t2) {
  
  # x1<-x1tr; x2<-x2tr; y<-ytr; cl<-cl; mat<-tmatrix; t1<-tree1; t2<-tree2
  
  # Determina o tamanho do quadtree
  tree_size <- floor(log2(min(c(length(t1),length(t2)))))+1
  
  # Inicia os indices dos tamanhos como [1,0,0,0]
  ind <- c(1,rep(0,tree_size-2))
  
  # Inicia a nova matriz de quadtree como nula
  mat_new <- NULL
  
  # Enquanto existir indices do quadtree
  while (any(ind!=0)) {
    
    # Determina os intervalos correspondentes a variavel 1
    lim1 <- c(sum((ind==rep(3,tree_size-1) | ind==rep(4,tree_size-1))*2^((tree_size-2):0))+1,0)
    if (all(ind!=0)) {
      lim1[2] <- lim1[1] + 1
    } else {
      lim1[2] <- lim1[1] + 2**(tree_size-(which(ind==0)[1]))
    }
    
    # Determina os intervalos correspondentes a variavel 2
    lim2 <- c(sum((ind==rep(2,tree_size-1) | ind==rep(4,tree_size-1))*2**((tree_size-2):0))+1,0)
    if (all(ind!=0)) {
      lim2[2] <- lim2[1] + 1
    } else {
      lim2[2] <- lim2[1] + 2**(tree_size-(which(ind==0)[1]))
    }
    
    # Determina a probabilidade realacionados aos intervalos
    num <- rep(0,length(cl))
    for (k in 1:length(cl)) {
      num[k] <- 1/length(y) * sum(x1[y==cl[k]]>t1[lim1[1]] & x1[y==cl[k]]<=t1[lim1[2]] & 
                                  x2[y==cl[k]]>t2[lim2[1]] & x2[y==cl[k]]<=t2[lim2[2]])
    }
    
    # Adicional o intervalo e a probabilidade a nova matriz de quadtree
    if (ind[length(ind)]!=0 || sum(num!=0)==1 || all(num==0) || length(cl)==1) {
      mat_new <- cbind(mat_new,c(t1[lim1],t2[lim2],num))
    }
    
    # Encerra o indice atual do quadtree
    if ((ind[length(ind)]==0 && (sum(num!=0)==1 || all(num==0))) || length(cl)==1) {
      ind[which(ind==0)[1]:length(ind)] <- 4
    }
    
    # Realiza o incremento do indice do quadtree
    if (ind[length(ind)]==0) {
      ind[which(ind==0)[1]] <- 1
    } else {
      if (ind[length(ind)]!=4){
        ind[length(ind)] <- ind[length(ind)] + 1
      } else {
        if (any(ind!=4)) {
          for (i in length(ind):(length(ind)-which(rev(ind)<4)[1]+2)){
            ind[i] <- 0
          }
          ind[i-1] <- ind[i-1] + 1
        } else {
          ind <- rep(0,length(ind))
        }
      }
    }
    
  }
  
  # Reordena a nova matriz de quadtree
  mat_new <- t(mat_new)
  
  # Combina as matrizes antiga e nova
  mat <- combineTree(mat, mat_new, cl)
  
  # Retorna a matriz resultante
  return(mat)
  
}

# Cria o modelo de acordo com as probabilidades
createModel <- function(mat) {
  
  # mat<-gmatrix; mat<-tmatrix
  
  # Inicia o modelo como nulo
  model <- matrix(0,nrow(mat),5)
  
  # Percorre os intervalos da matriz
  for (k in 1:nrow(mat)) {
    
    # Determina a saida para o intervalo dado
    out <- which.max(mat[k,5:ncol(mat)])
    out[sum(mat[k,5:ncol(mat)])==0] <- 0
    
    # Cria o modelo combinando o intervalo e a saida
    model[k,] <- c(mat[k,1:4],out)
    
  }
  
  # Retorna o model
  return(model)
  
}

# Realiza a predicao dos dados de acordo com o modelo
predModel <- function(x1, x2, cl, model) {
  
  # x1<-x1te; x2<-x2te; cl<-cl; model<-gmodel; model<-tmodel
  
  # Inicia os indices da predicao como nulos
  predind <- rep(0,length(x1))
  
  # Percorre cada amostra de teste
  for (k in 1:length(x1)) {
    
    # Determina o intervao da amostra k
    ind <- which(x1[k]>model[,1] & x1[k]<=model[,2] & 
                 x2[k]>model[,3] & x2[k]<=model[,4])[1]
    
    # Encontra seu modelo (aleatorio se nao encontrar)
    if (!is.na(ind) && model[ind,5] != 0) {
      predind[k] <- model[ind,5]
    } else {
      predind[k] <- sample(length(cl),1)
    }
    
  }
  
  # Encontra a classe das predicoes
  pred <- cl[predind]
  
  # Retorna a predicao
  return(pred)
  
}

# Exibe a base de dados
plotBase <- function(base, xrange, yrange) {
  
  # base<-base; xrange<-lim_var1; yrange<-lim_var2
  
  # Encontra as classes da base de dados
  cl <- unique(base[,3])
  
  # Altera as propriedades do grafico
  par(mar=c(2,2,1,1),mfrow=c(1,1))
  
  # Exibe a base de dados por classe
  for (k in 1:length(cl)) {
    plot(base[base[,3]==cl[k],1], base[base[,3]==cl[k],2], pch=20,
         xlim=xrange, ylim=yrange, col=rainbow(length(cl))[k])
    if (k < length(cl)) {par(new=TRUE)}
    
  }
  
}

# Plota o clasificador encontrado
plotClassifier <- function(gmodel, tmodel, cl, type) {
  
  # gmodel<-gmodel; tmodel<-tmodel; cl<-cl; type<-FALSE
  
  # Intervalos encontrados no grid
  lim1var <- c(min(gmodel[,1]),max(gmodel[,2]))
  lim2var <- c(min(gmodel[,3]),max(gmodel[,4]))
  step1var <- min(abs(diff(gmodel[,1:2])[diff(gmodel[,1:2])!=0]))
  step2var <- min(abs(diff(gmodel[,3:4])[diff(gmodel[,3:4])!=0]))
  grid1var <- seq(lim1var[1],lim1var[2],step1var)
  grid2var <- seq(lim2var[1],lim2var[2],step2var)
  
  # Inicia a matriz dos valores do modelo
  gnum <- array(0,c(length(grid1var)-1,length(grid2var)-1))
  
  # Determina o valores da matriz de acordo com as linhas do modelo
  for (m in 1:nrow(gmodel)) {
    ind1 <- (which(abs(gmodel[m,1]-grid1var)<2^-32)[1]):
            (which(abs(gmodel[m,2]-grid1var)<2^-32)[1]-1)
    ind2 <- (which(abs(gmodel[m,3]-grid2var)<2^-32)[1]):
            (which(abs(gmodel[m,4]-grid2var)<2^-32)[1]-1)
    for (j in ind2) {
      for (i in ind1) {
        gnum[i,j] <- gmodel[m,5]
      }
    }
  }
  
  # Intervalos encontrados no quadtree
  lim1var <- c(min(tmodel[,1]),max(tmodel[,2]))
  lim2var <- c(min(tmodel[,3]),max(tmodel[,4]))
  step1var <- min(abs(diff(tmodel[,1:2])[diff(tmodel[,1:2])!=0]))
  step2var <- min(abs(diff(tmodel[,3:4])[diff(tmodel[,3:4])!=0]))
  tree1var <- seq(lim1var[1],lim1var[2],step1var)
  tree2var <- seq(lim2var[1],lim2var[2],step2var)
  
  # Inicia a matriz dos valores do modelo
  tnum <- array(0,c(length(tree1var)-1,length(tree2var)-1))
  
  # Determina o valores da matriz de acordo com as linhas do modelo
  for (m in 1:nrow(tmodel)) {
    ind1 <- (which(abs(tmodel[m,1]-tree1var)<2^-32)[1]):
      (which(abs(tmodel[m,2]-tree1var)<2^-32)[1]-1)
    ind2 <- (which(abs(tmodel[m,3]-tree2var)<2^-32)[1]):
      (which(abs(tmodel[m,4]-tree2var)<2^-32)[1]-1)
    for (j in ind2) {
      for (i in ind1) {
        tnum[i,j] <- tmodel[m,5]
      }
    }
  }
  
  # Determina as cores da imagem do modelo do grid
  if (any(gnum==0)) {
    gcolor <- c('#FFFFFF',hsv(0:(length(cl)-1)/length(cl),1,1))
  } else{
    gcolor <- hsv(0:(length(cl)-1)/length(cl),1,1)
  }
  
  # Determina as cores da imagem do modelo do quadtree
  if (any(tnum==0)) {
    tcolor <- c('#FFFFFF',hsv(0:(length(cl)-1)/length(cl),1,1))
  } else{
    tcolor <- hsv(0:(length(cl)-1)/length(cl),1,1)
  }
  
  # Altera as propriedades do grafico
  par(mar=c(2,2,1,1),mfrow=c(1,2))
  
  # Exibe o modelo do grid com os intervalos
  image(grid1var,grid2var,gnum,col=gcolor)
  for (m in 1:nrow(gmodel)) {
    rect(gmodel[m,1],gmodel[m,3],gmodel[m,2],gmodel[m,4],lwd=2)
  }
  
  # Exibe o modelo do quadtree com os intervalos
  image(tree1var,tree2var,tnum,col=tcolor)
  for (m in 1:nrow(tmodel)) {
    rect(tmodel[m,1],tmodel[m,3],tmodel[m,2],tmodel[m,4],lwd=2)
  }
  
  # Espera um tempo para exibir o plot caso necessario
  if (type) {
    Sys.sleep(2)
    dev.off()
  }
  
}



# LIMITES DA BASE DE DADOS ----

# Tamanho do grid e do quadtree
grid_size <- c(num_parts,num_parts)
tree_size <- floor(log2(num_parts))+1

# Valores indicados para os limites das variaveis
bestlim <- rep(0,22*25)
for (k in 1:25) {
  bestlim[((k-1)*11+1):(k*11)] <- c(-9:-2,-1.5,-1.2,-1)*10**(13-k)
  bestlim[11*25+(((k-1)*11+1):(k*11))] <- c(1,1.2,1.5,2:9)*10**(k-13)
}

# Determina os limites minimos e maximos das variaveis
lim_var1 <- c(bestlim[which(min(base[,1])-0.04*(max(base[,1])-min(base[,1]))<bestlim)[1]-1],
              bestlim[which(max(base[,1])+0.04*(max(base[,1])-min(base[,1]))<=bestlim)[1]])
lim_var2 <- c(bestlim[which(min(base[,2])-0.04*(max(base[,2])-min(base[,2]))<bestlim)[1]-1],
              bestlim[which(max(base[,2])+0.04*(max(base[,2])-min(base[,2]))<=bestlim)[1]])

# Determina a sequencia dos intervalos dos grids de cada variavel
grid1 <- seq(lim_var1[1],lim_var1[2], length.out = grid_size[1]+1)
grid2 <- seq(lim_var2[1],lim_var2[2], length.out = grid_size[1]+1)

# Determina a sequencia dos intervalos dos quadtrees de cada variavel
tree1 <- seq(lim_var1[1],lim_var1[2], length.out = 2^(tree_size-1)+1)
tree2 <- seq(lim_var2[1],lim_var2[2], length.out = 2^(tree_size-1)+1)

# Exibe a base de dados
plotBase(base, lim_var1, lim_var2)



# CLASSIFICADOR ONLINE ----

# Inicia as variaveis de acuracia
gacc <- rep(0,floor(nrow(base)/batch_size))
tacc <- rep(0,floor(nrow(base)/batch_size))

# Inicia as variaveis do tamanho de memoria
gmem <- rep(0,floor(nrow(base)/batch_size))
tmem <- rep(0,floor(nrow(base)/batch_size))

# Percorre a quantidade de batches
for (n in 1:floor(nrow(base)/batch_size)) {
  
  # Determina os indices dos dados atuais (treino e teste)
  ind <- sample(((n-1)*batch_size+1):(n*batch_size))
  indtr <- ind[1:round(0.7*batch_size)]
  indte <- ind[(round(0.7*batch_size)+1):batch_size]
  
  # Variaveis 1 e 2 e rotulos dos dados de treino
  x1tr <- base[indtr,1]
  x2tr <- base[indtr,2]
  ytr <- base[indtr,3]
  
  # Variaveis 1 e 2 e rotulos dos dados de teste
  x1te <- base[indte,1]
  x2te <- base[indte,2]
  yte <- base[indte,3]
  
  # Determina as classes atuais combinando com as antigas
  if (!exists('cl')) {
    cl <- unique(ytr)
  } else {
    cl_new <- unique(ytr)[!(unique(ytr)%in%cl)]
    cl <- union(cl,cl_new)
  }
  
  # Cria a matriz de grid atual combinando com a antiga
  if (!exists('gmatrix')) {
    gmatrix <- array(0,c(prod(grid_size),4+length(cl)))
  } else {
    gmatrix_new <- array(0,c(nrow(gmatrix),length(cl_new)))
    gmatrix <- array(c(gmatrix,gmatrix_new),c(nrow(gmatrix),4+length(cl)))
  }
  
  # Cria a matriz de quadtree atual combinando com a antiga
  if (!exists('tmatrix')) {
    tmatrix_data <- c(range(tree1),range(tree2),rep(0,length(cl)))
    tmatrix <- array(tmatrix_data,c(1,4+length(cl)))
  } else {
    tmatrix_new <- array(0,c(nrow(tmatrix),length(cl_new)))
    tmatrix <- array(c(tmatrix,tmatrix_new),c(nrow(tmatrix),4+length(cl)))
  }
  
  # Encontra a matriz de grid, o modelo, a predicao, a acuracia e a memoria
  gmatrix <- createGrid(x1tr, x2tr, ytr, cl, gmatrix, grid1, grid2)
  gmodel <- createModel(gmatrix)
  gpred <- predModel(x1te, x2te, cl, gmodel)
  gacc[n] <- mean(gpred == yte)
  gmem[n] <- nrow(gmodel)
  
  # Encontra a matriz de quadtree, o modelo, a predicao, a acuracia e a memoria
  tmatrix <- createTree(x1tr, x2tr, ytr, cl, tmatrix, tree1, tree2)
  tmodel <- createModel(tmatrix)
  tpred <- predModel(x1te, x2te, cl, tmodel)
  tacc[n] <- mean(tpred == yte)
  tmem[n] <- nrow(tmodel)
  
  # Exibe o modelo encontrado
  plotClassifier(gmodel, tmodel, cl, FALSE)
  
}

# Exibe a media da acuracia e o tamanho da memoria
cat(sprintf("Grid:     %3.2f%%  -  %i elementos\n",100*mean(gacc),gmem[n]))
cat(sprintf("Quadtree: %3.2f%%  -  %i elementos\n",100*mean(tacc),tmem[n]))

# Exibe os valores de acuracia encontrados
par(mar=c(2,2,1,1),mfrow=c(1,1))
plot(gacc,type='l',ylim=c(0,1),col='blue',lwd=2)
par(new=TRUE)
plot(tacc,type='l',ylim=c(0,1),col='red',lwd=2)

# Exibe o tamanho da memoria utilizado
par(mar=c(2,2,1,1),mfrow=c(1,1))
plot(gmem,type='l',ylim=c(0,gmem[n]+1),col='blue',lwd=2)
par(new=TRUE)
plot(tmem,type='l',ylim=c(0,gmem[n]+1),col='red',lwd=2)