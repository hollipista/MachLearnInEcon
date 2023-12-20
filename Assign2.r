# Computer question a)
predictorX  <- rnorm(100)
noise <- rnorm(100)
x2 <- predictorX^2
x3 <- predictorX^3
x4 <- predictorX^4
x5 <- predictorX^5
x6 <- predictorX^6
x7 <- predictorX^7
x8 <- predictorX^8
x9 <- predictorX^9
x10 <- predictorX^10

# Computer question b)
ß0 = 1
ß1 = 2
ß2 = 3
ß3 = 4

Y <-ß0 + ß1* predictorX + ß2* x2 + ß3* x3 + noise
dataYX <- data.frame(predictorX, x2,x3,x4,x5,x6,x7,x8,x9,x10, Y)

# Computer question c)
library(tree)
library(randomForest)
library(ISLR2)

# run regression tree
set.seed(1)
train <- sample(1:nrow(dataYX), nrow(dataYX) / 2)
tree.data <- tree(Y ~ ., dataYX , subset = train) 
summary(tree.data)
plot(tree.data)
text(tree.data , pretty = 0)


á# K-fold validation and plots of the cross validation error 
cv.data <- cv.tree(tree.data) 
plot(cv.data$size , cv.data$dev, type = "b")

# reporting the coeffs 
prune.data <- prune.tree(tree.data , best = 5)
plot(prune.data)
text(prune.data , pretty = 0)

yhat <- predict(tree.data , newdata = dataYX[-train , ])
data.test <- dataYX[-train, "Y"]
plot(yhat , data.test)
abline(0, 1)
mean((yhat - data.test)^2)

# Computer question d) Bagging

set.seed(1)
bag.data <- randomForest(Y ~ ., data = dataYX, subset = train, mtry = 10, importance = TRUE)
yhat.bag <- predict(bag.data , newdata = dataYX[-train , ])
plot(yhat.bag , data.test)
abline(0, 1)
mean((yhat.bag - data.test)^2)

# Computer question e) Random Forest 

rf.data <- randomForest(Y ~ ., data = dataYX, subset = train , mtry = 10, importance = TRUE)
yhat.rf <- predict(rf.data, newdata = dataYX[-train, ])
mean((yhat.rf - data.test)^2)

# Importance of variables 
importance(rf.data) 
varImpPlot(rf.data)


# Computer question f)
library(gbm)

set.seed(1)
boost.data <- gbm(Y ~ ., data = dataYX[train , ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.data)
plot(boost.data , i = "x2")
plot(boost.data , i = "predictorX")
yhat.boost <- predict(boost.data , newdata = dataYX[-train , ], n.trees = 5000)
mean((yhat.boost - data.test)^2)

boost.data <- gbm(Y ~ ., data = dataYX[train , ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.data , newdata = dataYX[-train , ], n.trees = 5000)
mean((yhat.boost - data.test)^2)


MSEregressiontree = NULL
MSEbagging = NULL
MSErandomforest = NULL
MSEboost = NULL

for (i in 1:100)  {
 
  # Computer question a)
  predictorX  <- rnorm(100)
  noise <- rnorm(100)
  x2 <- predictorX^2
  x3 <- predictorX^3
  x4 <- predictorX^4
  x5 <- predictorX^5
  x6 <- predictorX^6
  x7 <- predictorX^7
  x8 <- predictorX^8
  x9 <- predictorX^9
  x10 <- predictorX^10
  
  Y <-ß0 + ß1* predictorX + ß2* x2 + ß3* x3 + noise
  dataYX <- data.frame(predictorX, x2,x3,x4,x5,x6,x7,x8,x9,x10, Y)
  

  # run regression tree
  set.seed(1)
  train <- sample(1:nrow(dataYX), nrow(dataYX) / 2)
  tree.data <- tree(Y ~ ., dataYX , subset = train) 
  summary(tree.data)
  text(tree.data , pretty = 0)
  
  # K-fold validation and plots of the cross validation error 
  cv.data <- cv.tree(tree.data) 
  
  # reporting the coeffs 
  prune.data <- prune.tree(tree.data , best = 5)
  text(prune.data , pretty = 0)
  
  yhat <- predict(tree.data , newdata = dataYX[-train , ])
  data.test <- dataYX[-train, "Y"]
  mean((yhat - data.test)^2)
  
  MSEregressiontree <- c(MSEregressiontree, mean((yhat - data.test)^2))
  
  # Computer question d) Bagging
  
  set.seed(1)
  bag.data <- randomForest(Y ~ ., data = dataYX, subset = train, mtry = 10, importance = TRUE)
  yhat.bag <- predict(bag.data , newdata = dataYX[-train , ])
  MSEbagging <- c(MSEbagging, mean((yhat.bag - data.test)^2))
  
  # Computer question e) Random Forest 
  
  rf.data <- randomForest(Y ~ ., data = dataYX, subset = train , mtry = 10, importance = TRUE)
  yhat.rf <- predict(rf.data, newdata = dataYX[-train, ])
  MSErandomforest <- c(MSErandomforest, mean((yhat.rf - data.test)^2))
  
  # Computer question f)
  set.seed(1)
  boost.data <- gbm(Y ~ ., data = dataYX[train , ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
  summary(boost.data)
  yhat.boost <- predict(boost.data , newdata = dataYX[-train , ], n.trees = 5000)
  mean((yhat.boost - data.test)^2)
  
  MSEboost <- c(MSEboost,mean((yhat.boost - data.test)^2))
  
}
 

mean(MSEregressiontree)
mean(MSEbagging)
mean(MSErandomforest)
mean(MSEboost)

MSEbagging
MSEregressiontree


























# Computer question g)
#  1. Mallows model


library(quadprog)

method = 1 
subset <- matrix(c(1,1,1,1,0,1,0,1,0,0,0,1),3,4)

y <- as.matrix(Y)
x <- as.matrix(predictorX)  
s <- as.matrix(subset)
n <- nrow(x)
p <- ncol(x)

if ((nrow(s)==1) && (ncol(s)==1)){
  if (subset == 1){
    s <- matrix(1,nrow=p,ncol=p)
    s[upper.tri(s)] <- 0
    zero <- matrix(0,nrow=1,ncol=p)
    s <- rbind(zero,s) 
  } 
  if (subset == 2){
    s <- matrix(0,nrow=2^p,ncol=p)
    s0 <- matrix(c(1,rep(0,p-1)),1,p)
    s1 <- matrix(c(rep(0,p)),1,p)
    for (i in 2:2^p){
      s1 <- s0 + s1
      for (j in 1:p){
        if (s1[1,j] == 2){
          s1[1,j+1] <- s1[1,j+1]+1
          s1[1,j] <- 0
        }
      }           
      s[i,] <- s1
    }   
  }   
}   

m <- nrow(s)
bbeta <- matrix(0,nrow=p,ncol=m)
if (method == 2) ee <- matrix(0,nrow=n,ncol=m)

for (j in 1:m){
  ss <- matrix(1,nrow=n,ncol=1) %*% s[j,]
  indx1 <- which(ss[,]==1)
  xs <- as.matrix(x[indx1])
  xs <- matrix(xs,nrow=n,ncol=nrow(xs)/n)
  if (sum(ss)==0){
    xs <- x
    betas <- matrix(0,nrow=p,ncol=1)
    indx2 <- matrix(c(1:p),nrow=p,ncol=1)  
  }  
  if (sum(ss)>0){
    betas <- solve(t(xs)%*%xs)%*%t(xs)%*%y 
    indx2 <- as.matrix(which(s[j,]==1))  
  }
  beta0 <- matrix(0,nrow=p,ncol=1)
  beta0[indx2] <- betas     
  bbeta[,j] <- beta0    
  if (method == 2){
    ei <- y - xs %*% betas
    hi <- diag(xs %*% solve(t(xs) %*% xs) %*% t(xs))
    ee[,j] <- ei*(1/(1-hi))
  }
}

if (method == 1){
  ee <- y %*% matrix(1,nrow=1,ncol=m) - x %*% bbeta
  ehat <- y - x %*% bbeta[,m]
  sighat <- (t(ehat) %*% ehat)/(n-p)
}

a1 <- t(ee) %*% ee
if (qr(a1)$rank<ncol(ee)) a1 <- a1 + diag(m)*1e-10
if (method == 1) a2 <- matrix(c(-sighat*rowSums(s)),m,1)  
if (method == 2) a2 <- matrix(0,nrow=m,ncol=1)
a3 <- t(rbind(matrix(1,nrow=1,ncol=m),diag(m),-diag(m)))
a4 <- rbind(1,matrix(0,nrow=m,ncol=1),matrix(-1,nrow=m,ncol=1))

w0 <- matrix(1,nrow=m,ncol=1)/m 
QP <- solve.QP(a1,a2,a3,a4,1)
w <- QP$solution
w <- as.matrix(w)
w <- w*(w>0)
w <- w/sum(w0)
betahat <- bbeta %*% w
ybar <- mean(y)
yhat <- x %*% betahat
ehat <- y-yhat
r2 <- sum((yhat-ybar)^2)/sum((y-ybar)^2)
if (method == 1) cn=(t(w) %*% a1 %*% w - 2*t(a2) %*% w)/n
if (method == 2) cn=(t(w) %*% a1 %*% w)/n
list(betahat=betahat,w=w,yhat=yhat,ehat=ehat,r2=r2,cn=cn)


#  2. Jackknife model average estimate
method = 2 
subset <- matrix(c(1,1,1,1,0,1,0,1,0,0,0,1),3,4)

y <- as.matrix(Y)
x <- as.matrix(predictorX)  
s <- as.matrix(subset)
n <- nrow(x)
p <- ncol(x)

if ((nrow(s)==1) && (ncol(s)==1)){
  if (subset == 1){
    s <- matrix(1,nrow=p,ncol=p)
    s[upper.tri(s)] <- 0
    zero <- matrix(0,nrow=1,ncol=p)
    s <- rbind(zero,s) 
  } 
  if (subset == 2){
    s <- matrix(0,nrow=2^p,ncol=p)
    s0 <- matrix(c(1,rep(0,p-1)),1,p)
    s1 <- matrix(c(rep(0,p)),1,p)
    for (i in 2:2^p){
      s1 <- s0 + s1
      for (j in 1:p){
        if (s1[1,j] == 2){
          s1[1,j+1] <- s1[1,j+1]+1
          s1[1,j] <- 0
        }
      }           
      s[i,] <- s1
    }   
  }   
}   

m <- nrow(s)
bbeta <- matrix(0,nrow=p,ncol=m)
if (method == 2) ee <- matrix(0,nrow=n,ncol=m)

for (j in 1:m){
  ss <- matrix(1,nrow=n,ncol=1) %*% s[j,]
  indx1 <- which(ss[,]==1)
  xs <- as.matrix(x[indx1])
  xs <- matrix(xs,nrow=n,ncol=nrow(xs)/n)
  if (sum(ss)==0){
    xs <- x
    betas <- matrix(0,nrow=p,ncol=1)
    indx2 <- matrix(c(1:p),nrow=p,ncol=1)  
  }  
  if (sum(ss)>0){
    betas <- solve(t(xs)%*%xs)%*%t(xs)%*%y 
    indx2 <- as.matrix(which(s[j,]==1))  
  }
  beta0 <- matrix(0,nrow=p,ncol=1)
  beta0[indx2] <- betas     
  bbeta[,j] <- beta0    
  if (method == 2){
    ei <- y - xs %*% betas
    hi <- diag(xs %*% solve(t(xs) %*% xs) %*% t(xs))
    ee[,j] <- ei*(1/(1-hi))
  }
}

if (method == 1){
  ee <- y %*% matrix(1,nrow=1,ncol=m) - x %*% bbeta
  ehat <- y - x %*% bbeta[,m]
  sighat <- (t(ehat) %*% ehat)/(n-p)
}

a1 <- t(ee) %*% ee
if (qr(a1)$rank<ncol(ee)) a1 <- a1 + diag(m)*1e-10
if (method == 1) a2 <- matrix(c(-sighat*rowSums(s)),m,1)  
if (method == 2) a2 <- matrix(0,nrow=m,ncol=1)
a3 <- t(rbind(matrix(1,nrow=1,ncol=m),diag(m),-diag(m)))
a4 <- rbind(1,matrix(0,nrow=m,ncol=1),matrix(-1,nrow=m,ncol=1))

w0 <- matrix(1,nrow=m,ncol=1)/m 
QP <- solve.QP(a1,a2,a3,a4,1)
w <- QP$solution
w <- as.matrix(w)
w <- w*(w>0)
w <- w/sum(w0)
betahat <- bbeta %*% w
ybar <- mean(y)
yhat <- x %*% betahat
ehat <- y-yhat
r2 <- sum((yhat-ybar)^2)/sum((y-ybar)^2)
if (method == 1) cn=(t(w) %*% a1 %*% w - 2*t(a2) %*% w)/n
if (method == 2) cn=(t(w) %*% a1 %*% w)/n
list(betahat=betahat,w=w,yhat=yhat,ehat=ehat,r2=r2,cn=cn)
