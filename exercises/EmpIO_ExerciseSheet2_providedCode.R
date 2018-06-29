## Bonus Exercise
somestatistics <- function(data){
  #Definition
  ncols <- ncol(data)
  nrows <- 2
  
  #Build up Matrix
  res1 <- array(rep(NA,nrows*ncols),dim=c(nrows,ncols))
  res1 <- as.data.frame(res1, row.names=c("mean", "variance"))
  colnames(res1) <- colnames(data)
  
  #Fill Matrix
  res1["mean",] <- format(apply(data,MARGIN=2,mean),digits=4)
  res1["variance",] <- format(apply(data,MARGIN=2,var),digits=4)
  
  res2 <- array(rep(NA,ncols/2*3),dim=c(ncols/2,3))
  res2 <- as.data.frame(res2, row.names=sapply(1:(ncols/2),FUN=function(x){paste0("combination ",x)}))
  colnames(res2) <- c("Covariance","Intercept","Regressor")
  
  for (i in 1:(ncols/2)){
    lm <- lm(data[,ncols/2+i] ~ data[,i])
    res2[i,"Covariance"] <- cov(data[,i],data[,ncols/2+i])
    res2[i,"Intercept"] <- coef(lm)[1]
    res2[i,"Regressor"] <- coef(lm)[2]
  }
  
  res <- list(vectorspecific=res1,combination=res2)
  
  return(res)
}
somestatistics(anscombe)
