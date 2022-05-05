
##### R function documentation description #####
#' @title Beta distribution using Method of Moments
#' @description Beta distribution using Method of Moments
#' 
#' @param mu is mean
#' @param sd is standard deviation
#' 
#' @return returns hyperparameters
#' 
#' @examples 
#' 
#' @export

# Beta distribution using Method of Moments
# 0 to 1 with mean and sd
beta.mom<-function(mu=NULL,sd=NULL){
  # Standard deviation
  v<-sd**2
  # # mean
  x<-mu
  # shape1
  a<-x*(x*(1-x)/v-1)
  # shape2
  b<-(1-x)*(x*(1-x)/v-1)
  # return hyperparameters
  return(c(a,b))
}

bm<-beta.mom(mu=0.4,sd=0.03)

rbeta(1,shape1=bm[1],shape2=bm[2])

n<-10000
hist(rbeta(n,shape1=bm[1],shape2=bm[2]))
mean(rbeta(n,shape1=bm[1],shape2=bm[2]))
sd(rbeta(n,shape1=bm[1],shape2=bm[2]))


