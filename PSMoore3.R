##Problemset 3

###############Problem 1############
##Make 1000 datasets of 20 observations and 5 covariates
dat<-rnorm(20*5*1000) ##Make random data
dat.array<-array(dat, dim=c(20,5,1000)) ##Fill data into array
dim(dat.array) ##Verify dimensions

Beta <- matrix(c(1,2,0,4,0), ncol=1) ##vector of covariates

###############Problem 2############
##Create function that multiplies x beta and adds random noise. 
ymaker<-function(x){
  (x%*%Beta)+rnorm(20)
}
ymaker(dat.array[,,1]) ##Test function on one dataset

the.yvals<-apply(dat.array, MARGIN=3, FUN=ymaker) ##Apply to all
dim(the.yvals) ##Check dimensions. Success! 20x1000

##############Problem 3#############

########Function to use in laply below. Allows me to apply over an index
myfun<-function(i,x,y, coef=TRUE){
  reg1<-lm(y[,i]~x[,,i]) ##Runs regression on 
  if(coef==TRUE){return(unname(reg1$coef)) ##Returns coefs if coef==TRUE
    }else {return(unname(coef(summary(reg1))[,3]))} ##Returns tvals otherwise
}

##Return regression coefs in matrix
regressions<-laply(1:1000, .fun=myfun, x=dat.array, y=the.yvals, coef=TRUE)
dim(regressions) ##Ensure correct dimensions

##Return t-vals in matrix
tvals<-laply(1:1000, .fun=myfun, x=dat.array, y=the.yvals, coef=FALSE)

