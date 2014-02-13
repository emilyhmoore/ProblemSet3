##Problemset 3

###############To Do#####################
###############Section A, Prob 7
###############Discuss sambling distribution, t-stat significance
###############Problem B-1
###############Problem B-2
###############Problem B-3
###############Problem B-4

###############Problem 1############
##Make 1000 datasets of 20 observations and 5 covariates
set.seed(12)
dat<-rnorm(20*5*1000) ##Make random data
dat.array<-array(dat, dim=c(20,5,1000)) ##Fill data into array
dim(dat.array) ##Verify dimensions

Beta <- matrix(c(1,2,0,4,0), ncol=1) ##vector of covariates

###############Problem 2############
##Create function that multiplies x beta and adds random noise. 
ymaker<-function(x){
  set.seed(12)
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
library(plyr)
regressions<-laply(1:1000, .fun=myfun, x=dat.array, y=the.yvals, coef=TRUE)
dim(regressions) ##Ensure correct dimensions

##Function for plotting coefficients
coefplotter<-function(x){
  plotdens<-function(x){
    plot(density(x), main="Coefficient Density")
  }
  apply(regressions, 2, plotdens) ##apply over the columns
}
par(mfrow=c(1,1)) ##Setting plot window
coefplotter(regressions) ##Note, It will produce all the plots, but you will 
##need to scroll through them or use par(mfrow=c(nr, nc)) to view all at once

##Return t-vals in matrix
tvals<-laply(1:1000, .fun=myfun, x=dat.array, y=the.yvals, coef=FALSE)

##Function for calculating significance
sigcalc<-function(x){
  identify<-function (x){length(which(abs(x)>1.96))}##Minifunction for number sig
  numbersig<-aaply(x, .margins=2, .fun=identify)##Apply function to each coef
  names(numbersig)=c("constant", "b1", "b2", "b3", "b4", "b5")##Names for coef
  return(list(Number_Significant=numbersig, 
              Percentage_Significant=numbersig/10)) ##Returns list with 
  ##Number and percentage of t-stats significant for each coef
}
sigcalc(tvals) ##Try it out. 

####################Section B########################

outofstep<-read.table("https://pages.wustl.edu/montgomery/incumbents.txt", header=TRUE)
outofstep<-outofstep[,c(2,3,7:19)] ##Dropping variables missing a lot of data
##As I don't plan to use them
outofstep<-na.omit(outofstep) ##Omit any cases still missing. Only a couple.

set.seed(12)
outind<-sample(1:6560, size=1000) ##Create sampling index
steptest<-outofstep[outind,] ##partition out test sample
steptraining<-outofstep[-outind,] ##rest become training sample

###Models
votemod<-lm(voteshare~year+presvote+inparty
            +seniority+urban+unemployed, data=steptraining)
votemod2<-lm(voteshare~year+presvote+urban+inparty*seniority, data=steptraining)
votemod3<-lm(voteshare~presvote+seniority+urban, data=steptraining)
naivevote<-lm(voteshare~1, data=steptraining)

##Predicted values
predval<-predict(votemod, newdata=steptest)
predval2<-predict(votemod2,newdata=steptest)
predval3<-predict(votemod3, newdata=steptest)
pmat<-cbind(predval, predval2, predval3)

##vector of naive predicted values
rvec<-predict(naivevote, newdata=steptest)

##Fit statistics function
fitstat<-function(y, p){
  rmse<-function(y,p){sum(abs(p-y)^2)/length(y)}
  aaply(p,2,rmse, y=y)
}

fitstat(steptest$voteshare, pmat)

