##Problemset 3

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

###############Problem 4###################
##Function for plotting coefficients
coefplotter<-function(x){
  plotdens<-function(x){
    plot(density(x), main="Coefficient Density")
  }
  a_ply(regressions, 2, plotdens) ##apply over the columns
}
par(mfrow=c(1,1)) ##Setting plot window
coefplotter(regressions) ##Note, It will produce all the plots, but you will 
##need to scroll through them or use par(mfrow=c(nr, nc)) to view all at once

######This represents the sampling distribution of the coefficients.
######It notes that for some coefficients, the two are clearly related and
######So for these coefficients, the effect is centered around the "true"
######value of the coefficient. The concern with error here is getting a false
######negative...that once in a while one may find that this coefficient
######is not significant when it really is. Conversely, some of the effects
######are centered around their true effect at 0 and have the possibility
######of getting a false positive error. 
#############Problem 5##################
##Return t-vals in matrix
tvals<-laply(1:1000, .fun=myfun, x=dat.array, y=the.yvals, coef=FALSE)

#############Problem6###################
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

#####For this seed, about 7.7 and 6.7 percent of b3 and b5 (respectively) are 
#####Significant even though the true value was zero. We expect this to be
#####around 5 percent. We should see that with repeated sampling of the 
#####t distributions of coefficients it will approach 5 percent.
#####In other words, we expect a coefficient to be signfiicant by chance
#####around 5 percent of the time when it truly has no effect. 
#####On the other hand b2 and b4 were significant 100 percent of the time.
#####However, the effect may be over or under estimated as shown in the
#####sampling distribution of the coefficients. The "true" values should
#####be 2 and 4. On the other hand, B1 is given a false negative about 2.7
#####percent of the time. 

################Problem 7##############

system.time(tvals<-laply(1:1000, .fun=myfun, x=dat.array, 
                         y=the.yvals, coef=FALSE)) 
##Look at system time of above code

library(doMC) ####ONLY WORKS ON MAC!!!!! Make sure this is installed!!!
registerDoMC(cores=4) ##Four cores
system.time(tvals2<-laply(1:1000, .fun=myfun, x=dat.array, 
                         y=the.yvals, coef=FALSE, .parallel=TRUE))            


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
fitstat<-function(y, p, r, s.rmse=TRUE, s.mad=TRUE, s.rmsle=TRUE, 
                  s.mape=TRUE, s.meape=TRUE, s.mrae=TRUE){
  ##Takes square root of the sum of squared error and divides by n
  rmse<-function(y,p){sqrt(sum(abs(p-y)^2))/length(y)}
  ##Applies internal function across the columns of p
  rmse1<-(aaply(p,2,rmse, y=y))
  ##takes median error
  mad<-function(y,p){median(abs(p-y))}
  ##applies across columns of p
  mad1<-(aaply(p,2, mad, y=y))
  ##takes the square root of the sum of the log of predicted values +1
  ##-the log of the observed values+1 squared then divides by n
  rmsle<-function(y,p){
    sqrt(sum((log(p+1)-log(y+1))^2))/length(y)
  }
  ##Applies to columns of p
  rmsle1<-(aaply(p,2, rmsle, y=y))
  ##Calculates average percentage error
  mape<-function(y,p){sum(((abs(p-y)/abs(y))*100))/length(y)}
  mape1<-(aaply(p,2,mape,y=y))
  ##calculates median percentage error
  meape<-function(y,p){median((abs(p-y)/abs(y))*100)}
  meape1<-(aaply(p,2,meape, y=y))
  ##calculates median error over null model error
  mrae<-function(y,p,r){
    b<-abs(r-y)
    median(abs(p-y)/b)}
  mrae1<-(aaply(p,2,mrae, y=y, r=r))
  ##construct matrix with values
  fitmat<-rbind(rmse1, mad1, rmsle1, mape1, meape1, mrae1)
  ##transpose to have columns be fit stats
  fitmat<-t(fitmat)
  ##create index to be used in display options. Negative columns will be 
  ##ommitted when it's time to return the matrix. 
  index<-NULL
  if(s.rmse==FALSE){index[1]=-1}
  if(s.mad==FALSE){index[2]=-2}
  if(s.rmsle==FALSE){index[3]=-3}
  if(s.mape==FALSE){index[4]=-4}
  if(s.meape==FALSE){index[5]=-5}
  if(s.mrae==FALSE){index[6]=-6}
  ##Returns index of everything if default is followed
  if(all(s.rmse,s.mad,s.rmsle,s.mape,s.meape,
         s.mrae)==TRUE){index<-c(1:6)}
  index<-na.omit(index) ##Needed for index to work properly. Otherwise NAs if
  ##one of the stat options is TRUE
  return(fitmat[,index])
}
fitstat(steptest$voteshare, pmat, rvec)
fitstat(steptest$voteshare, pmat, rvec, s.mrae=FALSE) ##Testing to ensure
##function works omitting mrae
fitstat(steptest$voteshare, pmat, rvec, s.mad=FALSE, s.meape=FALSE)##Testing 
##to see if the function still works without multiple stats

##############Part B-4####################
####The models are pretty good (and pretty similar in terms of fit).
####All of the stats would be 0 if the model predicted perfectly.
####All of these values are quite low, which bodes well
####For my constructed models. Unfortunately, the average percentage of
####error is at 11 percent. While this could be much lower, 11 percent
####is not all that high considering that percentage error has no
####upper limit. Similarly, meape is pretty low. The most highly 
####specified model actually has the lowest meape. 
####In practice, it seems that one would want an MRAE of 0, but in the
####absence of perfect estimate, one would prefer the value to be lower than 
####one. This is because a value of 1 would mean that the specified model
####has the same amount of error (no greater predictive power) than
####A null model (in this case just a constant). If the specified model
####has lower error than the null model, the MRAE value will be less than one.
####We find that happens here, though I am not familiar enough with standard
####practices in regards to MRAE to know what to expect out of a reasonably
####good model. 

