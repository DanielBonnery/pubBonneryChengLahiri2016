library(CompositeRegressionEstimation)
library(pubBonneryChengLahiri2016)
options(cores=8)
Totals      <-
  plyr::aaply(
    pubBonneryChengLahiri2016::S2comp,1,function(x){
  c("0"=sum(x[paste0("pumlr_n",3:4)]),
  "1"=sum(x[paste0("pumlr_n",1:2)]),
  "_1"=sum(x[paste0("pumlr_n",5:7)]))})
names(dimnames(Totals))<-c("month","pumlrR")
crossTotals<-pubBonneryChengLahiri2016::CountsChangePumlrR

syntheticcpspops<-syntheticcpsdataset(Totals,crossTotals)

syntheticcpspopsHA<-plyr::laply(syntheticcpspops,list.tablespopAf)


####################################################
# Computation of pop totals
####################################################
Populationtotals<-plyr::aaply(syntheticcpspopsHA,1:3,sum)
gc()

#Draw all probable samples
#Allsamples<-Allsamplesf(dimnames(Totals)$month)
#AllsamplesH<-Allsamples[,((1:160)*5),]/5
#Computation of month in sample totals.

#list.tablesA<-list.tablesAf(syntheticcpspopsHA,AllsamplesH)
misestimates<-800*plyr::maply(expand.grid(i=1:1000,m=1:85,misi=1:8),misH,syntheticcpspopsHA=syntheticcpspopsHA,.progress="text")
dimnames(misestimates)<-list(1:dimnames(misestimates)[1],dimnames(syntheticcpspopsHA)[c(1,3,4)])
names(dimnames(misestimates))<-c("i","m","j","s","y")
Hmisc::label(misestimates)<-"Month in sample estimate for longitudinal sample i, month m, rotation group mis j, synthetisation procedure s, employment statys y"





#Computation of direct estimator
#First method:
Direct<-plyr::aaply(misestimates,c(1:2,4:5),sum,.progress="text")


# Computation of Sigma          
Sigmas<-plyr::aaply(misestimates,4,function(x){
    Sigma=array(var(array(x,
                            c(dim(x)[1],prod(dim(x)[2:4])))),
                rep(dim(x)[2:4],2))
    dimnames(Sigma)<-rep(dimnames(x)[2:4],2)
    names(dimnames(Sigma))<-paste0(names(dimnames(Sigma)),rep(1:2,each=3))
    Sigma},.progress="text")
    
# Computation of coefficients for Best linear estimates (Yansaneh fuller)

CoeffYF<-function (Sigma) 
{
  nmonth<-dim(Sigma)[1]
  Sigg <- array(aperm(Sigma, c(3, 2, 1, 6, 5, 4)), rep(nmonth * 8 * 3, 2))
  XX <- t(matrix(rep(diag(3), 8), 3, 24))
  X <- as.matrix(Matrix::.bdiag(lapply(1:nmonth, function(i) {
    XX
  })))
  Xplus <- t(X)/8
  M <- diag(nmonth * 8 * 3) - X %*% Xplus
  gi <- MASS::ginv(M %*% Sigg %*% M)
  W <- Xplus %*% (diag(nmonth * 8 * 3) - M) %*% (diag(nmonth * 
                                                        8 * 3) - Sigg %*% gi)
  return(W)
}

coeffYF<-plyr::aaply(Sigmas,1,function(Sigma){CoeffYF(Sigma)},.progress="text")


# Computation of coefficients Best AK estimator

# Computation of coefficients Best AK estimator
