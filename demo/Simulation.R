#note that this program takes time. If you plan to shut down your computer before it ends. All results will be stored in this folder, and results alreadu stored will not be re-computed.
resultsfolder<-if(file.exists("~/R/Data/CPS")){"~/R/Data/CPS"}else{
  if(is.element("tcltk",installed.packages())){library(tcltk)
    tk_choose.dir(default = "", caption = "Please select a directory where to store results.")}else{getwd()}}
#1. Load libraries
library(CompositeRegressionEstimation)
library(pubBonneryChengLahiri2016)
library(Hmisc)
library(plyr)
library(dataCPS)
library(abind)
library(reshape2)
library(ggplot2)
#library(doParallel)
#2. Create synthetic populations
#2.0. Estimate counts from CPS web data.
if(!file.exists(file.path(resultsfolder,"Simu_list.tablesweb.rda"))){
  allmonths <- format(seq(as.Date("20050101", "%Y%m%d"),
                          as.Date("20120101", "%Y%m%d"),
                          by="month"), "%Y%m")
  names(allmonths)<-allmonths
  list.tablesweb<-plyr::alply(allmonths,1,function(x){
    eval(parse(text=paste0("data(cps",x,")")))
    y<-get(paste0("cps",x))
    y$pemlrR<-rep(c("1","0","_1"),c(2,2,4))[factor(y$pemlr,levels=c(1:7,"-1"))]
    y[	y$hrintsta=="1" & y$prpertyp %in% c("1","2"), ]
    y$employed=y$pemlrR=="1";y$unemployed=y$pemlrR=="0";
    y[,c("hrhhid","peage","pesex","pulineno","pehspnon","pemlr","pwsswgt","pwcmpwgt","pemlrR","hrmis","employed","unemployed")]
  },.progress = "text")
  rm(list=grep("cps",ls(),value=TRUE))
  names(list.tablesweb)<-allmonths
  save(list.tablesweb,file=file.path(resultsfolder,"Simu_list.tablesweb.rda"))
}
#2.1. Compute count estimates from CPS web data by employment status and month
if(!file.exists(file.path(resultsfolder,"Simu_Totals.rda"))){
  load(file.path(resultsfolder,"Simu_list.tablesweb.rda"))
  Totals<-CompositeRegressionEstimation::WS(list.tablesweb,weight ="pwsswgt" ,list.y = "pemlrR")
  dimnames(Totals)[[2]]<-substr(dimnames(Totals)[[2]],9,10)
  names(dimnames(Totals))<-c("m","y")
  Hmisc::label(Totals)<-"value of variable 'y' ('0':unemployed, '1', employed, '_1', NILF) in month m" 
  save(Totals,file=file.path(resultsfolder,"Simu_Totals.rda"))
  rm(list.tablesweb);gc()}
#2.2. Compute count estimates from CPS web data by month, employment status of the month, employment status of the previous month
if(!file.exists(file.path(resultsfolder,"Simu_crossTotals.rda"))){
  load(file.path(resultsfolder,"Simu_list.tablesweb.rda"))
  crossTotals<-CompositeRegressionEstimation::douuble (list.tablesweb,
                                                       w="pwsswgt",
                                                       id=c("hrhhid","pulineno"),y="pemlrR")$N01
  save(crossTotals,file=file.path(resultsfolder,"Simu_crossTotals.rda"))
  rm(list.tablesweb,crossTotals);gc()}
#2.3. Create 3 synthetic populations.
if(!file.exists(file.path(resultsfolder,"Simu_syntheticcpspops.rda"))){
  load(file.path(resultsfolder,"Simu_crossTotals.rda"))
  load(file.path(resultsfolder,"Simu_Totals.rda"))
  syntheticcpspops<-syntheticcpsdataset(Totals,crossTotals)
  syntheticcpspopsb<-syntheticcpspopsbf(syntheticcpspops)
  save(syntheticcpspops,syntheticcpspopsb,file=file.path(resultsfolder ,"Simu_syntheticcpspops.rda"))
  rm(Totals,crossTotals,syntheticcpspops,syntheticcpspopsb);gc()}
#2.3.2. Store employment status as array
if(!file.exists(file.path(resultsfolder,"Simu_syntheticcpspopsA.rda"))){
  load(file.path(resultsfolder,"Simu_syntheticcpspops.rda"))
  syntheticcpspopsA<-
    plyr::laply(syntheticcpspops,
                function(pop){
                  plyr::laply(pop,
                              function(l){
                                model.matrix(~0+pumlrR,l)==1})})
  rm(syntheticcpspops);gc()
  syntheticcpspopsbA<-
    plyr::laply(syntheticcpspopsb,
                function(pop){
                  plyr::laply(pop,
                              function(l){
                                model.matrix(~0+pumlrR,l)==1})})
  rm(syntheticcpspopsb);gc()
  syntheticcpspopsA<-abind(syntheticcpspopsA,syntheticcpspopsbA,along=5)
  dimnames(syntheticcpspopsA)<-c(list(
    s=c("Independent","2dorder","2dorder-indexdependent"),
    m=1:85,
    k=NULL,
    y=c("0","1","_1"),
    b=c("false","true")
  ))
  rm(syntheticcpspopsbA);gc()
  save(syntheticcpspopsA,file=file.path(resultsfolder,"Simu_syntheticcpspopsA.rda"))
  syntheticcpspopsHA<-plyr::aaply(syntheticcpspopsA,c(1:2,4:5),function(x){colSums(matrix(x, nrow=5))})
  rm(syntheticcpspopsA);gc()
  names(dimnames(syntheticcpspopsHA))[5]<-"h"
  save(syntheticcpspopsHA,file=file.path(resultsfolder,"Simu_syntheticcpspopsHA.rda"))
  rm(syntheticcpspopsHA);gc()
}

#2.5. Computation of (true) population totals
if(!file.exists(file.path(resultsfolder,"Simu_Populationtotals.rda"))){
  load(file.path(resultsfolder,"Simu_syntheticcpspopsA.rda"))
  Populationtotals<-plyr::aaply(syntheticcpspopsA[,,,,1],c(1:2,4),sum,.progress="text")#.parallel=TRUE)
  Populationtotals<-addUtoarray(Populationtotals)
  Populationtotals<-adddifftoarray(Populationtotals)
  Hmisc::label(Populationtotals)<-"Population value for synthetisation method s, variable y ('1' total employed '0' total unemployed '_1' total NILF 'r' unemployment rate, diff: month to month difference), and month m"
  save(Populationtotals,file=file.path(resultsfolder ,"Simu_Populationtotals.rda"))
  rm(syntheticcpspopsHA);gc()}
#3. Estimation
#3.0. Aggregation of employment status by atomic sample and save in an array
if(!file.exists(file.path(resultsfolder,"Simu_syntheticcpspopsS.rda"))){
  load(file.path(resultsfolder,"Simu_syntheticcpspopsA.rda"))
  syntheticcpspopsS<-plyr::daply(data.frame(i=1:1000),~i,
                                 function(d){
                                   plyr::aaply(syntheticcpspopsA[,,sampleruleS(d$i),,],c(1:2,4:5),sum)},.progress="text")
  Hmisc::label(syntheticcpspopsS)<-"Total for atomic sample i, synthetisation method s, employment status y, and month m"
  save(syntheticcpspopsS,file=file.path(resultsfolder,"Simu_syntheticcpspopsS.rda"))
  rm(syntheticcpspopsA,syntheticcpspopsS);gc()}

#3.1. Compute all month in sample estimates
if(!file.exists(file.path(resultsfolder,"Simu_misestimates.rda"))){
  load(file.path(resultsfolder ,"Simu_syntheticcpspopsS.rda"))
  misestimates<-1000*plyr::daply(expand.grid(i=1:1000,m=1:85,h=1:8,stringsAsFactors = FALSE),
                                 ~i+m+h,
                                 function(d){
                                   syntheticcpspopsS[(d$m-1)+c(16:13,4:1)[d$h],,d$m,,c(1,1+(d$h==8))]},.progress="text")
  dimnames(misestimates)[[6]][2]<-"true"
  Hmisc::label(misestimates)<-"Month in sample estimate for longitudinal sample i, month m, rotation group mis h, synthetisation procedure s, employment statys y"
  save(misestimates,file=file.path(resultsfolder ,"Simu_misestimates.rda"))
  rm(misestimates,syntheticcpspopsS);gc()}

#3.2. Computation of direct estimator
if(!file.exists(file.path(resultsfolder,"Simu_Direct.rda"))){
  load(file.path(resultsfolder ,"Simu_misestimates.rda"))
  Direct<-plyr::aaply(misestimates,
                      match(c("i","m","s","y","b"),names(dimnames(misestimates))),
                      sum,.progress="text")
  Hmisc::label(Direct)<-"Direct estimate for month m, variable y, synthetic population s, seed i, presence of bias b"
  Direct<-addUtoarray(Direct)
  Direct<-adddifftoarray(Direct)
  save(Direct,file=file.path(resultsfolder ,"Simu_Direct.rda"))
  rm(misestimates);gc()
  MSE_Direct<-compMSE(Direct,Populationtotals)
  Hmisc::label(MSE_Direct)<-"array of MSE of Direct estimate of y in synthetic population s and month m  and presence of bias b"
  save(MSE_Direct,file=file.path(resultsfolder ,"Simu_MSE_Direct.rda"))
  rm(MSE_Direct,Direct);gc()
}

#3.3. Computation of the variance covariance matrix of the month in sample estimates
if(!file.exists(file.path(resultsfolder,"Simu_Sigmas.rda"))){
  load(file.path(resultsfolder ,"Simu_misestimates.rda"))
  Sigmas<-varA(misestimates,fixindex = c("s","b"),
               varyingindex = "i",
               variableindex = c("m","h","y"))
  Hmisc::label(Sigmas)<-"array: for population s, covariance between months in sample estimate for month m1m group h1, and employment status y1 and month in sample estimate for month m2, group h2, and status y2"
  save(Sigmas,file=file.path(resultsfolder ,"Simu_Sigmas.rda"))
  rm(misestimates,Sigmas);gc()}

#3.4. Computation of coefficients for Best linear estimates (Yansaneh fuller)
if(!file.exists(file.path(resultsfolder,"Simu_Sigmas.rda"))){
  load(file.path(resultsfolder ,"Simu_Sigmas.rda"))
  YF_weights<-plyr::aaply(Sigmas[,1,,,,,,],
                          match(c("s"),names(dimnames(Sigmas)[-2])),
                          function(Sigma){
                            CompositeRegressionEstimation::CoeffYF(Sigma)},.progress="text")
  YF_weights<-array(YF_weights,c(dim(YF_weights)[1],85,3,85,8,3),
                    c(dimnames(YF_weights)[1],list("m"=1:85,"y"=c("0","1","_1"),"m1"=1:85,"h1"=1:8,"y1"=c("0","1","_1"))))
  save(YF_weights,file=file.path(resultsfolder ,"Simu_YF_weights.rda"))
  rm(YF_weights,Sigmas,Sigmas);gc()}

#3.5. Computation of coefficients Best AK estimator
if(!file.exists(file.path(resultsfolder,"Simu_coeffAK3.rda"))){
  load(file.path(resultsfolder,"Simu_Sigmas.rda"))
  load(file.path(resultsfolder,"Simu_Populationtotals.rda"))
  coeffAK3<-plyr::aaply(1:3,1,function(s){CompositeRegressionEstimation::bestAK3(Sigmas[s,,,,,,],t(Populationtotals[s,,]))},.progress="text")
  dimnames(coeffAK3)[1]<-dimnames(Populationtotals)[1]
  names(dimnames(coeffAK3))<-c("s","c")
  Hmisc::label(coeffAK3)<-"matrix M of 6-length vectors, where M[s,c] is the set of ak coefficients (a1, a2, a3, k1, k2, k3) optimum for population s and criterium c"
  coeffAK3<-cbind(coeffAK3, CPSmethod=rep(list(numeric(6)),3))
  coeffAK3[[1,4]]<-coeffAK3[[2,4]]<-coeffAK3[[3,4]]<-CPS_AK()
  save(coeffAK3,file=file.path(resultsfolder ,"Simu_coeffAK3.rda"))
  rm(Sigmas,Populationtotals,coeffAK3s,Sigmas);gc()}

#3.6. Computation of coefficients Best AK constrained estimator
if(!file.exists(file.path(resultsfolder,"Simu_coeffAK3sconstraint.rda"))){
  coeffAK3sconstraint<-plyr::aaply(1:3,1,function(i){
    CompositeRegressionEstimation::bestAK3contraint(Sigmas[i,,,,,,],t(Populationtotals[i,,]))},.progress="text")
  save(coeffAK3sconstraint,file=file.path(resultsfolder ,"Simu_coeffAK3sconstraint.rda"))
  rm(coeffAK3sconstraint,Sigmas);gc()}

#3.7. Computation of YF linear estimators
if(!file.exists(file.path(resultsfolder,"Simu_YFcomprep.rda"))){
  load(file.path(resultsfolder ,"Simu_misestimates.rda"))
  load(file.path(resultsfolder ,"Simu_Populationtotals.rda"))
  load(file.path(resultsfolder ,"Simu_YF_weights.rda"))
  dimnames(misestimates)<-lapply(dimnames(misestimates),function(x){1:length(x)})
  YFcomprep<-TensorDB::"%.%"(YF_weights,misestimates,
                             I_A=list(c="s",n=c("y","m"),p=c( "y1", "h1", "m1")),
                             I_B=list(c="s",p=c("y","h","m"),q=c("i","b")))
  
  #YFcomprep<-plyr::daply(as.data.frame(dimnames(YF_weights)[c("s","b")]),~s+b, function(d){
  #  plyr::aaply(misestimates[,,,d$s,,d$b],1,function(X){
  #    array(YF_weights[d$s,,]%*%c(X),c(3,dim(YF_weights)[2]/3))})
  #})
  YFcomprep<-addUtoarray(YFcomprep)
  YFcomprep<-adddifftoarray(YFcomprep)
  Hmisc::label(YFcomprep)<-"YF Estimate for month m, variable y, seed i, population s"
  save(YFcomprep,file=file.path(resultsfolder ,"Simu_YFcomprep.rda"))
  MSE_YF<-compMSE(YFcomprep,Populationtotals)
  Hmisc::label(MSE_YF)<-"array of MSE of YF estimate of y in synthetic population s and month m  and presence of bias b"
  save(MSE_YF,file=file.path(resultsfolder ,"Simu_MSE_YF.rda"))
  rm(MSE_YF,YFcomprep,misestimates,YF_weights);gc()
}


#3.8. Computation of AK linear estimators
if(!file.exists(file.path(resultsfolder,"Simu_AKcomprep.rda"))){
  load(file.path(resultsfolder ,"Simu_misestimates.rda"))
  load(file.path(resultsfolder ,"Simu_coeffAK3.rda"))
  #compute weights
  AK3_weights<-
    plyr::aaply(coeffAK3,1:2,function(x){
      CPS_AK_coeff.array.f(dim(misestimates)[match("m",names(dimnames(misestimates)))],x,simplify=FALSE)})
  #compute estimates
  AKcomprep<-TensorDB::"%.%"(AK3_weights,misestimates,
                             I_A=list(c=c("s"),n=c("c","y2","m2"),p=c( "y1", "h1", "m1")),
                             I_B=list(c="s",p=c("y","h","m"),q=c("i","b")))
  
  names(dimnames(AKcomprep))[match(c("y2","m2"),names(dimnames(AKcomprep)))]<-c("y","m")
  AKcomprep<-pubBonneryChengLahiri2016::addUtoarray(AKcomprep)
  AKcomprep<-adddifftoarray(AKcomprep)
  save(AKcomprep,file=file.path(resultsfolder ,"Simu_AKcomprep.rda"))
  MSE_AK<-compMSE(AKcomprep,Populationtotals)
  Hmisc::label(MSE_AK)<-"array of MSE of AK estimate of y in synthetic population s and month m  and presence of bias b"
  save(MSE_AK,file=file.path(resultsfolder ,"Simu_MSE_AK.rda"))
  rm(MSE_AK,AKcomprep,Misestimates,AK_weights);gc()
  rm(AKcomprep,AK3_weights,coeffAK3,Misestimates);gc()
}

#3.9.  Computation of Regression Composite
if(!file.exists(file.path(resultsfolder,"Simu_MRRcompb0.rda"))){
  load(file.path(resultsfolder ,"Simu_syntheticcpspops.rda"))
  popnums<-names(syntheticcpspops)
  hrmis=as.factor(rep(8:1,each=100))
  un=rep(1,800)
  
  MRRcomp<-plyr::aaply(popnums,1,function(s){
    plyr::aaply(1:1000,1,function(i){
      list.tables<-lapply(1:85,function(m){    
        cbind(syntheticcpspops[[s]][[m]][samplerule(i,1:800,m),],hrmis,un)})
      names(list.tables)<-names(syntheticcpspops[[1]])
      #MRR
      mrr<-CompositeRegressionEstimation::MR(list.tables=list.tables, w="pwsswgt", id=c("hrlongid",  "pulineno"), 
                                             list.xMR="pumlrR", list.x1="un", list.x2=NULL,list.y="pumlrR", 
                                             list.dft.x2=NULL,Alpha=seq(0,1,length.out=21),theta=3/4)$dfEst},
      .progress = "text")})
  dimnames(MRRcomp)[[1]]<-popnums
  names(dimnames(MRRcomp))[1:2]<-c("population","seed")
  MRRcompb0<-addUtoarray(MRRcomp,4,uenames=c(u="pumlrR_n0",e="pumlrR_n1","r"="r"))
  save(MRRcompb0,file=file.path(resultsfolder,"Simu_MRRcompb0.rda"))
  rm(MRRcompb0,MRRcomp,syntheticcpspops);gc()}

#computation of regression composite on population with bias
if(!file.exists(file.path(resultsfolder,"Simu_MRRcompbias.rda"))){
  load(file.path(resultsfolder ,"Simu_syntheticcpspops.rda"))
  popnums<-names(syntheticcpspops)
  hrmis=as.factor(rep(8:1,each=100))
  un=rep(1,800)
  MRRcompbias<-plyr::aaply(popnums,1,function(s){
    plyr::aaply(1:1000,1,function(i){
      list.tables<-plyr::alply(1:85,1,function(m){
        cbind(rbind(syntheticcpspops[[s]][[m]][samplerule(i,1:700,m),],
                    syntheticcpspopsb[[s]][[m]][samplerule(i,701:800,m),]),hrmis,un)})
      names(list.tables)<-names(syntheticcpspops[[1]])
      #all(replicate(50,(function(){i=sample(1000,1);m=sample(85,1);popnum=sample(3,1);misestimates[i,m,1,popnum,1]/1000==table(cbind(syntheticcpspops[[popnum]][[m]][samplerule(i,1:800,m),],hrmis,un)[701:800,]$pumlrR)[["0"]]})()))
      #MRR
      mrr<-CompositeRegressionEstimation::MR(list.tables=list.tables, w="pwsswgt", id=c("hrlongid",  "pulineno"), 
                                             list.xMR="pumlrR", list.x1="un", list.x2=NULL,list.y="pumlrR", 
                                             list.dft.x2=NULL,Alpha=seq(0,1,length.out=21),theta=3/4)$dfEst
    },
    .progress = "text")})
  dimnames(MRRcompbias)[[1]]<-popnums
  names(dimnames(MRRcompbias))[1:2]<-c("population","seed")
  MRRcompbias<-addUtoarray(MRRcompbias,4,uenames=c(u="pumlrR_n0",e="pumlrR_n1","r"="r"))
  save(MRRcompbias,file=file.path(resultsfolder,"Simu_MRRcompbias.rda"))
  rm(MRRcompbias,syntheticcpspops);gc()}

if(!file.exists(file.path(resultsfolder,"Simu_MRRcomp.rda"))){
  load(file.path(resultsfolder ,"Simu_MRRcompb0.rda"))
  load(file.path(resultsfolder ,"Simu_MRRcompbias.rda"))
  MRRcomp<-abind::abind(False=MRRcompb0,True=MRRcompbias,along=6)
  rm(MRRcompb0,MRRcompbias);gc()
  names(dimnames(MRRcomp))<-c("s","i","m","y","e","b")
  MRRcomp<-adddifftoarray(MRRcomp);gc()
  Hmisc::label(MRRcomp)<-"Estimate of synthetic population s, seed i, month m, variable y, estimator e, bias b"
  save(MRRcomp,file=file.path(resultsfolder,"Simu_MRRcomp.rda"))
  load(file.path(resultsfolder,"Simu_Populationtotals.rda"))
  MSE_MR<-compMSE(MRRcomp,Populationtotals)
  rm(MRRcomp,Populationtotals);gc()
  Hmisc::label(MSE_MR)<-"array of MSE of MR estimate of y in synthetic population s and month m  and presence of bias b"
  save(MSE_MR,file=file.path(resultsfolder ,"Simu_MSE_MR.rda"))
  rm(MSE_MR);gc()
}

#3.9.Estimate Sigma
if(!file.exists(file.path(resultsfolder,"Simu_Sigmahat.rda"))){
  load(file.path(resultsfolder ,"Simu_syntheticcpspopsHA.rda"))
  load(file.path(resultsfolder ,"Simu_Sigmas.rda"))
  dd<-dimnames(Sigmas);rm(Sigmas);gc()
  allcor<-do.call(expand.grid,
                  list(m1=1:85,
                  m2=1:85,
                  stringsAsFactors=FALSE))
  all_i_s_b<-do.call(expand.grid,list(i=1:1000,
                                      s=dimnames(syntheticcpspopsHA)[match("s",names(dimnames(syntheticcpspopsHA)))][[1]],
                                      b=c("true","false"),
                                      stringsAsFactors=FALSE))
  Sigmahat<-plyr::daply(all_i_s_b,
                        ~i+s+b,
                        function(d){
                          gc();
                          plyr::daply(allcor,~m1+m2,function(d2){
                            estimatesigma(d$s,d$i,d2$m1,d2$m2,d$b,syntheticcpspopsHA)
                          })
                          },.progress = "text")
  rm(syntheticcpspopsHA,all_i_s_b,allcor,dd);gc()
  system("kilall chrome; killall VLC;killall firefox;")
  save(Sigmahat,file=file.path(resultsfolder,"Simu_Sigmahat.rda"))
  rm(Sigmahat);gc()
  }

if(!file.exists(file.path(resultsfolder,"Simu_Sigmahat.rda"))){
  
  load(file.path(resultsfolder,"Simu_Populationtotals.rda"))
  SigmahatH=Sigmahatf(Sigmahat)
  coeffAK3hat<-CompositeRegressionEstimation::bestAK3(SigmahatH,t(Populationtotals[d$s,,]))
  load(file.path(resultsfolder ,"Simu_misestimates.rda"))
  load(file.path(resultsfolder ,"Simu_coeffAK3.rda"))
  #compute weights
  Ak3weightshat<-  plyr::laply(coeffAK3hat,function(x){CPS_AK_coeff.array.f(85,x)})
  #compute estimates
  AKhatcomprep<-TensorDB::"%.%"(Ak3weightshat,Misestimates,
                             I_A=list(c=c("s"),n=c("c","y2","m2"),p=c( "y1", "h1", "m1")),
                             I_B=list(c="s",p=c("y","j","m"),q=c("i","b")))
  
  names(dimnames(AKcomprep))[match(c("y2","m2"),names(dimnames(AKhatcomprep)))]<-c("y","m")
  AKhatcomprep<-pubBonneryChengLahiri2016::addUtoarray(AKhatcomprep)
  AKhatcomprep<-adddifftoarray(AKhatcomprep)
  names(dimnames(coeffAK3))<-c("s","c")

  MSE_AKhat<-compMSE(MRRcomp,Populationtotals)
  Hmisc::label(MSE_AKhat)<-"array of MSE of MR estimate of y in synthetic population s and month m  and presence of bias b"
  save(MSE_AKhat,file=file.path(resultsfolder ,"Simu_MSE_MR.rda"))
  rm(MSE_AKhat,AKhatcomprep);gc()

  }



#4. Compute best alpha.
load(file.path(resultsfolder ,"Simu_MSE_MR.rda"))
Cost<-plyr::aaply(MSE_MR[,,c("r","diffr"),,],match(c("e","s","b","y"),names(dimnames(MSE_MR))),mean)
Cost<-abind::abind(Cost,Cost[,,,"r",drop=FALSE]+Cost[,,,"diffr",drop=FALSE],along=4)
dimnames(Cost)[[4]][3]<-"r+diffr"
names(dimnames(Cost))<-c("e","s","b","y")
BestAlpha=plyr::aaply(Cost,2:4,function(x){dimnames(Cost)[[1]][which.min(x)]})
save(BestAlpha,file=file.path(resultsfolder ,"Simu_BestAlpha.rda"))

#5. Compute MSE_bestMR.
MSE_BestMR=plyr::daply(do.call(expand.grid,c(dimnames(MSE_MR[,,c("r","diffr"),,])[c(1,3,5)],list(stringsAsFactors=FALSE))),
                       c("s","y","b"),
                       function(d){MSE_MR[d$s,,d$y,BestAlpha[d$s,d$b,d$y],d$b,drop=FALSE]})
save(MSE_BestMR,file=file.path(resultsfolder ,"Simu_MSE_BestMR.rda"))
MSE_BestAK=drop(abind(MSE_AK[,"level","r",,,drop=FALSE],MSE_AK[,"change","diffr",,,drop=FALSE],along=3))
names(dimnames(MSE_BestAK))<-c("s","y","m","b")
dimnames(MSE_BestAK)$m<-dimnames(MSE_BestMR)$m
save(MSE_BestAK,file=file.path(resultsfolder ,"Simu_MSE_BestAK.rda"))

###############################################3

#Table 2
load(file.path(resultsfolder ,"Simu_coeffAK3.rda"))
table2<-plyr::aaply(coeffAK3,1:2,function(x){x})[,,c(1:2,4:5)]
table2.a<-matrix(paste("(",signif(table2[,,1],3),",",signif(table2[,,3],3),")"),dim(table2[,,1]))
table2.b<-matrix(paste("(",signif(table2[,,2],3),",",signif(table2[,,4],4),")"),dim(table2[,,1]))
dimnames(table2.a)<-dimnames(table2.b)<-dimnames(table2[,,1])

# Table 3 (Best alpha)
load(file.path(resultsfolder ,"Simu_BestAlpha.rda"))
table3<-plyr::aaply(BestAlpha,c(3,1),function(x){if(x[1]==x[2]){x[1]}else{paste0(x[2]," (",x[1],")")}})
# Figure 1
load(file.path(resultsfolder ,"Simu_MSE_BestMR.rda"))
load(file.path(resultsfolder ,"Simu_MSE_AK.rda"))
load(file.path(resultsfolder ,"Simu_MSE_Direct.rda"))
relMSE_BestMR<-relMSE(MSE_BestMR[,c("r","diffr"),,],MSE_Direct[,,c("r","diffr"),])
relMSE_Direct<-relMSE(MSE_Direct[,,c("r","diffr"),],MSE_Direct[,,c("r","diffr"),])
relMSE_BestAK<-relMSE(MSE_BestAK,MSE_Direct[,,c("r","diffr"),])
graphdata<-reshape2::melt(abind::abind(BMR=aperm(relMSE_BestMR,match(names(dimnames(MSE_Direct)),names(dimnames(relMSE_BestMR)))),
                                       Direct=relMSE_Direct[,,c("r","diffr"),],
                                       BAK=relMSE_BestAK,along=5))
names(graphdata)<-c(names(dimnames(relMSE_Direct)),"e","value")
graphdata$m<-as.Date(paste0(graphdata$m,"01"), format="%Y%m%d")
figure1<-ggplot(graphdata[graphdata$b=="false",],aes(x=m,y=value,colour=e))+geom_line() + 
  facet_grid(s~y )+ylab("")+scale_y_log10()
# Table 4

# Table 5

#Same with measurement error

#Table 6 
#Dispersion and mean of the relative mean squared errors for different population
#and unemployment level estimators

#Table 7
#Dispersion and mean of the relative mean squared errors for different population
#and unemployment change estimators

save(figure1,table2.a,table2.b,table3,file=file.path(resultsfolder ,"Simu_alltablesandfigures.rda"))
