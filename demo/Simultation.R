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
  save(syntheticcpspops,file=file.path(resultsfolder ,"Simu_syntheticcpspops.rda"))
  rm(Totals,crossTotals,syntheticcpspops);gc()}


#2.4. Aggregation of employment status by household and save in an array
if(!file.exists(file.path(resultsfolder,"Simu_syntheticcpspopsHA.rda"))){
  load(file.path(resultsfolder,"Simu_syntheticcpspops.rda"))
  syntheticcpspopsHA<-plyr::laply(syntheticcpspops,syntheticccpspopHAf,.progress="text")
  names(dimnames(syntheticcpspopsHA))[1]<-c("s")
  dimnames(syntheticcpspopsHA)[1]<-list(names(syntheticcpspops))
  Hmisc::label(syntheticcpspopsHA)<-"Total for household h, synthetisation method z, employment status y, and month m"
  save(syntheticcpspopsHA,file=file.path(resultsfolder,"Simu_syntheticcpspopsHA.rda"))
  rm(syntheticcpspops,syntheticcpspopsHA);gc()}
#2.5. Computation of (true) population totals
if(!file.exists(file.path(resultsfolder,"Simu_Populationtotals.rda"))){
  load(file.path(resultsfolder,"Simu_syntheticcpspopsHA.rda"))
  Populationtotals<-plyr::aaply(syntheticcpspopsHA,c(1,3:4),sum,.progress="text")#.parallel=TRUE)
  Populationtotals<-addUtoarray(Populationtotals)
  Populationtotals<-adddifftoarray(Populationtotals)
  Hmisc::label(Populationtotals)<-"Population value for synthetisation method s, variable y ('1' total employed '0' total unemployed '_1' total NILF 'r' unemployment rate, diff: month to month difference), and month m"
  save(Populationtotals,file=file.path(resultsfolder ,"Simu_Populationtotals.rda"))
  rm(syntheticcpspopsHA);gc()}

#3. Estimation

load(file.path(resultsfolder ,"Simu_Populationtotals.rda"))
#3.1. Compute all month in sample estimates
if(!file.exists(file.path(resultsfolder,"Simu_misestimates.rda"))){
  load(file.path(resultsfolder ,"Simu_syntheticcpspopsHA.rda"))
  misestimates<-1000*plyr::maply(expand.grid(i=1:1000,m=1:85,misi=1:8),misH,syntheticcpspopsHA=syntheticcpspopsHA,.progress="text")#.parallel=TRUE)
  dimnames(misestimates)[[2]]<-dimnames(syntheticcpspopsHA)[[4]]
  names(dimnames(misestimates))<-c("i","m","j","s","y")
  Hmisc::label(misestimates)<-"Month in sample estimate for longitudinal sample i, month m, rotation group mis j, synthetisation procedure s, employment statys y"
  save(misestimates,file=file.path(resultsfolder ,"Simu_misestimates.rda"))
  rm(misestimates,syntheticcpspopsHA);gc()}

if(!file.exists(file.path(resultsfolder,"Simu_misestimatesbias.rda"))){
  set.seed(1)
  load(file.path(resultsfolder,"Simu_misestimates.rda"))
  misestimatesbias<-misestimates
  misestimatesbias[,,1,,]<-1000*plyr::aaply(misestimates[,,1,,]/1000,1:3,
                                            function(x){if(x[2]>0){matrix(x+rbinom(1,x[2],.2)*c(1,-1,0))}else{x}},.progress = "text")
  Hmisc::label(misestimatesbias)<-"Rotation group biased month in sample estimate for longitudinal sample i, month m, rotation group mis j, synthetisation procedure s, employment statys y"
  save(misestimatesbias,file=file.path(resultsfolder ,"Simu_misestimatesbias.rda"))
  Misestimates<-abind::abind("false"=misestimates,"true"=misestimatesbias,along=6)
  names(dimnames(Misestimates))<-c(names(dimnames(misestimates)),"b")
  save(Misestimates,file=file.path(resultsfolder ,"Simu_MMisestimates.rda"))
  rm(misestimates,Misestimates,misestimatesbias);gc()}

#3.2. Computation of direct estimator
if(!file.exists(file.path(resultsfolder,"Simu_Direct.rda"))){
  load(file.path(resultsfolder ,"Simu_MMisestimates.rda"))
  Direct<-plyr::aaply(Misestimates,c(1:2,4:6),sum,.progress="text")
  names(dimnames(Direct))<-c("i","m","s","y","b")
  Hmisc::label(Direct)<-"Direct estimate for month m, variable y, synthetic population s, seed i, presence of bias b"
  Direct<-addUtoarray(Direct)
  Direct<-adddifftoarray(Direct)
  save(Direct,file=file.path(resultsfolder ,"Simu_Direct.rda"))
  rm(Misestimates,Direct);gc()
  MSE_Direct<-compMSE(Direct,Populationtotals)
  Hmisc::label(MSE_Direct)<-"array of MSE of Direct estimate of y in synthetic population s and month m  and presence of bias b"
  save(MSE_Direct,file=file.path(resultsfolder ,"Simu_MSE_Direct.rda"))
  }

#3.3. Computation of the variance covariance matrix of the month in sample estimates
if(!file.exists(file.path(resultsfolder,"Simu_Sigmas.rda"))){
  load(file.path(resultsfolder ,"Simu_misestimates.rda"))
  Sigmas<-plyr::aaply(misestimates,4,function(x){
    Sigma=array(var(array(x,
                          c(dim(x)[1],prod(dim(x)[2:4])))),
                rep(dim(x)[2:4],2))
    dimnames(Sigma)<-rep(dimnames(x)[2:4],2)
    names(dimnames(Sigma))<-paste0(names(dimnames(Sigma)),rep(1:2,each=3))
    Sigma},.progress="text")
  Hmisc::label(Sigmas)<-"array: for population s, covariance between months in sample estimate for month m1m group j1, and employment status y1 and month in sample estimate for month m2, group j2, and status y2"
  save(Sigmas,file=file.path(resultsfolder ,"Simu_Sigmas.rda"))
  rm(misestimates,Sigmas);gc()}

#3.4. Computation of coefficients for Best linear estimates (Yansaneh fuller)
if(!file.exists(file.path(resultsfolder,"Simu_Sigmas.rda"))){
  load(file.path(resultsfolder ,"Sigmas.rda"))
  YF_weights<-plyr::aaply(Sigmas,1,function(Sigma){CompositeRegressionEstimation::CoeffYF(Sigma)},.progress="text")
  names(dimnames(YF_weights)[[1]])<-dimnames(YF_weights)[[1]]
  save(YF_weights,file=file.path(resultsfolder ,"Simu_YF_weights.rda"))
  rm(YF_weights,Sigmas,Sigmas);gc()}

#3.5. Computation of coefficients Best AK estimator
if(!file.exists(file.path(resultsfolder,"Simu_coeffAK3.rda"))){
  load(file.path(resultsfolder,"Simu_Sigmas.rda"))
  load(file.path(resultsfolder,"Simu_Populationtotals.rda"))
  coeffAK3<-plyr::aaply(1:3,1,function(i){CompositeRegressionEstimation::bestAK3(Sigmas[i,,,,,,],t(Populationtotals[i,,]))},.progress="text")
  dimnames(coeffAK3)[1]<-dimnames(Populationtotals)[1]
  names(dimnames(coeffAK3))<-c("s","c")
  Hmisc::label(coeffAK3)<-"matrix M of 6-length vectors, where M[s,c] is the set of ak coefficients (a1, a2, a3, k1, k2, k3) optimum for population s and criterium c"
  coeffAK3<-cbind(coeffAK3, CPSmethod=rep(list(numeric(6)),3))
  coeffAK3[[1,4]]<-coeffAK3[[2,4]]<-coeffAK3[[3,4]]<-CPS_AK()
  save(coeffAK3,file=file.path(resultsfolder ,"Simu_coeffAK3.rda"))
  rm(coeffAK3s,Sigmas);gc()}

#3.6. Computation of coefficients Best AK constrained estimator
if(!file.exists(file.path(resultsfolder,"Simu_coeffAK3sconstraint.rda"))){
  coeffAK3sconstraint<-plyr::aaply(1:3,1,function(i){
    CompositeRegressionEstimation::bestAK3contraint(Sigmas[i,,,,,,],t(Populationtotals[i,,]))},.progress="text")
  save(coeffAK3sconstraint,file=file.path(resultsfolder ,"Simu_coeffAK3sconstraint.rda"))
  rm(coeffAK3sconstraint,Sigmas);gc()}

#3.7. Computation of YF linear estimators
if(!file.exists(file.path(resultsfolder,"Simu_YFcomprep.rda"))){
  load(file.path(resultsfolder ,"Simu_misestimates.rda"))
  load(file.path(resultsfolder ,"Simu_YF_weights.rda"))
  YFcomprep<-plyr::aaply(dimnames(YF_weights)[[1]],1, function(i){
    plyr::aaply(misestimates[,,,i,],1,function(X){
      array(YF_weights[i,,]%*%c(X),c(3,dim(YF_weights)[2]/3))})
  })
  dimnames(YFcomprep)[3:4]<-dimnames(Sigmas)[c(4,2)]
  names(dimnames(YFcomprep))<-c("s","i","y","m")
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
  load(file.path(resultsfolder ,"Simu_MMisestimates.rda"))
  load(file.path(resultsfolder ,"Simu_coeffAK3.rda"))
  #compute weights
  AK3_weights<-
    plyr::aaply(coeffAK3,1:2,function(x){
      CPS_AK_coeff.array.f(dim(Misestimates)[match("m",names(dimnames(Misestimates)))],x,simplify=FALSE)})
  #compute estimates
  AKcomprep<-TensorDB::"%.%"(AK3_weights,Misestimates,
                             I_A=list(c=c("s"),n=c("c","y2","m2"),p=c( "y1", "mis1", "m1")),
                             I_B=list(c="s",p=c("y","j","m"),q=c("i","b")))
  
  AKcomprep<-pubBonneryChengLahiri2016::addUtoarray(AKcomprep,"y2")
  AKcomprep<-adddifftoarray(AKcomprep,"y2","m2")
  names(dimnames(AKcomprep))[match(c("y2","m2"),names(dimnames(AKcomprep)))]<-c("y","m")
  save(AKcomprep,file=file.path(resultsfolder ,"Simu_AKcomprep.rda"))
  MSE_AK<-compMSE(AKcomprep,Populationtotals)
  Hmisc::label(MSE_AK)<-"array of MSE of AK estimate of y in synthetic population s and month m  and presence of bias b"
  save(MSE_AK,file=file.path(resultsfolder ,"Simu_MSE_AK.rda"))
  rm(MSE_AK,AKcomprep,misestimates,AK_weights);gc()
  rm(AKcomprep,AK3_weights,coeffAK3,Misestimates);gc()
}


#3.9.  Computation of Regression Composite
if(!file.exists(file.path(resultsfolder,"Simu_MRRcompb0.rda"))){
  load(file.path(resultsfolder ,"Simu_syntheticcpspops.rda"))
  popnums<-names(syntheticcpspops)
  hrmis=as.factor(rep(8:1,each=100))
  un=rep(1,800)
  
  MRRcomp<-plyr::aaply(popnums,1,function(popnum){
    plyr::aaply(1:1000,1,function(i){
      list.tables<-lapply(1:85,function(j){    
        cbind(syntheticcpspops[[popnum]][[j]][samplerule(i,1:800,j),],hrmis,un)})
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



if(!file.exists(file.path(resultsfolder,"Simu_MRRcompbias.rda"))){
  load(file.path(resultsfolder ,"Simu_syntheticcpspops.rda"))
  popnums<-names(syntheticcpspops)
  hrmis=as.factor(rep(8:1,each=100))
  un=rep(1,800)
  load(file.path(resultsfolder,"Simu_misestimates.rda"))
  load(file.path(resultsfolder,"Simu_misestimatesbias.rda"))
  BB<-(misestimatesbias-misestimates)/1000
  
  
  MRRcompbias<-plyr::aaply(popnums,1,function(popnum){
    plyr::aaply(1:1000,1,function(i){
      list.tables<-plyr::alply(1:85,1,function(m){
        ll<-cbind(syntheticcpspops[[popnum]][[m]][samplerule(i,1:800,m),],hrmis,un)
        x=sample((701:800)[ll[["pumlrR"]][701:800]=="1"],BB[i,m,1,popnum,1])
        ll[["pumlrR"]][x]<-"0"
        ll})
      names(list.tables)<-names(syntheticcpspops[[1]])
      #all(replicate(50,(function(){i=sample(1000,1);m=sample(85,1);popnum=sample(3,1);misestimates[i,m,1,popnum,1]/1000==table(cbind(syntheticcpspops[[popnum]][[m]][samplerule(i,1:800,m),],hrmis,un)[701:800,]$pumlrR)[["0"]]})()))
      #MRR
      mrr<-CompositeRegressionEstimation::MR(list.tables=list.tables, w="pwsswgt", id=c("hrlongid",  "pulineno"), 
                                             list.xMR="pumlrR", list.x1="un", list.x2=NULL,list.y="pumlrR", 
                                             list.dft.x2=NULL,Alpha=seq(0,1,length.out=21),theta=3/4)$dfEst},
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
  names(dimnames(MRRcomp))<-c("s","i","m","y","e","b")
  MRRcomp<-adddifftoarray(MRRcomp)
  Hmisc::label(MRRcomp)<-"Estimate of synthetic population s, seed i, month m, variable y, estimator e, bias b"
  save(MRRcomp,file=file.path(resultsfolder,"Simu_MRRcomp.rda"))
  
  MSE_MR<-compMSE(MRRcomp,Populationtotals)
  Hmisc::label(MSE_MR)<-"array of MSE of MR estimate of y in synthetic population s and month m  and presence of bias b"
  save(MSE_MR,file=file.path(resultsfolder ,"Simu_MSE_MR.rda"))
  
  rm(MSE_MR,MRRcompbias,MRRcompb0MRRcomp,MRRcomp);gc()
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




























    
    #below is old stuff maybe  
    machin<-function(toto,dimee=FALSE,adde2=adde2){
      
      if(is.element(toto,what)){
        sapply(paste0(toto,"comp",adde1,1:nrep,adde2),charge)  
        XX=addUto1000matrices(toto,"_rep")
        assign(paste0(toto,"comprep"),XX)
        if(dimee){dimnames(XX)[[3]]<-paste0(toto,dimnames(XX)[[3]])}
        eval(parse(text=Sauve(paste0(toto,"comprep"),adde2)))
        rm(list=paste0(toto,"comp_rep",1:nrep))
        system(paste0("cd ",resultsfolder,";rm ",paste(paste0(toto,"comp",adde1,1:nrep,adde2,".Rdata"),collapse=" ")))}
    }
  
  machin("MRR",TRUE,adde2)
  
  if(is.element("S",what)){
    sapply(list.adde2bis,
           function(adde2){
             charge(paste0("Scomppop",adde2))
             ScomppopU <-unemploymentcount(Scomppop)[,studyvar]
             ScomppopUdiff<-ScomppopU[-1,studyvar]-ScomppopU[-nrow(ScomppopU),studyvar]  
             ScompUrep<-array(0,c(85,length(studyvar),1,7))
             dimnames(ScompUrep)<-list(
               tables.entree,studyvar,"S",c("mean" ,   "var"   ,  "bias"   , "mse"   ,  "una"  ,   "relbias", "cv"))
             ScompUrep[,studyvar,"S","mean"]<-ScomppopU[,studyvar]
             
             ScompUrepdiff<-ScompUrep[-1,studyvar,,,drop=FALSE]-
               ScompUrep[-dim(ScompUrep)[1],studyvar,,,drop=FALSE]
             
             #sapply(c("S2comprep","RAcomprep","BCLcomprep","BCL0comprep","BCL2comprep","MRRcomprep","AK2_papacomprep","BCLratiocomprep"),charge)
             eval(parse(text=Sauve("ScompUrep",adde2)))
             eval(parse(text=Sauve("ScompUrepdiff",adde2)))
             eval(parse(text=Sauve("ScomppopU",adde2)))
             eval(parse(text=Sauve("ScomppopUdiff",adde2)))})}
  
  
  
  
  
  
  if(any(is.element(what,c("S","S2","BCL0","BCL2","AK3","AK2","estAK3","MRR","YF","estYF","MA")))){  
    sapply(list.adde2bis,
           function(adde2){
             sapply(paste0(c("S2","BCL0","BCL2","AK3","AK2",#"estAK2",
                             "estAK3","MRR","YF","estYF"),paste0("comprep",adde2)),charge)
             # charge(paste0("AK3compreptest",adde2)
             charge(paste0("ScompUrep",adde2))
             charge(paste0("ScompUrepdiff",adde2))
             charge(paste0("ScomppopU",adde2))
             charge(paste0("ScomppopUdiff",adde2))
             Recap<-abind(ScompUrep[,studyvar,,,drop=FALSE],
                          calcvarmeana(S2comprep),
                          calcvarmeana(MRRcomprep),
                          calcvarmeana(AK2comprep),
                          calcvarmeana(AK3comprep),
                          #    calcvarmeana(BCL2comprep),
                          calcvarmeana(estAK3comprep),
                          calcvarmeana(YFcomprep),
                          calcvarmeana(estYFcomprep),
                          calcvarmeana(BCL0comprep),
                          along = 3)
             compUrepdiff<-
               abind(calcvarmeana(difff(S2comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(MRRcomprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(AK2comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(AK3comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     #calcvarmeana(difff(BCL2comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(estAK3comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(YFcomprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(estYFcomprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(BCL0comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     along = 3)
             Recapdiff<-abind(ScompUrepdiff,
                              compUrepdiff, along = 3) 
             Recap<-abind(Recap,
                          array(
                            apply(Recap[,,,"mse"],3,function(x){x/Recap[,,"S2","mse"]}),
                            dim(Recap)[1:3])
                          ,along=4)
             dimnames(Recap)[[4]][8]<-"ratmse"        
             Recapdiff<-abind(Recapdiff,
                              array(
                                apply(Recapdiff[,,,"mse"],3,function(x){x/Recapdiff[,,"S2","mse"]}),
                                dim(Recapdiff)[1:3])
                              ,along=4)  
             dimnames(Recapdiff)[[4]][8]<-"ratmse"
             
             dimnames(Recap)[[3]][3:24]<-paste0("MRR",dimnames(Recap)[[3]][3:24])
             dimnames(Recapdiff)[[3]][3:24]<-paste0("MRR",dimnames(Recapdiff)[[3]][3:24])
             save(     Recap,
                       Recapdiff,
                       file=paste0("replications",adde2,".Rdata"))})}
  LL<-lapply(list.adde2,function(adde2){
    load(paste0("replications",adde2,".Rdata"))
    return(list(Recap=Recap,
                Recapdiff=Recapdiff))});
  names(LL)<-list.adde2
  RecapA<-do.call(abind,c(lapply(LL,function(l){l$Recap}),list(along=5)))
  RecapdiffA<-do.call(abind,c(lapply(LL,function(l){l$Recapdiff}),list(along=5)))
  save(     RecapA,
            RecapdiffA,
            file="replications.Rdata")
}


#
if(FALSE){
  adde2="_3";
  load(paste0("replications",adde2,".Rdata"));
  Recap[,"unemployment",c("S2","MRR0.75","AK_CPS","AK_level","AK2_level"),"bias"]
  Recap[,"pumlrR_n1",c("S2","MRR0.75","AK_CPS","AK_level","level"),"bias"]
  Recap[,"unemployment",c("S2","MRR0.75","AK_CPS","AK_level","level"),"ratmse"]
  graphs(Recap[,,"AK2"])
}
#Compute_S2MRAK_rep(what=c("S2"))




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
graphdata<-reshape2::melt(abind::abind(BMR=aperm(relMSE_BestMR,match(names(dimnames(MSE_Direct)),names(dimnames(relMSE_BestMR)))),
                                       Direct=relMSE_Direct[,,c("r","diffr"),],along=5))
names(graphdata)<-c(names(dimnames(relMSE_Direct)),"e","value")
graphdata$m<-as.Date(paste0(graphdata$m,"01"), format="%Y%m%d")
figure1<-ggplot(graphdata[graphdata$b=="false",],aes(x=m,y=value,colour=e))+geom_line() + 
  facet_grid(s~y )+ylab("")+scale_y_log10()



# Table 4
Quantiles and mean of the relative mean squared errors for different population
and unemployment level estimators

# Table 5

Same with measurement error

#Table 6 
Dispersion and mean of the relative mean squared errors for different population
and unemployment level estimators

#Table 7
Dispersion and mean of the relative mean squared errors for different population
and unemployment change estimators


save(figure1,table2.a,table2.b,table3,file=file.path(resultsfolder ,"Simu_alltablesandfigures.rda"))
