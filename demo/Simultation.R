#1. Load libraries
library(CompositeRegressionEstimation)
library(pubBonneryChengLahiri2016)
library(plyr)
library(dataCPS)
setwd(file.path(Mydirectories::Dropbox.directory(),"CensusBAE/R/Computation of FR/packages/pubBonneryChengLahiri2016/"))
#library(doParallel)
resultsfolder<-if(file.exists("datanotpushed")){"datanotpushed"}else{tempdir()}

#nodes <- detectCores()
#cl <- makeCluster(nodes)
#registerDoParallel(cl)
#on.exit(stopCluster(cl))
#2. Create synthetic populations
#2.0. Estimate counts from CPS web data.
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

Totals<-WS(list.tablesweb,list.y = "pemlrR",weight ="pwsswgt" )
dimnames(Totals)[[2]]<-substr(dimnames(Totals)[[2]],9,10)
save(Totals,file=file.path(resultsfolder,"Simu_Totals.rda"))
crossTotals<-douuble (list.tablesweb,
                    w="pwsswgt",
                    id=c("hrhhid","pulineno"),list.y="pemlrR")$N01
save(crossTotals,file=file.path(resultsfolder,"Simu_Totals.rda"))
rm(list.tablesweb);gc()
#2.1. Create 3 synthetic populations.
syntheticcpspops<-syntheticcpsdataset(Totals,crossTotals)
save(syntheticcpspops,file=file.path(resultsfolder ,"Simu_syntheticcpspops.rda"))
rm(Totals,crossTotals)
#2.2. Aggregation of employment status by household and save in an array
load(file.path(resultsfolder,"Simu_syntheticcpspops.rda"))
syntheticcpspopsHA<-plyr::laply(syntheticcpspops,syntheticccpspopHAf,.progress="text")
names(dimnames(syntheticcpspopsHA))[1]<-c("s")
dimnames(syntheticcpspopsHA)[1]<-list(names(syntheticcpspops))
Hmisc::label(syntheticcpspopsHA)<-"Total for household h, synthetisation method z, employment status y, and month m"
rm(syntheticcpspops)
gc()
#2.3. Computation of (true) population totals
Populationtotals<-plyr::aaply(syntheticcpspopsHA,c(1,3:4),sum,.progress="text")#.parallel=TRUE)
Hmisc::label(Populationtotals)<-"Total for synthetisation method z, employment status y, and month m"
save(Populationtotals,file=file.path(resultsfolder ,"Simu_Populationtotals.rda"))

#stopCluster(cl)


#2.4. Estimation (for more details, see misH function)
#list.tablesA<-list.tablesAf(syntheticcpspopsHA,AllsamplesH)
misestimates<-800*plyr::maply(expand.grid(i=1:1000,m=1:85,misi=1:8),misH,syntheticcpspopsHA=syntheticcpspopsHA,.progress="text")#.parallel=TRUE)
dimnames(misestimates)[[2]]<-dimnames(syntheticcpspopsHA)[[4]]
names(dimnames(misestimates))<-c("i","m","j","s","y")
Hmisc::label(misestimates)<-"Month in sample estimate for longitudinal sample i, month m, rotation group mis j, synthetisation procedure s, employment statys y"
gc()
save(misestimates,file=file.path(resultsfolder ,"Simu_misestimates.rda"))
#2.5. Computation of direct estimator
Direct<-plyr::aaply(misestimates,c(1:2,4:5),sum,.progress="text")
save(Direct,file=file.path(resultsfolder ,"Simu_Direct.rda"))
#2.6. Computation of Sigma          
Sigmas<-plyr::aaply(misestimates,4,function(x){
    Sigma=array(var(array(x,
                            c(dim(x)[1],prod(dim(x)[2:4])))),
                rep(dim(x)[2:4],2))
    dimnames(Sigma)<-rep(dimnames(x)[2:4],2)
    names(dimnames(Sigma))<-paste0(names(dimnames(Sigma)),rep(1:2,each=3))
    Sigma},.progress="text")
Hmisc::label(Sigmas)<-"array: for population s, covariance between months in sample estimate for month m1m group j1, and employment status y1 and month in sample estimate for month m2, group j2, and status y2"
save(Sigmas,file=file.path(resultsfolder ,"Simu_Sigmas.rda"))
#load(file.path(resultsfolder,"Simu_Sigmas.rda"))
# Computation of coefficients for Best linear estimates (Yansaneh fuller)

YF_coeffs<-plyr::aaply(Sigmas,1,function(Sigma){CoeffYF(Sigma)},.progress="text")
names(dimnames(YF_coeffs)[[1]])<-dimnames(YF_coeffs)[[1]]
save(YF_coeffs,file=file.path(resultsfolder ,"Simu_YF_coeffs.rda"))


# Computation of coefficients Best AK estimator


####################################################
#
load(file.path(resultsfolder,"Simu_Sigmas.rda"))
load(file.path(resultsfolder,"Simu_Populationtotals.rda"))
coeffAK3s<-plyr::aaply(1:3,1,function(i){bestAK3(Sigmas[i,,,,,,],t(Populationtotals[i,,]))})
save(coeffAK3s,file=file.path(resultsfolder ,"Simu_coeffAK3.rda"))

coeffAK3sconstraint<-plyr::aaply(1:3,1,function(i){
  bestAK3contraint(Sigmas[i,,,,,,],t(Populationtotals[i,,]))})
save(coeffAK3sconstraint,file=file.path(resultsfolder ,"Simu_coeffAK3.rda"))

####################################################  
#Computation of linear estimators
  #YF
load(file.path(resultsfolder ,"Simu_misestimates.rda"))
load(file.path(resultsfolder ,"Simu_YF_coeffs.rda"))
load(file.path(resultsfolder,"Simu_Sigmas.rda"))

YFcomprep<-plyr::aaply(dimnames(YF_coeffs)[[1]],1, function(i){
  plyr::aaply(misestimates[,,,i,],1,function(X){
    array(YF_coeffs[i,,]%*%c(X),c(3,dim(YF_coeffs)[2]/3))})
  })
dimnames(YFcomprep)[3:4]<-dimnames(Sigmas)[c(4,2)]
YFcomprep<-addUtoarray(YFcomprep,3)
save(YFcomprep,file=file.path(resultsfolder ,"Simu_coeffAK3.rda"))

####################################################
# Computation of Reg Comp, MA
####################################################
load(file.path(resultsfolder ,"Simu_syntheticcpspops.rda"))
popnums<-dimnames(Sigmas)[[1]]
biass=c("","bias")
  for(popnum in popnums){
    for (bias in biass){
      adde1="_rep"
      adde2f<-function(bias,popnum){paste(popnum,bias,sep="-")}
      adde2=adde2f(bias,popnum)
      ##---------------------------------------------------------------
      #Computations
      pluserror<-function(x){
        if(sum(x[700,800]=="0")>2){
          x[sample((700:800)[x[700,800]=="0"],2)]<-"1"}
        x}
      
      pluserror<-function(x,diffe){
        for( z in (1:20)[diffe[1:20]>0]){
          x[((5*(z-1)+1):(5*z))[x[((5*(z-1)+1):(5*z))]=="1"][1:diffe[z]]]<-"0"}
        x}

      hrmis=as.factor(rep(8:1,each=100))
      un=rep(1,800)
      u=1:1000
      i=1
      mclapply(u,function(i){
        print(i)
        list.tables<-lapply(1:85,function(j){    
          if(bias==""){
            cbind(syntheticcpspops[[popnum]][[j]][samplerule(i,1:160,j)],hrmis,un)}
          else{ll<-
            cbind((list.tablespop[[j]])[Toussamples[[i]]$Samplei[,j],],
                  hrmis,un)
          ll[["pumlrR"]]<-pluserror(ll[["pumlrR"]],diff[,j,i])
          ll}})
        names(list.tables)<-names(list.tablespop)
        #MRR
        if(is.element("MRR",what)){
          list.yMR=c("pumlrR")
          list.xMR=c("pumlrR")
          file=paste0("MRRcomp",adde1,i,sep="")
          mrr<-  MR(list.tables=list.tables, w=w, id=id, 
                    list.xMR=list.xMR, list.x1=list.x1, list.x2=list.x2,list.y=list.yMR, list.dft.x2=list.dft.x2,Alpha=Alpha,theta=3/4)
          assign(file,mrr)
          eval(parse(text=Sauve(file,adde2)))}
        #MRR
        if(is.element("S2check",what)){
          list.y=c("pumlrR")
          file=paste0("S2checkcomp",adde1,i,sep="")
          s2<-  WS(list.tables=list.tables, w=w, list.y=list.y)
          assign(file,s2)
          eval(parse(text=Sauve(file,adde2)))}
        
        
        #BCL0
        if(is.element("BCL0",what)){
          file=paste("BCL0comp",adde1,i,sep="")
          assign(file,
                 BCL0(list.tables,
                      w,
                      id,
                      list.x2="pumlrRlag", #external
                      list.y="pumlrR",
                      calibmethod="linear",
                      list.dft.x2=list.dft.x2BCL0))
          eval(parse(text=Sauve(file,adde2)))}
        if(is.element("BCL2",what)){
          list.yMR=c("pumlrR")
          list.xMR=c("pumlrR")
          file=paste("BCL2comp",adde1,i,sep="")
          assign(file,
                 BCL2(list.tables,
                      w,
                      Alpha=c(0,.5,.75,.9,1,2,3,4,5,10,20),
                      id,list.xMR="pumlrR",
                      list.x1=list.x1, list.x2=list.x2,list.y=list.yMR, list.dft.x2=list.dft.x2))
          eval(parse(text=Sauve(file,adde2)))}
        
        return(0)})
      machin<-function(toto,dimee=FALSE,adde2=adde2){
        if(is.element(toto,what)){
          sapply(paste0(toto,"comp",adde1,1:nrep,adde2),charge)  
          XX=addUto1000matrices(toto,"_rep")
          assign(paste0(toto,"comprep"),XX)
          if(dimee){dimnames(XX)[[3]]<-paste0(toto,dimnames(XX)[[3]])}
          eval(parse(text=Sauve(paste0(toto,"comprep"),adde2)))
          rm(list=paste0(toto,"comp_rep",1:nrep))
          system(paste0("cd ",Resultsfolder,";rm ",paste(paste0(toto,"comp",adde1,1:nrep,adde2,".Rdata"),collapse=" ")))}
      }
      machin("BCL2",TRUE,adde2)        
      machin("MRR",TRUE,adde2)
      machin("BCL0",TRUE,adde2)}}}

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

BestAK3

# Table 3

Best alpha

# Figure 1

Relative MSE Direct best ak best alpha

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
