library(CompositeRegressionEstimation)
library(pubBonneryChengLahiri2016)
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
dimnames(syntheticcpspopsHA)<-
gc()

#Draw all probable samples
#Allsamples<-Allsamplesf(dimnames(Totals)$month)
#AllsamplesH<-Allsamples[,((1:160)*5),]/5
#Computation of month in sample totals.

#list.tablesA<-list.tablesAf(syntheticcpspopsHA,AllsamplesH)
misestimates<-125*plyr::maply(expand.grid(i=1:1000,m=1:85,misi=1:8),misH,syntheticcpspopsHA=syntheticcpspopsHA,.progress="text")
dimnames(misestimates)<-list(1:1000,dimnames(syntheticcpspopsHA)[c(1,3,4)])
names(dimnames(misestimates))<-c("i (longitudinal sample)", "m (month)","j (month in sample)","Employment status")
Hmisc::label(misestimates)<-"Month in sample estimate for longitudinal sample i, month m, rotation group mis j"


#Computation of direct estimator
#First method:
Direct<-plyr::aaply(misestimates,c(),mean)
#second method



#Computation of month in sample totals
hrmis=factor(rep(8:1,each=100))



    what="S2"
    what="MRR"
    what="S2"
    what="S";what="mistotals";what="AK2_papa";what="S2";what="MRR";what="BCL2";what="BCL0";what="BCLratio"
    what=c("AK2_papa","S2")
    what=c("MRR","BCL2","BCL0","BCLratio")
    u=1:nrep
    u=NULL
    popnum=1
    popnum=2
    bias=""
    bias="bias"
    
    #   Compute_S2MRAK_rep(what=what,u=u)
    u=1:nrep;
    what=c("mistotals","covar","bestak")
    Compute_S2MRAK_rep(what=what,u=u,popnum=2)
    Compute_S2MRAK_rep(what=what,u=u,popnum=3)
    what=c("AK2_papa","S2","MRR","BCL2","BCL0","BCLratio")
    Compute_S2MRAK_rep(what=what,u=u,popnum=2)
    Compute_S2MRAK_rep(what=what,u=u,popnum=3)
    bias="bias"
    what=c("mistotals","covar","bestak")
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=2)
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=3)
    what=c("AK2_papa","S2","MRR")#,"BCL0","BCLratio")
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=2)
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=3)
    what=c("BCL2")#,"BCL0","BCLratio")
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=2)
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=3)
    
    
    what=c("coeffYF")#,"BCL0","BCLratio")
    Compute_S2MRAK_rep(what=what,u=u,popnum=1)
    Compute_S2MRAK_rep(what=what,u=u,popnum=2)
    Compute_S2MRAK_rep(what=what,u=u,popnum=3)
    
    what=c("YF","S2lin")#,"BCL0","BCLratio")
    Compute_S2MRAK_rep(what=what,u=u,popnum=1)
    Compute_S2MRAK_rep(what=what,u=u,popnum=2)
    Compute_S2MRAK_rep(what=what,u=u,popnum=3)
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=1)
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=2)
    Compute_S2MRAK_rep(what=what,bias="bias",u=u,popnum=3)
    #Compute_S2MRAK_rep(what=c("AK_papa","RA","S2"),u=NULL)
    rm(list.tablespop)
  }
  
  Compute_S2(list.y=if(web){c("pumlrR")}else{list.y=c("pumlr","pehspnon","pesex")})
  Compute_AK(list.tables=list.tables,
             list.y=if(web){c("pumlrR")}else{list.y=c("pumlr","pehspnon","pesex")},
             id=if(web){c("hrlongid","pulineno")}else{NULL},
             groupvar=if(web){NULL}else{"hrmis"})
  #Compute_Ratio(list.tables,pcserv)
  computemistotalssimple(list.tables)
  Compute_AK2(list.tables=list.tables)
  
  #Compute_MRP()
  #Compute_MRPR()
  #Compute_BCL()
  Compute_BCL2(list.tables)
  Compute_MR()
  Compute_MRR(list.tables,pcserv)
  #rm(list.tables)
  #load(paste0(tablesfolder,"/list.tablesregroup",pcserv,".Rdata"))
  #Compute_MRRH()
  #rm(list.tablesregroup)
}}}
#Analysis
if(steps[6]){if(PC){
  analysisalpha(TRUE)}}
