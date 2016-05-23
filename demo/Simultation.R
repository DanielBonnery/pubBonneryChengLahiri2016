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

syntheticcpspopsA<-plyr::laply(syntheticcpspops,list.tablespopAf,.parallel=TRUE)
gc()

#Draw all probable samples
Allsamples<-Allsamplesf(dimnames(Totals)$month)
AllsamplesH<-Allsamples[,((0:159)*5)+1,]-1/5+1
#Computation of month in sample totals.

system.time(list.tablesA<-plyr::laply(syntheticcpspopsA,list.tablesAf,ToussamplesH=AllsamplesH,.parallel=TRUE))

    dd<-dim(list.tablesA)
    mis<-array(list.tablesA,c(dd[1]/8,8,dd[2:4]))
    mistotalscomp<-aperm(8*125*apply(mis,2:5,sum),c(3,1,2,4))[,8:1,,]
    dimnames(mistotalscomp)[[1]]<-tables.entree
    dimnames(mistotalscomp)[[2]]<-paste0("hrmis",1:8)
    dimnames(mistotalscomp)[[3]]<-dimnames(list.tablesA)[[2]]
    dimnames(mistotalscomp)[[4]]<-paste0("rep",1:nrep)
    mistotalscomp<-mistotalscomp[,,listpumlrR,]
    eval(parse(text=Sauve("mistotalscomp",adde2)))
    mtc<-array(aperm(mistotalscomp,c(3,2,1,4)),c(prod(dim(mistotalscomp)[1:3]),dim(mistotalscomp)[4]))
    eval(parse(text=Sauve("mtc",adde2)))})}



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
