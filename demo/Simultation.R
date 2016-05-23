#library("CompositeRegressionEstimation)
library(pubBonneryChengLahiri2016)
#List of tables
if(FALSE){allmonthsD<-seq(as.Date("20050101", "%Y%m%d"),
                as.Date("20120101", "%Y%m%d"),
                by="month")
allmonths <- format(seq(as.Date("20050101", "%Y%m%d"),
                       as.Date("20120101", "%Y%m%d"),
                       by="month"), "%b%Y")
allmonthsD2<-as.Date(paste0("01",allmonths),"%d%b%Y")}

startingyear<-2005
startingmonth<-1
currentyear<-2012
currentmonth<-1
listpumlrRmod <- c(0,1,"_1")#factor(c("Unemployed","Employed","Not in the labor force"))
listpumlrmod <- c(1:7,"_1")#factor(c("Unemployed","Employed","Not in the labor force"))
listpumlrR<-paste0("pumlrR_n",listpumlrRmod)
listpumlr<-paste0("pumlr_n",listpumlrmod)
studyvar<-c("unemployment",listpumlrR)
#adde2f<-function(bias,popnum){c(outer(paste0("_",bias),popnum,paste0))}
biass=c("","bias")
popnums=1:3
list.adde2<-(function(bias,popnum){c(outer(paste0("_",bias),popnum,paste0))})(biass,popnums)

##----------------------------------------------------------------                                                                                                                                                                                                                                                              
tables.entree<-lta(startingmonth,currentmonth,startingyear,currentyear)

nmois <- length(tables.entree)
nmonth<-nmois
##----------------------------------------------------------------
#Create synthetic datasets of tables


Totals      <-
  plyr::aaply(
    pubBonneryChengLahiri2016::S2comp,1,function(x){
  c("0"=sum(x[paste0("pumlr_n",3:4)]),
  "1"=sum(x[paste0("pumlr_n",1:2)]),
  "_1"=sum(x[paste0("pumlr_n",5:7)]))})
names(dimnames(Totals))<-c("month","pumlrR")
crossTotals<-CountsChangePumlrR


syntheticcpspops<-syntheticcpsdataset(Totals,crossTotals)
Hmisc::label(syntheticcpspops,'Synthetic "CPS populations" datasets')
#Draw all probable samples
system.time(Toussamples<-Createtoutsamples(dimnames(Totals)$month))
system.time(Toussamples2<-Createtoutsamples2(dimnames(Totals)$month))

#
hrmis=factor(rep(8:1,each=100))

list.tables<-lapply(1:85,function(j){cbind((list.tablespop[[j]])[Toussamples[[1]]$Samplei[,j],],hrmis)  })
names(list.tables)<-tables.entree
save(list.tables,file=paste0(tablesfolder,"/list.tablessimu.Rdata"))
#list.tablesregroup <- Regroupe(list.tables,"hwniwgt")
#names(list.tablesregroup)<-tables.entree
#save(list.tablesregroup,file=paste0(tablesfolder,"/list.tablesregroupsimu.Rdata"))
rm(list.tablespop,list.tables)#,list.tablesregroup)


computemistotals(list.tables)

if(steps[4]){  
  if(simu){
    load(paste0(tablesfolder,"/list.tablespop.Rdata"))
    list.dft.x2<- lapply(list.tablespop,function(l){WS(list(l),list.y=c("pehspnon","pesex"))})
    eval(parse(text=Sauve("list.dft.x2",name.add="")));rm(list.dft.x2)
    rm(list.tablespop)}
  if(web){list.dft.x2<-lapply(list.tables,function(l){WS(list(l),list.y=c("pesex"),weight="pwsswgt")})}

  if(simu){
    
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
