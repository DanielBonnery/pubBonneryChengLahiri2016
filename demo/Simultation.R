#library("CompositeRegressionEstimation)

  progfolder<-getwd()
  CREfolder<-"../CompositeRegressionEstimation/R"
  pubfolder<-"R"
  Resultsfolder<-"/home/daniel/R/Resultats"
  if(wd5t){Resultsfolder<-paste0(getwd(),"/../Resultats")}
  #Resultsfolder<-"/home/daniel/R/Ressauv"
  tablesfolder<-"/home/daniel/R/tables"


#List of tables
startingyear<-2005
startingmonth<-1
currentyear<-2012
currentmonth<-1
listpumlrRmod <- c(0,1,"_1")
listpumlrmod <- c(1:7,"_1")
listpumlrR<-paste0("pumlrR_n",listpumlrRmod)
listpumlr<-paste0("pumlr_n",listpumlrmod)
studyvar<-c("unemployment",listpumlrR)
mc.cores<-if(wd5t){100}else{6}
adde2f<-function(bias,popnum){c(outer(paste0("_",bias),popnum,paste0))}
biass=c("","bias")
popnums=1:3
list.adde2<-adde2f(biass,popnums)



sapply(c("0.creepackage.R",
         "1.1.listetables.R",
         "1.2.table_generation_functions.R",
         "1.3.import_functions.R",
         "1.4.loadtables.R",
         if(!PC){"abind.r"}else{character(0)},
         if(FALSE){"1.5.cpsmicrodatadownload.R"}else{character(0)},
         "2.0.Functions_to_save_or_load_results.R",
         "2.1.compute_total_estimates.R",
         "2.1.1.estimatesigma.R",
         if(PC){c("2.1.compute_total_estimates_on rep.R",
                  "2.2.Functions_to_compute_employment_rates.R",
                  "2.3.Compute_employment_rates_on_replications_functions.R",
                  "2.4.Function_to_compute_employment_rates_on_true_data.R",
                  "3.1.functions_for_graphics.R",
                  "3.2.functions_for_analysis.R",
                  "3.3.functions_that_create_all_graphics.R")}else{character(0)}),
       function(file){source(paste("../CompositeRegressionEstimation/R",file,sep="/"));return(0)})
for(f in c(list.files(path=pubfolder,pattern="\\.R$"),list.files(path=pubfolder,pattern="\\.r$"))){
  print(f)
  if(!is.element(tolower(f),tolower(c("CRE-internal.R","CRE-Ex.R")))){
    source(paste(CREfolder,f,sep="/"))}}
rm(f)
##----------------------------------------------------------------                                                                                                                                                                                                                                                              
tables.entree<-lta(startingmonth,currentmonth,startingyear,currentyear)

nmois <- length(tables.entree)
nmonth<-nmois
##----------------------------------------------------------------
#Get or creation of tables
Createfalsetables(tables.entree)

##----------------------------------------------------------------
#Load tables in R
    list.tablespop<-mclapply(paste0(tablesfolder,"/",tables.entree,"_pop.csv"),read.csv)
    list.tablespop<-Chargetablespop(list.tablespop)
    names(list.tablespop)<-tables.entree
    save(list.tablespop,file=paste0(tablesfolder,"/list.tablespop.Rdata"))
    
    load(paste0(tablesfolder,"/Toussamples.Rdata"))
    hrmis=as.factor(rep(8:1,each=100))
    list.tables<-lapply(1:85,function(j){
      cbind((list.tablespop[[j]])[Toussamples[[1]]$Samplei[,j],],
            hrmis)  })
    names(list.tables)<-tables.entree
    save(list.tables,file=paste0(tablesfolder,"/list.tablessimu.Rdata"))
    #list.tablesregroup <- Regroupe(list.tables,"hwniwgt")
    #names(list.tablesregroup)<-tables.entree
    #save(list.tablesregroup,file=paste0(tablesfolder,"/list.tablesregroupsimu.Rdata"))
    rm(list.tablespop,list.tables)#,list.tablesregroup)
    
    lmonyea<-lmoye(12,currentmonth,startingyear,currentyear)
    list.tables <-lapply(lmonyea,creeRtablefromDB)
    list.tables<-Chargetables(list.tables)
    save(list.tables,file=paste0(tablesfolder,"/list.tablesweb.Rdata"))
    #list.tablesregroup <- Regroupe(list.tables,"hwniwgt")
    #save(list.tablesregroup,file=paste0(tablesfolder,"/list.tablesegroupweb.Rdata"))

        list.tablesbrut<-Importesas(wd,tables.entree)
    list.tables<-Chargetables(list.tablesbrut)
    save(list.tables,file=paste0(tablesfolder,"/list.tablesserv.Rdata"))
    #    list.tablesregroup <- Regroupe(list.tables,"hwniwgt")
    #   save(list.tablesregroup,file=paste0(tablesfolder,"/list.tablesregroupserv.Rdata"))}
    rm(list.tables)    
    #rm(list.tablesregroup)
  
#Launch computations
typetable<-c("simu")
  pcserv="simu"
    load(paste0(tablesfolder,"/list.tablessimu.Rdata"))
    computemistotals(list.tables)
    
    if(steps[4]){  
      if(simu){
        load(paste0(tablesfolder,"/list.tablespop.Rdata"))
        list.dft.x2<- lapply(list.tablespop,function(l){WS(list(l),list.y=c("pehspnon","pesex"))})
        eval(parse(text=Sauve("list.dft.x2",name.add="")));rm(list.dft.x2)
        rm(list.tablespop)}
      if(web){list.dft.x2<-lapply(list.tables,function(l){WS(list(l),list.y=c("pesex"),weight="pwsswgt")})}
      if(!PC ){
        list.dft.x2  <- lapply(list.tables,function(l){WS(list(l),list.y=c("pehspnon","pesex"),weight="pwsswgt")})
        #list.dft.x2H <- lapply(list.tables,function(l){WS(list(l),list.y=c("gestrec","pesex","pehspnon","prwtrace","prblnonb","peage"))})}
        #list.dft.x2R <-list.dft.x2
        #if(!PC|simu){list.dft.x2RH <-list.dft.x2H}
        
        save(list.dft.x2,file=paste("list.dft.x2.",pcserv,".Rdata",sep=""))  
        #save(list.dft.x2R,file=paste("list.dft.x2R.",pcserv,".Rdata",sep=""))
        #if(!PC|simu){save(list.dft.x2H,file=paste("list.dft.x2H.",pcserv,".Rdata",sep=""))  
        #             save(list.dft.x2RH,file=paste("list.dft.x2RH.",pcserv,".Rdata",sep=""))}
      }
      
      
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
