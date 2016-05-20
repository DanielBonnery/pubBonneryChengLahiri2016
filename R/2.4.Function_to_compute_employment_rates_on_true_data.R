computeU<-function(pcserv){
  what<-c(paste0("pumlrR_n",c("_1",0:1)),"unemployment")
  uewhat<-function(df){unemploymentcount(df)[,what]}
  #sapply(paste(c("AK","AK_papa","S2","MR","MRR","MRRH","MRP","MRPR"),"comp",APCserv,sep=""),charge)
  sapply(paste(c("AK","AK_papa","S2","MR","MRR","RA","BCL2"),"comp",pcserv,sep=""),charge)
  BCL2compU<-lapply(BCL2comp$dfEst,uewhat);
  names(BCL2compU)<-paste0("BCL2",names(BCL2compU))
  MRcompU<-lapply(MRcomp$dfEst,uewhat);
  names(MRcompU)<-paste0("MR",names(MRcompU))
  MRRcompU<-lapply(MRRcomp$dfEst,uewhat);
  names(MRRcompU)<-paste0("MRR",names(MRRcompU))
  #MRRHcompU<-lapply(MRRHcomp$dfEst,uewhat);names(MRRHcompU)<-paste0("MRRH",names(MRRHcompU))
  S2compU<-uewhat(S2comp)
  dimnames(S2compU)[[1]]<-tables.entree
  RAcompU<-lapply(lapply(RAcomp,data.frame),uewhat);names(RAcompU)<-paste0("RA",names(RAcomp))
  AKcompU<-uewhat(AKcomp)
  L=c(BCL2compU,MRcompU,MRRcompU,RAcompU,list(S2=S2compU),list(AK=AKcompU))
  A=array(unlist(L),dim=c(nrow(L[[1]]),ncol(L[[1]]),length(L)))
  dimnames(A)[[1]]<-rownames(S2compU)      
  dimnames(A)[[2]]<-colnames(S2compU)      
  dimnames(A)[[3]]<-names(L)
  Estimates<-A
  Estimatesdiff<-A[-1,,,drop=FALSE]-A[-dim(A)[1],,,drop=FALSE]
  save(Estimates,Estimatesdiff,file=paste0(Resultsfolder,"/resultats",pcserv,".Rdata"))
}


#computeU("serv")
