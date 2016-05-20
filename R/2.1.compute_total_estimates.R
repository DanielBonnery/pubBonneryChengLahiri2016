Regroupe <- function(list.tables,weight)
{lapply(list.tables,
        function(table){
          facto <- table[setdiff(names(table),"puhhmem")]
          facto$count <- 1
          facto <- factorisedf(facto,list.y=setdiff(names(facto),c("hrlongid","pulineno","hrmis")))
          facto <- cbind(table[setdiff(names(table),c(names(facto$fdf),"pulineno",facto$aconvertir))],facto$fdf)
          #for(nom in setdiff(names(facto),c("hrlongid","pulineno","hrmis"))){
          #facto[nom] <- facto$pwsswgt*facto[nom]}
          facto <- aggregate(facto[,setdiff(names(facto),c("hrmis","puhhmem", "gesbwgt"
                                                           ,"pwsswgt",  "pwcmpwgt" )) ],by=list(as.factor(as.vector(paste(facto[["hrlongid"]],facto[["hrmis"]],sep="_")))),"sum")
          facto$hrlongid <-sapply(strsplit(as.character(facto$Group.1),split="_"),function(i){i[1]})
          facto$hrmis <- sapply(strsplit(as.character(facto$Group.1),split="_"),function(i){i[2]})
          facto$weight <- facto[weight]/facto$count
          facto <- facto[,names(facto)!="Group.1"]
          return(facto)
        })}
Compute_pop <- function(list.tablespop=list.tablespop,adde2=""){
  list.y=c("pumlrR","pehspnon","pesex")
  Scomppop<-WS(list.tablespop,list.y=list.y);
  eval(parse(text=Sauve("Scomppop",adde2)))}

Compute_S2 <- function(list.y=c("pumlr","pehspnon","pesex"),w="pwsswgt"){
  
  S2comp <- WS(list.tables=list.tables,
               weight="pwsswgt", list.y=list.y)
  eval(parse(text=Sauve("S2comp",pcserv)))}



Compute_BCL <- function(list.tables){
  w="pwsswgt"
  mu=NULL
  id=c("hrlongid","pulineno" )
  list.y="pumlrR"
  list.x1=NULL    #computed
  list.x2=NULL #external
  mu0=NULL
  list.xBCL="pumlrR"
  calibmethod="linear"
  list.dft.x2=NULL
  dft0.xBCL=NULL
  analyse=FALSE
  calibmethod="linear"
  
  BCLcomp<-BCL(list.tables,
               w,
               id,
               list.xBCL,
               list.x1, #computed
               list.x2, #external
               list.y=NULL,
               calibmethod="linear",
               list.dft.x2,
               dft0.xBCL=NULL,
               mu0=NULL,
               analyse=FALSE)
  
  eval(parse(text=Sauve("BCLcomp",pcserv)))}


Compute_BCL2 <- function(list.tables){
  w="pwsswgt"
  mu=NULL
  id=c("hrlongid","pulineno" )
  list.y="pumlrR"
  list.x1=NULL    #computed
  list.x2=NULL #external
  mu0=NULL
  list.xBCL="pumlrR"
  calibmethod="linear"
  list.dft.x2=NULL
  dft0.xBCL=NULL
  analyse=FALSE
  calibmethod="linear"
  
  BCL2comp<-BCL2(list.tables,
                 w,
                 id,
                 Alpha=(c(0:20,30,40,45,50,55,60,65,70,80,100,150,200))/20,
                 theta=3/4,
                 list.xMR="pumlrR",
                 list.x1=NULL, #computed
                 list.x2=NULL, #external
                 list.y="pumlrR",
                 calibmethod="linear",
                 list.dft.x2=NULL,
                 dft0.xMR=NULL,
                 analyse=FALSE)
  
  
  eval(parse(text=Sauve("BCL2comp",pcserv)))}





computemistotals<-function(list.tables){
  l=list.tables[[1]]
  levg<-levels(l$gestfips)
  levp<-levels(l$pumlrR)
  vars<-c(outer(paste0("gestfips",levg),paste0("pumlrR",levp),paste,sep=":"))
  list.tablesgp<-lapply(list.tables,function(l){
    for(g in levg){
      for(p in levp){
        l[paste0("gestfips",g,":","pumlrR",p)]<-(l$gestfips==g)*(l$pumlrR==p)}}
    return(
      l[c("pwsswgt","hrmis",vars)]
    )})
  
  mistotalsgp<-WSrg(list.tablesgp,weight="pwsswgt",list.y=vars,rg="hrmis")
  
  miscountsgp<-WSrg(list.tablesgp,weight=1,list.y=vars,rg="hrmis")
  
  eval(parse(text=Sauve("mistotalsgp",pcserv)))
  eval(parse(text=Sauve("miscountsgp",pcserv)))}



computemistotalssimple<-function(list.tables){
  mistotals<-lapply(list.tables,function(l){WSrg(list.tables,weight="pwsswgt",list.y="pumlr")})
  eval(parse(text=Sauve("mistotals",pcserv)))}


Compute_MRP <- function(){
  w="pwsswgt";
  id=c("hrlongid","pulineno")
  list.xMRP="pumlr"
  list.x1=character(0)
  list.x2=character(0)
  list.y=c("pumlr")
  calibmethod="linear"
  list.dft.x2<-NULL
  dft0.xMRP=NULL
  Lagvec <-c(1,2,3,8,9,10,11,12,13,14,15)
  add.var=FALSE
  returncalibvar=FALSE
  returnweight=FALSE
  MRPcomp<-MRP(list.tables,
               w,
               id,
               list.xMRP,
               list.x1=character(0), #computed
               list.x2=character(0), #external
               list.y=list.y,
               calibmethod="linear",
               list.dft.x2,
               dft0.xMRP=NULL,
               Lagvec=c(1,2,3,8,9,10,11,12,13,14,15),
               add.var=FALSE,
               returncalibvar=FALSE,
               returnweight=FALSE)
  eval(parse(text=Sauve("MRPcomp",pcserv)))}

Compute_MRPR <- function(){
  w="pwsswgt";
  id=c("hrlongid","pulineno")
  list.xMRP="pumlrR"
  list.x1=character(0)
  list.x2=character(0)
  list.y=c("pumlr")
  calibmethod="linear"
  list.dft.x2<-NULL
  dft0.xMRP=NULL
  Lagvec <-c(1,2,3,8,9,10,11,12,13,14,15)
  add.var=FALSE
  returncalibvar=FALSE
  returnweight=FALSE
  MRPRcomp<-MRP(list.tables,
                w,
                id,
                list.xMRP,
                list.x1=character(0), #computed
                list.x2=character(0), #external
                list.y=list.y,
                calibmethod="linear",
                list.dft.x2,
                dft0.xMRP=NULL,
                Lagvec=c(1,2,3,8,9,10,11,12,13,14,15),
                add.var=FALSE,
                returncalibvar=FALSE,
                returnweight=FALSE)
  eval(parse(text=Sauve("MRPRcomp",pcserv)))}

Compute_Ratio <- function(list.tables,pcserv){
  ##----------------------------------------------------------------
  #variables declaration
  w="pwsswgt"
  id=c("hrlongid","pulineno")
  list.y=c("pumlrR")
  Alpha=c(0,.25,.5,.75,1)
  assign(paste("RAcomp",sep=""),
         RA(list.tables=list.tables, w=w, list.y=list.y,id=id,  Alpha=Alpha))
  eval(parse(text=Sauve("RAcomp",pcserv)))
}


  Compute_AK <- function(list.tables){
    ##----------------------------------------------------------------
     
    AKcomp<-WS(list.tables=list.tables, weight="pwcmpwgt", list.y="pumlr")
   eval(parse(text=Sauve("AKcomp",pcserv)))}



Compute_AK2 <- function(list.tables=list.tables,w="pwsswgt",
                        list.y="pumlr"){
  AK2_papacomp<-AK2(WSrg(list.tables,weight="pwsswgt",list.y="pumlr"), A=(0:10)/10,K=(0:10)/10)
  eval(parse(text=Sauve("AK2_papacomp",pcserv)))
}





##----------------------------------------------------------------
#Computations
#Computes for PUMLR
Compute_MR <- function(){
  ##----------------------------------------------------------------
  #variables declaration
  w="pwsswgt"
  theta=3/4
  mu=NULL
  id=c("hrlongid","pulineno" )
  list.y=if(web){"pumlr"}else{c("pumlr","pehspnon","pesex")}
  list.x1="un"    #external NULL
  list.x2=NULL#if(web){NULL}else{c("pehspnon","pesex")} #computed
  load(paste("list.dft.x2.",pcserv,".Rdata",sep=""))  
  list.xMR=c("pumlr")
  dft0.y=NULL
  Alpha<-(0:20)/20
  dft0.xMR=NULL
  mu0=NULL
  calibmethod="linear"
  Singh=TRUE 
  list.tables<-lapply(list.tables,function(l){l$un=1;return(l)})
  
  MRcomp<-MR(list.tables=list.tables, w=w, id=id, list.xMR=list.xMR, 
             list.x1=list.x1, list.x2=list.x2,list.y=list.y, list.dft.x2=NULL,#list.dft.x2,
             Alpha=Alpha,theta=theta)
  save(MRcomp,file=paste(Resultsfolder,"/MRcomp",pcserv,".Rdata",sep=""))}



Compute_MRR <- function(list.tables,pcserv){
  ##------------------------------------------------------------
  #variables declaration
  w="pwsswgt"
  theta=3/4
  mu=NULL
  id=c("hrlongid","pulineno" )
  list.y=c("pumlrR")
  list.x1="un"    #computed
  list.x2=NULL #external
  list.dft.x2=NULL#if(!web){lapply(lapply(list.tables,list),WS,list.y=list.x2,weight=w)}else{NULL}
  list.xMR=c("pumlrR")
  dft0.y=NULL
  dft0.xMR=NULL
  mu0=NULL
  calibmethod="linear"
  Alpha<-(0:20)/20
  Singh=TRUE
  list.tables<-lapply(list.tables,function(l){l$un=1;return(l)})
  ##----------------------------------------------------------------
  #Computations
  MRRcomp<-MR(list.tables=list.tables, w=w, id=id, list.xMR=list.xMR, list.x1=list.x1, list.x2=list.x2,list.y=list.y, list.dft.x2=list.dft.x2,Alpha=Alpha,theta=3/4)
  save(MRRcomp,file=paste(Resultsfolder,'/MRRcomp',pcserv,'.Rdata',sep=''))}

#Computes for emploi
Compute_MRRH <- function(){
  ##-----------------------------------------------------------------#variables declaration
  w="hwniwgt"
  theta=3/4
  Alpha=(0:20)/20
  Alphaadd<-c(.75,.95)
  mu=NULL
  id=c("hrlongid")
  list.y=paste("pumlrR_n",0:1,sep="")
  list.x1=NULL    #external
  list.x2=setdiff(c(paste("gestrec",1:52,sep='_n'),paste("pesex",1:2,sep='_n'),"peage","pehspnon_n1",paste("prwtrace",1:2,sep="_n"),"prblnonb_n1"),
                  paste("gestrec",c(3,6,7,14,36,43,52),sep="_n"))#computed
  load(paste("list.dft.x2RH.",pcserv,".Rdata",sep=""))
  list.xMR=paste("pumlrR",c("_1","0","1"),sep="_n")
  dft0.y=NULL
  dft0.xMR=NULL
  mu0=NULL
  calibmethod="linear"
  add.var<-FALSE
  ##--------------------------------------------------------------------------
  #Computations
  MRRHcomp<-MR(list.tables=list.tablesregroup, w=w, id=id, list.xMR=list.xMR, list.x1=list.x1, list.x2=list.x2,list.y=list.y, list.dft.x2=list.dft.x2RH,Alpha=Alpha,theta=3/4,add.var=FALSE)
  MRRHwcomp<-MR(list.tables=list.tablesregroup, w=w, id=id, list.xMR=list.xMR, list.x1=list.x1, list.x2=list.x2,list.y=list.y, list.dft.x2=list.dft.x2RH,Alpha=Alphaadd,theta=3/4,add.var=TRUE)
  save(MRRHcomp,file=paste(Resultsfolder,'/MRRHcomp',pcserv,'.Rdata',sep=''))
  save(MRRHwcomp,file=paste(Resultsfolder,'/MRRHwcomp',pcserv,'.Rdata',sep=''))}






if(FALSE){
  a1000tot<-
    abind(S2comprep[,studyvar,,,drop=FALSE],
          BCLcomprep[,studyvar,,,drop=FALSE],
          BCL0comprep[,studyvar,,,drop=FALSE],
          MRRcomprep[,studyvar,,,drop=FALSE],
          #calcvarmeana(MRRHcomprep),
          #RAcomprep[,studyvar,,,drop=FALSE],
          #            AK_papacomprep,
          AK2_papacomprep[,studyvar,,,drop=FALSE],
          #MRcomprep[,studyvar,,,drop=FALSE],
          along = 3)
  
  
  
  
  
  a1000totdiff<-difff(a1000tot)
  eval(parse(text=Sauve("a1000tot","")))
  eval(parse(text=Sauve("a1000totdiff","")))
  
  compUrepdiff<-calcvarmeana(a1000totdiff,Ecomp=ScomppopUdiff[,studyvar])
  
  a1000totdiff<-difff(a1000tot)
  eval(parse(text=Sauve("a1000tot","")))
  eval(parse(text=Sauve("a1000totdiff","")))
  
}