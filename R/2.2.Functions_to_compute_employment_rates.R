#----------------------------------------------------------------------------
# unemployment applied on dataframes 
addUto1000matrices<-function(toto,adde){
  nnames<-toto
  obje<-get(paste0(toto,"comp",adde,1))
  dfest<-is.element("dfEst",names(obje))
  if(dfest){obje=obje$dfEst}
  if(is.list(obje)&!is.data.frame(obje)){nnames<-names(obje);ll3<-length(obje)} else{
    if (length(dim(obje))==2){ll3<-1;nnames=toto}else{
      if(is.null(dim(obje)[3])){ll3<-1;}else{ll3<-numeric(0);nnames=dimnames(obje)[[3]]}}}
  L<-lapply(1:nrep,function(i){
    obje<-get(paste0(toto,"comp",adde,i))
    if(dfest){obje=obje$dfEst}
    if(!is.list(obje)|is.data.frame(obje)){obje<-list(obje)}
    return(lapply(obje,function(thearray){
      unemploymentcountarray(thearray,2)}))})
  dime=c(dim(L[[1]][[1]]), ll3,length(L))
  A<-      array(unlist(L), dim = dime)
  dimnames(A)[[1]]<-tables.entree
  dimnames(A)[[2]]<-studyvar
  dimnames(A)[[3]]<-nnames
  dimnames(A)[[4]]<-paste0("rep",1:nrep)
  return(A)}



addU<-function(thearray){
  thearrayU<-abind(thearray,
                   apply(thearray,(1:length(dim(thearray)))[-2],
                         function(x){x["pumlrR_n0"]/(x["pumlrR_n0"]+x["pumlrR_n1"])})
                   ,along=2)
  dimnames(thearrayU)[[2]]<-c(dimnames(thearray)[[2]],"unemployment")
  return(thearrayU)
}  


calcvarmeana <- function(comp,Ecomp=ScomppopU[,studyvar]){
  meane <- apply(comp[,studyvar,,,drop=FALSE],1:3,mean)
  vare <- apply(comp[,studyvar,,,drop=FALSE],1:3,function(x){var(x)*(1-1/length(x))})
  biase <- apply(meane,3,function(x){x-Ecomp[,studyvar]})
  biase<-array(biase,c(dim(biase)[1]/length(studyvar),length(studyvar),dim(biase)[2]))
  dimnames(biase)[[2]]<-studyvar
  msee <- biase^2+vare
  una<-comp[,studyvar,,1,drop=FALSE]
  relbiase <- apply(meane,3,function(x){(x-Ecomp)/Ecomp})
  relbiase<-array(relbiase,c(dim(relbiase)[1]/length(studyvar),length(studyvar),dim(relbiase)[2]))
  cv <- apply(msee,3,function(x){sqrt(x)/Ecomp})
  cv<-array(cv,c(dim(cv)[1]/length(studyvar),length(studyvar),dim(cv)[2]))
  A=array(c(meane,vare,biase,msee,una,relbiase,cv),c(dim(comp[,studyvar,,,drop=FALSE])[1:3],7))
  dimnames(A)[[1]]<-dimnames(comp)[[1]]
  dimnames(A)[[2]]<-studyvar
  dimnames(A)[[3]]<-dimnames(comp)[[3]]
  dimnames(A)[[4]]<-c("mean","var","bias","mse","una","relbias","cv")
  return(A)}
difff<-function(comp){comp[-1,studyvar,,,drop=FALSE]-
                        comp[-dim(comp)[1],studyvar,,,drop=FALSE]}



calcvarmeanadiff <- function(comp,ScomppopUdiff){   
  calcvarmeana(difff(comp),Ecomp=ScomppopUdiff[,studyvar])}
unemployment <- function(comp){
  for (a in c(1:7,"_1")[is.element(paste0("pumlr",c(1:7,"_1")),names(comp))])
  {comp[paste0("pumlr_n",a)]<-comp[paste0("pumlr",a)]}  
  for (i in 1:4){
    if (!is.element(paste("pumlr_n",i,sep=''),names(comp)))
    {comp[paste("pumlr_n",i,sep='')] <- 0}}
  comp$unemployment <-
    apply(data.matrix(comp[paste0("pumlr_n",3:4)]),1,sum)/
    apply(data.matrix(comp[paste0("pumlr_n",1:4)]),1,sum)
  comp$pumlrR_n0 <-
    apply(data.matrix(comp[paste0("pumlr_n",3:4)]),1,sum)
  comp$pumlrR_n1 <-
    apply(data.matrix(comp[paste0("pumlr_n",1:2)]),1,sum)
  comp$pumlrR_n_1 <-
    apply(data.matrix(comp[paste0("pumlr_n",c("_1",5:7))]),1,sum)
  return(comp)}


#----------------------------------------------------------------------------
# unemployment applied on dataframes and matrices
#


unemploymentcount <- function(comp){
  compdf<-comp
  if(is.matrix(comp)){compdf<-data.frame(comp)}
  names(compdf)<-names(compdf)
  for (a in c(1:7,"_1")[is.element(paste0("pumlr",c(1:7,"_1")),names(compdf))])
  {compdf[paste0("pumlr_n",a)]<-compdf[paste0("pumlr",a)]}  
  for (a in c(0,1,"_1")[is.element(paste0("pumlrR",c(0,1,"_1")),names(compdf))])
  {compdf[paste0("pumlrR_n",a)]<-compdf[paste0("pumlrR",a)]}
  rr<-any(is.element(paste0("pumlr_n",c(1:7,"_1")),names(compdf)))
  rR<-any(is.element(paste0("pumlrR_n",c(0:1,"_1")),names(compdf)))
  if(rR){
    for (i in c("_1",0:1)){
      if (!is.element(paste("pumlrR_n",i,sep=''),names(compdf)))
      {compdf[paste("pumlrR_n",i,sep='')] <- 0}}
    for (i in c(1:7,"_1")){
      compdf[paste("pumlr_n",i,sep='')] <- NA}}  
  if(rr&!rR){
    for (i in c(1:7,"_1")){
      if (!is.element(paste("pumlr_n",i,sep=''),names(compdf)))
      {compdf[paste("pumlr_n",i,sep='')] <- 0}}
    compdf$pumlrR_n_1<-compdf$pumlr_n5+compdf$pumlr_n6+compdf$pumlr_n7+compdf$pumlr_n_1
    compdf$pumlrR_n0<-compdf$pumlr_n3+compdf$pumlr_n4
    compdf$pumlrR_n1<-compdf$pumlr_n1+compdf$pumlr_n2}
  compdf$unemployment <-compdf$pumlrR_n0/(compdf$pumlrR_n0+compdf$pumlrR_n1)
  return(data.matrix(compdf))}



unemploymentcountarray <- function(thearray,dime){
  ndime<-length(dim(thearray))
  perme<-1:ndime;  perme[1]<-dime;  perme[dime]<-1
  thearray<-aperm(thearray,perme)
  nnames<-dimnames(thearray)[[1]]
  #for (a in c(1:7,"_1")[is.element(paste0("pumlr",c(1:7,"_1")),nnames)])
  #{nnames[nnames<-paste0("pumlr",a)]<-paste0("pumlr_n",a)}  
  #for (a in c(0,1,"_1")[is.element(paste0("pumlrR",c(0,1,"_1")),nnames)])
  #{nnames[nnames<-paste0("pumlrR",a)]<-paste0("pumlrR_n",a)}
  #dimnames(thearray)[[1]]<-nnames
  rr<-any(is.element(paste0("pumlr_n",c(1:7,"_1")),nnames))
  rR<-any(is.element(paste0("pumlrR_n",c(0:1,"_1")),nnames))
  if(rR){
    for (a in c("_1",0:1)){
      if(!is.element(paste0("pumlrR_n",a),nnames)){
        dime2<-dim(thearray);dime2[1]<-1
        thearray<-abind(thearray,array(0,dime2,along=1))
        dimnames(thearray)[[1]]<-c(nnames,paste0("pumlrR_n",a))
        nnames<-dimnames(thearray)[[1]]}}}
  if(rr){
    for (a in c(1:7,"_1")){
      if(!is.element(paste0("pumlr_n",a),nnames)){
        dime2<-dim(thearray);dime2[1]<-1
        thearray<-abind(thearray,array(0,dime2),along=1)
        dimnames(thearray)[[1]]<-c(nnames,paste0("pumlr_n",a))
        nnames<-dimnames(thearray)[[1]]}}}
  if(!rr){
    for (a in c(1:7,"_1")){
      dime2<-dim(thearray);dime2[1]<-1
      thearray<-abind(thearray,array(NA,dime2),along=1)
      dimnames(thearray)[[1]]<-c(nnames,paste0("pumlr_n",a))
      nnames<-dimnames(thearray)[[1]]}}
  if(rr&!rR){
    dime2<-dim(thearray);dime2[1]<-1
    thearray<-abind(thearray,
                    array(
                      apply(thearray,(1:ndime)[-1],function(x){   
                        x["pumlr_n5"]+x["pumlr_n6"]+x["pumlr_n7"]+x["pumlr_n_1"]}),dime2),along=1)
    dimnames(thearray)[[1]]<-c(nnames,"pumlrR_n_1")
    nnames<-dimnames(thearray)[[1]]
    thearray<-abind(thearray,array(
      apply(thearray,(1:ndime)[-1],function(x){   
        x["pumlr_n3"]+x["pumlr_n4"]}),dime2),along=1)
    dimnames(thearray)[[1]]<-c(nnames,"pumlrR_n0")
    nnames<-dimnames(thearray)[[1]]
    thearray<-abind(thearray,array(
      apply(thearray,(1:ndime)[-1],function(x){   
        x["pumlr_n1"]+x["pumlr_n2"]}),dime2),along=1)
    dimnames(thearray)[[1]]<-c(nnames,"pumlrR_n1")
    nnames<-dimnames(thearray)[[1]]
  }
  
  dime2<-dim(thearray);dime2[1]<-1
  thearray<-abind(thearray,array(
    apply(thearray,(1:ndime)[-1],function(x){   
      x["pumlrR_n0"]/(x["pumlrR_n0"]+x["pumlrR_n1"])}),dime2),along=1)
  dimnames(thearray)[[1]]<-c(nnames,"unemployment")
  nnames<-dimnames(thearray)[[1]]
  if (ndime==2){thearray<-thearray[studyvar,,drop=FALSE]}
  if (ndime==3){thearray<-thearray[studyvar,,,drop=FALSE]}
  if (ndime==4){thearray<-thearray[studyvar,,,,drop=FALSE]}
  if (ndime==5){thearray<-thearray[studyvar,,,,,drop=FALSE]}
  if (ndime==6){thearray<-thearray[studyvar,,,,,,drop=FALSE]}
  thearray<-aperm(thearray,perme)
  return(thearray)}




#----------------------------------------------------------------------------
# desuet
#
unemploymentR <- function(comp){
  for (a in c(0,1,"_1")[is.element(paste0("pumlrR",c(0,1,"_1")),names(comp))])
  {comp[paste0("pumlrR_n",a)]<-comp[paste0("pumlrR",a)]}
  for (i in -1:1){
    if (!is.element(paste("pumlrR_n",i,sep=''),names(comp)))
    {comp[paste("pumlrR_n",i,sep='')] <- 0}}
  comp$unemployment <-
    apply(data.matrix(comp[paste("pumlrR",0,sep='_n')]),1,sum)/
    apply(data.matrix(comp[paste("pumlrR",0:1,sep='_n')]),1,sum)
  return(comp)}


