roundv<-function(n,x){y<-floor(n*x);y[length(x)]<-n-sum(y[-length(x)]);y}
rectif<-function(varr,prob_1){
  eff_0=unlist(table(varr))
  names(eff_0)<-paste0("pumlrR_n",names(eff_0))
  eff_0<-eff_0[listpumlrR]
  eff_1<-roundv(N,prob_1)[listpumlrR]
  eff<-eff_1-eff_0
  qui<-substr(names(eff)[eff<0],9,11)
  remps<-substr(names(eff)[eff>0],9,11)
  varr[sample((1:N)[is.element(varr,qui)],sum(eff[eff>0]))]<-rep(remps,eff[eff>0])
  return(as.factor(varr))}
changevar<-function(varr,prob2,prob,toujourslesmemes=1){
  var2<-varr
  for(i in listpumlrRmod){
    n<-sum(varr==i)
    if(toujourslesmemes==2){
      var2[varr==i]<-rep(listpumlrRmod,roundv(n,prob2[,i]))[order(runif(n))]}
    if(toujourslesmemes==3){var2[varr==i]<-rep(listpumlrRmod,roundv(n,prob2[,i]))}}
  rectif(var2,prob)}


Createfalsetables<-function(Total=NULL,
                            T=if(is.null(Total)){85}else{
                              if(!is.null(names(Total)[[1]])){names(Total)[[1]]}else{dim(Totals)[[1]]}},
                            cluster.size=5,
                            cluster.all.sample.rate=1/10,
                            cluster.single.sample.size=20,
                            seed=1){
  if(length(T)==1!is.integer(T)){T=1:T}
  if(is.null(Total)){Total=apply(plyr::aaply(T,function(x){y=runif(3))}
  nmois<-length(tables.entree)
  nb.samples<-nmois+15
  cluster.single.sample.rate <-cluster.all.sample.rate/nb.samples
  N<-cluster.size*
    cluster.single.sample.size/
    cluster.all.sample.rate*
    (nb.samples)
  
  nb.clusters<-N/cluster.size
  sample.rate<-8*cluster.single.sample.size/nb.clusters
  set.seed(seed)
  
  S2comprop <- Estimates[,listpumlrR,"S2"]
  S2comprop <- S2comprop/apply(S2comprop,1,sum)
  prob <- S2comprop[,listpumlrR]
  
  creevar<-function(nmod,prob=NULL){
    modal <- nmod
    if( is.integer(nmod)&length(nmod)==1){modal <- 1:nmod} 
    as.factor(sample(modal,N,replace=TRUE, prob=prob))}
  
  creevarpumlr<-function(prob,N=100000){
    return(rep(listpumlrRmod,roundv(N,prob))[order(runif(N))])}
  
  
  charge("doubbleserv")
  prob2<-lapply(doubble,function(l){apply(l$N01,2,function(x){x/sum(x)})})
  
  
  Popu<-data.frame(
    hrlongid=rep(1:(N/cluster.size),each=cluster.size),
    pulineno=rep(1:cluster.size,N/cluster.size),
    pwsswgt=rep(1/sample.rate,N),
    pumlrR   =creevarpumlr(prob[1,]),
    pumlrRlag   =creevarpumlr(prob[1,]))
  
  lapply(1:3,function(j){
    list.tablespop<-list(Popu)
    for (i in (2:nmois)){
      Popu<-data.frame(
        hrlongid=Popu$hrlongid,
        pulineno=Popu$pulineno,
        pwsswgt=Popu$pwsswgt,
        pumlrRlag   =Popu$pumlrR,
        pumlrR   =changevar(Popu$pumlrR,prob2[[i-1]],prob[i,],toujourslesmemes=j))
      list.tablespop<-c(list.tablespop,list(Popu))}
    list.tablespop})}

pumlrR <- function(i){replace(x<-as.character(-1+2*is.element(i,c("1","2"))+is.element(i,as.character(3:4))),x=="-1","_1")}

#This function modify the dataframes of the list list.tables
#and returns a list of modified tables







Chargetables <- function(list.tables){
  L<-lapply(list.tables,
            function(dff){
              names(dff)<-tolower(names(dff))
              colnames<-intersect(tolower(c("GESTFIPS","PUMLR","HRMIS","puhhmem",
                                            'prpertyp',"pumlrlag",
                                            'pesex'   ,
                                            'prwtrace', 
                                            'pehspnon', 
                                            'PRBLNONB', 
                                            'GESTREC')),names(dff))
              colnamesnum<-setdiff(tolower(names(dff)),colnames)
              dff[colnamesnum] <- lapply(dff[colnamesnum] ,as.numeric)
              dff[colnames] <- lapply(dff[colnames] ,as.factor)
              dff<-transform(dff,pwsswgt=pwsswgt/10000)
              dff<-transform(dff,pwcmpwgt=pwcmpwgt/10000)
              if(is.element("hwniwgt",colnames)){dff<-transform(dff,hwniwgt=hwniwgt/10000)}
              dff$pumlrR <- as.factor(pumlrR(dff$pumlr))
              if(is.element("pumlrlag",names(dff))){dff$pumlrRlag <- as.factor(pumlrR(dff$pumlrlag))
              }
              #assign(tables.entree[[i]],dff, envir = .GlobalEnv)
              return(dff)})
  names(L)<-names(list.tables)
  return(L)}
#This function modify the dataframes of the list list.tablespop
#and returns a list of modified tables


Chargetablespop <- function(list.tablespop){
  L<-mclapply(list.tablespop,
              function(dff){
                names(dff)<-tolower(names(dff))
                colnames<-intersect(tolower(c("GESTFIPS","PUMLR","puhhmem",
                                              'prpertyp',"pumlrlag",
                                              'pesex'   ,
                                              'prwtrace', 
                                              'pehspnon', 
                                              'PRBLNONB', 
                                              'GESTREC')),names(dff))
                colnamesnum<-setdiff(tolower(names(dff)),colnames)
                dff[colnamesnum] <- lapply(dff[colnamesnum] ,as.numeric)
                #dff[colnames] <- lapply(dff[colnames] ,as.character, stringsAsFactors=FALSE)
                dff[colnames] <- lapply(dff[colnames] ,as.factor)
                dff<-transform(dff,pwsswgt=pwsswgt/10000)
                dff<-transform(dff,pwcmpwgt=pwcmpwgt/10000)
                dff<-transform(dff,hwniwgt=hwniwgt/10000)
                dff$pumlrR <- as.factor(pumlrR(dff$pumlr))
                dff$pumlrRlag <- as.factor(pumlrR(dff$pumlrlag))
                
                #assign(paste(tables.entree[[i]],"_pop",sep=""),dff, envir = .GlobalEnv)
                return(dff)})
  names(L)<-names(list.tablespop)
  return(L)}

ChargetablespopA<-function(){
  load(paste0(tablesfolder,"/list.tablespop.Rdata"))
  tablespopA<-do.call(abind,c(lapply(list.tablespop,function(l){
    model.matrix(~0+pumlrR,l)}),list(along=3)))  
  save(tablespopA,file=paste0(tablesfolder,"/tablespopA.Rdata"))
}

list.tablespopAf<-function(popnum){
  load(paste0(tablesfolder,"/list.tablespop",popnum,".Rdata"))
  list.tablespopA<-
    array(unlist(lapply(list.tablespop,function(l){
      apply(array(
        cbind(pumlrR_n0=l$pumlrR=="0",
              pumlrR_n1=l$pumlrR=="1",
              pumlrR_n_1=l$pumlrR=="_1")*1,c(5,20000,3)),2:3,sum)})),c(20000,3,length(list.tablespop)))
  dimnames(list.tablespopA)[[2]]<-listpumlrR
  save(list.tablespopA,file=paste0(tablesfolder,"/list.tablespopA",popnum,".Rdata"))}
#sapply(1:3,list.tablespopAf)