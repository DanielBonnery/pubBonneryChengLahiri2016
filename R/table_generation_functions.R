#' Returns the jth selected person in month m when the random number drawn is i.
#'
#' @param i An integer
#' @param j An integer
#' @param m An integer
#' @return The \code{j}th selected person in month \code{m} when the random number drawn is \code{i}.
#' @examples
#' samplerule(1,1:160,1)
samplerule<-function(i,j,m){((5*(m-1)+
                                5*(i-1)+
                                5000*((j-1)%/%5)+((j-1)%%5)+5*((j-1)%/%100)+40*((j-1)%/%400)))%%100000+1}

sampleruleS<-function(i){samplerule(i,1:100,1)}
sampleruleS<-function(i){((5*(i-1)+5000*((0:99)%/%5)+((0:99)%%5)+5*((0:99)%/%100)))%%100000+1}
samplerule2<-function(i,m){c(sapply((m+i-2)+c(1:4,13:16),sampleruleS))}
#' identical(samplerule2(1,1),samplerule(1,1:800,1))
sampleruleH<-function(i,j,m){(((m-1)+
                                (i-1)+
                                1000*((j-1))+((j-1)%/%20)+8*((j-1)%/%80)))%%20000+1}

misH<-function(i,m,misi,syntheticcpspopsHA){apply(syntheticcpspopsHA[,sampleruleH(i,(8-misi)*20+(1:20),m),,m],c(1,3),sum)}

mis<-function(i,m,misi,syntheticcpspopsA){apply(syntheticcpspopsA[,samplerule(i,(8-misi)*100+(1:100),m),,m],c(1,3),sum)}


#system.time(misestimates<-plyr::maply(expand.grid(i=1:3,m=1:85,misi=1:8),mis,syntheticcpspopsA=syntheticcpspopsA,.parallel=TRUE))
#system.time(misestimates<-plyr::maply(expand.grid(i=1:3,m=1:85,misi=1:8),mis,syntheticcpspopsA=syntheticcpspopsA))
#system.time(misestimates<-plyr::maply(expand.grid(i=1:20,m=1:85,misi=1:8),misH,syntheticcpspopsHA=syntheticcpspopsHA,.parallel=TRUE))
#system.time(misestimates<-plyr::maply(expand.grid(i=1:20,m=1:85,misi=1:8),misH,syntheticcpspopsHA=syntheticcpspopsHA))


#mis(1,2,1,syntheticcpspopsHA)
roundv<-function(n,x){y<-floor(n*x);y[length(x)]<-n-sum(y[-length(x)]);y}
rectif<-function(varr,prob_1){
  N<-length(varr)
  eff_0=unlist(table(varr))[names(prob_1)]
  eff_1<-roundv(N,prob_1)
  eff<-eff_1-eff_0
  qui<-names(eff)[eff<0]
  remps<-names(eff)[eff>0]
  varr[sample((1:N)[is.element(varr,qui)],sum(eff[eff>0]))]<-rep(remps,eff[eff>0])
  return(as.factor(varr))}
changevar<-function(varr,prob2=NULL,prob,iterative.synthetic.model="Independent"){
  var2<-varr
  zz=unique(varr)
  for(i in zz){
    n<-sum(varr==i)
      if(iterative.synthetic.model=="2dorder"){
        var2[varr==i]<-rep(zz,roundv(n,prob2[i,]))[order(runif(n))]}
    if(iterative.synthetic.model=="2dorder-indexdependent"){
      var2[varr==i]<-rep(zz,roundv(n,prob2[i,]))}}
  rectif(var2,prob)
  }

syntheticcpsdataset<-
  function(Totals,
           crossTotals=NULL,
           cluster.size=5,
           cluster.all.sample.rate=1/10,
           cluster.single.sample.size=20,
           seed=1,
           iterative.synthetic.models=c("Independent","2dorder","2dorder-indexdependent")){
    if(is.null(Totals)){
      Totals=plyr::aaply(1:85,1,function(x){y=runif(3);y/sum(y)})}
    L=nrow(Totals)
    nb.samples<-L+15
    cluster.single.sample.rate <-cluster.all.sample.rate/nb.samples
    N<-cluster.size*
      cluster.single.sample.size/
      cluster.all.sample.rate*
      (nb.samples)
    
    nb.clusters<-N/cluster.size
    sample.rate<-8*cluster.single.sample.size/nb.clusters
    set.seed(seed)
    
    prob <- Totals/apply(Totals,1,sum)
    prob2<-plyr::aaply(crossTotals,1:2,function(x){matrix(x/sum(x))})
    dimnames(prob2)<-dimnames(crossTotals)
    Hmisc::label(prob2)<-"P(PumlrR(t+1)=j|PumlrR(t)=i)"
    
    creevar<-function(nmod,prob=NULL){
      modal <- nmod
      if( is.integer(nmod)&length(nmod)==1){modal <- 1:nmod} 
      as.factor(sample(modal,N,replace=TRUE, prob=prob))}
    
    creevarpumlr<-function(prob,N=100000){
      return(rep(names(prob),roundv(N,prob))[order(runif(N))])}
    
    
    Popu<-data.frame(
      hrlongid=rep(1:(N/cluster.size),each=cluster.size),
      pulineno=rep(1:cluster.size,N/cluster.size),
      pwsswgt=rep(1/sample.rate,N),
      pumlrR      =creevarpumlr(prob[1,],N),
      pumlrRlag   =creevarpumlr(prob[1,],N))
    
    LL<-lapply(iterative.synthetic.models,function(j){
      list.tablespop<-list(Popu)
      Popu2=Popu
      for (i in (2:L)){
        Popu2<-data.frame(
          hrlongid=Popu$hrlongid,
          pulineno=Popu$pulineno,
          pwsswgt=Popu$pwsswgt,
          pumlrRlag   =Popu$pumlrR,
          pumlrR      =changevar(Popu2$pumlrR,prob2[i-1,,],prob[i,],iterative.synthetic.model=j))
        list.tablespop<-c(list.tablespop,list(Popu2))}
      names(list.tablespop)<-dimnames(Totals)$month
      Hmisc::label(list.tablespop,paste0('Synthetic "CPS populations" datasets, ',j," method"))
      list.tablespop})
    names(LL)<-iterative.synthetic.models
    Hmisc::label(LL)<-'Synthetic "CPS populations" datasets'
    LL
    }

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

syntheticcpspopsbf<-function(syntheticcpspops){
  plyr::llply(syntheticcpspops,function(l){
    plyr::llply(l,function(ll){
      ll[["pumlrR"]][sample((1:nrow(ll))[ll$"pumlrR"=="1"],ceiling(0.2*sum(ll$"pumlrR"=="1")))]<-"0"
      ll
    })
  })}

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

ChargetablespopA<-function(list.tablespop){
  plyr::laply(list.tablespop,
              function(l){
                model.matrix(~0+pumlrR,l)})}

syntheticccpspopHAf<-function(list.tablespop){
  list.tablespopA<-
    array(unlist(lapply(list.tablespop,function(l){
      apply(array(
        cbind(pumlrR_n0=l$pumlrR=="0",
              pumlrR_n1=l$pumlrR=="1",
              pumlrR_n_1=l$pumlrR=="_1")*1,c(5,20000,3)),2:3,sum)})),c(20000,3,length(list.tablespop)))
  dimnames(list.tablespopA)[2:3]<-list(c("0","1","_1"),names(list.tablespop))
  names(dimnames(list.tablespopA))<-c("h","y","m")
  list.tablespopA}


#sapply(1:3,list.tablespopAf)