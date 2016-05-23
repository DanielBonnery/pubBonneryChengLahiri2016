Allsamplesf2<-function(months=NULL,
                       nmois=length(months),
                       cluster.size=5,
                       cluster.all.sample.rate=1/10,
                       cluster.single.sample.size=20){
  nb.samples<-nmois+15
  cluster.single.sample.rate <-cluster.all.sample.rate/nb.samples
  N<-cluster.size*
    cluster.single.sample.size/
    cluster.all.sample.rate*
    (nb.samples)
  nb.clusters<-N/cluster.size
  sample.rate<-8*cluster.single.sample.size/nb.clusters
  nrep <- 1000
  set.seed(1)
  ecart.personne.meme.cluster.echantillon <- N/cluster.single.sample.size    
  ecart2<-cluster.size#(N/nb.clusters)/cluster.all.sample.rate
  Toussamples <- plyr::aaply(1:nrep,1,
                             function(nr){
                               starte <- cluster.size*(nr-1)#sample(1:(N/cluster.size),1)
                               monthinsample8<-sapply(1:nb.samples,
                                                      function(i){
                                                        (starte-1+(rep((0:(cluster.single.sample.size-1))*
                                                                         ecart.personne.meme.cluster.echantillon,
                                                                       each=cluster.size)+
                                                                     rep((i-1)*ecart2+(1:cluster.size),
                                                                         cluster.single.sample.size)))%%N+1})
                               sapply(1:nmois,function(i){(monthinsample8[,i+rep(c(0,12),each=4)+rep(0:3, 2)])})})
  names(dimnames(Toussamples))<-c("i (longitudinal sample)","j (sample element index)","m (month)")
  dimnames(Toussamples)<-list(1:nrep,
                              paste0("mis: ", rep(8:1,each=cluster.single.sample.size*cluster.size),", id:",
                                     rep(1:(cluster.single.sample.size*cluster.size),8)),
                              months)
  Hmisc::label(Toussamples)<-c("Population index for selected element of sample index j in month m and longitudinal sample i")
  
  Toussamples}


Allsamplesf<-function(months=NULL,
                      nmois=length(months),
                      cluster.size=5,
                      cluster.all.sample.rate=1/10,
                      cluster.single.sample.size=20){
  nb.samples<-nmois+15
  cluster.single.sample.rate <-cluster.all.sample.rate/nb.samples
  N<-cluster.size*
    cluster.single.sample.size/
    cluster.all.sample.rate*
    (nb.samples)
  nb.clusters<-N/cluster.size
  sample.rate<-8*cluster.single.sample.size/nb.clusters
  nrep <- 1000
  set.seed(1)
  ecart.personne.meme.cluster.echantillon <- N/cluster.single.sample.size    
  ecart2<-cluster.size#(N/nb.clusters)/cluster.all.sample.rate
  monthinsample8s <- plyr::aaply(1:nrep,1,
                                 function(nr){
                                   starte <- cluster.size*(nr-1)#sample(1:(N/cluster.size),1)
                                   sapply(1:nb.samples,
                                          function(i){
                                            (starte-1+(rep((0:(cluster.single.sample.size-1))*
                                                             ecart.personne.meme.cluster.echantillon,
                                                           each=cluster.size)+
                                                         rep((i-1)*ecart2+(1:cluster.size),
                                                             cluster.single.sample.size)))%%N+1})})
  Toussamples<-do.call(abind::abind,
                       c(lapply(rep(c(0,12),each=4)+rep(0:3, 2),
                                function(i){monthinsample8s[,,(1+i):(85+i)]}),along=2))
  names(dimnames(Toussamples))<-c("i (longitudinal sample)","j (sample element index)","m (month)")
  dimnames(Toussamples)<-list(1:nrep,
                              paste0("mis - ", rep(8:1,each=cluster.single.sample.size*cluster.size),
                                     rep(1:cluster.single.sample.size*cluster.size,8)),
                              months)
  Hmisc::label(Toussamples)<-c("Population index for selected element of sample index j in month m and longitudinal sample i")
  
  Toussamples}




#Housheholds sample
ToussamplesHf<-function(){
  load(paste0(tablesfolder,"/Toussamples.Rdata"))
  ToussamplesH<-
    lapply(Toussamples,function(l){Sample<-((l$Samplei[((0:159)*5)+1,])-1)/5+1})
  save(ToussamplesH,file=paste0(tablesfolder,"/ToussamplesH.Rdata"))}





#ToussamplesHf()
list.tablesAf<-function(list.tablespopA,AllsamplesH){
  plyr::maply(1:dim(Allsamples)[1],function(i){
    plyr::maply(1:dim(Allsamples)[3],function(j){
      list.tablespopA[Allsamples[i,,j],,j]})})}




list.tablesAbiasf<-function(list.tablesA){
  list.tablesA[1:20,,,]<-aperm(array(apply(list.tablesA[1:20,,,],
                                           c(1,3:4),
                                           function(x){if(x[2]>0){x+rbinom(1,x[2],.2)*c(1,-1,0)}else{x}}),dim(list.tablesA[1:20,,,])[c(2,1,3:4)]),c(2,1,3,4))
  list.tablesAbias}



Faitout<-function(){
  load(paste0(tablesfolder,"/ToussamplesH.Rdata"))
  mclapply(1:3,function(popnum){
    list.tablespopAf(popnum)
    list.tablesAf(popnum)})}
#Faitout()
Faitout2<-function(){
  for(popnum in 1:3){for (bias in c("","bias")){
    load(paste0(tablesfolder,"/list.tablesA_",bias,popnum,".Rdata"))
    assign(paste0("x",bias,popnum),list.tablesA)}}
  list.tablesA<-abind(x1,x2,x3,xbias1,xbias2,xbias3,along=5)
  list.tablesA<-array(list.tablesA,c(dim(x1),3,2))
  dimnames(list.tablesA)<-c(dimnames(x1),list(1:3,c("","bias")))
  dimnames(list.tablesA)[[3]]<-tables.entree
  dimnames(list.tablesA)[[4]]<-1:nrep
  save(list.tablesA,file=paste0(tablesfolder,"/list.tablesA.Rdata"))}

