Createtoutsamples<-function(tables.entree,
                            cluster.size=5,
                            cluster.all.sample.rate=1/10,
                            cluster.single.sample.size=20){
  nmois<-length(tables.entree)
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
  Toussamples <- lapply(1:nrep,
                          function(nr){
                            starte <- cluster.size*(nr-1)#sample(1:(N/cluster.size),1)
                            
                            samplei<-sapply(1:nb.samples,
                                            function(i){
                                              (starte-1+(rep((0:(cluster.single.sample.size-1))*
                                                               ecart.personne.meme.cluster.echantillon,
                                                             each=cluster.size)+
                                                           rep((i-1)*ecart2+(1:cluster.size),
                                                               cluster.single.sample.size)))%%N+1})
                            return(list(samplei=samplei,
                                        Samplei=sapply(1:nmois,function(i){
                                          (samplei[,i+rep(c(0,12),each=4)+rep(0:3, 2)])})))})
  Toussamples}


ToussamplesHf<-function(){
  load(paste0(tablesfolder,"/Toussamples.Rdata"))
  ToussamplesH<-
    lapply(Toussamples,function(l){Sample<-((l$Sample[((0:159)*5)+1,])-1)/5+1})
  save(ToussamplesH,file=paste0(tablesfolder,"/ToussamplesH.Rdata"))}





#ToussamplesHf()
list.tablesAf<-function(popnum){
  if(!exists("ToussamplesH")){load(paste0(tablesfolder,"/ToussamplesH.Rdata"))}
  load(paste0(tablesfolder,"/list.tablespopA",popnum,".Rdata"))
  list.tablesA=do.call(abind,
                       c(lapply(ToussamplesH,function(l){
                         do.call(abind, c(lapply(1:nmonth,function(j){
                           list.tablespopA[l[,j],,j]}),list(along=3)))})
                         ,list(along=4)))
  save(list.tablesA,file=paste0(tablesfolder,"/list.tablesA_",popnum,".Rdata"))
  
  list.tablesA[1:20,,,]<-aperm(array(apply(list.tablesA[1:20,,,],
                                           c(1,3:4),
                                           function(x){if(x[2]>0){x+rbinom(1,x[2],.2)*c(1,-1,0)}else{x}}),dim(list.tablesA[1:20,,,])[c(2,1,3:4)]),c(2,1,3,4))
  save(list.tablesA,file=paste0(tablesfolder,"/list.tablesA_bias",popnum,".Rdata"))}

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

