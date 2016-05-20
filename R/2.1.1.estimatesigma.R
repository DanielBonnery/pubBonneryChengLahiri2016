deltalist<-function(m1,m2){
  mon1=min(m1,m2);mon2<-max(m1,m2)
  if(mon1==mon2  ){x=cbind(     1:160         ,1:160)}
  if(mon1==mon2- 1){x=cbind(c(21:80,101:160),c(1:60,81:140))}
  if(mon1==mon2- 2){x=cbind(c(41:80,121:160),c(1:40,81:120))}
  if(mon1==mon2- 3){x=cbind(c(61:80,141:160),c(1:20,81:100))}
  if(mon1==mon2- 4){x=cbind(numeric(0),numeric(0))}
  if(mon1==mon2- 5){x=cbind(numeric(0),numeric(0))}
  if(mon1==mon2- 6){x=cbind(numeric(0),numeric(0))}
  if(mon1==mon2- 7){x=cbind(numeric(0),numeric(0))}
  if(mon1==mon2- 8){x=cbind(numeric(0),numeric(0))}
  if(mon1==mon2- 9){x=cbind(81:100,61:80)}
  if(mon1==mon2-10){x=cbind(81:120,41:80)}
  if(mon1==mon2-11){x=cbind(81:140,21:80)}
  if(mon1==mon2-12){x=cbind(81:160,  1:80)}
  if(mon1==mon2-13){x=cbind(101:160,  1:60)}
  if(mon1==mon2-14){x=cbind(121:160,  1:40)}
  if(mon1==mon2-15){x=cbind(141:160,  1:20)}
  if(mon1< mon2-15){x=cbind(numeric(0),numeric(0))}
  if(m1>m2){x=x[,c(2,1)]}
  x}

delta<-function(m,i){m-(i-1+8*(i>4))}
delta(12,1);
delta(12,2);
delta(12,3);
delta(12,4);
delta(12,5);




#
if(FALSE){any(replicate(1000,(function(){
  x<-sample(85,1);y<-sample(20000,1);
  z<-list.tablespop[[x]]$pumlrR[(y-1)*5+(1:5)];
  z<-apply(cbind(pumlrR_n0=z=="0",
                 pumlrR_n1=z=="1",
                 pumlrR_n_1=z=="_1")*1,2,sum)
  z!=list.tablespopA[y,,x]
})()))}

estimatesigma<-function(list.tablesA,m1,m2){
  sigma2<-array(0,c(3,3,1000))
  if(is.element(abs(m1-m2),c(0,1,2,3,9,10,11,12,13,14,15))){
    x<-deltalist(m1,m2)
    y<-abind(list.tablesA[x[,1],,m1,,drop=FALSE],
             list.tablesA[x[,2],,m2,,drop=FALSE],along=3)
    x<-y[,,,1]
    z<-array(apply(y,4,function(x){
      mm<-apply(x,2:3,mean);
      x<-array(aperm(apply(x,1,function(z){z-mm}),2:1),dim(x))
    }),dim(y))
    sigma2<-array(apply(z,4,function(x){
      t(x[,,1])%*%x[,,2]/(dim(z)[1]-1)}),rep(dim(z)[c(2,4)],c(2,1)))}
  sigma2}


multsigmaAf<-function(m1,m2,n=100,N=100000){
  FUN<-function(r1,r2){
    -N+(delta(m1,r1)==delta(m2,r2))*(N+((1-(n/8)/N)*N^2/(n/8)))}
  outer(1:8,1:8,FUN)
}

estimateSigma<-function(adde2){
  load(paste0(tablesfolder,"/list.tablesA",adde2,".Rdata"))
  sigma2hatA<-array(unlist(mclapply(1:nmonth,
                                    function(m2){
                                      sapply(1:nmonth,
                                             function(m1){
                                               estimatesigma(list.tablesA,m1,m2)})})),c(3,3,1000,nmonth,nmonth))
  eval(parse(text=Sauve("sigma2hatA",adde2)))
}

if(FALSE){
any(replicate(100,(function(){x<-sample(85,1);y<-sample(85,1);rep<-sample(1000,1);
                              !identical(sigma2hatA[,,rep,x,y],estimatesigma(list.tablesA,x,y)[,,rep])})()))}

if(FALSE){
  sapply(outer(c("_","_bias"),1:3,paste0),estimateSigma)
}

Sigmahatf<-function(rep,sigma2hatA){
  sigma2<-sigma2hatA[,,rep,,]
  Sigmahat<-aperm(array(unlist(mclapply(1:nmonth,
                                        function(m2){
                                          sapply(1:nmonth,
                                                 function(m1){
                                                   multsigmaA<-multsigmaAf(m1,m2)
                                                   aperm(outer(multsigmaA,sigma2[,,m1,m2],"*"),c(1,3,2,4))
                                                 })})),c(8,3,8,3,nmonth,nmonth)),c(5,1,2,6,3,4))
  dimnames(Sigmahat)<-rep(list(tables.entree,paste0("hrmis",1:8),listpumlrR),2)
  Sigmahat}

if(FALSE){
  charge("Sigma1")
  charge("sigma2hatA1")
  XX<-Sigmahat(500,sigma2hatA)
  plot(2:8,XX[1,1,1,c(2,3,4,12,13,14,15),1,1],type="l")
  
  YY<-Sigma
  plot(2:85,YY[1,1,3,2:85,1,3],type="l")
  
  plot(XX[1,1,3,2:85,1,3],YY[1,1,3,2:85,1,3],type="l")
}


bestmachinf<-function(adde2){
  charge(paste0("sigma2hatA",adde2))
  charge(paste0("S2comprep",adde2))
  # charge(paste0("ak",adde2))
  charge(paste0("ak3",adde2))
  XX=
    mclapply(1:nrep,function(repe){
      if(file.exists(paste0(Resultsfolder,"/bb",adde2,"_",repe,".Rdata"))){charge(paste0("bb",adde2,"_",repe))}else{
        Sigmahat<-Sigmahatf(repe,sigma2hatA)
        bb<-list(#ak =bestAK (Sigmahat,S2comprep[,listpumlrR,1,rep],initval=ak, what=list(compromise=varAKcompratmean)),
          ak3=bestAK3(Sigmahat,S2comprep[,listpumlrR,1,repe],
                      startval=ak6to4(ak3$compromise),
                      what=list(compromise=varAK3compratmean4),itnmax=50)$compromise,
          W=CoeffYF(nmonth,Sigmahat))
        eval(parse(text=Sauve("bb",paste0(adde2,"_",repe))))}
      bb
    })
  akestrep<-array(    unlist(      lapply(XX,function(l){l$ak3})),c(4,nrep))
  eval(parse(text=Sauve("akestrep",adde2)))
  coeffAK3estrep<-coeffAK3(nmonth,lapply(XX,function(l){ak4to6(l$ak3)}))
  eval(parse(text=Sauve("coeffAK3estrep",adde2)))
  coeffYFestrep<-array(unlist(lapply(XX,function(l){l$W})),c(nmonth*3,nmonth*8*3,nrep))
  eval(parse(text=Sauve("coeffYFestrep",adde2)))
}

bestmachinYFtotf<-function(adde2,factor=1000){
  charge(paste0("sigma2hatA",adde2))
  charge(paste0("S2comprep",adde2))
  # charge(paste0("ak",adde2))
  charge(paste0("ak",adde2))
  XX=
    mclapply((1:nrep)[order(runif(nrep))],function(repe){
      if(!file.exists(paste0(Resultsfolder,"/What",adde2,"_",repe,".Rdata"))){
        if(file.exists(paste0(Resultsfolder,"/bb",adde2,"_",repe,".Rdata"))){charge(paste0("bb",adde2,"_",repe));What=bb$W}
        else{
          Sigmahat<-Sigmahatf(repe,sigma2hatA)
          What=CoeffYF(nmonth,Sigmahat/factor)}
        eval(parse(text=Sauve("What",paste0(adde2,"_",repe))))}
      return(0)})}

bestmachinAKchargef<-function(adde2,repe){
  charge(paste0("sigma2hatA",adde2))
  charge(paste0("S2comprep",adde2))
  charge(paste0("ak3",adde2))
Sigmahat<-Sigmahatf(repe,sigma2hatA)
AK3hat=bestAK3(Sigmahat,S2comprep[,listpumlrR,1,repe],
               startval=ak6to4(ak3$compromise),
               what=list(compromise=varAK3compratmean4),itnmax=50)$compromise
eval(parse(text=Sauve("AK3hat",paste0(adde2,"_",repe))))
}

bestmachinAKtotf<-function(adde2){
  charge(paste0("sigma2hatA",adde2))
  charge(paste0("S2comprep",adde2))
  charge(paste0("ak3",adde2))
  XX=
    mclapply((1:nrep)[order(runif(nrep))],function(repe){
      if(!file.exists(paste0(Resultsfolder,"/AK3hat",adde2,"_",repe,".Rdata"))){
        if(file.exists(paste0(Resultsfolder,"/bb",adde2,"_",repe,".Rdata"))){
          charge(paste0("bb",adde2,"_",repe));AK3hat<-bb$ak3}else{
            Sigmahat<-Sigmahatf(repe,sigma2hatA)
            AK3hat=bestAK3(Sigmahat,S2comprep[,listpumlrR,1,repe],
                           startval=ak6to4(ak3$compromise),
                           what=list(compromise=varAK3compratmean4),itnmax=50)$compromise}
        eval(parse(text=Sauve("AK3hat",paste0(adde2,"_",repe))))}
    return(0)})}
  #  akestrep<-array(    unlist(      lapply(XX,function(l){l$ak3})),c(4,nrep))
  # eval(parse(text=Sauve("akestrep",adde2)))
  #  coeffAK3estrep<-coeffAK3(nmonth,lapply(XX,function(l){ak4to6(l$ak3)}))
  #  eval(parse(text=Sauve("coeffAK3estrep",adde2)))


if(FALSE){
  sapply(outer(c("_","_bias"),1:3,paste0)[c(3,5,2,4,6)],bestmachinYFf)
  adde2="_1"
  bestmachinAKf("_1")
  bestmachinYFtotf("_1")
  bestmachinYFf("_2")
  bestmachinAKtotf("_2")
  bestmachinYFtotf("_3")
  bestmachinAKtotf("_3")
bestmachinYFtotf("_3",factor=1000000)
bestmachinYFtotf("_3",factor=10000000)
  
  bestmachinYFtotf("_bias2",factor=100000)
  bestmachinAKtotf("_bias2",factor=100000)
  bestmachinAKtotf("_bias1",factor=100000)
  bestmachinYFtotf("_bias1",factor=100000)
  bestmachinYFtotf("_bias3",factor=100000)
  bestmachinAKtotf("_bias3",factor=100000)

bestmachinYFtotf("_bias2",factor=100000)
bestmachinAKtotf("_bias2",factor=100000)
bestmachinAKtotf("_bias1",factor=100000)
bestmachinYFtotf("_bias1",factor=100000)
bestmachinYFtotf("_bias3",factor=100000)
bestmachinAKtotf("_bias3",factor=100000)
  
}
