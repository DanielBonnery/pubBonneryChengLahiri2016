tablesAf<-function(s,i,m,b,syntheticcpspops,BB){
  A=syntheticcpspops[[s]][[m]][samplerule(i,1:800,m),"pumlrR"]
  if(b=="true"){
    if(BB[i,m,1,s,1]>0){
    x=(701:800)[A[701:800]=="1"][1:BB[i,m,1,s,1]]
    A[x]<-"0"}
    }
  A<-model.matrix(~A+0,data.frame(A=A))
  colnames(A)<-gsub("A","",colnames(A))
  A}

tablesAf(1,1,1,"false",syntheticcpspops,BB)



deltalist<-function(m1,m2){
  diff=abs(m1-m2)
  x=cbind(numeric(0),numeric(0))
  if(diff== 0){x=cbind(     1:160         ,1:160)}
  if(diff== 1){x=cbind(c(21:80,101:160),c(1:60,81:140))}
  if(diff== 2){x=cbind(c(41:80,121:160),c(1:40,81:120))}
  if(diff== 3){x=cbind(c(61:80,141:160),c(1:20,81:100))}
  if(diff== 9){x=cbind(81:100,61:80)}
  if(diff==10){x=cbind(81:120,41:80)}
  if(diff==11){x=cbind(81:140,21:80)}
  if(diff==12){x=cbind(81:160,  1:80)}
  if(diff==13){x=cbind(101:160,  1:60)}
  if(diff==14){x=cbind(121:160,  1:40)}
  if(diff==15){x=cbind(141:160,  1:20)}
  if(m1>m2){x=x[,c(2,1)]}
  x}

delta<-function(m,i){m-(i-1+8*(i>4))}
delta(12,1);
delta(12,2);
delta(12,3);
delta(12,4);
delta(12,5);



#' estimatesigma(1,1,1,2,"false",syntheticcpspops,BB)
estimatesigma<-function(s,i,m1,m2,b,syntheticcpspops,BB){
  sigma2<-array(0,c(3,3))
  if(is.element(abs(m1-m2),c(0,1,2,3,9,10,11,12,13,14,15))){
    x<-deltalist(m1,m2)
    y<-abind(m1=tablesAf(s,i,m1,b,syntheticcpspops,BB)[x[,1],],m2=tablesAf(s,i,m2,b,syntheticcpspops,BB)[x[,2],],along=3)
    mm<-plyr::aaply(y,2:3,mean);
    x<-plyr::aaply(y,1,function(z){z-mm})
    sigma2<-t(x[,,1])%*%x[,,2]/(dim(x)[3]-1)}
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



multsigmaAf<-function(m1,m2,n=100,N=100000){
  FUN<-function(r1,r2){
    -N+(delta(m1,r1)==delta(m2,r2))*(N+((1-(n/8)/N)*N^2/(n/8)))}
  outer(X = 1:8,1:8,FUN)
}

Sigmahatf<-function(Sigmahat){
  SigmahatH<-plyr::daply(do.call(expand.grid,dimnames(Sigmahat)[match(c("m1","y1","m2","y2"),names(dimnames(Sigmahat)))]),~m1+m2,
                         function(d){
               multsigmaA<-multsigmaAf(d$m1,d$m2)
               dimnames(multsigmaA)<-list(h1=1:8,h2=1:8)
               outer(multsigmaA,Sigmahat[d$m1,d$m2,,],"*")
             })
  dimnames(SigmahatH)<-c(dimnames(Sigmahat)[match(c("m1","y1","m2","y2"),names(dimnames(Sigmahat)))],
                         list(h1=1:8,h2=1:8))[c(1:2,5,3:4,6)]
  SigmahatH}

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
