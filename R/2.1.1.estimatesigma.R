#' Adds unemployment rate to an array
#' 
#' @param A  a_1 x ... x a_p
#' @param dime an integer or a string indicating the dimention of the array that corresponds to the employed and unemployed
#' @return uenames, a character string indicating the dimension names corresponding to employed,unemployed and rate( will be created)
#' @examples
#' A=array(1:(prod(2:6)),2:6);dimnames(A)<-list(s=1:2,m=1:3,y=c("1","0","_1","r"),e=1:5,i=1:6);
#' i1=1:3;i2=4:5;varX(A,i1,i2)

varX<-function(A,i1,i2=setdiff(1:length(dim(A)),i1)){
  B<-array(var(array(aperm(A,c(i1,i2)),c(prod(dim(A)[i1]),prod(dim(A)[i2])))),rep(dim(A)[i2],2),rep(dimnames(A)[i2],2))
  names(dimnames(B))<-paste0(rep(names(dimnames(A)[i2]),2),rep(1:2, each=length(i2)))
  B
}

varA<-function(A,varyingindex,variableindex,fixindex=NULL){
  if(!is.numeric(fixindex)&!is.null(fixindex)){i0<-match(fixindex,names(dimnames(A)))}
  i_0<-setdiff(1:length(dim(A)),i0)
  if(!is.numeric(varyingindex)){i1<-match(varyingindex,names(dimnames(A))[i_0])}
  if(!is.numeric(variableindex)){i2<-match(variableindex,names(dimnames(A))[i_0])}
  if(!is.null(fixindex)){
    plyr::aaply(A,
                i0,
                varX,
                i1=i1,
                i2=i2)
  }else{varX(A,i1,i2)}}






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






#' tablesAf(1,1,1,"false",syntheticcpspops,BB)



deltalist<-function(m1,m2){
  diff=abs(m1-m2)
  x=cbind(numeric(0),numeric(0))
  if(diff== 0){x=cbind(c(8:5,4:1),c(8:5,4:1))}
  if(diff== 1){x=cbind(c(7:5,3:1),c(8:6,4:2))}
  if(diff== 2){x=cbind(c(6:5,2:1),c(8:7,4:3))}
  if(diff== 3){x=cbind(c(5:5,1:1),c(8:8,4:4))}
  if(diff== 9){x=cbind(4:4,5:5)}
  if(diff==10){x=cbind(4:3,6:5)}
  if(diff==11){x=cbind(4:2,7:5)}
  if(diff==12){x=cbind(4:1,8:5)}
  if(diff==13){x=cbind(3:1,8:6)}
  if(diff==14){x=cbind(2:1,8:7)}
  if(diff==15){x=cbind(1:1,8:8)}
  if(m1>m2){x=x[,c(2,1)]}
  x}

delta<-function(m,i){m-(i-1+8*(i>4))}
delta(12,1);
delta(12,2);
delta(12,3);
delta(12,4);
delta(12,5);

tableA3f<-function(s,i,m,b,x,syntheticcpspopsHA){
  rbind(syntheticcpspopsHA[s,m,,b      ,sampleruleH(i,intersect(1,x),m)],
        syntheticcpspopsHA[s,m,,"false",sampleruleH(i,setdiff  (x,1),m)])}

#' estimatesigma(1,1,1,2,"false",syntheticcpspops,BB)
estimatesigma<-function(s,i,m1,m2,b,syntheticcpspopsHA){
  sigma2<-array(0,c(3,3))
  if(is.element(abs(m1-m2),c(0,1,2,3,9,10,11,12,13,14,15))){
    x<-deltalist(m1,m2)
    sigma2<-var(m1=tableA3f(s,i,m1,b,x[,1]),
                m2=tableA3f(s,i,m2,b,x[,2]))}
  names(dimnames(sigma2))<-c("y1","y2")
  sigma2}




tablesAf2<-function(s,m,i,b,syntheticcpspopsA){
  syntheticcpspopsA[s,m,sampleruleS(i),,b]}

estimatesigma2<-function(s,i0,m,lag,b,syntheticcpspopsA){
  sigma2<-var(tablesAf2(s,m,i0,b,syntheticcpspopsA),
              tablesAf2(s,m+lag,i0,if(lag==0){b}else{"false"},syntheticcpspopsA))
  names(dimnames(sigma2))<-c("y1","y2")
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
  FUN<-function(r1,r2){-N+(delta(m1,r1)==delta(m2,r2))*(N+((1-(n/8)/N)*N^2/(n/8)))}
  outer(X = 1:8,1:8,FUN)
}

Sigmahatf<-function(Sigmahat){
  SigmahatH<-plyr::daply(do.call(expand.grid,c(lapply(dimnames(Sigmahat)[match(c("m1","m2"),names(dimnames(Sigmahat)))],strtoi),list(stringsAsFactors=FALSE))),~m1+m2,
                         function(dd){
               multsigmaA<-multsigmaAf(dd$m1,dd$m2)
               dimnames(multsigmaA)<-list(h1=1:8,h2=1:8)
               outer(multsigmaA,Sigmahat[dd$m1,dd$m2,,],"*")
             })
  aperm(SigmahatH,match(c("m1","h1","y1","m2","h2","y2"),names(dimnames(SigmahatH))))}

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
