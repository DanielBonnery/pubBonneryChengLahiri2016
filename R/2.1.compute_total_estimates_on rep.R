
what=c("mistotals","covar","coeffYF","best.coeffAK2","best.coeffAK3","best.constrained.coeffAK3","best.coeffAK3.sep","AKCPS",
       "S","AK3","estAK3","YF","estYF","S2","MRR")
changeak<-function(ak){ak[c(1,4,3,2,5,6)]}

Compute_S2MRAK_rep <- function(
  what=c("mistotals",
         "covar",
         "estimatesigma",
         "coeffYF",
         "best.coeffAK2",
         "best.coeffAK3",
         "best.constrained.coeffAK3",
         "best.coeffAK3.sep",
         "AKCPS",
         "S",
         "AK",
         "estAK2",
         "estAK3",
         "YF",
         "estYF",
         "S2",
         "MRR"),
  u=1:nrep,
  popnums=1:3,
  biass=c("","bias")){
  adde1="_rep"
  list.adde2bis<-c(adde2f(biass,popnums))
  #  what=c("S","AK3","estAK3","S2","MRR","BCL2","BCL0","BCL","BCLratio")
  ##----------------------------------------------------------------
  #variables declaration
  #studyvar<-c("unemployment",paste0("pumlr_n",c(1:7,"_1")),paste0("pumlrR_n",c(0:1,"_1")))
  pcserv="simu"
  #load(paste0(tablesfolder,"/list.tablessimu.Rdata"))
  charge("list.dft.x2");
  
  list.y=c("pumlrR")
  w="pwsswgt"
  theta=3/4
  Alpha=(0:20)/20
  mu=NULL
  #For MR,MRR
  id=c("hrlongid","pulineno" )
  list.x1="un" #computed
  list.x2=NULL#c("pehspnon","pesex")    #external
  list.xMR=c("pumlr")
  if(any(what!="AK")){
    load("list.dft.x2.simu.Rdata")
    load("list.dft.x2RH.simu.Rdata")
    load("list.dft.x2R.simu.Rdata")
    load("list.dft.x2H.simu.Rdata")}
  dft0.y=NULL
  dft0.xMR=NULL
  mu0=NULL
  calibmethod="linear"
  add.var=FALSE
  w="pwsswgt"
  nmonth<-85
  weight="pwsswgt"
  
  ####################################################
  # Computation of pop totals
  ####################################################
  if(is.element("S",what)){
    sapply(popnums,function(popnum){
      load(paste0(tablesfolder,"/list.tablespopA",popnum,".Rdata"))
      Scomppop<-t(apply(list.tablespopA,2:3,sum))
      dimnames(Scomppop)[[1]]<-tables.entree
      eval(parse(text=Sauve("Scomppop",paste0("_",popnum))))
      eval(parse(text=Sauve("Scomppop",paste0("_bias",popnum))))})}
  ####################################################
  # Computation of month in sample totals          
  ####################################################
  if(is.element("mistotals",what)){
    sapply(list.adde2bis,function(adde2){
      load(paste0(tablesfolder,"/list.tablesA",adde2,".Rdata"))
      dd<-dim(list.tablesA)
      mis<-array(list.tablesA,c(dd[1]/8,8,dd[2:4]))
      mistotalscomp<-aperm(8*125*apply(mis,2:5,sum),c(3,1,2,4))[,8:1,,]
      dimnames(mistotalscomp)[[1]]<-tables.entree
      dimnames(mistotalscomp)[[2]]<-paste0("hrmis",1:8)
      dimnames(mistotalscomp)[[3]]<-dimnames(list.tablesA)[[2]]
      dimnames(mistotalscomp)[[4]]<-paste0("rep",1:nrep)
      mistotalscomp<-mistotalscomp[,,listpumlrR,]
      eval(parse(text=Sauve("mistotalscomp",adde2)))
      mtc<-array(aperm(mistotalscomp,c(3,2,1,4)),c(prod(dim(mistotalscomp)[1:3]),dim(mistotalscomp)[4]))
      eval(parse(text=Sauve("mtc",adde2)))})}
  ####################################################
  # Computation of Sigma          
  ####################################################
  if(is.element("covar",what)){
    mclapply(list.adde2bis,function(adde2){
      charge(paste0("mistotalscomp",adde2))
      Sigma=array(var(t(array(mistotalscomp,
                              c(prod(dim(mistotalscomp)[1:3]),dim(mistotalscomp)[4])))),
                  rep(dim(mistotalscomp)[1:3],2))
      dimnames(Sigma)<-rep(dimnames(mistotalscomp)[1:3],2)
      eval(parse(text=Sauve("Sigma",adde2)))})}
  ####################################################
  # Estimation of Sigma          
  ####################################################
  if(is.element("estimatesigma",what)){sapply(list.adde2bis,estimateSigma)}
  ####################################################
  # Computation of best coefficients
  ####################################################
  if(is.element("coeffYF",what)){
    mclapply(list.adde2bis,function(adde2){
      charge(paste0("Sigma",adde2))  
      coeffYF<-CoeffYF(nmonth,Sigma)
      eval(parse(text=Sauve("coeffYF",adde2)))})}
  ####################################################
  if(is.element("best.coeffAK2",what)){
    sapply(popnums,function(popnum){
      charge(paste0("Sigma",adde2f("",popnum)))
      charge(paste0("Scomppop",adde2f("",popnum)))
      ak2=bestAK2(Sigma,Scomppop)
      eval(parse(text=Sauve("ak2",adde2f("",popnum))))
      eval(parse(text=Sauve("ak2",adde2f("bias",popnum))))})}
  ####################################################
  if(is.element("best.coeffAK3",what)){
    sapply(popnums,function(popnum){
      charge(paste0("Sigma",adde2f("",popnum)))
      charge(paste0("Scomppop",adde2f("",popnum)))
      ak3=bestAK3(Sigma,Scomppop)
      eval(parse(text=Sauve("ak3",adde2f("",popnum))))
      eval(parse(text=Sauve("ak3",adde2f("bias",popnum))))})}
  ####################################################
  if(is.element("best.coeffAK3.sep",what)){
    sapply(popnums,function(popnum){
      charge(paste0("Sigma",adde2f("",popnum)))
      ak3sep=bestAK3sep(Sigma)
      eval(parse(text=Sauve("ak3sep",adde2f("",popnum))))
      eval(parse(text=Sauve("ak3sep",adde2f("bias",popnum))))})}
  ####################################################
  if(is.element("best.constrained.coeffAK3",what)){
    sapply(popnums,function(popnum){
      charge(paste0("Sigma",adde2f("",popnum)))
      charge(paste0("Scomppop",adde2f("",popnum)))
      ak3c=bestAK3contraint(Sigma,Scomppop)
      eval(parse(text=Sauve("ak3c",adde2f("",popnum))))
      eval(parse(text=Sauve("ak3c",adde2f("bias",popnum))))})}
  ####################################################
  if(is.element("bestAKtest",what)){
    sapply(list.adde2bis,function(adde2){
      charge(paste0("ak3",adde2))
      ak3=lapply(ak3,changeak)
      eval(parse(text=Sauve("ak3",paste0("test",adde2))))})}
  ####################################################
  # Computation of CPS coefficients
  ####################################################
  if(is.element("allAK",what)){
    n=3
    X=seq(0,1,length=10^n+1)
    ak3all<-lapply(X,function(x){y<-floor(x*10^(1:n));y<-(y-10*c(0,y[1:(n-1)]))/10;c(y[1:2],0,y[3],1,0)})
    names(ak3all)<-paste0("AK3_",0:10^n)
    eval(parse(text=Sauve("ak3all","")))
  }  
  if(is.element("AKCPS",what)){
    ak2CPS<-list(CPS=c(.3,.4))
    ak3CPS<-list(CPS=c(.3,.4,0,.4,.7,0),CPScheck=c(.3,.3,.3,.4,.4,.4))
    ak3S2<-list(S2=c(0,0,0,0,0,0))
    eval(parse(text=Sauve("ak3CPS","")))
    eval(parse(text=Sauve("ak3S2","")))
    eval(parse(text=Sauve("ak2CPS","")))}
  ####################################################
  # Computation of linear estimators
  ####################################################
  if(is.element("S2",what)){
    sapply(list.adde2bis,function(adde2){
      load(paste0(tablesfolder,"/list.tablesA",adde2,".Rdata"))
      S2comprep<-array(aperm(125*apply(list.tablesA,2:4,sum),c(2,1,3))[,listpumlrR,],c(nmonth,3,1,nrep))
      dimnames(S2comprep)<-list(tables.entree,listpumlrR,"S2",paste0("rep",1:nrep))
      S2comprep<-addU(S2comprep)[,studyvar,,,drop=FALSE]
      eval(parse(text=Sauve("S2comprep",adde2)))})}
  ####################################################  
  if(is.element("YF",what)){
    sapply(list.adde2bis,function(adde2){
      charge(paste0("coeffYF",adde2))    
      charge(paste0("mtc",adde2))
      YFcomprep<-aperm(array(apply(mtc,2,function(X){coeffYF%*%X}),c(1,3,nmonth,nrep)),c(3,2,1,4))
      dimnames(YFcomprep)<-list(tables.entree,listpumlrR,"YF",paste0("rep",1:nrep))
      YFcomprep<-addU(YFcomprep)[,studyvar,,,drop=FALSE]
      eval(parse(text=Sauve("YFcomprep",adde2)))})}
  ####################################################
  if(is.element("AK3",what)){
    charge("ak3CPS");
    charge("ak3S2")
    sapply(list.adde2bis,function(adde2){
      charge(paste0("ak3",adde2))
      charge(paste0("ak3c",adde2))
      charge(paste0("ak3sep",adde2))
      ak<-c(ak3,ak3c,ak3sep,ak3CPS,ak3S2)
      coeffAK<-CoeffAK3(nmonth,ak)
      charge(paste0("mtc",adde2))
      AK3comprep<-aperm(array(apply(mtc,2,function(X){apply(coeffAK,3,function(coeff){coeff%*%X})}),c(3,nmonth,length(ak)      ,nrep)),c(2,1,3,4))
      dimnames(AK3comprep)<-list(tables.entree,listpumlrR,paste0("AK3_",names(ak)),paste0("rep",1:nrep))
      AK3comprep<-addU(AK3comprep)[,studyvar,,,drop=FALSE]
      eval(parse(text=Sauve("AK3comprep",adde2)))})}
  ####################################################
  if(is.element("AK2",what)){
    charge("ak2CPS");
    mclapply(list.adde2bis,function(adde2){
      charge(paste0("ak2",adde2))
      ak<-c(ak2,ak2CPS)
      coeffAK<-CoeffAK2(nmonth,ak,simplify=TRUE)
      charge(paste0("mtc",adde2))
      AK2comprep<-aperm(array(apply(mtc,2,function(X){apply(coeffAK,3,function(coeff){coeff%*%X})}),c(3,nmonth,length(ak),nrep)),c(2,1,3,4))
      dimnames(AK2comprep)<-list(tables.entree,listpumlrR,paste0("AK2_",names(ak)),paste0("rep",1:nrep))
      AK2comprep<-addU(AK2comprep)[,studyvar,,,drop=FALSE]
      eval(parse(text=Sauve("AK2comprep",adde2)))})}
  
  if(is.element("AKtest",what)){
    charge("ak3CPS");
    mclapply(list.adde2bis,function(adde2){
      charge(paste0("aktest",adde2))
      ak<-c(ak,lapply(ak3CPS,changeak))
      coeffAK<-CoeffAK3(nmonth,ak)
      charge(paste0("mtc",adde2))
      AK3comprep<-aperm(array(apply(mtc,2,function(X){apply(coeffAK,3,function(coeff){coeff%*%X})}),c(3,nmonth,length(ak),nrep)),c(2,1,3,4))
      dimnames(AK3comprep)<-list(tables.entree,listpumlrR,names(ak),paste0("rep",1:nrep))
      AK3comprep<-addU(AK3comprep)[,studyvar,,,drop=FALSE]
      eval(parse(text=Sauve("AK3comprep",paste0("test",adde2))))})}
  ####################################################
  # Computation of estimated best in Clin, AK
  ####################################################
  if(is.element("estcoeffAK3",what)){
    sapply(list.adde2bis,function(adde2){
      charge(paste0("sigma2hatA",adde2))
      charge(paste0("S2comprep",adde2))
      charge(paste0("ak3",adde2))
      XX=sapply((1:nrep)[order(runif(nrep))],function(repe){
        Sigmahat<-Sigmahatf(repe,sigma2hatA)
        AK3hat=bestAK3(Sigmahat,S2comprep[,listpumlrR,1,repe],
                       startval=ak6to4(ak3$compromise),
                       what=list(compromise=varAK3compratmean4),itnmax=50)$compromise
        eval(parse(text=Sauve("AK3hat",paste0(adde2,"_",repe))))})})}
  ####################################################
  if(is.element("estcoeffYF",what)){
    sapply(list.adde2,function(adde2){
    charge(paste0("sigma2hatA",adde2))
    charge(paste0("S2comprep",adde2))
    XX=
      mclapply((1:nrep)[order(runif(nrep))],function(repe){
            Sigmahat<-Sigmahatf(repe,sigma2hatA)
            What=CoeffYF(nmonth,Sigmahat/factor)
          eval(parse(text=Sauve("What",paste0(adde2,"_",repe))))})})}
  ####################################################
  if(is.element("estAK3",what)){
    mclapply(list.adde2bis,function(adde2){
      charge(paste0("mtc",adde2))
      estAK3comprep<-aperm(array(sapply(1:nrep,function(i){
        charge(paste0("AK3hat",adde2,"_",i));
        CoeffAK3(nmonth,list(AK3hat))[,,1]%*%mtc[,i]}),c(3,nmonth,1,nrep)),c(2,1,3,4))
      dimnames(estAK3comprep)<-list(tables.entree,listpumlrR,"AK3_est",paste0("rep",1:nrep))
      estAK3comprep<-addU(estAK3comprep)[,studyvar,,,drop=FALSE]
      eval(parse(text=Sauve("estAK3comprep",adde2)))})}
  ####################################################
  if(is.element("estAK2",what)){
    mclapply(list.adde2bis,function(adde2){
      charge(paste0("mtc",adde2))
      estAK2comprep<-aperm(array(sapply(1:nrep,function(i){
        charge(paste0("AK2hat",adde2,"_",i));
        CoeffAK2(nmonth,list(AK2hat))[,,1]%*%mtc[,i]}),c(3,nmonth,1,nrep)),c(2,1,3,4))
      dimnames(estAK2comprep)<-list(tables.entree,listpumlrR,"AK2_est",paste0("rep",1:nrep))
      estAK2comprep<-addU(estAK2comprep)[,studyvar,,,drop=FALSE]
      eval(parse(text=Sauve("estAK2comprep",adde2)))})}
  ####################################################
  if(is.element("estYF",what)){ 
    mclapply(list.adde2bis,function(adde2){
      charge(paste0("mtc",adde2))
      estYFcomprep<-aperm(array(sapply(1:nrep,function(i){
        charge(paste0("What",adde2,"_",i))    
        What%*%mtc[,i]}),c(1,3,nmonth,nrep)),c(3,2,1,4))
      dimnames(estYFcomprep)<-list(tables.entree,listpumlrR,"estYF",paste0("rep",1:nrep))
      estYFcomprep<-addU(estYFcomprep)[,studyvar,,,drop=FALSE]
      eval(parse(text=Sauve("estYFcomprep",adde2)))})}
  ####################################################
  # Computation of Reg Comp, MA
  ####################################################
  if(any(is.element(what,c("MRR","S2check","BCL2","BCL0","BCL","BCLratio")))){
    for(popnum in popnums){
      for (bias in biass){
        adde1="_rep"
        adde2=adde2f(bias,popnum)
        load(paste0(tablesfolder,"/list.tablespop",popnum,".Rdata"))  
        if(is.element("BCL0",what)){
          charge(paste0("Scomppop",popnum))
          list.dft.x2BCL0<-unemploymentcountarray(Scomppop,2)[c(1,1:85),paste0("pumlrR_n",c(0,1,"_1"))]
          colnames(list.dft.x2BCL0)<-paste0("pumlrRlag_n",c(0,1,"_1"))}
        ##---------------------------------------------------------------
        #Computations
        pluserror<-function(x){
          if(sum(x[700,800]=="0")>2){
            x[sample((700:800)[x[700,800]=="0"],2)]<-"1"}
          x}
        
        pluserror<-function(x,diffe){
          for( z in (1:20)[diffe[1:20]>0]){
            x[((5*(z-1)+1):(5*z))[x[((5*(z-1)+1):(5*z))]=="1"][1:diffe[z]]]<-"0"}
          x}
        load(paste0(tablesfolder,"/Toussamples.Rdata"))
        hrmis=as.factor(rep(8:1,each=100))
        un=rep(1,800)
        
        lff<-list.files(path="~/R/Resultats/")
        if(is.null(u)){
          u=(1:nrep)[apply(
            outer(paste0(what[is.element(what,c("MRR","S2check","BCL2","BCL0","BCL","BCLratio"))],"comp"),
                  paste0(adde1,1:nrep,adde2,".Rdata"),
                  paste0),
            2,function(f){any(
              !is.element(f,lff))})    ]}
        print(u)
        
        if(bias=="bias"){
          load(paste0("/home/daniel/R/tables/list.tablesA_",popnum,".Rdata"))
          nonbiase=list.tablesA[,1,,]
          load(paste0("/home/daniel/R/tables/list.tablesA_bias",popnum,".Rdata"))
          biase=list.tablesA[,1,,]
          diff<-biase-nonbiase}      
        mclapply(u,function(i){
          print(i)
          list.tables<-lapply(1:85,function(j){    
            if(bias==""){
              cbind((list.tablespop[[j]])[Toussamples[[i]]$Samplei[,j],],
                    hrmis,un)}
            else{ll<-
                   cbind((list.tablespop[[j]])[Toussamples[[i]]$Samplei[,j],],
                         hrmis,un)
                 ll[["pumlrR"]]<-pluserror(ll[["pumlrR"]],diff[,j,i])
                 ll}})
          names(list.tables)<-names(list.tablespop)
          #MRR
          if(is.element("MRR",what)){
            list.yMR=c("pumlrR")
            list.xMR=c("pumlrR")
            file=paste0("MRRcomp",adde1,i,sep="")
            mrr<-  MR(list.tables=list.tables, w=w, id=id, 
                      list.xMR=list.xMR, list.x1=list.x1, list.x2=list.x2,list.y=list.yMR, list.dft.x2=list.dft.x2,Alpha=Alpha,theta=3/4)
            assign(file,mrr)
            eval(parse(text=Sauve(file,adde2)))}
          #MRR
          if(is.element("S2check",what)){
            list.y=c("pumlrR")
            file=paste0("S2checkcomp",adde1,i,sep="")
            s2<-  WS(list.tables=list.tables, w=w, list.y=list.y)
            assign(file,s2)
            eval(parse(text=Sauve(file,adde2)))}
          
          
          #BCL0
          if(is.element("BCL0",what)){
            file=paste("BCL0comp",adde1,i,sep="")
            assign(file,
                   BCL0(list.tables,
                        w,
                        id,
                        list.x2="pumlrRlag", #external
                        list.y="pumlrR",
                        calibmethod="linear",
                        list.dft.x2=list.dft.x2BCL0))
            eval(parse(text=Sauve(file,adde2)))}
          if(is.element("BCL2",what)){
            list.yMR=c("pumlrR")
            list.xMR=c("pumlrR")
            file=paste("BCL2comp",adde1,i,sep="")
            assign(file,
                   BCL2(list.tables,
                        w,
                        Alpha=c(0,.5,.75,.9,1,2,3,4,5,10,20),
                        id,list.xMR="pumlrR",
                        list.x1=list.x1, list.x2=list.x2,list.y=list.yMR, list.dft.x2=list.dft.x2))
            eval(parse(text=Sauve(file,adde2)))}
          
          return(0)})
        machin<-function(toto,dimee=FALSE,adde2=adde2){
          if(is.element(toto,what)){
            sapply(paste0(toto,"comp",adde1,1:nrep,adde2),charge)  
            XX=addUto1000matrices(toto,"_rep")
            assign(paste0(toto,"comprep"),XX)
            if(dimee){dimnames(XX)[[3]]<-paste0(toto,dimnames(XX)[[3]])}
            eval(parse(text=Sauve(paste0(toto,"comprep"),adde2)))
            rm(list=paste0(toto,"comp_rep",1:nrep))
            system(paste0("cd ",Resultsfolder,";rm ",paste(paste0(toto,"comp",adde1,1:nrep,adde2,".Rdata"),collapse=" ")))}
        }
        machin("BCL2",TRUE,adde2)        
        machin("MRR",TRUE,adde2)
        machin("BCL0",TRUE,adde2)}}}
  
  if(is.element("S",what)){
    sapply(list.adde2bis,
           function(adde2){
             charge(paste0("Scomppop",adde2))
             ScomppopU <-unemploymentcount(Scomppop)[,studyvar]
             ScomppopUdiff<-ScomppopU[-1,studyvar]-ScomppopU[-nrow(ScomppopU),studyvar]  
             ScompUrep<-array(0,c(85,length(studyvar),1,7))
             dimnames(ScompUrep)<-list(
               tables.entree,studyvar,"S",c("mean" ,   "var"   ,  "bias"   , "mse"   ,  "una"  ,   "relbias", "cv"))
             ScompUrep[,studyvar,"S","mean"]<-ScomppopU[,studyvar]
             
             ScompUrepdiff<-ScompUrep[-1,studyvar,,,drop=FALSE]-
               ScompUrep[-dim(ScompUrep)[1],studyvar,,,drop=FALSE]
             
             #sapply(c("S2comprep","RAcomprep","BCLcomprep","BCL0comprep","BCL2comprep","MRRcomprep","AK2_papacomprep","BCLratiocomprep"),charge)
             eval(parse(text=Sauve("ScompUrep",adde2)))
             eval(parse(text=Sauve("ScompUrepdiff",adde2)))
             eval(parse(text=Sauve("ScomppopU",adde2)))
             eval(parse(text=Sauve("ScomppopUdiff",adde2)))})}
  
  
  
  
  
  
  if(any(is.element(what,c("S","S2","BCL0","BCL2","AK3","AK2","estAK3","MRR","YF","estYF","MA")))){  
    sapply(list.adde2bis,
           function(adde2){
             sapply(paste0(c("S2","BCL0","BCL2","AK3","AK2",#"estAK2",
                             "estAK3","MRR","YF","estYF"),paste0("comprep",adde2)),charge)
             # charge(paste0("AK3compreptest",adde2)
             charge(paste0("ScompUrep",adde2))
             charge(paste0("ScompUrepdiff",adde2))
             charge(paste0("ScomppopU",adde2))
             charge(paste0("ScomppopUdiff",adde2))
             Recap<-abind(ScompUrep[,studyvar,,,drop=FALSE],
                          calcvarmeana(S2comprep),
                          calcvarmeana(MRRcomprep),
                          calcvarmeana(AK2comprep),
                          calcvarmeana(AK3comprep),
                          #    calcvarmeana(BCL2comprep),
                          calcvarmeana(estAK3comprep),
                          calcvarmeana(YFcomprep),
                          calcvarmeana(estYFcomprep),
                          calcvarmeana(BCL0comprep),
                          along = 3)
             compUrepdiff<-
               abind(calcvarmeana(difff(S2comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(MRRcomprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(AK2comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(AK3comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     #calcvarmeana(difff(BCL2comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(estAK3comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(YFcomprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(estYFcomprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     calcvarmeana(difff(BCL0comprep[,studyvar,,,drop=FALSE]),Ecomp=ScomppopUdiff[,studyvar]),
                     along = 3)
             Recapdiff<-abind(ScompUrepdiff,
                              compUrepdiff, along = 3) 
             Recap<-abind(Recap,
                          array(
                            apply(Recap[,,,"mse"],3,function(x){x/Recap[,,"S2","mse"]}),
                            dim(Recap)[1:3])
                          ,along=4)
             dimnames(Recap)[[4]][8]<-"ratmse"        
             Recapdiff<-abind(Recapdiff,
                              array(
                                apply(Recapdiff[,,,"mse"],3,function(x){x/Recapdiff[,,"S2","mse"]}),
                                dim(Recapdiff)[1:3])
                              ,along=4)  
             dimnames(Recapdiff)[[4]][8]<-"ratmse"
             
             dimnames(Recap)[[3]][3:24]<-paste0("MRR",dimnames(Recap)[[3]][3:24])
             dimnames(Recapdiff)[[3]][3:24]<-paste0("MRR",dimnames(Recapdiff)[[3]][3:24])
             save(     Recap,
                       Recapdiff,
                       file=paste0("replications",adde2,".Rdata"))})}
  LL<-lapply(list.adde2,function(adde2){
    load(paste0("replications",adde2,".Rdata"))
    return(list(Recap=Recap,
                Recapdiff=Recapdiff))});
  names(LL)<-list.adde2
  RecapA<-do.call(abind,c(lapply(LL,function(l){l$Recap}),list(along=5)))
  RecapdiffA<-do.call(abind,c(lapply(LL,function(l){l$Recapdiff}),list(along=5)))
  save(     RecapA,
            RecapdiffA,
            file="replications.Rdata")
}


#
if(FALSE){
  adde2="_3";
  load(paste0("replications",adde2,".Rdata"));
  Recap[,"unemployment",c("S2","MRR0.75","AK_CPS","AK_level","AK2_level"),"bias"]
  Recap[,"pumlrR_n1",c("S2","MRR0.75","AK_CPS","AK_level","level"),"bias"]
  Recap[,"unemployment",c("S2","MRR0.75","AK_CPS","AK_level","level"),"ratmse"]
  graphs(Recap[,,"AK2"])
}
#Compute_S2MRAK_rep(what=c("S2"))