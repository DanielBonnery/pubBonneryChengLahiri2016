analysisrep <- function(){
  APCserv="PC"
  ##----------------------------------------------------------------
  #Load tables
  load("replications.Rdata")
  ##----------------------------------------------------------------
  #Time series graphics  
  # mean
  default<-c("S2" ,"AK2_papa0.5-0.5","MRR0.95","MRR0.75","MRR1","MRR01","RA0.5")
  default<-c(paste0("RA",(0:20)/20),"S2" ,  "AK_papa")
  funccc<-c("unemployment","pumlrR_n0","pumlrR_n1","pumlrR_n_1")
  funcc2<-c("unemployment","pumlrRn0","pumlrRn1","pumlrRn_1")
  for(varabl in funccc){
    for(type in dimnames(Recap)[[4]]){
      for(R in c("","diff")){ 
        graphs(get(paste0("Recap",R))[,varabl,default,type]
               ,texte=paste0(type,varabl,R)
        )}}}  
  
  figures<-
    paste0("  \\clearpage \\begin{center}\n\\begin{figure} \n    \\caption{",
           outer(
             outer(
               paste0(dimnames(Recap)[[4]], " of "),
               funcc2,paste0),
             c(""," (in variation)"),paste0))
  labelsmes<-
    paste0(
      "}\\label{fig:",
      outer(
        outer(dimnames(Recap)[[4]],funcc2,paste0),
        c("","diff"), paste0),
      "}\n")
  inputs<-
    paste0(
      "\\input{graphs/graph_",
      outer(
        outer(dimnames(Recap)[[4]],funccc,paste0),
        c("","diff"), paste0),
      ".tex}  \n\\end{figure}\n\\end{center}")
  
  cat(paste0(
    figures,labelsmes,inputs,
    collapse="\n"),file=
      "/home/daniel/Dropbox/CensusBAE/Composite estimation/Survey methodology/Article/allgraphs2.tex")
  
  
  
  graphs(Recap[,"unemployment",default,"una"])  
  graphs(Recap[,"unemployment",default,"una"])  
  graphs(Recap[,"unemployment",default,"una"])  
  graphs(Recap[,"unemployment",default,"mse"])  
  
  graphs(Recapdiff[,"unemployment",default,"mse"])  
  graphs(Recap[,"unemployment",c("S2" ,"AK2_papa0.5-0.5","MRR0.95","MRR1","MRR01","RA0.5"),"mse"])  
  graphs(Recapdiff[,"unemployment",c("S2" ,"AK2_papa0.5-0.5","MRR0.95","MRR1","MRR01","RA0.5"),"mse"])  
  
  graphs(Recap[,"unemployment",paste0("MRR",c(0,1,"01")),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",(0:20)/20),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",(8:20)/20),"mse"]) 
  graphs(Recap[,"unemployment",paste0("MRR",(10:20)/20),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",c(12:20)/20),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",(13:20)/20),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",(14:20)/20),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",c(16:20)/20),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",c(17:20)/20),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",c(18:20)/20),"mse"])  
  graphs(Recap[,"unemployment",paste0("MRR",c(19:20)/20),"mse"])  
  
  
  graphs(Recap[,"unemployment",paste0("MRR",c(19:20)/20),"var"])  
  graphs(Recap[,"unemployment",paste0("MRR",c(19:20)/20),"bias"])  
  
  
  
  graphs(Recapdiff[,"unemployment",paste0("MRR",c(0,1)),"mse"])  
  graphs(Recapdiff[,"unemployment",paste0("MRR",(0:20)/20),"mse"])  
  graphs(Recapdiff[,"unemployment",paste0("MRR",c(15:20)/20),"mse"])  
  graphs(Recapdiff[,"unemployment",paste0("MRR",c(16:20)/20),"mse"])  
  graphs(Recapdiff[,"unemployment",paste0("MRR",c(17:20)/20),"mse"])  
  graphs(Recapdiff[,"unemployment",paste0("MRR",c(18:20)/20),"mse"])  
  graphs(Recapdiff[,"unemployment",paste0("MRR",c(19:20)/20),"mse"])  
  
  graphs(Recapdiff[,"unemployment",paste0("MRR",c(19:20)/20),"var"])  
  graphs(Recapdiff[,"unemployment",paste0("MRR",c(19:20)/20),"bias"])  
  
  
  
  
  
  graphs(Recap[,"unemployment",default,"mean"]
         ,texte="mean"
  )  
  #  bias
  graphs(Recap[,"pumlrR_n0",default,"bias"]
         ,texte="bias"
  )  
  # mse  
  graphs(Recap[,"unemployment",default,"mse"]
         ,texte="mse"
  )  
  #var
  graphs(Recap[,"unemployment",default,"var"]
         ,         texte="var"
  )  
  graphs(Recapdiff[,"unemployment",default,"mse"]
         ,         texte="msediff"
  )  
  
  #qrticle fcsm 2d graphic
  
  sauvegraph(texte="fcsm2",pres=prese,mare=c(0,5.1,0,0),heighte=6,
             widthe=if(prese){4.7}else{6})
  par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
  layout(matrix(1:4, 4, 1, byrow = TRUE), 
         heights=c(.1,rep(.3,3)))
  
  plot.new()
  legend(0,1,c("Direct"), lty=ltypecourbe()["S2"],lwd=3,col="green",bty="n");
  legend(0.2,1,c("AK"),bty="n", lty=ltypecourbe()["AK"],lwd=3,col="blue")       
  legend(0.4,1,c("R.C., 2 categories, $\\alpha=0.95$ "),bty="n", lty=ltypecourbe()["MRR0.95"],lwd=3,col="red")       
  legend(0.7,1,c("MR2"), lty=ltypecourbe()["MRR1"],lwd=3,col="brown",bty="n");
  legend(0.9,1,c("MR12"), lty=ltypecourbe()["MR01"],lwd=3,col="violet",bty="n");
  
  graphs(Recap[,"unemployment",c("S", "AK_papa" ,  "S2","MRR0.95" ,"MRR1","MRR01" ),"mean"],
         #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
         legende="",
         ylabe="(a) Mean",
         #ltye=rep(1,4),
         printlabels=FALSE)  
  #  bias
  graphs(Recap[,"unemployment",c( "AK_papa" ,  "S2","MRR0.95","MRR1","MRR01"  ),"bias"],
         #colore=c("blue","green","red"),#legende=c("AK","Direct","Comp. Reg."),
         legende="",
         #ltye=rep(1,3),
         borne=c(-1,1),legendepos=c(1,-0.004) ,ylabe="(b) Bias",printlabels=FALSE)
  # mse  
  graphs(Recap[,"unemployment",c( "AK_papa" ,  "S2","MRR0.95","MRR1","MRR01"  ),"mse"],
         #colore=c("blue","green","red"),#legende=c("AK","Direct","Comp. Reg."),
         #ltye=rep(1,3),
         legende="",
         borne=c(0,1),ylabe="(c) MSE")
  
  dev.off()
  
  
  
  
  
  #graphs(cbind(  ScomppopU[,1],AK_papacompUrep[,1],
  #  S2compUrep[,2],
  #  MRR95compUrep[,1]),
  #       colore=c("black",rep("gray7",3)),legende=c("Pop.","AK","Direct","Comp. Reg. - 2 var."),
  #       borne=c(-1,1),legendepos=c(1,.12) ,texte="una",ltye=c(1,3,2,4))
  ##month to month
  # bias
  sauvegraph("diff_bias",pres=prese,mare=c(0,5.1,0,0),heighte=5,
             widthe=if(prese){4.7}else{6})
  par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
  layout(matrix(1:6, 6, 1, byrow = TRUE), 
         heights=c(.1,rep(.3,5)))
  plot.new()
  legend(0,1,c("Direct"), lty=1,lwd=3,col="green",bty="n");
  legend(0.15,1,c("AK"),bty="n", lty=1,lwd=3,col="blue")       
  legend(0.3,1,c("R.C., 2 categories $\\alpha=0.95$"),bty="n", lty=1,lwd=3,col="red")       
  legend(0.65,1,c("MR2"), lty=1,lwd=3,col="brown",bty="n");
  legend(0.8,1,c("MR12"), lty=1,lwd=3,col="violet",bty="n");
  
  #  par(mfrow=c(3,1),mar=c(0,6.1,0,0),oma=c(3,3,0,0),mgp=c(4,1,0))
  rangex=range(Recapdiff[,"unemployment",c("AK_papa","S2", "MRR0.95","MRR1","MRR01"),"bias"])
  AKp=as.matrix(Recapdiff[,"unemployment","AK_papa","bias",drop=FALSE]);colnames(AKp)<-"AK_papa"
  S2p=as.matrix(Recapdiff[,"unemployment","S2","bias",drop=FALSE]);colnames(S2p)<-"S2"
  MRp=as.matrix(Recapdiff[,"unemployment","MRR0.95","bias",drop=FALSE]);colnames(MRp)<-"MRR0.95"
  Sip=as.matrix(Recapdiff[,"unemployment","MRR01","bias",drop=FALSE]);colnames(Sip)<-"MRR01"
  R1p=as.matrix(Recapdiff[,"unemployment","MRR1","bias",drop=FALSE]);colnames(R1p)<-"MRR1"
  graphs(S2p,
         legende="",
         #colore="blue",
         borne=c(-1,1),
         ltye=1,
         rangex=rangex,ylabe="Direct",
         printlabels=FALSE,mare=c(0,5.1,0,0))
  graphs(AKp,
         #colore="green",
         legende="",
         borne=c(-1,1),ltye=1,rangex=rangex,ylabe="AK",printlabels=FALSE,
         mare=c(0,5.1,0,0))
  graphs(MRp,
         #colore="green",
         legende="",
         borne=c(-1,1),ltye=1,rangex=rangex,ylabe="R.C., \\\\alpha=0.95",
         printlabels=FALSE,
         mare=c(0,5.1,0,0))
  graphs(R1p,
         #colore="green",
         legende="",
         borne=c(-1,1),ltye=1,rangex=rangex,ylabe="MR2",
         printlabels=FALSE,
         mare=c(0,5.1,0,0))
  graphs(Sip,
         #colore="red",
         legende="",
         ltye=1,
         borne=c(-1,1),rangex=rangex,ylabe="MR12")
  dev.off()
  #mse  
  sauvegraph("diff_mse",pres=prese,mare=c(0,5.1,0,0),heighte=3,widthe=if(prese){4.7}else{6})
  par(oma=c(2,0,1,0),mar=c(0,5,0,0),mgp=c(4,1,0))
  layout(matrix(1:2, 2, 1, byrow = TRUE), 
         heights=c(.15,.85))
  plot.new()
  legend(0,1,c("Direct"), lty=ltypecourbe()["S2"],lwd=3,col="green",bty="n");
  legend(0.15,1,c("AK"),bty="n", lty=ltypecourbe()["AK"],lwd=3,col="blue")       
  legend(0.3,1,c("R.C., 2 categories $\\alpha=0.95$"),bty="n", lty=ltypecourbe()["MR0.5"],lwd=3,col="red")       
  legend(0.7,1,c("MR2"), lty=ltypecourbe()["MR1"],lwd=3,col="brown",bty="n");
  legend(0.85,1,c("MR12"), lty=ltypecourbe()["MR01"],lwd=3,col="violet",bty="n");
  
  #par(mfrow=c(2,1),mar=c(0,6.1,0,0),oma=c(3,3,0,0),mgp=c(4,1,0))
  #par(oma=c(0,0,0,0),mar=mare,mfrow=c(1,1))
  graphs(Recapdiff[,"unemployment",c("AK_papa" ,  "S2",    "MRR0.95","MRR1","MRR01"),"mse"  ],
         legende="",
         #colore=c("blue","green","red"), ltye=c(1,1,1),
         #legende=c("AK","Direct","Comp. Reg. - 2 var."),ltye=rep(1,3),
         borne=c(-1,1))
  dev.off()
}
#analysisrep()
