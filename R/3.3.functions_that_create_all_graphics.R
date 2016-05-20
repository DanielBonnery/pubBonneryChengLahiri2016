#--------------------------------------------------------------
# Loading packages
#--------------------------------------------------------------
#--------------------------------------------------------------
# Pour le bon fonctionnement de tikz
documentDeclaration<-1
getDocumentPointsize<-function(a){10}

#--------------------------------------------------------------
# paramêtres pour les graphiques par défaut
prese=TRUE
mare=c(3,3,.1,.1)
omae=c(.1,.1,.1,.1)



analysisalpha <- function(pcserv){
  
  ##----------------------------------------------------------------
  #Load tables
  charge("resultatsweb")
  charge("MRRcompweb")
  
  
  popquan<-function(q,N){
    names(q)<-c(paste0("",.25*(0:4)),"mean")
    x=c(q["0"]+((0:N)/N)*(q["0.25"]-q["0"]),
        q["0.25"]+((0:N)/N)*(q["0.5"]-q["0.25"]),
        q["0.5"]+((0:N)/N)*(q["0.75"]-q["0.5"]),
        q["0.75"]+((0:N)/N)*(q["1"]-q["0.75"]))
    for(i in 2:(N-1)){
      if(q["mean"]-mean(x)>0){
        x[i]=x[i]+min(4*(N+1)*(q["mean"]-mean(x)),q["0.25"]-x[i])}
      if(q["mean"]-mean(x)<0){
        x[i]=x[i]+max(4*(N+1)*(q["mean"]-mean(x)),q["0"]-x[i])}
    }
    for(i in (N+3):(2*N)){
      if(q["mean"]-mean(x)>0){
        x[i]=x[i]+min(4*(N+1)*(q["mean"]-mean(x)),q["0.5"]-x[i])}
      if(q["mean"]-mean(x)<0){
        x[i]=x[i]+max(4*(N+1)*(q["mean"]-mean(x)),q["0.25"]-x[i])}
    }
    for(i in ((2*N+4):(3*N+1))){
      if(q["mean"]-mean(x)>0){
        x[i]=x[i]+min(4*(N+1)*(q["mean"]-mean(x)),q["0.75"]-x[i])}
      if(q["mean"]-mean(x)<0){
        x[i]=x[i]+max(4*(N+1)*(q["mean"]-mean(x)),q["0.5"]-x[i])}
    }
    for(i in ((3*N+5):(4*N+2))){
      if(q["mean"]-mean(x)>0){
        x[i]=x[i]+min(4*(N+1)*(q["mean"]-mean(x)),q["1"]-x[i])}
      if(q["mean"]-mean(x)<0){
        x[i]=x[i]+max(4*(N+1)*(q["mean"]-mean(x)),q["0.75"]-x[i])}
    }
    return(x)}
  
  N=249;g=rnorm((N+1)*4);q=c(quantile(g),mean=mean(g));
  names(q)[1:5]<-paste0("",.25*(0:4));x=popquan(q,N);
  c(quantile(x),mean(x))-q;x;plot(sort(g),x);abline(0,1)
  
  
  
  plot(c(1,73),range(MRRcomp$weightdisp[-6,-22,]),type="n")
  for(i in 1:7){for(k in 1:22){points(1:73,MRRcomp$weightdisp[i,k,],type="l",col=i)}}
  
  plot(c(1,73),range(MRRcomp$weightdisp[-6,"0.95",]),type="n")
  for(i in 1:7){points(1:73,MRRcomp$weightdisp[i,"0.95",],type="l",col=i)}
  
  plot(c(1,73),range(MRRcomp$weightdisp[-6,"0.75",]),type="n")
  for(i in 1:7){points(1:73,MRRcomp$weightdisp[i,"0.75",],type="l",col=i)}
  load(paste0(tablesfolder,"/list.tablesweb.Rdata"))
  
  AAA<-sapply(list.tables[-1],function(l){gg<-(l$pwcmpwgt/l$pwsswgt)[l$pwcmpwgt>0];return(c(quantile(gg,na.rm=TRUE),mean(gg,na.rm=TRUE)))})
  AAAK<-apply(AAA,2,popquan,N=249)
  
  
  
  sauvegraph(texte="box",pres=FALSE,mare=c(0,5.1,0,0),heighte=3.5,widthe=if(prese){4.7}else{6})
  par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
  layout(matrix(1:5, 5, 1, byrow = TRUE))
  #http://www.statmethods.net/advgraphs/layout.html
  
  at <- (12*(0:6))+1
  labels<-2005+0:6
  
  
  boxplot(AAAK,range=0,ylab="(a) AK",xaxt="n",xlab="",las=1,tck=1,mare=c(0,5.1,0,0))
  points(AAA[6,],pch=4)
  
  
  AAA<-apply(MRRcomp$weightdisp[-6,"0.75",],2,popquan,N=249)
  
  boxplot(AAA,range=0,ylab="(b) 0.75",xaxt="n",xlab="",las=1,tck=1,mare=c(0,5.1,0,0))
  points(MRRcomp$weightdisp[7,"0.75",],pch=4)
  
  AAA<-apply(MRRcomp$weightdisp[-6,"0.95",],2,popquan,N=249)
  boxplot(AAA,range=0,ylab="(c) 0.95",xaxt="n",xlab="",las=1,tck=1,mare=c(0,5.1,0,0))
  points(MRRcomp$weightdisp[7,"0.95",],pch=4)
  
  
  AAA<-apply(MRRcomp$weightdisp[-6,"1",],2,popquan,N=249)
  boxplot(AAA,range=0,ylab="(d) MR2",xaxt="n",xlab="",las=1,tck=1,mare=c(0,5.1,0,0))
  points(MRRcomp$weightdisp[7,"1",],pch=4)
  
  AAA<-apply(MRRcomp$weightdisp[-6,"01",],2,popquan,N=249)
  boxplot(AAA,range=0,ylab="(e) MR12",xaxt="n",xlab="",las=1,tck=1,)
  points(MRRcomp$weightdisp[7,"01",],pch=4)
  axis(1, at=at, labels=labels,las=1,tck=1,col = "grey", lty = "dotted")
  dev.off()
  
  
  }
              
                
    if(FALSE){
  boxplot(abs(accumulated.return) ~ trend.number, col='pink')
    }
  
    function(){
  charge("resultatsserv")
  
  
  
  
  ##----------------------------------------------------------------
  #Time series graphics for presentation
graphs(Estimates[,"unemployment",c("S2","MR0.95","AK")],xref=Estimates[,"unemployment",c("S2")])
graphs(Estimates[,"unemployment",c(paste0("MR",(10:20)/20),"S2","AK")],ltye=rep(1,21),xref=0,lwde=c(rep(1,11),3,3))
graphs(Estimates[,"unemployment",c(paste0("MRR",(10:20)/20),"S2","AK")],xref=Estimates[,"unemployment","S2"],ltye=rep(1,21),lwde=c(rep(1,11),3,3),borne=c(-1,1))
graphs(Estimates[,"unemployment",c("MRR01","MRR0.75","MRR1","S2","AK")],xref=Estimates[,"unemployment","S2"],ltye=rep(1,21),lwde=c(rep(1,11),3,3),borne=c(-1,1))
graphs(Estimates[,"unemployment",c("MRR0.75","MRR1","S2","AK","RA0")],xref=Estimates[,"unemployment","S2"],ltye=rep(1,21),lwde=c(rep(1,11),3,3),borne=c(-1,1))
graphs(Estimates[,"unemployment",c(paste0("MRRH",c(0.75,.85,.95)),"AK")],xref=Estimates[,"unemployment","S2"],colore=c( rep("red",3),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,3),3),borne=c(-1,1),
         legende=c("AK", "Comp. Reg - 2 var. household  "),coloreleg=c("blue","red"))  
  
  
  graphs(cbind(MRcompU[,10:20],AKcompU,S2compU),colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),xref=0,lwde=c(rep(1,11),3,3),
       legende=c("Direct", "AK", "Comp. Reg"),coloreleg=c("green","blue","red"),texte="serv_una")
  rangex=range(apply(cbind(MRRcompU[,10:20],MRcompU[,10:20],AKcompU),2,function(x){x-S2compU}))
  sauvegraph(texte="serv_MR_dir")
graphs(cbind(MRcompU[,10:20],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),lwde=c(rep(1,11),3),borne=c(-1,1),rangex=rangex,
       legende=c("AK", "Comp. Reg - 8 var."),coloreleg=c("blue","red"))
  text(rep(80,11),MRcompU[78,10:20]-S2compU[78],labels=paste('$\\alpha=',5*(10:20)/100,"$",sep=""),col="red")
  dev.off()
graphs(cbind(MRRcompU[,10:20],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),lwde=c(rep(1,11),3),borne=c(-1,1),rangex=rangex,
       legende=c("AK", "Comp. Reg - 2 var."),coloreleg=c("blue","red"),texte="serv_MRR_dir")
graphs(cbind(MRRHcompU,AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",3),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,3),3),borne=c(-1,1),
         legende=c("AK", "Comp. Reg - 2 var. household  "),coloreleg=c("blue","red"),texte="serv_MRRH_dir")  

         
graphs(cbind(MRRcompU[,10:20],AKcompU,S2compU),colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),xref=0,lwde=c(rep(1,11),3,3))
graphs(cbind(MRRHcompU,AKcompU,S2compU),colore=c( rep("red",3),"blue","green"),ltye=rep(1,21),xref=0,lwde=c(rep(1,3),3,3))
graphs(MRcompU,xref=Estimates[,"unemployment","S2"],colore=1:3,ltye=rep(1,21),borne=c(-1,1))
graphs(cbind(MRcompU[,10:20],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",11),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,11),3),borne=c(-1,1))
graphs(cbind(MRRcompU[,10:20],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",11),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,11),3),borne=c(-1,1))
graphs(cbind(MRRHcompU,AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",3),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,3),3,3),borne=c(-1,1))
graphs(cbind(MRRHcompU[,1],MRRcompU[,16],MRcompU[,15],AKcompU),xref=Estimates[,"unemployment","S2"],colore=1:4,ltye=rep(1,21),borne=c(-1,1))
graphs(cbind(MRRHcompU[,3],MRRcompU[,20],MRcompU[,19],AKcompU),xref=Estimates[,"unemployment","S2"],colore=1:4,ltye=rep(1,21),borne=c(-1,1))
#########################################################################################
#########################################################################################
#                           GRAPHS on TRUE DATA
#########################################################################################
#########################################################################################

#----------------------------------------------------------------------------------------
#                                   Estimates
#----------------------------------------------------------------------------------------

# Partha estimator
graphs(cbind(MRPcompU,MRPRcompU,AKcompU,S2compU),xref=0,lwde=c(rep(1,11),3,3),
       legende=c("Direct", "AK", "Comp. Reg"),coloreleg=c("black","black","grey"))
# MR, AK, Direct:  
graphs(cbind(MRcompU[,10:20],AKcompU,S2compU),colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),xref=0,lwde=c(rep(1,11),3,3),
       legende=c("Direct", "AK", "Comp. Reg"),coloreleg=c("green","blue","red"))
# MR, AK, Direct:  
graphs(cbind(MRRcompU[,1:21],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",21),"blue","green"),ltye=rep(1,21),lwde=c(rep(1,21),3),borne=c(-1,1),rangex=rangex,
       legende=c("AK", "Comp. Reg - 2 var."),coloreleg=c("blue","red"))
graphs(cbind(MRRHcompU,AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",3),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,3),3),borne=c(-1,1),
       legende=c("AK", "Comp. Reg - 2 var. household  "),coloreleg=c("blue","red"))  

# graphs on true data for paper:  

graphs(Estimates[,"unemployment",c(paste0("MR",(0:5)/5),"AK","S2","MRR01")],
       #colore=c( rep("red",11),"blue","green"),
       #ltye=rep(1,21),xref=0,lwde=c(rep(1,11),3,3),
       #legende=c("Direct", "AK", "Reg Comp."),
       #coloreleg=c("green","blue","red"),
       borne=c(-1,1),ylabe="Difference with direct",
       printlabels=FALSE,mare=c(0,5.1,0,0))



toto<-function(XX,xref){range(apply(XX,2,function(x){x-xref}))}
rangexx<-matrix(NA,3,2)
rangexx[1,]=toto(Estimates[,"unemployment",c(paste0("MR",(10:20)/20),"AK")],xref=Estimates[,"unemployment","S2"])
rangexx[2,]=toto(Estimates[,"unemployment",c(paste0("MRR",(0:20)/20),"AK")],xref=Estimates[,"unemployment","S2"])
rangexx[3,]=toto(Estimates[,"unemployment",c(paste0("MRRH",c(0.75,0.95)),"AK")],xref=Estimates[,"unemployment","S2"])
rantot<-sum(rangexx[,2])-sum(rangexx[,1])

sauvegraph(texte="fcsm1",pres=FALSE,mare=c(0,5.1,0,0),heighte=7,widthe=if(prese){4.7}else{6})
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:4, 4, 1, byrow = TRUE), 
       heights=c(.05,.95*(rangexx[,2]-rangexx[,1])/rantot))
#http://www.statmethods.net/advgraphs/layout.html

plot.new()
legend(0,1,c("AK"), lty=ltypecourbe()["AK"],lwd=3,col=colorelege(TRUE,"AK"),bty="n"); 
legend(0.2,1,c("R.C."),bty="n", lty=ltypecourbe()["MR0.5"],lwd=3,col=colorelege(TRUE,"MR0.5"))
legend(0.4,1,c("MR1"), bty="n", lty=ltypecourbe()["MR0"],  lwd=3,col=colorelege(TRUE,"MR0"))
legend(0.6,1,c("MR2"), bty="n", lty=ltypecourbe()["MR1"],  lwd=3,col=colorelege(TRUE,"MR1"))       
graphs(Estimates[,"unemployment",c(paste0("MR",(5:10)/10),"AK")],xref=Estimates[,"unemployment","S2"],
       #colore=c( rep("red",11),"blue","green"),
       #ltye=rep(1,12),
       # lwde=c(rep(1,6),3),
       legende="",
       borne=c(-1,1),#rangex=rangexx[1,],
       #legende=c("AK", "Comp. Reg - 8 var."),coloreleg=c("blue","red"),
       ylabe="(a) 8 categories",printlabels=FALSE,mare=c(0,5.1,0,0))
text(rep(80,11),Estimates[78,"unemployment",c(paste0("MR",(5:10)/10))]-Estimates[78,"unemployment","S2"],
     labels=paste('$\\alpha=',(5:10)/10,"$",sep=""),col="black")

graphs(Estimates[,"unemployment",c(paste0("MRR",(0:5)/5),"AK")],xref=Estimates[,"unemployment","S2"],
       #colore=c("blue", rep("red",21)),
       #ltye=rep(1,21),lwde=c(3,rep(1,21)),
       legende="",
       borne=c(-1,1),#rangex=rangex,
       # legende=c("AK", "Comp. Reg - 2 var."),coloreleg=c("blue","red"),
       ylabe="(b) 2 categories",printlabels=FALSE,mare=c(0,5.1,0,0),axisyat=c(-0.001,0,0.001))
graphs(Estimates[,"unemployment",c(paste0("MRRH",c(0.75,.85,.95)),"AK")],xref=Estimates[,"unemployment","S2"],
       #colore=c( rep("red",3),"blue","black"),
       #ltye=rep(1,21),lwde=c(rep(1,3),3),
       borne=c(-1,1),
       legende="",
       ylabe="(c) 2 categories, grouped"
       #legende=c("AK", "Comp. Reg - 2 var. household  "),coloreleg=c("blue","red")
)  
dev.off()

###diff
toto<-function(XX,xref){range(apply(XX,2,function(x){x-xref}))}
rangexx<-matrix(NA,3,2)
rangexx[1,]=toto(Estimatesdiff[,"unemployment",c(paste0("MR",(10:20)/20),"AK")],xref=Estimatesdiff[,"unemployment","S2"])
rangexx[2,]=toto(Estimatesdiff[,"unemployment",c(paste0("MRR",(0:20)/20),"AK")],xref=Estimatesdiff[,"unemployment","S2"])
rangexx[3,]=toto(Estimatesdiff[,"unemployment",c(paste0("MRRH",c(0.75,0.95)),"AK")],xref=Estimatesdiff[,"unemployment","S2"])
rantot<-sum(rangexx[,2])-sum(rangexx[,1])

sauvegraph(texte="fcsm1diff",pres=FALSE,mare=c(0,5.1,0,0),heighte=7,widthe=if(prese){4.7}else{6})
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:4, 4, 1, byrow = TRUE), 
       heights=c(.05,.95*(rangexx[,2]-rangexx[,1])/rantot))
#http://www.statmethods.net/advgraphs/layout.html

plot.new()
legend(0,1,c("AK"),    lty=ltypecourbe()["AK"],lwd=3,col=colorelege(TRUE,"AK"),bty="n"); 
legend(0.2,1,c("R.C."),bty="n", lty=ltypecourbe()["MR0.5"],lwd=3,col=colorelege(TRUE,"MR0.5"))
legend(0.4,1,c("MR1"), bty="n", lty=ltypecourbe()["MR0"],  lwd=3,col=colorelege(TRUE,"MR0"))
legend(0.6,1,c("MR2"), bty="n", lty=ltypecourbe()["MR1"],  lwd=3,col=colorelege(TRUE,"MR1"))       
graphs(Estimatesdiff[,"unemployment",c(paste0("MR",(5:10)/10),"AK")],xref=Estimatesdiff[,"unemployment","S2"],
       #colore=c( rep("red",11),"blue","green"),
       #ltye=rep(1,12),
       # lwde=c(rep(1,6),3),
       legende="",
       borne=c(-1,1),#rangex=rangexx[1,],
       #legende=c("AK", "Comp. Reg - 8 var."),coloreleg=c("blue","red"),
       ylabe="(a) 8 categories",printlabels=FALSE,mare=c(0,5.1,0,0))
text(rep(80,11),Estimatesdiff[78,"unemployment",c(paste0("MR",(5:10)/10))]-Estimatesdiff[78,"unemployment","S2"],
     labels=paste('$\\alpha=',(5:10)/10,"$",sep=""),col="black")

graphs(Estimatesdiff[,"unemployment",c(paste0("MRR",(0:5)/5),"AK")],xref=Estimatesdiff[,"unemployment","S2"],
       #colore=c("blue", rep("red",21)),
       #ltye=rep(1,21),lwde=c(3,rep(1,21)),
       legende="",
       borne=c(-1,1),#rangex=rangex,
       # legende=c("AK", "Comp. Reg - 2 var."),coloreleg=c("blue","red"),
       ylabe="(b) 2 categories",printlabels=FALSE,mare=c(0,5.1,0,0),axisyat=c(-0.001,0,0.001))
graphs(Estimatesdiff[,"unemployment",c(paste0("MRRH",c(0.75,.85,.95)),"AK")],xref=Estimatesdiff[,"unemployment","S2"],
       #colore=c( rep("red",3),"blue","black"),
       #ltye=rep(1,21),lwde=c(rep(1,3),3),
       borne=c(-1,1),
       legende="",
       ylabe="(c) 2 categories, grouped"
       #legende=c("AK", "Comp. Reg - 2 var. household  "),coloreleg=c("blue","red")
)  
dev.off()







graphs(cbind(MRcompU[,10:20],AKcompU,S2compU),colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),xref=0,lwde=c(rep(1,11),3,3),
       legende=c("Direct", "AK", "Comp. Reg"),coloreleg=c("green","blue","red"),texte="serv_una")
rangex=range(apply(cbind(MRRcompU[,10:20],MRcompU[,10:20],AKcompU),2,function(x){x-S2compU}))
sauvegraph(texte="serv_MR_dir")
graphs(cbind(MRcompU[,10:20],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),lwde=c(rep(1,11),3),borne=c(-1,1),rangex=rangex,
       legende=c("AK", "Comp. Reg - 8 var."),coloreleg=c("blue","red"))
text(rep(80,11),MRcompU[78,10:20]-S2compU[78],labels=paste('$\\alpha=',5*(10:20)/100,"$",sep=""),col="red")
dev.off()
graphs(cbind(MRRcompU[,10:20],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),lwde=c(rep(1,11),3),borne=c(-1,1),rangex=rangex,
       legende=c("AK", "Comp. Reg - 2 var."),coloreleg=c("blue","red"),texte="serv_MRR_dir")
graphs(cbind(MRRHcompU,AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",3),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,3),3),borne=c(-1,1),
       legende=c("AK", "Comp. Reg - 2 var. household  "),coloreleg=c("blue","red"),texte="serv_MRRH_dir")  


graphs(cbind(MRRcompU[,10:20],AKcompU,S2compU),colore=c( rep("red",11),"blue","green"),ltye=rep(1,21),xref=0,lwde=c(rep(1,11),3,3))
graphs(cbind(MRRHcompU,AKcompU,S2compU),colore=c( rep("red",3),"blue","green"),ltye=rep(1,21),xref=0,lwde=c(rep(1,3),3,3))
graphs(MRcompU,xref=Estimates[,"unemployment","S2"],colore=1:3,ltye=rep(1,21),borne=c(-1,1))
graphs(cbind(MRcompU[,10:20],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",11),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,11),3),borne=c(-1,1))
graphs(cbind(MRRcompU[,10:20],AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",11),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,11),3),borne=c(-1,1))
graphs(cbind(MRRHcompU,AKcompU),xref=Estimates[,"unemployment","S2"],colore=c( rep("red",3),"blue","black"),ltye=rep(1,21),lwde=c(rep(1,3),3,3),borne=c(-1,1))
graphs(cbind(MRRHcompU[,1],MRRcompU[,16],MRcompU[,15],AKcompU),xref=Estimates[,"unemployment","S2"],colore=1:4,ltye=rep(1,21),borne=c(-1,1))
graphs(cbind(MRRHcompU[,3],MRRcompU[,20],MRcompU[,19],AKcompU),xref=Estimates[,"unemployment","S2"],colore=1:4,ltye=rep(1,21),borne=c(-1,1))}
