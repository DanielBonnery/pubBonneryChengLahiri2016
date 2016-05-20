#Graphs for 20140525

load("replications.Rdata")
#default<-c("S2" ,  "AK2_papa0.5-0.5","MRR0.95","MRR1","MRR01")


graphs(Recap[,"unemployment",c("S", "AK2_papa0.5-0.5" ,  "S2","MRR0.95"),"una"]
       ,text="una"
       )  


graphs(Recap[,"unemployment",c("S", "AK2_papa0.5-0.5" ,  "S2","MRR0.95" ),"una"]
       ,xref=Recap[,"unemployment","S","mean"]
       ,text="unaref"
       )  

graphs(Recapdiff[,"unemployment",c("S", "AK2_papa0.5-0.5" ,  "S2","MRR0.95" ),"una"]
       ,xref=Recapdiff[,"unemployment","S","mean"]
       ,text="unarefdiff",
       )  



minak<-apply(Recapdiff[,"unemployment",paste0("AK2_papa",outer(paste0((0)/10,"-"),(8:10)/10,paste0)),"mse"],1,min)
argminak<-apply(Recapdiff[,"unemployment",paste0("AK2_papa",outer(paste0((0:10)/10,"-"),(0:10)/10,paste0)),"mse"],1,function(x){(1:length(x))[x==min(x)]})

bestak<-paste0("AK2_papa",outer(paste0((0:10)/10,"-"),(0:10)/10,paste0))[111]
bestak



sauvegraph(texte="meanbiasmse",pres=prese,mare=c(0,5.1,0,0))
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:4, 4, 1, byrow = TRUE), 
       heights=c(.1,rep(.3,3)))

plot.new()
legend(0,1,c("Direct"), lty=ltypecourbe()["S2"],lwd=3,col="green",bty="n");
legend(0.4,1,c("AK"),bty="n", lty=ltypecourbe()["AK"],lwd=3,col="blue")       
legend(0.7,1,c("R.C., $\\alpha=0.95$ "),bty="n", lty=ltypecourbe()["MRR0.95"],lwd=3,col="red")       

graphs(Recap[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.95" ),"mean"],
       #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
       legende="",
       ylabe="(a) Mean",
       #ltye=rep(1,4),
       printlabels=FALSE)  

#  bias
graphs(Recap[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.95"),"bias"],
       #colore=c("blue","green","red"),#legende=c("AK","Direct","Comp. Reg."),
       legende="",
       #ltye=rep(1,3),
       borne=c(-1,1),legendepos=c(1,-0.004) ,ylabe="(b) Bias",printlabels=FALSE)
# mse  
graphs(Recap[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.95" ),"mse"],
       #colore=c("blue","green","red"),#legende=c("AK","Direct","Comp. Reg."),
       #ltye=rep(1,3),
       legende="",
       borne=c(0,1),ylabe="(c) MSE")

dev.off()


sauvegraph(texte="meanbiasmsediff",pres=prese,mare=c(0,5.1,0,0))
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:4, 4, 1, byrow = TRUE), 
       heights=c(.1,rep(.3,3)))

plot.new()
legend(0,1,c("Direct"), lty=ltypecourbe()["S2"],lwd=3,col="green",bty="n");
legend(0.4,1,c("AK"),bty="n", lty=ltypecourbe()["AK"],lwd=3,col="blue")       
legend(0.7,1,c("R.C., $\\alpha=0.95$ "),bty="n", lty=ltypecourbe()["MRR0.95"],lwd=3,col="red")       

graphs(Recapdiff[,"unemployment",c("AK2_papa0.5-0.5" ,  "S2","MRR0.95"),"mean"],
       #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
       legende="",
       ylabe="(a) Mean",
       #ltye=rep(1,4),
       printlabels=FALSE)  

#  bias
graphs(Recapdiff[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.95"  ),"bias"],
       #colore=c("blue","green","red"),#legende=c("AK","Direct","Comp. Reg."),
       legende="",
       #ltye=rep(1,3),
       borne=c(-1,1),legendepos=c(1,-0.004) ,ylabe="(b) Bias",printlabels=FALSE)
# mse  
graphs(Recapdiff[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.95" ),"mse"],
       #colore=c("blue","green","red"),#legende=c("AK","Direct","Comp. Reg."),
       #ltye=rep(1,3),
       legende="",
       borne=c(0,1),ylabe="(c) MSE")

dev.off()




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


if(FALSE){
plot(c(1,73),range(MRRcomp$weightdisp[-6,-22,]),type="n")
for(i in 1:7){for(k in 1:22){points(1:73,MRRcomp$weightdisp[i,k,],type="l",col=i)}}

plot(c(1,73),range(MRRcomp$weightdisp[-6,"0.95",]),type="n")
for(i in 1:7){points(1:73,MRRcomp$weightdisp[i,"0.95",],type="l",col=i)}

plot(c(1,73),range(MRRcomp$weightdisp[-6,"0.75",]),type="n")
for(i in 1:7){points(1:73,MRRcomp$weightdisp[i,"0.75",],type="l",col=i)}
}
load(paste0(tablesfolder,"/list.tablesweb.Rdata"))
 


sauvegraph(texte="box",pres=FALSE,mare=c(0,5.1,0,0),heighte=3.5,widthe=if(prese){4.7}else{6})
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:2, 2, 1, byrow = TRUE))
#http://www.statmethods.net/advgraphs/layout.html

at <- (12*(0:6))+1
labels<-2005+0:6

AAA1<-sapply(list.tables[-1],function(l){gg<-(l$pwcmpwgt/l$pwsswgt)[l$pwcmpwgt>0];return(c(quantile(gg,na.rm=TRUE),mean(gg,na.rm=TRUE)))})
AAAK1<-apply(AAA1,2,popquan,N=249)


boxplot(AAAK1,range=0,ylab="(a) AK",xaxt="n",xlab="",las=1,tck=1,mare=c(0,5.1,0,0))
points(AAA1[6,],pch=4)
# 
# 
# AAA2<-apply(MRRcomp$weightdisp[-6,"0.75",],2,popquan,N=249)
# 
# boxplot(AAA2,range=0,ylab="(b) 0.75",xaxt="n",xlab="",las=1,tck=1,mare=c(0,5.1,0,0))
# points(MRRcomp$weightdisp[7,"0.75",],pch=4)

AAA3<-apply(MRRcomp$weightdisp[-6,"0.95",],2,popquan,N=249)
boxplot(AAA3,range=0,ylab="(b) R.C., $\\alpha=0.95$",xaxt="n",xlab="",las=1,tck=1)
points(MRRcomp$weightdisp[7,"0.95",],pch=4)

# 
# AAA4<-apply(MRRcomp$weightdisp[-6,"1",],2,popquan,N=249)
# boxplot(AAA4,range=0,ylab="(d) MR2",xaxt="n",xlab="",las=1,tck=1,mare=c(0,5.1,0,0))
# points(MRRcomp$weightdisp[7,"1",],pch=4)
# 
# AAA5<-apply(MRRcomp$weightdisp[-6,"01",],2,popquan,N=249)
# boxplot(AAA5,range=0,ylab="(e) MR12",xaxt="n",xlab="",las=1,tck=1)
# points(MRRcomp$weightdisp[7,"01",],pch=4)
axis(1, at=at, labels=labels,las=1,tck=1,col = "grey", lty = "dotted")
dev.off()

sauvegraph(texte="box2",pres=FALSE,mare=c(0,5.1,0,0),heighte=3.5,widthe=if(prese){5}else{6})
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
#http://www.statmethods.net/advgraphs/layout.html



AAA1<-do.call(c,lapply(list.tables[-1],function(l){gg<-(l$pwcmpwgt/l$pwsswgt)[l$pwcmpwgt>0];return(gg[!is.na(gg)])}))
AAAK1<-popquan(c(quantile(AAA1,na.rm=TRUE),mean(AAA1,na.rm=TRUE)),N=249)
AAA2<-popquan(MRRcomp$weightdisp[-6,"0.75",],N=249)
AAA3<-popquan(MRRcomp$weightdisp[-6,"0.95",],N=249)
AAA4<-popquan(MRRcomp$weightdisp[-6,"1",],N=249)


boxplot(cbind(AAAK1,AAA3),range=0,
        xaxt="n",xlab="",las=1,tck=1,mare=c(0,5.1,0,0),col=c("blue","red"))
axis(1,at=1:2, labels=c("AK","R.C.,$\\alpha=0.95$"),las=1,tck=1,col = "grey", lty = "dotted")
dev.off()









charge("resultatsserv")
##----------------------------------------------------------------
#Time series graphics for presentation
#graphs(Estimates[,"unemployment",c("S2","MRR0.95","MRR01","MRR0","MRR1","AK")])
graphs(Estimates[,"unemployment",c("AK","S2","MRR0.95")],xref=Estimates[,"unemployment",c("S2")],text="true")
#graphs(Estimatesdiff[,"unemployment",c("AK","S2","MRR0.95")],xref=Estimatesdiff[,"unemployment",c("S2")])
graphs(Estimatesdiff[,"unemployment",c("AK","S2","MRR0.95")],xref=Estimatesdiff[,"unemployment",c("S2")],text="truediff")
