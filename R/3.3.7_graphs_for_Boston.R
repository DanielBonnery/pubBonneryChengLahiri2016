ledir<-"~/Dropbox/CensusBAE/Composite_estimation/Conferences/20140807_JSM_Boston/Presentacion/graphs"
load("replications.Rdata")
charge("resultatsserv")


sauvegraph(texte="msesim",pres=TRUE,heighte=3.2,widthe=4.5,mare=c(0,5.1,0,0),directory=ledir)
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:3, 3, 1, byrow = TRUE), 
       heights=c(.1,rep(.45,2)))

plot.new()
legend(0,1,c("Direct"), lty=1,lwd=3,col="green",bty="n");
legend(0.18,1,c("AK"),bty="n", lty=1,lwd=3,col="blue")       
legend(0.32,1,c("R.C., $\\alpha=0.75$ "),bty="n", lty=1,lwd=3,col="red")       
legend(0.65,1,c("New R.C, $\\alpha=3$ "),bty="n", lty=1,lwd=3,col="black")       

graphs(Recap[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.75","BCL23" ),"ratmse"],
       #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
       legende="",
       ylabe="(a) Level",
       colore=c("blue","green","red","black"),
       ltye=rep(1,4),
       #ltye=rep(1,4),
       printlabels=FALSE)  

graphs(Recapdiff[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.75","BCL23" ),"ratmse"],
       legende="",
       ltye=rep(1,4),
       colore=c("blue","green","red","black"),
       borne=c(0,1),ylabe="(b) Change")

dev.off()

sauvegraph(texte="cps",pres=TRUE,heighte=3.2,widthe=4.5,mare=c(0,5.1,0,0),directory=ledir)
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:3, 3, 1, byrow = TRUE), 
       heights=c(.1,rep(.45,2)))

plot.new()
legend(0,1,c("Direct"), lty=1,lwd=3,col="green",bty="n");
legend(0.18,1,c("AK"),bty="n", lty=1,lwd=3,col="blue")       
legend(0.32,1,c("R.C., $\\alpha=0.75$ "),bty="n", lty=1,lwd=3,col="red")       
legend(0.65,1,c("New R.C., $\\alpha=3$ "),bty="n", lty=1,lwd=3,col="black")       

graphs(Estimates[,"unemployment",c("AK","S2","MRR1","BCL23")],xref=Estimates[,"unemployment",c("S2")],
       #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
       legende="",
       colore=c("blue","green","red","black"),
       ltye=rep(1,4),
       ylabe="(a) Level",
       #ltye=rep(1,4),
       printlabels=FALSE)  

graphs(Estimatesdiff[,"unemployment",c("AK","S2","MRR1","BCL23")],xref=Estimatesdiff[,"unemployment",c("S2")],
       legende="",
       colore=c("blue","green","red","black"),
       ltye=rep(1,4),
       ylabe="(b) Change")

dev.off()
