ledir<-"~/../Dropbox/Travail/Recherche/Présentations/201408_Yang/graphs"
ledir<-"~/Dropbox/Travail/Recherche/Présentations/201408_Yang/graphs"
load("replications.Rdata")


sauvegraph(texte="msesim",pres=prese,mare=c(0,5.1,0,0),directory=ledir)
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:3, 3, 1, byrow = TRUE), 
       heights=c(.1,rep(.45,2)))

plot.new()
legend(0,1,c("Direct"), lty=ltypecourbe()["S2"],lwd=3,col="green",bty="n");
legend(0.4,1,c("AK"),bty="n", lty=ltypecourbe()["AK"],lwd=3,col="blue")       
legend(0.7,1,c("R.C., $\\alpha=0.95$ "),bty="n", lty=ltypecourbe()["MRR0.95"],lwd=3,col="red")       

graphs(Recap[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.75" ),"mse"],
       #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
       legende="",
       ylabe="(a) Level",
       #ltye=rep(1,4),
       printlabels=FALSE)  

graphs(Recapdiff[,"unemployment",c( "AK2_papa0.5-0.5" ,  "S2","MRR0.95" ),"mse"],
       legende="",
       borne=c(0,1),ylabe="(b) Change")

charge("resultatsserv")


sauvegraph(texte="msecps",pres=prese,mare=c(0,5.1,0,0),directory=ledir)
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:3, 3, 1, byrow = TRUE), 
       heights=c(.1,rep(.45,2)))

plot.new()
legend(0,1,c("Direct"), lty=ltypecourbe()["S2"],lwd=3,col="green",bty="n");
legend(0.4,1,c("AK"),bty="n", lty=ltypecourbe()["AK"],lwd=3,col="blue")       
legend(0.7,1,c("R.C., $\\alpha=0.75$ "),bty="n", lty=ltypecourbe()["MRR0.75"],lwd=3,col="red")       

graphs(Estimates[,"unemployment",c("AK","S2","MRR1")],xref=Estimates[,"unemployment",c("S2")],
       #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
       legende="",
       colore=c("blue","green","red"),
       ltye=rep(1,3),
       ylabe="(a) Level",
       #ltye=rep(1,4),
       printlabels=FALSE)  

graphs(Estimatesdiff[,"unemployment",c("AK","S2","MRR1")],xref=Estimatesdiff[,"unemployment",c("S2")],
       legende="",
       colore=c("blue","green","red"),
       ltye=rep(1,3),
       ylabe="(b) Change")

dev.off()
