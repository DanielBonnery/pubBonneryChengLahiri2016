graphdirectory<-"/home/daniel/Dropbox/CensusBAE/Composite_estimation/Articles/An Evaluation of Different Composite Estimators Using the Current Population Survey Data/Article/Second submission/graphs"
charge("resultatsserv")
load(paste0("replications.Rdata"))

orderadde2<-c(1,3,5,2,4,6)
namesadde2<-c(1:3, paste0(1:3, " with bias"))
#########################################################################
#table 1: best AK coefficients
########################################################################3
bestak<-sapply(1:3,function(i){charge(paste0("ak3_",i)); signif(unlist(ak3),3)})
coltablename<-c("",paste0("Population ",1:3))
rowtablename<-c("Level","Compromise","Change")
bestaktext<-  cbind(rowtablename,
                    sapply(1:3,function(i){paste0("$(",signif(bestak[1+6*(0:2),i],5),",",signif(bestak[4+6*(0:2),i],5),")$")}),
                    sapply(1:3,function(i){paste0("$(",signif(bestak[2+6*(0:2),i],5),",",signif(bestak[5+6*(0:2),i],5),")$")}))
bestaktextu<-  cbind(rowtablename,
                    sapply(1:3,function(i){paste0("$(",signif(bestak[1+6*(0:2),i],5),",",signif(bestak[4+6*(0:2),i],5),")$")}))
bestaktexte<-  cbind(rowtablename,
                    sapply(1:3,function(i){paste0("$(",signif(bestak[2+6*(0:2),i],5),",",signif(bestak[5+6*(0:2),i],5),")$")}))

colnamesbestak<-c("Level","Change","Compromise")
latexcode<-
  paste0("
        \\begin{tabular}{@{}rrrr@{}}
        \\toprule
         ",
         paste(coltablename,collapse=" & "),
         "\\\\ \\midrule 
         $(a_1,k_1)$ (unemployed)\\\\",
        paste(apply(bestaktextu,1,paste0,collapse=" & ") ,collapse="\\\\"),
        "\\\\\\midrule $(a_2,k_2)$ (employed)\\\\",
        paste(apply(bestaktexte,1,paste0,collapse=" & ") ,collapse="\\\\"),
        "\\\\ \\bottomrule
        \\end{tabular}
        "
        )
cat(latexcode,file=paste0(graphdirectory,"/table1.tex"))

########################################################################
#table 2
########################################################################3
popnumbias<-outer(1:3, c("","bias"),function(popnum,bias){lapply(popnum,function(l){list(popnum=l,bias=bias)})})
LL<-lapply(list.adde2[1:6],function(adde2){
  default<-paste0("MRR",(0:20)/20)
       XX<-apply(RecapA[,"unemployment",default,"ratmse",adde2],2,function(x){
         c(quantile(x),mean(x))})
       bestalphaMR<-((0:20)/20)[XX[6,]==min(XX[6,])]
       bestMR<-default[XX[6,]==min(XX[6,])]
       XXdiff=apply(RecapdiffA[,"unemployment",default,"ratmse",adde2],2,function(x){c(quantile(x),mean(x))})
       bestMRdiff<-default[XXdiff[6,]==min(XXdiff[6,])]
       bestalphaMRdiff<-((0:20)/20)[XXdiff[6,]==min(XXdiff[6,])]     
       bestMRcomp<-default[XXdiff[6,]+XX[6,]==min(XXdiff[6,]+XX[6,])]
       bestalphaMRcomp<-((0:20)/20)[XXdiff[6,]+XX[6,]==min(XXdiff[6,]+XX[6,])]
       
  return(list( bestMR=bestMR,
               bestMRdiff=bestMRdiff,
               bestMRcomp=bestMRcomp,
               bestalphaMR=bestalphaMR,
               bestalphaMRdiff=bestalphaMRdiff,
               bestalphaMRcomp=bestalphaMRcomp))})
names(LL)<-list.adde2


BESTMR<-sapply(LL,function(l){x<-
  paste0("$",c(l$bestalphaMRcomp,l$bestalphaMRdiff,l$bestalphaMRcomp),"$")
  names(x)<-c("Level","Change","Compromise")
  x})
BESTMR<-BESTMR[,orderadde2]
colnames(BESTMR)<-namesadde2



latexcode<-
  paste0("&",paste(dimnames(BESTMR)[[2]],collapse=" & "),
         "\\\\
        \\midrule 
        ",
         paste(apply(cbind(dimnames(BESTMR)[[1]],BESTMR),1,paste0,collapse=" & ") ,collapse="\\\\"))

cat(latexcode,file=paste0(graphdirectory,"/table2.tex"))
BESTMR3<-matrix("",3,3)
BESTMR2<-matrix("",3,3)
BESTMR3[,]<-paste0("(",BESTMR[,4:6],")")
BESTMR3[BESTMR[,1:3]==BESTMR[,4:6]]<-""
BESTMR2[,]<-paste(BESTMR[,1:3],BESTMR3)
latexcode<-
  paste0("&",paste(paste0("Population ",1:3),collapse=" & "),
         "\\\\
         \\midrule 
         ",
         paste(apply(cbind(dimnames(BESTMR)[[1]],BESTMR2),1,paste0,collapse=" & ") ,collapse="\\\\"))

cat(latexcode,file=paste0(graphdirectory,"/table2.tex"))




########################################################################3
# figure 1
########################################################################3

sauvegraph(texte="graph1",pres=FALSE,heighte=5,#.2,widthe=4.5,
           mare=c(0,5.1,0,0),directory=graphdirectory)
par(oma=c(2,0,1,0),mar=c(0,3,0,0),mgp=c(4,1,0))
layout(matrix(1:8, 4, 2, byrow = TRUE), 
       heights=c(.1,rep(.3,3)))

plot.new()
text(.4,0.3,"Level")
legend(0,1,c("Direct"), lty=1,lwd=3,col="green",bty="n");
legend(0.7,1,c("Best AK"),bty="n", lty=1,lwd=3,col="blue")       
#legend(0.65,1,c("M.A., $\\alpha=3$ "),bty="n", lty=1,lwd=3,col="black")       
plot.new()
text(0.4,0.3,"Change")
legend(0,1,c("R.C., Best $\\alpha$ "),bty="n", lty=1,lwd=3,col="red")       

TTRUE<-c(FALSE,FALSE,TRUE)
sapply(1:3,function(i){
  adde2=list.adde2[c(1,3,5)][i]
  graphs(RecapA[,"unemployment",c( "AK3_level" ,  "S2",LL[[adde2]]$bestMR ),"ratmse",adde2],
         #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
         legende="",
         ylabe=paste0("(",letters[i],") Population ",i),
         colore=c("blue","green","red","black"),
         ltye=rep(1,4),
         #borne=c(0,1),
         #ltye=rep(1,4),
         printlabels=TTRUE[i])
  graphs(RecapdiffA[,"unemployment",c( "AK3_level" ,  "S2",LL[[i]]$bestMR,LL[[i]]$bestBCL ),"ratmse",adde2],
         #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
         legende="",
         ylabe="",#paste0("(",letters[i],") Population ",i),
         colore=c("blue","green","red","black"),
         ltye=rep(1,4),
         printlabels=TTRUE[i])})  
dev.off()


LLL<-lapply(list.adde2,function(adde2){    
  XX<-apply(RecapA[,"unemployment",,"ratmse",adde2],2,function(x){
    c(quantile(x,na.rm=TRUE),mean(x))})
  XXdiff=apply(RecapdiffA[,"unemployment",,"ratmse",adde2],2,function(x){c(quantile(x,na.rm=TRUE),mean(x))})  
  XXcomp<-XXdiff+XX  
  return(list(XX=XX,XXdiff=XXdiff,XXcomp=XXcomp))})
names(LLL)<-list.adde2
LLL<-LLL[orderadde2]

########################################################################3
#tables 3
########################################################################3
YY<-lapply(list.adde2[c(1,3,5)],function(i){
  default=c("AK3_compromise","AK3_est","AK3_CPS",LL[[i]]$bestMRcomp,"MRR0.75")
  signif(LLL[[i]]$XX[,default],3)})
  YY<-do.call(cbind,YY)
  rownames(YY)<-c(paste0(25*(0:4),"\\%"),"Mean")

  latexcode<-
           paste(apply(cbind(rownames(YY),matrix(paste0("$",YY,"$"),dim(YY))),1,
        function(x){paste(c(x[1:6],'',x[7:11],"",x[12:16]),
                          collapse=" & ")}),collapse="\\\\")
  cat(latexcode,file=paste0(graphdirectory,"/table3.tex"))

########################################################################3
#tables 4

########################################################################3
YY<-lapply(list.adde2[c(1,3,5)],function(i){
  default=c("AK3_compromise","AK3_est","AK3_CPS","MRR0.75",LL[[i]]$bestMRcomp)
  signif(LLL[[i]]$XXdiff[,default],3)})
YY<-do.call(cbind,YY)
rownames(YY)<-c(paste0(25*(0:4),"\\%"),"Mean")

latexcode<-
  paste(apply(cbind(rownames(YY),matrix(paste0("$",YY,"$"),dim(YY))),1,
              function(x){paste(c(x[1:6],'',x[7:11],"",x[12:16]),
                                collapse=" & ")}),collapse="\\\\")
cat(latexcode,file=paste0(graphdirectory,"/table4.tex"))


########################################################################3
#tables 5
########################################################################3
YY<-lapply(list.adde2[c(1,3,5,2,4,6)],function(i){
  default=c("AK3_compromise",LL[[i]]$bestMRcomp)
  signif(LLL[[i]]$XX[,default],3)})
YY<-do.call(cbind,YY)
rownames(YY)<-c(paste0(25*(0:4),"\\%"),"Mean")

latexcode<-
  paste(apply(cbind(rownames(YY),matrix(paste0("$",YY,"$"),dim(YY))),1,
              function(x){paste(c(x[1:3],'',x[4:5],"",x[6:7],"",x[8:9],"",x[10:11],"",x[12:13]),
                                collapse=" & ")}),collapse="\\\\")
cat(latexcode,file=paste0(graphdirectory,"/table5.tex"))

########################################################################3
#tables 6

########################################################################3
YY<-lapply(list.adde2[c(1,3,5,2,4,6)],function(i){
  default=c("AK3_compromise",LL[[i]]$bestMRcomp)
  signif(LLL[[i]]$XXdiff[,default],3)})
YY<-do.call(cbind,YY)
rownames(YY)<-c(paste0(25*(0:4),"\\%"),"Mean")

latexcode<-
  paste(apply(cbind(rownames(YY),matrix(paste0("$",YY,"$"),dim(YY))),1,
              function(x){paste(c(x[1:3],'',x[4:5],"",x[6:7],"",x[8:9],"",x[10:11],"",x[12:13]),
                                collapse=" & ")}),collapse="\\\\")
cat(latexcode,file=paste0(graphdirectory,"/table6.tex"))

########################################################################3


#Figure 2

########################################################################3
toto<-function(XX,xref){range(apply(XX,2,function(x){x-xref}))}
rangexx<-matrix(NA,2,2)
rangexx[1,]=toto(Estimates[,"unemployment",c(paste0("MR",(10:20)/20),"AK")],xref=Estimates[,"unemployment","S2"])
rangexx[2,]=toto(Estimates[,"unemployment",c(paste0("MRR",(0:20)/20),"AK")],xref=Estimates[,"unemployment","S2"])
rantot<-sum(rangexx[,2])-sum(rangexx[,1])

sauvegraph(texte="graph2",pres=FALSE,mare=c(0,5.1,0,0),directory=graphdirectory,heighte=4,widthe=6)
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:3, 3, 1, byrow = TRUE), 
       heights=c(.1,.45,.45))
#http://www.statmethods.net/advgraphs/layout.html

plot.new()
legend(0,1,c("AK"), lty=ltypecourbe()["AK3_level"],lwd=3,col=colorelege(TRUE,"AK3_level"),bty="n"); 
legend(0.2,1,c("R.C. $\\alpha=0.75$"),bty="n", lty=ltypecourbe()["MR0.5"],lwd=3,col=colorelege(TRUE,"MR0.5"))
legend(0.4,1,c("MR1"), bty="n", lty=ltypecourbe()["MR0"],  lwd=3,col=colorelege(TRUE,"MR0"))
legend(0.6,1,c("MR2"), bty="n", lty=ltypecourbe()["MR1"],  lwd=3,col=colorelege(TRUE,"MR1"))              
XX=Estimates[,"unemployment",c(paste0("MRR",c(0,3.75,5)/5),"AK")]
dimnames(XX)[[2]][4]<-"AK3_level"
graphs(XX,
       xref=Estimates[,"unemployment","S2"],
       #colore=c( rep("red",11),"blue","green"),
       #ltye=rep(1,12),
       # lwde=c(rep(1,6),3),
       legende="",
       borne=c(-1,1),#rangex=rangexx[1,],
       #legende=c("AK", "Comp. Reg - 8 var."),coloreleg=c("blue","red"),
       ylabe="(a) Level",printlabels=FALSE,mare=c(0,5.1,0,0))
#text(rep(80,11),Estimates[78,"unemployment",c(paste0("MRR0.75",c(0,3.75,5)/5))]-Estimates[78,"unemployment","S2"],
 #    labels=paste('$\\alpha=',(5:10)/10,"$",sep=""),col="black")
XX<-rbind(jan05cps=0,Estimatesdiff[,"unemployment",c(paste0("MRR",c(0,3.75,5)/5),"AK")])
dimnames(XX)[[2]][4]<-"AK3_level"
graphs(XX,
       xref=c(jan05cps=0,Estimatesdiff[,"unemployment","S2"]),
       #colore=c( rep("red",11),"blue","green"),
       #ltye=rep(1,12),
       # lwde=c(rep(1,6),3),
       legende="",
       borne=c(-1,1),#rangex=rangexx[1,],
       #legende=c("AK", "Comp. Reg - 8 var."),coloreleg=c("blue","red"),
       ylabe="(b) Change",printlabels=TRUE,mare=c(0,5.1,0,0))


dev.off()







#Figure 5 (weight dispersion)



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







#Figure 6




########################################################################3
# figure 1
########################################################################3

sauvegraph(texte="old",pres=FALSE,heighte=3,#.2,widthe=4.5,
           mare=c(0,5.1,0,0),directory=graphdirectory)
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:4, 4, 1, byrow = TRUE), 
       heights=c(.1,rep(.3,3)))

plot.new()
legend(0,1,c("Direct"), lty=1,lwd=3,col="green",bty="n");
legend(0.18,1,c("AK"),bty="n", lty=1,lwd=3,col="blue")       
legend(0.32,1,c("R.C., $\\alpha=0.75$ "),bty="n", lty=1,lwd=3,col="red")       
#legend(0.65,1,c("M.A., $\\alpha=3$ "),bty="n", lty=1,lwd=3,col="black")       
TTRUE<-c(FALSE,FALSE,TRUE)
sapply(1:3,function(i){
  adde2=list.adde2[c(1,3,5)][i]
  graphs(RecapA[,"unemployment",c( "AK3_level" ,  "S2",LL[[adde2]]$bestMR ),"ratmse",adde2],
         #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
         legende="",
         ylabe=paste0("(",letters[i],") Population ",i),
         colore=c("blue","green","red","black"),
         ltye=rep(1,4),
         #borne=c(0,1),
         #ltye=rep(1,4),
         printlabels=TTRUE[i])})  
dev.off()

########################################################################3
# graph 2
########################################################################3


sauvegraph(texte="old",pres=FALSE,heighte=3,#widthe=4.5,
           mare=c(0,5.1,0,0),
           directory=graphdirectory)
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:4, 4, 1, byrow = TRUE), 
       heights=c(.1,rep(.3,3)))

plot.new()
legend(0,1,c("Direct"), lty=1,lwd=3,col="green",bty="n");
legend(0.18,1,c("AK"),bty="n", lty=1,lwd=3,col="blue")       
legend(0.32,1,c("R.C."),bty="n", lty=1,lwd=3,col="red")       
#legend(0.65,1,c("M.A."),bty="n", lty=1,lwd=3,col="black")       
TTRUE<-c(FALSE,FALSE,TRUE)

sapply(1:3,function(i){
  adde2=list.adde2[c(1,3,5)][i]
  graphs(RecapdiffA[,"unemployment",c( "AK3_level" ,  "S2",LL[[i]]$bestMR,LL[[i]]$bestBCL ),"ratmse",adde2],
         #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
         legende="",
         ylabe=paste0("(",letters[i],") Population ",i),
         colore=c("blue","green","red","black"),
         ltye=rep(1,4),
         #  borne=c(0,1),
         #ltye=rep(1,4),
         printlabels=TTRUE[i])})  
dev.off()
