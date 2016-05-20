graphdirectory<-"/home/daniel/Dropbox/CensusBAE/Composite_estimation/Articles/Model-assisted_estimation_of_unemployment_rates_for_longitudinal_surveys_with_an_application_in_the_Current_Population_Survey/JSM proceedings/graphs"
charge("resultatsserv")

#########################################################################
#table 1: best AK coefficients
########################################################################3
charge("ak1")
bestak<-ak
x=unlist(ak)
bestaktext<-sapply(0:2,function(i){paste0("$(",signif(x[1+2*i],5),",",signif(x[2+2*i],5),")$")})
names(bestaktext)<-c("Level","Compromise","Change")
latexcode<-
  paste0("",paste(c("",names(bestaktext)),collapse=" & "),
         "\\\\
         \\midrule 
         ",
         paste(c("$(A,K)$",bestaktext),collapse=" & "))
cat(latexcode,file=paste0(graphdirectory,"/table1.tex"))

########################################################################
#

########################################################################3

load("replications_1.Rdata")

default<-paste0("MRR",(0:20)/20)
XX<-apply(Recap[,"unemployment",default,"ratmse"],2,function(x){
  c(quantile(x),mean(x))})
bestalphaMR<-((0:20)/20)[XX[6,]==min(XX[6,])]
bestMR<-default[XX[6,]==min(XX[6,])]
XXdiff=apply(Recapdiff[,"unemployment",default,"ratmse"],2,function(x){c(quantile(x),mean(x))})
bestMRdiff<-default[XXdiff[6,]==min(XXdiff[6,])]
bestalphaMRdiff<-((0:20)/20)[XXdiff[6,]==min(XXdiff[6,])]     
bestMRcomp<-default[XXdiff[6,]+4*XX[6,]==min(XXdiff[6,]+4*XX[6,])]
bestMRcomp<-default[4*XXdiff[6,]+XX[6,]==min(4*XXdiff[6,]+XX[6,])]
bestalphaMRcomp<-((0:20)/20)[XXdiff[6,]+XX[6,]==min(XXdiff[6,]+XX[6,])]
bestMRcomp<-"MRR0.8"
bestalphaMRcomp<-0.8
alpha<-c(0.75,1,2,2.5,3,3.5,4,5,7.5,10);default<-paste0("BCL2",alpha);
XX<-apply(Recap[-(1:10),"unemployment",default,"ratmse"],2,function(x){c(quantile(x),mean(x))})
bestBCL<-default[XX[6,]==min(XX[6,])]
bestalphaBCL<-alpha[XX[6,]==min(XX[6,])]
XXdiff=apply(Recapdiff[-(1:10),"unemployment",default,"ratmse"],2,function(x){c(quantile(x),mean(x))})
bestBCLdiff<-default[XXdiff[6,]==min(XXdiff[6,])]
bestalphaBCLdiff<-alpha[XXdiff[6,]==min(XXdiff[6,])]
bestalphaBCLcomp<-alpha[XXdiff[6,]+XX[6,]==min(XXdiff[6,]+XX[6,])]
bestBCLcomp<-default[XXdiff[6,]+XX[6,]==min(XXdiff[6,]+XX[6,])]
LL<-list(Recap=Recap,
         Recapdiff=Recapdiff,
         bestMR=bestMR,
         bestMRdiff=bestMRdiff,
         bestMRcomp=bestMRcomp,
         bestBCL=bestBCL,
         bestBCLdiff=bestBCLdiff,
         bestBCLcomp=bestBCLcomp,
         bestalphaMR=bestalphaMR,
         bestalphaMRdiff=bestalphaMRdiff,
         bestalphaMRcomp=bestalphaMRcomp,
         bestalphaBCL=bestalphaBCL,
         bestalphaBCLdiff=bestalphaBCLdiff,
         bestalphaBCLcomp=bestalphaBCLcomp)



BESTMR<-paste0("$",c(LL$bestalphaMRcomp,LL$bestalphaMRdiff,LL$bestalphaMRcomp),"$")
BESTBCL<-paste0("$",c(LL$bestalphaBCLcomp,LL$bestalphaBCLdiff,LL$bestalphaBCLcomp),"$")

BEST<-rbind(BESTMR,BESTBCL)

colnames(BEST)<-c("Level","Change","Compromise")
rownames(BEST)<-c("Regression composite", "Model assisted")

latexcode<-
  paste0("",paste(dimnames(BEST)[[2]],collapse=" & "),
         "\\\\
         \\midrule 
         ",
         paste(apply(cbind(dimnames(BEST)[[1]],BEST),1,paste0,collapse=" & ") ,collapse="\\\\"))
cat(latexcode,file=paste0(graphdirectory,"/table2.tex"))


########################################################################3
#graph 1
########################################################################3


sauvegraph(texte="graph1",pres=FALSE,#heighte=3.2,widthe=4.5,
           mare=c(0,5.1,0,0),
           directory=graphdirectory)
par(oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0))
layout(matrix(1:3, 3, 1, byrow = TRUE), 
       heights=c(.1,rep(.45,2)))

plot.new()
legend(0,1,c("Direct"), lty=1,lwd=3,col="green",bty="n");
legend(0.18,1,c("AK"),bty="n", lty=1,lwd=3,col="blue")       
legend(0.32,1,c("R.C."),bty="n", lty=1,lwd=3,col="red")       
legend(0.65,1,c("M.A."),bty="n", lty=1,lwd=3,col="black")       

graphs(Recap[-1,"unemployment",c( "AK_level" ,  "S2",LL$bestMRcomp,LL$bestBCLcomp ),"ratmse"],
       #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
       legende="",
       ylabe="(a) Level",
       colore=c("blue","green","red","black"),
       ltye=rep(1,4),
       borne=c(0,1),
       #ltye=rep(1,4),
       printlabels=FALSE)

graphs((Recapdiff)[,"unemployment",c( "AK_level" ,  "S2",LL$bestMRcomp,LL$bestBCLcomp ),"ratmse"],
       #colore=c("black","blue","green","red"),#legende=c("True","AK","Direct","Comp. Reg.\ \ "),
       legende="",
       ylabe="(b) Change ",
       colore=c("blue","green","red","black"),
       ltye=rep(1,4),
       borne=c(0,1),
       #ltye=rep(1,4),
       printlabels=TRUE)  
dev.off()

XX<-apply(Recap[,"unemployment",,"ratmse"],2,function(x){
  c(quantile(x),mean(x))})
XXdiff=apply(Recapdiff[,"unemployment",,"ratmse"],2,function(x){c(quantile(x),mean(x))})  
XXcomp<-XXdiff+XX

LLL<-list(XX=XX,XXdiff=XXdiff,XXcomp=XXcomp)

########################################################################3
#tables 4

########################################################################3
default=c("AK_compromise",LL$bestMRcomp,LL$bestBCLcomp)
YY<-  cbind(signif(LLL$XX[,default],3),signif(LLL$XXdiff[,default],3))
rownames(YY)<-c(paste0(25*(0:4),"\\%"),"Mean")

latexcode<-
  paste(apply(cbind(rownames(YY),matrix(paste0("$",YY,"$"),dim(YY))),1,
              function(x){paste(c(x[1:4],"",x[5:7]),collapse=" & ")}),collapse="\\\\")
cat(latexcode,file=paste0(graphdirectory,"/table4.tex"))



