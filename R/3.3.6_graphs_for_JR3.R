if(FALSE){load("replications.Rdata")
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

sell=list(c(1,5,10,15,20)/20,
          c(10,12,14,16,18,20)/20,
          c(15,16,17,18,19,20)/20)

lapply(1:3,function(x){
  sele=sell[[x]]
  pos=c(84,84,84)
  for(R in c("","diff")){ 
    default3<-c(paste0("MRR",sele),if(R=="diff"){character(0)}else{"S2"})                                      
    XX=get(paste0("Recap",R))[,"unemployment",default3,"ratmse"]
    graphs(XX,widthe=6,heighte=8
           ,texte=paste0("ratmse",R,x)
           ,directory="/home/daniel/Dropbox/CensusBAE/Composite estimation/Survey methodology/Article/Rao comments/graphs"
           ,addgraph=paste0("text(rep(",pos[x]+2,",",length(sele)-1,"),c(",paste(XX[pos[x],1:(length(sele)-1)],collapse=","),"),c(",paste(sele[-length(sele)],collapse=","),"))")
    )}})



figures<-
  paste0("\\begin{center}\n\\begin{figure} \n    \\caption{MSE of Regression Composite divided by MSE od Direct",
         rep(c("(for estimation of level)"," (for estimation of change)"),each=3),"}")
inputs<-
  paste0(
    "\\input{graphs/graph_ratmse",
    rep(c("","diff"),each=3),rep(1:3,2),"}\\end{figure}\\end{center}")

cat(paste0(
  figures,inputs,
  collapse="\n"),file=
      "/home/daniel/Dropbox/CensusBAE/Composite estimation/Articles/An Evaluation of Different Composite Estimators Using the Current Population Survey Data/Rticle/Rao comments/allgraphs2.tex")



default2<-c("S2" ,"MRR01",paste0("MRR",(0:3)/4))
default3<-c(paste0("MRR",(16:20)/20))
default<-c("S2" ,"AK2_papa0.5-0.5","MRR01")
default<-c(paste0("RA",(0:20)/20),"S2" ,  "AK_papa")
funccc<-c("unemployment","pumlrR_n0","pumlrR_n1","pumlrR_n_1")
funcc2<-c("unemployment","pumlrRn0","pumlrRn1","pumlrRn_1")

rrr=c((1:5)*10,60:85)
rrr2=c((1:5)*10,60:84)

Recapa=Recap[,,2:24,]
Recapdiffa=Recapdiff[,,2:24,]

dimnames(Recapa)[[3]][2:23]<-c(paste0("$\\alpha=",(0:20)/20,"$"),"MR3")

dimnames(Recapdiffa)[[3]][2:23]<-c(paste0("$\\alpha=",(0:20)/20,"$"),"MR3")

library(stargazer)
default2<-c("MR3",paste0("$\\alpha=",(0:3)/4,"$"))
default3<-paste0("$\\alpha=",(16:20)/20,"$")
x=stargazer(title= "MSE of Regression Composite divided by MSE of Direct for level estimates",signif(Recapa[rrr,"unemployment",default2,"ratmse"],3))
y=stargazer(title= "MSE of Regression Composite divided by MSE of Direct for level estimates",signif(Recapa[rrr,"unemployment",default3,"ratmse"],3))
z=stargazer(title= "MSE of Regression Composite divided by MSE of Direct for change estimates",signif(Recapdiffa[rrr2,"unemployment",default2,"ratmse"],3))
w=stargazer(title= "MSE of Regression Composite divided by MSE of Direct for  change estimates",signif(Recapdiffa[rrr2,"unemployment",default3,"ratmse"],3))
x=gsub("textbackslash ","",c(x,y))
z=gsub("textbackslash ","",c(z,w))
x=gsub("\\$","$",x,fixed=TRUE)
z=gsub("\\$","$",z,fixed=TRUE)
zzz=c(4+56*(0:20),(16*56)+c(9,10,12,16,17,19,23,24,26))

cat(paste(c(x,z),collapse="\n"),file=
    "/home/daniel/Dropbox/CensusBAE/Composite estimation/Articles/An Evaluation of Different Composite Estimators Using the Current Population Survey Data/Rticle/Rao comments/tables.tex")

}