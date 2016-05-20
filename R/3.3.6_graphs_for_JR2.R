load("replications.Rdata")
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
  
  default3<-c(paste0("MRR",(16:20)/20),"S2")
  for(R in c("","diff")){ 
      for(i in 1:3){
    XX=get(paste0("Recap",R))[,"unemployment",default3[i:if(R=="diff"){5}else{6}],"ratmse"]
    graphs(XX,widthe=6,heighte=8
         ,texte=paste0("ratmse",R,i)
         ,directory="/home/daniel/Dropbox/CensusBAE/Composite estimation/Survey methodology/Article/Rao comments/graphs"
          ,addgraph=paste0("text(rep(80,",5-i,"),XX[84,1:",5-i,"],(",15+i,":19)/20)"))
  }}

  
  
  figures<-
    paste0("\\begin{center}\n\\begin{figure} \n    \\caption{Ratio of estimate MSE with Direct MSE",
               rep(c(""," (for estimation of change)"),each=3),"}")
  inputs<-
    paste0(
      "\\input{graphs/graph_ratmse",
      rep(c("","diff"),each=3),rep(1:3,2),"}\\end{figure}\\end{center}")
  
  cat(paste0(
    figures,inputs,
    collapse="\n"),file=
      "/home/daniel/Dropbox/CensusBAE/Composite estimation/Survey methodology/Article/Rao comments/allgraphs2.tex")
  

