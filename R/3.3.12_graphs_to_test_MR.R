adde2="_1"
load(paste0("replications.Rdata"))
posMR=dimnames(RecapA)[[3]][grep("MRR",dimnames(RecapA)[[3]])]
posMR


## BIAS control
options(max.print=2000)
XX<-signif(apply(RecapA[,,c( posMR,"S2","S" ),"bias",],c(2,4,3),function(x){max(abs(x))}),3)
XX[abs(XX)<1e-10]<-0
XX[XX>=1e-10&(!is.na(XX))]<-ceiling(log(abs(XX[XX>=1e-10&!is.na(XX)]))/log(10))
XX


XX=cbind(
  RecapA[,"unemployment",c("MRR0.55"),"ratmse","_1"],
  RecapA[,"unemployment",c("MRR0.65"),"ratmse","_2"],
  RecapA[,"unemployment",c("MRR0.45"),"ratmse","_3"])

XX=cbind(
  RecapdiffA[,"unemployment",c("MRR1"),"ratmse","_1"],
  RecapdiffA[,"unemployment",c("MRR1"),"ratmse","_2"],
  RecapdiffA[,"unemployment",c("MRR0.75"),"ratmse","_3"])


colnames(XX)<-paste0("_",1:3)
graphs(XX)



graphs(RecapA[,"unemployment",c("MRR0.75","S2","AK3_level"),"ratmse","_1"])
graphs(RecapdiffA[,"unemployment",c("MRR0.75","AK3_change","S2"),"ratmse","_1"])
graphs(RecapA[,"unemployment",c("MRR0.75","S2","AK3_level"),"ratmse","_2"])
graphs(RecapdiffA[,"unemployment",c("MRR0.75","AK3_change","S2"),"ratmse","_2"])
graphs(RecapA[,"unemployment",c("MRR0.75","S2","AK3_level"),"ratmse","_3"])
graphs(RecapdiffA[,"unemployment",c("MRR0.65","S2"),"ratmse","_3"])

graphs(RecapA[,"unemployment",c("MRR0.75","S2"),"ratmse","_bias1"])
graphs(RecapdiffA[,"unemployment",c("MRR0.75","S2"),"ratmse","_bias1"])
graphs(RecapA[,"unemployment",c("MRR0.75","S2"),"ratmse","_bias2"])
graphs(RecapdiffA[,"unemployment",c("MRR0.75","S2"),"ratmse","_bias2"])
graphs(RecapA[,"unemployment",c("MRR0.75","S2"),"ratmse","_3"])
graphs(RecapdiffA[,"unemployment",c("MRR0.75","S2"),"ratmse","_3"])

graphs(RecapA[,"unemployment","MRR0.75","ratmse",paste0("_",1:3)])
graphs(RecapdiffA[,"unemployment","AK3_level","mse",paste0("_",1:3)])
graphs(RecapA[,"unemployment","AK3_level","var",list.adde2])



