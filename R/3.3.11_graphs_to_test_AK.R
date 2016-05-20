adde2="_1"
load(paste0("replications.Rdata"))
posAK=dimnames(RecapA)[[3]][grep("AK",dimnames(RecapA)[[3]])]
posAK


## BIAS control
options(max.print=2000)
XX<-signif(apply(RecapA[,,c( posAK,"S2","S" ),"bias",],c(2,4,3),function(x){max(abs(x))}),3)
XX[abs(XX)<1e-10]<-0
XX[XX>=1e-10&(!is.na(XX))]<-ceiling(log(abs(XX[XX>=1e-10&!is.na(XX)]))/log(10))
XX

charge("ak3_1");ak3
charge("ak3sep_1");ak3sep
charge("ak3_2");ak3
charge("ak3sep_2");ak3sep
charge("ak3_3");ak3
charge("ak3sep_3");ak3sep


charge("estAK3comprep_3")
XX<-estAK3comprep[,"unemployment",1,];range(XX)
XX<-estAK3comprep[,"pumlrR_n0",1,];range(XX)
XX<-estAK3comprep[,"pumlrR_n1",1,]+estAK3comprep[,"pumlrR_n0",1,];range(XX)

graphs(RecapA[,"unemployment",c("AK3_CPS"),"ratmse",])
graphs(RecapA[,"unemployment",c("AK3_level"),"ratmse",])
graphs(RecapA[,"unemployment",c("AK3_levelsep"),"ratmse",])

graphs(RecapA[,"pumlrR_n0",c("YF"),"ratmse",paste0("_",1:3)])
graphs(RecapA[,"pumlrR_n0",c("AK3_CPS"),"ratmse",paste0("_",1:3)])
graphs(RecapA[,"pumlrR_n0",c("AK3_level"),"ratmse",paste0("_",1:3)])
graphs(RecapA[,"pumlrR_n0",c("AK3_levelsep"),"ratmse",paste0("_",1:3)])
graphs(RecapA[,"pumlrR_n0",c("AK3_levelsep","S2","YF"),"ratmse","_3"])




sapply(1:3,function(i){
graphs(RecapA[,"unemployment",c("AK3_CPS","AK3_level"),"ratmse",paste0("_",i)],colore=c("blue","red"),legende=c("best","CPS"),borne=c(0,2))
})


graphs(RecapA[,"unemployment",c("AK3_level","S2"),"ratmse","_1"])
graphs(RecapA[,"unemployment",c("AK3_levelc","S2"),"ratmse","_1"])
graphs(RecapdiffA[,"unemployment",c("AK3_change","S2"),"ratmse","_1"])
graphs(RecapdiffA[,"unemployment",c("AK3_changec","S2"),"ratmse","_1"])
graphs(RecapA[,"unemployment",c("AK3_level","S2"),"ratmse","_2"])
graphs(RecapdiffA[,"unemployment",c("AK3_change","S2"),"ratmse","_2"])

graphs(RecapA[,"pumlrR_n1","YF","ratmse",paste0("_",1:3)])
graphs(RecapA[,"pumlrR_n1","AK3_level","bias",paste0("_",1:3)])
graphs(RecapA[,"pumlrR_n1","AK3_levelc","bias",paste0("_",1:3)])
graphs(RecapA[,"unemployment","AK3_level","ratmse",paste0("_",1:2)])
graphs(RecapA[,"unemployment","AK3_levelc","ratmse",paste0("_",1:2)])
graphs(RecapdiffA[,"unemployment","AK3_level","mse",paste0("_",1:2)])
graphs(RecapA[,"unemployment","AK3_level","var",list.adde2])




