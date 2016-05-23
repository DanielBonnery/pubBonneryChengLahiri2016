#################################33
#Analysis of true data
####################################
analyseestimates<-function(){
charge("resultatsserv")
# table 1

AAA<-apply(Estimates[,"unemployment",],2,function(x){x-Estimates[,"unemployment","S2"]})
AAA<-signif(apply(AAA^2,2,mean),4)

cat(paste(
paste(
  c("$0$ (MR1)",paste0("$",(1:19)/20,"$"),"$1$ (MR2)","$0$\\& 1 (MR12)"),
  AAA[c(paste0("MR",(0:20)/20),"MR01")],
  AAA[c(paste0("MRR",(0:20)/20),"MRR01")],
  c(rep("",15),AAA["MRRH0.75"],"",AAA["MRRH0.85"],"",AAA["MRRH0.95"],"",""),
  c(AAA["AK"],rep("",21)),
  sep="&"),collapse="\\\\\n"))

AAA
plot((0:20)/20,AAA[paste0("MR",(0:20)/20)])
abline(h=AAA["MR01"])
plot(0:1,range(AAA[c(paste0("MRR",(0:20)/20),"MRR01")]))
plot((0:20)/20,AAA[paste0("MRR",(0:20)/20)])
abline(h=AAA["MRR01"])




charge(paste0("MRRwcomp","simu"))

L<-lapply(MRRwcomp$list.tablesplus[-1],
       function(l){data.matrix(l[names(l)[grep("W.",names(l))]])/125})
dime=c(nrow(L[[1]]),ncol(L[[1]]),length(MRRwcomp$list.tablesplus[-1]))
A<-      array(unlist(L), dim = dime)
dimnames(A)[2:3]<-list(colnames(L[[1]]),names(L))
varrat<-t(apply(A,2:3,var))
meanrat<-t(apply(A,2:3,mean))

plot(apply(varrat,2,mean),type="l")

var(Recapdiff[,"unemployment",c("MRR01","MRR0.95","AK_papa","S2"),"bias"])

load("replications.Rdata")

Res<-lapply(list(Recap, Recapdiff),function(R){
cvrat<-t(apply(R[,"unemployment",,"cv"],2,mean))
relbiasrat<-t(apply(R[,"unemployment",,"relbias"],2,mean))
mserat<-t(apply(R[,"unemployment",,"mse"],2,mean))
plot(apply(cvrat,2,mean),type="l")
biasrat[,order(abs(relbiasrat))]
cvrat[,order(cvrat)]
return(list(cvrat=cvrat,
            relbiasrat=relbiasrat, 
            mserat=mserat, 
            bestbiasrat=biasrat[,order(abs(relbiasrat))],
            bestcvrat=cvrat[,order(abs(cvrat))],
            bestmserat=mserat[,order(mserat)]))})

Res[[1]]$bestbiasrat
Res[[2]]$bestbiasrat
Res[[1]]$bestcvrat
Res[[2]]$bestcvrat
Res[[1]]$bestmserat
Res[[2]]$bestmserat


load(paste0(tablesfolder,"/list.tables",pcserv,".Rdata"))
AA=sapply(list.tables,function(l){w=l$pwcmpwgt/l$pwsswgt;return(list(mean(w,na.rm=TRUE),var(w,na.rm=TRUE)))})

t(sapply(list.tables,names))


dbGetQuery(conn,"SELECT * FROM sqlite_master WHERE type='table'")}