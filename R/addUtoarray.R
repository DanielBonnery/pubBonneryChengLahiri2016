addUtoarray<-function(A,
                      dime,
                      uenames=c("u"="0","e"="1","r"="r")){
  indices<-rep(list(T),length(dim(A)))
indicesu=indices
indicesu[[dime]]=uenames["u"]
indicese=indices
indicese[[dime]]=uenames["e"]

U=do.call("[",c(list(A),indicesu,list(drop=FALSE)))
E=do.call("[",c(list(A),indicese,list(drop=FALSE)))
R=U/(U+E)
dimnames(R)[[dime]]<-uenames["r"]
A2=abind::abind(A,R,along=dime)
names(dimnames(A2))<-names(dimnames(A))
Hmisc::label(A2)<-Hmisc::label(A)
A2
}


if(FALSE){
  A=array(rnorm(24),c(3,2,4))
  Hmisc::label(A)<-"test"
  dimnames(A)[[2]]<-c("0","1")
  names(dimnames(A))<-c("month","es","toto")
  A2<-addUtoarray(A,dime=2)
}