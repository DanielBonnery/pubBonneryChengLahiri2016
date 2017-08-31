#' Adds unemployment rate to an array
#' 
#' @param A  a_1 x ... x a_p
#' @param dime an integer or a string indicating the dimention of the array that corresponds to the employed and unemployed
#' @return uenames, a character string indicating the dimension names corresponding to employed,unemployed and rate( will be created)
#' @examples
#' A=array(1:(prod(2:5)),2:5);dimnames(A)[[2]]<-c("unemp","emp","nilf");names(dimnames(A))[2]<-"variable";addUtoarray(A,2,c("u"="unemp","e"="emp","r"="r"));addUtoarray(A,"variable",c("u"=1,"e"=2,"r"=4))
#' X=array(1:(prod(4:6)),4:6); "%.%"(W,X,j=2);
#' W%.%.%.%t(X);

addUtoarray<-function(A,
                      dime="y",
                      uenames=c("u"="0","e"="1","r"="r")){
  indices<-rep(list(T),length(dim(A)))
  names(indices)<-names(dimnames(A))
  if(is.character(dime)){dime<-which(names(dimnames(A))==dime)}
indicesu=indices
indicesu[[dime]]=uenames["u"]
indicese=indices
indicese[[dime]]=uenames["e"]

U=do.call("[",c(list(A),indicesu,list(drop=FALSE)))
E=do.call("[",c(list(A),indicese,list(drop=FALSE)))
R=U/(U+E)
dimnames(R)[[dime]]<-if(is.na(uenames["r"])){"Unemployment Rate"}else{uenames["r"]}
A2=abind::abind(A,R,along=dime)
names(dimnames(A2))<-names(dimnames(A))
Hmisc::label(A2)<-Hmisc::label(A)
A2
}
