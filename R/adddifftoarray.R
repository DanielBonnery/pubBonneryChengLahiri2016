#' Adds unemployment rate to an array
#' 
#' @param A  a_1 x ... x a_p
#' @param dime an integer or a string indicating the dimention of the array that corresponds to the employed and unemployed
#' @return uenames, a character string indicating the dimension names corresponding to employed,unemployed and rate( will be created)
#' @examples
#' A=array(1:(prod(2:6)),2:6);dimnames(A)<-list(s=1:2,m=1:3,y=c("1","0","_1","r"),e=1:5,i=1:6);
#' adddifftoarray(A)

adddifftoarray<-function(A,timev="m",varsv="y"){
  indices<-rep(list(T),length(dim(A)))
  names(indices)<-names(dimnames(A))
  if(is.character(timev)){timev<-which(names(dimnames(A))==timev)}
  if(is.character(varsv)){varsv<-which(names(dimnames(A))==varsv)}
  indices_0=indices
  indices_0[[timev]]=-1
  indices_1=indices
  indices_1[[timev]]=-dim(A)[timev]
  indices0<-indices
  indices0[[timev]]=1
  A_0=do.call("[",c(list(A),indices_0,list(drop=FALSE)))
  A_1=do.call("[",c(list(A),indices_1,list(drop=FALSE)))
  A0=do.call("[",c(list(A),indices0,list(drop=FALSE)))
  Adiff<-abind::abind(A0,A_0-A_1,along=timev)
  AA<-abind::abind(A,Adiff, along=varsv)
  x<-dimnames(A)
  x[[varsv]]<-c(x[[varsv]],paste0("diff",x[[varsv]]))
  dimnames(AA)<-x
  AA
}