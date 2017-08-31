#' Compute MSE
#' 
#' @param A an array with names(dimnames(A))!=NULL
#' @param Aref an array with names(dimnames(Aref))!=NULL, and names(dimnames(Aref)) is a subset of names(dimnames(A));if Aref is null only variance and mean are reported
#' @param averageindex, a vector of strings, such that names(dimnames(Aref)) \cup averageindex\subset names(dimnames(A))
#' @return MSE of A
#' @examples
#' A=array(1:(prod(2:6)),2:6);dimnames(A)<-list(s=1:2,m=1:3,y=c("1","0","_1","r"),e=1:5,i=1:6));
#' Aref=array(1:(prod(3:4)),3:4);dimnames(Aref)=dimnames(A)[2:3];
#' compMSE(A,Aref)



compMSE<-function(A,Aref,averageindex="i"){
  nA<-names(dimnames(A))
  np<-names(dimnames(Aref))
  i1<-sapply(averageindex,function(x){which(nA!=x)})
  VarA=plyr::aaply(A,i1,var)
  MeanA=plyr::aaply(A,i1,mean)
  if(!is.null(Aref)){
  i2<-match(setdiff(nA[i1],np),nA[i1])
  imerge<-match(nA[i1][if(length(i2)>0){-i2}else{T}],np)
  BiasA=plyr::aaply(MeanA,i2,function(x){x-aperm(Aref,imerge)})
  BiasA=aperm(BiasA,match(names(dimnames(VarA)),names(dimnames(BiasA))))
  MSEA=VarA+BiasA^2
  }
  return(if(is.null(Aref)){list(VarA=VarA,MeanA=MeanA)}else{MSEA=MSEA})
  }
