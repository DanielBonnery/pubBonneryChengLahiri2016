#' Compute relative MSE
#' 
#' @param MSEA an array with names(dimnames(A))!=NULL
#' @param MSEAref an array with names(dimnames(Aref))!=NULL, and names(dimnames(Aref)) is a subset of names(dimnames(A));if Aref is null only variance and mean are reported
#' @return MSEA/MSEAref
#' @examples
#' MSEA=array(1:(prod(2:6)),2:6);dimnames(MSEA)<-list(s=1:2,m=1:3,y=c("1","0","_1","r"),e=1:5,i=1:6);
#' MSEAref=array(1:(prod(3:4)),3:4);dimnames(MSEAref)=dimnames(MSEA)[2:3];
#' relMSE(MSEA,MSEAref)



relMSE<-function(MSEA,MSEAref){
  nA<-names(dimnames(MSEA))
  np<-names(dimnames(MSEAref))
  i<-match(setdiff(nA,np),nA)
  imerge<-match(np,nA[if(length(i)>0){-i}else{T}])
  plyr::aaply(MSEA,i,function(x){aperm(x,imerge)/MSEAref})
}
