douuble <- function(list.tables,
                    w="pwsswgt",
                    id=c("hrhhid","pulineno"),
                    list.y="pemlrR"){
  LL<-length(list.tables)
  keep= c(id,w,list.y)
  doubble=plyr::llply(2:LL,function(i){
    df<-merge(list.tables[[i-1]][keep],list.tables[[i]][keep],by=id,all=FALSE)
    df0<-merge(list.tables[[i-1]][keep],list.tables[[i]][keep],by=id,all.x=TRUE)
    df1<-merge(list.tables[[i-1]][keep],list.tables[[i]][keep],by=id,all.y=TRUE)
    df0<-df0[is.na(df0[[paste0(list.y,".y")]]),]
    df1<-df1[is.na(df1[[paste0(list.y,".x")]]),]
    NN=aggregate(df["pwsswgt.y"],sum,by=list(factor(df[[paste0(list.y,".x")]]),factor(df[[paste0(list.y,".y")]])))    
    N01<-matrix(0,nlevels(NN$Group.1),nlevels(NN$Group.2))
    for (i in 1:nrow(NN)){N01[NN$Group.1[i],NN$Group.2[i]]<-NN$pwsswgt.y[i]}
    rownames(N01)<-levels(NN$Group.1)
    colnames(N01)<-levels(NN$Group.2)
    NN0=aggregate(df0["pwsswgt.x"],sum,by=list(factor(df0[[paste0(list.y,".x")]])))
    NN1=aggregate(df1["pwsswgt.y"],sum,by=list(factor(df1[[paste0(list.y,".y")]])))
    N0=NN0$pwsswgt.x;names(N0)<-NN0$Group.1
    N1=NN1$pwsswgt.y;names(N1)<-NN1$Group.1
    list(N01=N01,N0=N0,N1=N1)},.progress = "text")
  
  N01<-plyr::laply(doubble,function(l){l$N01})
  N1<-plyr::laply(doubble,function(l){l$N1})
  N0<-plyr::laply(doubble,function(l){l$N0})
  dimnames(N01)[[1]]<-dimnames(N0)[[1]]<-dimnames(N1)[[1]]<-names(list.tables)[-1]
  names(dimnames(N01))<-c("t","i","j")
  names(dimnames(N0))<-c("t","i")
  names(dimnames(N1))<-c("t","j")
  Hmisc::label(N0)<-paste0("Counts of ",list.y,"(t-1)=i, t missing  weighted by ",w,"(t-1)")
  Hmisc::label(N1)<-paste0("Counts of ",list.y,"(t)=j), t-1 missing, weighted by ",w,"(t)")
  Hmisc::label(N01)<-paste0("Counts of ",list.y,"(t-1)=i,",list.y,"(t)=j) weighted by ",w,"(t)")
  
  #eval(parse(text=Sauve("doubble","serv")))
  return(list(N01=N01,N0=N0,N1=N1))}
