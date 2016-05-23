functionsforgraphics<-function(){
#--------------------------------------------------------------
#Fonction qui crée un graphique
sauvegraph <- function(texte,pres=prese,mare=c(3,3,0,0),
                       omae=c(.1,.1,0,0),heighte=NULL,widthe=NULL,
                       directory=NULL){
  par(oma=omae,mar=mare)
  if(is.null(directory)){directory=getwd()}
  fich<-paste0(directory,"/graph_",texte,".tex")
  if(is.null(widthe)){widthe<-if(pres){4.5}else{6}}
  if(is.null(heighte)){heighte<-if(pres){3.5}else{5}}
  tikz(file=fich, standAlone = FALSE,width=widthe,height=heighte, 
       footer = getOption("tikzFooter"),pointsize=12)}
list.adde2names<-paste0(rep("Pop. ",6),rep(1:3,each=2),rep(c(""," with bias"),3))

possiblenames<-c("S","S2",
                 "YF","estYF",
                 outer(c("AK2","AK3"),paste0("_",c(outer(c("level","change","compromise"),c("","c","sep"),paste0),"est","CPS")),paste0),
                 outer(c("MR","MRR","MRRH"),c((0:20)/20,"01"),paste0),
                 paste0("RA",(0:20)/20),
                 "BCL","BCL0",
                 paste0("BCL2",c(1:200)/20),
                 paste0("BCLratio",c(0:20)/20),
                 list.adde2)

schemaX<-c(1,1,2,22,3,57,3,3,21,2,200,21,rep(1,6))
#length(possiblenames);sum(schemaX)
namesccc<-c("True","Direct","YF","AK","MR1","RC","MR2","MR3","RA","BCL","BCL0","MA",list.adde2names)
couleurbase<-function(pres){
  ccc<-
    #if(pres){
    c("black","green","darkblue","blue","pink","red","brown","violet","orange","gray","darkgray","gray",1:6)
  #}else{rep("black",8)}
  names(ccc)<-namesccc;return(ccc)}

couleurcourbe<-function(pres){
  ccc<-rep(couleurbase(pres),schemaX)
  names(ccc)<-possiblenames;return(ccc)}

ltypecourbe<-function(){
  #  if(!pres){
  ccc<-rep(c(7,5,3,2,4,6,1,1,1,1,1,1,rep(1,6)),schemaX)
  #else{ccc=rep(1,length(possiblenames))}
  names(ccc)<-possiblenames;
  return(ccc)}

legendee<-function(pres,sel){
  ccc<-rep(namesccc,schemaX)
  names(ccc)<-possiblenames;return(unique(ccc[sel]))}

colorelege<-function(pres,sel){  couleurbase(pres)[legendee(pres,sel)]}

graphs <- function(XX,sel=NULL,xref=0,texte="",pres=prese,
                   legende=NULL,colore=NULL,
                   rangex=NULL,bornes=NULL,
                   ylabe="",maine="",ltye=NULL,lwde=NULL,
                   legendepos=NULL,printlabels=TRUE,mare=c(3,3,.1,.1),
                   coloreleg=NULL,
                   axisyat=NULL,directory=NULL,addgraph=NULL,widthe=NULL,heighte=NULL){
  if(is.null(sel)){
    if(is.matrix(XX)){sel<-colnames(XX)}
    if(is.data.frame(XX)){sel<-names(XX)}}
  
  XX<-as.matrix(XX[,sel])
  if(is.null(legende)){legende=legendee(pres,sel)}
  if(is.null(colore)){colore=couleurcourbe(pres)[sel]}
  if(is.null(ltye)){ltye=ltypecourbe()[sel]}
  if(is.null(coloreleg)){coloreleg<-colorelege(pres,sel)}
  if(is.vector(XX)){XX<-matrix(XX,length(XX),1)}
  
  nmois <- nrow(XX);nvar <- ncol(XX)
  at <- (12*(0:((nmois-1)%/%12)))+1
  if(printlabels){labels<-2005+0:((nmois-1)%/%12)}
  if(!printlabels){labels<-rep("",1+((nmois-1)%/%12))}
  T <-1:nmois
  XX <- apply(XX,2,function(x){x-xref})  
  if(is.null(ltye)){ltye=rep(1:6,(nvar%/%6)+1)}
  if(is.null(lwde)){lwde<-rep(3,nvar)}
  if(is.null(rangex)){rangex=range(XX)}
  if(!is.null(bornes)){rangex<-c(max(rangex[1],bornes[1]),min(rangex[2],bornes[2]))}
  if(is.null(legendepos)){legendepos <- c(1,rangex[2])}
  if(is.null(colore)){colore=rep("darkgray",nvar)}
  if(is.null(coloreleg)){coloreleg=colore}
  if(texte!=""){sauvegraph(texte,pres=pres,directory=directory,widthe=widthe,heighte=heighte)}
  if(legende[1]!=""){
    par(mfrow=c(2,1),oma=c(2,0,1,0),mar=c(0,6,0,0),mgp=c(4,1,0),cex=if(pres){.5}else{1})
    layout(matrix(1:2, 2, 1, byrow = TRUE), 
           heights=c(.15,.85))
    plot.new()
    for (i in 1:length(legende)){
      pos=(i-1)/(length(legende))
      legend( pos,1,legend=legende[i],  col = coloreleg[i],lty=ltye[i],lwd=2,bty="n")}
  }
  
  
  plot(c(1,nmois),rangex,type="n",
       ylab=ylabe, main =maine, xlab="",xaxt="n",yaxt="n",las=1,tck=1,mar=mare)
  axis(1, at=at, labels=labels,las=1,tck=1,col = "grey", lty = "dotted")
  if(is.null(axisyat)){axis(2,las=1,tck=1,col = "grey", lty = "dotted")}
  if(!is.null(axisyat)){axis(2,at=axisyat,las=1,tck=1,col = "grey", lty = "dotted")}
  sapply(1:nvar,function(i){points(1:nmois,XX[,i],col=colore[i],type="l",lty=ltye[i],lwd=lwde[i])})
  if(!is.null(addgraph)){eval(parse(text=addgraph))}
  if(texte!=""){dev.off()}
}}