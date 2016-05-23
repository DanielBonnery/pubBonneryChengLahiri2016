Sauve <-function(file,name.add){
    commande <- paste("save(",
                      file,
                      ",file=paste(Resultsfolder,'/','",file,name.add,"','.Rdata',sep=''))", sep="") 
return(commande)}

charge <- function(file){
  load(paste(Resultsfolder,"/",file,".Rdata",sep=""),envir=.GlobalEnv)}
