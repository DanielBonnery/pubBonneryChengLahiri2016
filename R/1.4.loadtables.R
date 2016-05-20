pumlrR <- function(i){replace(x<-as.character(-1+2*is.element(i,c("1","2"))+is.element(i,as.character(3:4))),x=="-1","_1")}

#This function modify the dataframes of the list list.tables
#and returns a list of modified tables

Chargetables <- function(list.tables){
  L<-mclapply(list.tables,
           function(dff){
             names(dff)<-tolower(names(dff))
             colnames<-intersect(tolower(c("GESTFIPS","PUMLR","HRMIS","puhhmem",
                                 'prpertyp',"pumlrlag",
                                 'pesex'   ,
                                 'prwtrace', 
                                 'pehspnon', 
                                 'PRBLNONB', 
                                 'GESTREC')),names(dff))
             colnamesnum<-setdiff(tolower(names(dff)),colnames)
             dff[colnamesnum] <- lapply(dff[colnamesnum] ,as.numeric)
             dff[colnames] <- lapply(dff[colnames] ,as.factor)
             dff<-transform(dff,pwsswgt=pwsswgt/10000)
             dff<-transform(dff,pwcmpwgt=pwcmpwgt/10000)
             if(is.element("hwniwgt",colnames)){dff<-transform(dff,hwniwgt=hwniwgt/10000)}
             dff$pumlrR <- as.factor(pumlrR(dff$pumlr))
             if(is.element("pumlrlag",names(dff))){dff$pumlrRlag <- as.factor(pumlrR(dff$pumlrlag))
             }
                                        #assign(tables.entree[[i]],dff, envir = .GlobalEnv)
             return(dff)})
names(L)<-names(list.tables)
return(L)}
#This function modify the dataframes of the list list.tablespop
#and returns a list of modified tables

Chargetablespop <- function(list.tablespop){
  L<-mclapply(list.tablespop,
           function(dff){
             names(dff)<-tolower(names(dff))
             colnames<-intersect(tolower(c("GESTFIPS","PUMLR","puhhmem",
                                 'prpertyp',"pumlrlag",
                                 'pesex'   ,
                                 'prwtrace', 
                                 'pehspnon', 
                                 'PRBLNONB', 
                                 'GESTREC')),names(dff))
             colnamesnum<-setdiff(tolower(names(dff)),colnames)
             dff[colnamesnum] <- lapply(dff[colnamesnum] ,as.numeric)
                                        #dff[colnames] <- lapply(dff[colnames] ,as.character, stringsAsFactors=FALSE)
             dff[colnames] <- lapply(dff[colnames] ,as.factor)
             dff<-transform(dff,pwsswgt=pwsswgt/10000)
             dff<-transform(dff,pwcmpwgt=pwcmpwgt/10000)
             dff<-transform(dff,hwniwgt=hwniwgt/10000)
             dff$pumlrR <- as.factor(pumlrR(dff$pumlr))
             dff$pumlrRlag <- as.factor(pumlrR(dff$pumlrlag))
             
                                        #assign(paste(tables.entree[[i]],"_pop",sep=""),dff, envir = .GlobalEnv)
             return(dff)})
  names(L)<-names(list.tablespop)
  return(L)}

ChargetablespopA<-function(){
load(paste0(tablesfolder,"/list.tablespop.Rdata"))
tablespopA<-do.call(abind,c(lapply(list.tablespop,function(l){
  model.matrix(~0+pumlrR,l)}),list(along=3)))

save(tablespopA,file=paste0(tablesfolder,"/tablespopA.Rdata"))
}

