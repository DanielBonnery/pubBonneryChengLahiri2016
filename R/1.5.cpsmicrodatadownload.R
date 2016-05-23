downloadmicro_data<-function(){
startingyear<-2005
currentyear<-2013
startingmonth<-1
currentmonth <- 10
dpath<-"~/R/tables/nber"
cps.dbname <- "cps.db"

if ( file.exists( paste( getwd() , cps.dbname , sep = "/" ) ) ){
  warning( "the database file already exists in your working directory.\nyou might encounter an error if you are running the same year as before or did not allow the program to complete.\ntry changing the cps.dbname in the settings above." )}


usePackages("RSQLite")     
usePackages("data.table")
usePackages("downloader")
usePackages("SAScii")         # load the SAScii package (imports ascii data with a SAS script)
usePackages("descr")                 # load the descr package (converts fixed-width files to delimited files)
usePackages("downloader")        # downloads and then runs the source() function on scripts from github
source_url( "https://raw.github.com/ajdamico/usgsd/master/SQLite/read.SAScii.sqlite.R" , prompt = FALSE )

#Creation of the list of the tables names.
lmoye<-function(startingmonth,currentmonth,startingyear,currentyear){
apply(
    expand.grid(
        month=1:12,
        year=startingyear:currentyear),
    1,
    FUN=as.list)[startingmonth:(12*(currentyear-startingyear)+currentmonth)]
}
#Zip file name
zipfilename<-function(m){ paste0(cps.tablenamem(m),".zip")}
#Download a zip file function
telzipfile<-function(m){
    file.location<-
        paste0( "http://www.nber.org/cps-basic/" ,monthnm(m),yearnm(m), "pub.zip" ) 
    download.file(file.location , zipfilename(m) , mode = "wb" )}
#Unzip function
unzipfile<-function(m){
    fn <- unzip(  zipfilename(m), overwrite = T )
    file.rename(fn,cps.tablenamem(m))
}
#Instruction file  download function
telinstructionfile <- function(instrfic){download.file(
    paste0( "http://www.nber.org/data/progs/cps-basic/",instrfic),
mode="wb",destfile=instrfic)}
#Instruction file name function 
instructionfile<- function(m){
      paste0(
          ifelse(100*m$year+m$month>=201301,'cpsbjan13',
                 ifelse(100*m$year+m$month>=201205,'cpsbmay12',
                        ifelse(100*m$year+m$month>=201004,'cpsbjan10',
                               ifelse(100*m$year+m$month>=200901,'cpsbjan09',
                                      ifelse(100*m$year+m$month>=200701,'cpsbjan07',
                                                      ifelse(100*m$year+m$month>=200508,'cpsbaug05',
                                                             ifelse(100*m$year+m$month>=200405,'cpsbmay04',"")))))))
        ,".sas" )
  
}
#Month function
monthnm <- function(m){tolower(month.abb[m$month])}
#Year function
yearnm <- function(m){formatC(m$year%%100, width = 2, format = "d", flag = "0")}
#CPS table name  function
cps.tablenamem <- function(m){tolower(paste0(monthnm(m),yearnm(m),"cps"))}
#Create an R file function
creeRfile <- function( cps.tablename,instructions){
    read.SAScii( 
        cps.tablename, 
        instructions , 
        zipped = FALSE)}



creeRfilem <- function(m){    
    assign( cps.tablenamem(m),
           creeRfile( cps.tablenamem(m), 
                     instructionfile(m))
           ,env=parent.frame)}



#For database:
addtodb <- function(m){     
    db <- dbConnect( SQLite() , paste(dpath,cps.dbname,sep="/") )
    read.SAScii.sqlite ( 
        cps.tablenamem(m) , 
        instructionfile(m) , 
        zipped = FALSE ,
        tl = TRUE ,
        tablename = cps.tablenamem(m) ,
        conn = db )
    dbDisconnect( db )}

                                        #lapply(lmonyea,telzipfile)

#lapply(list('bjan13','bmay12','bjan10','bjan09','rwdec07','bjan07','baug05','bmay04'),telinstructionfile)
#lapply(lmonyea,creeRfile)

allsteps <- function(m){
    print(cps.tablenamem(m))
    telzipfile(m)
    unzipfile(m)
    addtodb(m)
    file.remove(cps.tablenamem(m),
                zipfilename(m))}
#dbSendQuery(db, "DROP TABLE IF EXISTS may11cps")
#
Getweb<-function(){
  lmonyea<-lmoye(startingmonth,currentmonth,2005,currentyear)
  sapply(unique(sapply(lmonyea,instructionfile)),telinstructionfile)
  lapply(lmonyea,allsteps)
  sapply(unique(sapply(lmonyea,instructionfile)),file.remove)}

varlist  <- tolower(c(
             'pulineno',             'pwsswgt',
             'pwcmpwgt',
             'hwniwgt',
             'PUMLR'   ,
             'Pemlr'   ,
             'PEHSPNON',
             'prtage',
             'peage',
             'PESEX'   ,
             'GESTFIPS',
             'puhhmem' ,
             'prpertyp',
    'PTDTRACE',
             'prwtrace',
             'PRBLNONB',
             'GESTREC'))

vartab<-function(m){
  con <- dbConnect( SQLite() , paste(dpath,cps.dbname,sep="/") )

dd <- data.table(dbGetQuery(con,paste0( "SELECT *
 from ",cps.tablenamem(m), " LIMIT 1")))  
dbDisconnect(con)

return(!is.element(varlist,names(dd)))}
#tt <- sapply(lmonyea,vartab)
#varlist[apply(tt,1,any)]
#varlist[apply(tt,1,all)]
#cbind(varlist,apply(tt,1,sum))
downloadcps <- function(){
    lapply(lmonyea,allsteps)
}


creeRtablefromDB<-function(m){
  conn  <- dbConnect( SQLite() , paste(dpath,cps.dbname,sep="/") )
dn <- names(data.table(dbGetQuery(conn,paste0( "SELECT *
 from ",cps.tablenamem(m), " LIMIT 1"))))
  peage <- intersect(c('peage','prtage'),dn)

  dd <- data.frame(dbGetQuery(conn,tolower(paste0( "SELECT
 hrhhid as hrlongid,
             pulineno,
             pwsswgt,
             pwcmpwgt,
             pemlr as pumlr,"
            ,peage, " as peage,
             PESEX   ,
             GESTFIPS,
             prpertyp
 from ",cps.tablenamem(m)))))  
dbDisconnect(conn)
  return(dd)}

if(FALSE){
load("nov05cps.Rdata")
usePackages("plyr")
nov05cps<-rename(nov05cps,c("hrlongid"="hrhhid","pumlr"="pemlr","pesex"="PESEX","gestfips"="GESTFIPS"))
conn <- dbConnect(SQLite() , paste(dpath,cps.dbname,sep="/"))
dbRemoveTable(conn,"nov05cps")
dbWriteTable(conn, "nov05cps", nov05cps)
dbDisconnect(conn)
}
}