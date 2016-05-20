#Creation of the list of the tables names.
lta<-function(startingmonth,currentmonth,startingyear,currentyear){
paste(tolower(as.vector((outer(substr(month.name,1,3),formatC((startingyear:currentyear)-2000, width = 2, format = "d", flag = "0"),FUN=paste,sep='')))),"cps",sep='')[startingmonth:(12*(currentyear-startingyear)+currentmonth)]}
