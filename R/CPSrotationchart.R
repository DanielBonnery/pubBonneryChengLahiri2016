CPSrotationchart<-function(){
n<-20
i=rep(5:7,each=12)[1:20]
j=rep(month.abb,3)[1:20]
yy<-function(x){if(is.element(x[2]-x[1],c(0:3,12:15))){paste0("$S_{",x[1],"!,!",(x[2]-x[1])%%8+1,"}$")}else{""}}
zz<-function(i,j){apply(cbind(i,j),1,yy)}
A=outer(1:n,1:n,zz)
colnamesA<-paste(c("",paste0("$G_{",1:n,"}$")),collapse="&")
rownames(A)<-paste0(j," 0",i)
A<-cbind(rownames(A),A)
x=stargazer::stargazer(A,caption="CPS rotation chart",label="table:rotchart",column.sep.width = "0pt")
x=gsub("\\\\\\\\","toto",x[-(1:3)]) 
x=gsub("\\\\","",x) 
x=gsub("toto","\\\\\\\\",x) 
x<-c(x[1:5],paste0(colnamesA,"\\\\\\\\"),x[-(1:5)])
x=gsub("hline","\\\\hline",x) 
x=gsub("begin","\\\\begin",x) 
x=gsub("end","\\\\end",x) 
x=gsub("caption","\\\\caption",x) 
x=gsub("label","\\\\label",x) 
x=gsub("centering","\\\\centering",x) 
x=gsub("extracolsep","\\\\extracolsep",x) 
x=gsub("!","\\\\!",x[-((length(x)-12):(length(x)-10))]) 
cat(x)}

