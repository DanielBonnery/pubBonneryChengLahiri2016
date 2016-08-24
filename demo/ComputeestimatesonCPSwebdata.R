library(pubBonneryChengLahiri2016)
library('sampling')
library("abind")
library("optimx")
library("Matrix")
library("Hmisc")
library("MASS")
library('filehash') 
library(dataCPS)
library(CompositeRegressionEstimation)
library(plyr)
library(ggplot2)
library(scales)

#1. List of tables
allmonths <- format(seq(as.Date("20050101", "%Y%m%d"),
                        as.Date("20120101", "%Y%m%d"),
                        by="month"), "%Y%m")
names(allmonths)<-allmonths

list.tablesweb<-plyr::alply(allmonths,1,function(x){
  eval(parse(text=paste0("data(cps",x,")")))
  y<-get(paste0("cps",x))
  y$pemlrR<-rep(c("1","0","_1"),c(2,2,4))[factor(y$pemlr,levels=c(1:7,"-1"))]
  y[	y$hrintsta=="1" & y$prpertyp %in% c("1","2"), ]
  y$employed=y$pemlrR=="1";y$unemployed=y$pemlrR=="0";
  y[,c("hrhhid","peage","pesex","pulineno","pehspnon","pemlr","pwsswgt","pwcmpwgt","pemlrR","hrmis","employed","unemployed")]
},.progress = "text")
rm(list=grep("cps",ls(),value=TRUE))
names(list.tablesweb)<-allmonths


#2. Computation of direct estimates
Estimates_Direct_web<-WS(list.tablesweb,list.y = "pemlrR",weight ="pwsswgt" )
names(dimnames(Estimates_Direct_web))<-c("Month","Variable")
Hmisc::label(Estimates_Direct_web)<-"Direct estimates of number of persons for each month and variable"

#3. Computation of month in samples groups contributions to estimates.
Estimates_Mis_web<-WSrg(list.tables = list.tablesweb,list.y="pemlrR",weight = "pwsswgt",rg = "hrmis")
names(dimnames(Estimates_Mis_web))<-c("Month","Months in sample","Variable")
Hmisc::label(Estimates_Mis_web)<-"Contribution of each 'months in sample' rotation group to direct estimates of number of persons for each month and Variable"

#One may check that the sum of mis contribution add up to direct estimates
all(abs(plyr::aaply(Estimates_Mis_web,c(1,3),sum)-Estimates_Direct_web)<1e-4)

#4. Computation of AK estimates
Estimates_AK_web<-AK3(dfest = 8*Estimates_Mis_web)
dimnames(Estimates_AK_web)[[2]]<-allmonths
#AKestimatesweb2<-AK2(dfest = mistotalsweb,ak = do.call(c,plyr::alply((0:10)/10,1,function(a){plyr::alply((0:10)/10,1, function(k){list(a=a,k=k)})})))
Estimates_AK_e_web<-AK(list.tables = list.tablesweb,w = "pwsswgt",id = c("hrhhid","pulineno"),groupvar = "hrmis",
                       groups_1 =c("1","2","3","5","6","7"),groups_0=paste0(c(2:4,6:8),""),list.y="employed",A=ACPSemployed(),K=KCPSemployed())
Estimates_AK_u_web<-AK(list.tables = list.tablesweb,w = "pwsswgt",id = c("hrhhid","pulineno"),groupvar = "hrmis",
                       groups_1 =c("1","2","3","5","6","7"),groups_0=paste0(c(2:4,6:8),""),list.y="unemployed",A=ACPSunemployed(),K=KCPSunemployed())
# Estimation using composite weights
Estimates_AK4_web<-WS(list.tablesweb,list.y = "pemlrR",weight ="pwcmpwgt" )

#Check we get the same estimates with the two different methods:
cbind(Estimates_AK_web[1,,1:2],Estimates_AK_u_web,Estimates_AK_e_web, Estimates_Direct_web,Estimates_AK4_web)

all(abs(Estimates_AK_web[1,,1:2]-cbind(Estimates_AK_u_web,Estimates_AK_e_web))<1.e-4)
#5.Computation of Modified regression estimates, computation of X2 totals.
list.dft.x2<-lapply(list.tablesweb,function(l){WS(list(l),list.y=c("pesex"),weight="pwsswgt")})
Estimates_MR_web<-MR(list.tables=list.tablesweb,
                     w = "pwsswgt",
                     id =c("hrhhid","pulineno"),
                     list.y = "pemlrR",
                     list.xMR=c("pemlrR"),
                     theta=3/4,
                     Alpha=c(0,0.75,1),
                     mu=NULL,
                     list.x1=NULL, #computed
                     list.x2=c("pesex"),
                     list.dft.x2=list.dft.x2)$dfEst
Estimates_MR_webA<-plyr::laply(Estimates_MR_web,function(x){x})
dimnames(Estimates_MR_webA)[[1]]<-paste0("MR",names(Estimates_MR_web))
dimnames(Estimates_MR_webA)[[2]]<-allmonths

#6. Put all estimates in the same dataframe
Estimates_all_web<-abind::abind(Estimates_MR_webA,Estimates_AK_web,Direct=array(Estimates_Direct_web,c(1,dim(Estimates_Direct_web)))
                                ,AK=array(Estimates_AK4_web,c(1,dim(Estimates_AK4_web))),along=1)

names(dimnames(Estimates_all_web))<-c("Estimator",names(dimnames(Estimates_Direct_web)))

Estimates_all_web<-addUtoarray(Estimates_all_web,3)
Estimates_all_rel_web<-plyr::aaply(Estimates_all_web,1,function(x){x-Estimates_all_web["Direct",,]})
dimnames(Estimates_all_rel_web)<-dimnames(Estimates_all_web)
Estimates_all_change_rel_web<-Estimates_all_rel_web[,-1,]-Estimates_all_rel_web[,-85,]
Estimates_all_rel_webD<-reshape2::melt(Estimates_all_rel_web,varnames=names(dimnames(Estimates_all_rel_web)))
Estimates_all_rel_webD$Month<-as.Date(paste0(Estimates_all_rel_webD$Month,"01"), "%Y%m%d")
Estimates_all_change_rel_webD<-reshape2::melt(Estimates_all_change_rel_web,varnames=names(dimnames(Estimates_all_change_rel_web)))
Estimates_all_change_rel_webD$Month<-as.Date(paste0(Estimates_all_change_rel_webD$Month,"01"), "%Y%m%d")

#7. Create figure 2 of "Multivariate Composite Estimation with An Application to the U.S. Labor Force Statistics"
figure2.a<-ggplot(data=Estimates_all_rel_webD[Estimates_all_rel_webD$Variable=="r"&
                                 is.element(Estimates_all_rel_webD$Estimator,c("MR0","MR1","MR0.75","AK")),],
       aes(x=Month,y=value)) + geom_line(aes(color=Estimator))+ scale_y_continuous("Unemployment rate",labels = scales::percent)
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels="%Y")+xlab("Time") 


figure2.b<-ggplot(data=Estimates_all_change_rel_webD[Estimates_all_change_rel_webD$Variable=="r"&
                                                is.element(Estimates_all_change_rel_webD$Estimator,c("MR0","MR1","MR0.75","AK")),],
                  aes(x=Month,y=value)) + geom_line(aes(color=Estimator))+ scale_y_continuous("Unemployment rate",labels = scales::percent) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels="%Y")+xlab("Time") 




