
#initial scrapping of the data 
library(readr)
a1 <- read_delim("http://portal.amfiindia.com/DownloadNAVHistoryReport_Po.aspx?frmdt=01-Jan-2016&todt=05-Jan-2017&tp=2", 
                 ";", escape_double = FALSE, col_types = cols(`Scheme Code` = col_number()), 
                 trim_ws = TRUE)
a1=na.omit(a1)
nrow(a1)
summary(a1)

#function to get the data till date
attach_till_today=function(a)
{
  last_date_in_a=as.character(a[nrow(a),6])
  a=as.Date(last_date_in_a, "%d-%b-%Y")+1
  from=format(a, "%d-%b-%Y")
  to=format(Sys.time(), "%d-%b-%Y")
  
  library("urltools")
  z=param_set("http://portal.amfiindia.com/DownloadNAVHistoryReport_Po.aspx?frmdt=01-Dec-2016&todt=05-Jan-2017&tp=2","frmdt",from)
  z=param_set(z,"todt",to)
  
  c = read_delim(z, ";", escape_double = FALSE, col_types = cols(`Scheme Code` = col_number()), 
                 trim_ws = TRUE)
  c=na.omit(c)
  
  #new_data=rbind(a,c)
  return(c)
}

#attacing data till date to the already present data
c1<-attach_till_today(a1)
f1<-rbind(a1,c1)
#mydata=attach_till_today(a1)


#write the data in csv
setwd("E:/Aegis/Project Session Geetanjali/Data/generated")
write.csv(x = f1,file = "~/SS.csv")

#removing the first column of the data
s1<-read.csv(file = "~/SS.csv")
s1<-s1[,-1]

#ordering the data according to scheme code
ind<-order(s1$Scheme.Code)
new_s1<-s1[ind,]

#write the sorted data in new csv
write.csv(x = new_s1,file = "Sort SS.csv",row.names = FALSE)

#read the sorted csv
new_s1=read.csv("Sort SS.csv")

nrow(new_s1)
summary(new_s1)

#TO remove the funds having NAV more than 100 i.e outlier
new_s1=subset(new_s1,Net.Asset.Value<100)
summary(new_s1)
anyNA(new_s1)

#To normalize NAV 
minmax=function(x){
  xnew=(x-min(x))/(max(x)-min(x))
  return(xnew)
}

#applynig normalization
new_s1[3:5]=minmax(new_s1[3:5])
summary(new_s1)

#plot of two different schemes
z=subset(new_s1,Scheme.Code==101834)
z1=subset(new_s1,Scheme.Code==131483)
library(ggplot2)
plot(c(1:nrow(z)),z$Net.Asset.Value,type="l")
par(new=TRUE)
plot(c(1:nrow(z1)),z1$Net.Asset.Value,type="l",col= "red")

########################################################################################################################
#reading sorted data
library(data.table)
nav<-fread(input = "Sort SS.csv")

colnames(nav)

category<-c()
str(nav)
#grep("Growth Option","HDFC FMP 371D August 2013 (1)-Direct Option-Growth Option")

#getting the NAV names
nav_name<-nav[,2]
nav_name

#loop to categorise the schemes based on there names
for(i in 1:nrow(nav_name)){
  
  if(grepl(pattern = "Dividend Option",x = nav_name[i]))
  {
    category=c(category,1)
  }
  else if(grepl(pattern = "Growth Option",x = nav_name[i]))
  {
    category=c(category,2)
  }
  else if(grepl(pattern = "Dividend Plan",x = nav_name[i]))
  {
    category=c(category,3)
  }
  else if(grepl(pattern = "Growth Plan",x = nav_name[i]))
  {
    category=c(category,4)
  }
  else if(grepl(pattern = "Regular Plan",x = nav_name[i]))
  {
    category=c(category,5)
  }
  else if(grepl(pattern = "Institutional Dividend",x = nav_name[i]))
  {
    category=c(category,6)
  }
  else if(grepl(pattern = "Dividend",x = nav_name[i]))
  {
    category=c(category,7)
  }
  else if(grepl(pattern = "Growth",x = nav_name[i]))
  {
    category=c(category,8)
  }
  else if(grepl(pattern = "Cum",x = nav_name[i]))
  {
    category=c(category,9)
  }
  else if(grepl(pattern = "Div",x = nav_name[i]))
  {
    category=c(category,10)
  }
  else if(grepl(pattern = "Direct Plan",x = nav_name[i]))
  {
    category=c(category,11)
  }
  else if(grepl(pattern = "Direct Plan-Normal Dividend",x = nav_name[i]))
  {
    category=c(category,12)
  }
  else if(grepl(pattern = "Direct Plan-Growth",x = nav_name[i]))
  {
    category=c(category,13)
  }
  else if(grepl(pattern = "Regular Plan-Growth",x = nav_name[i]))
  {
    category=c(category,14)
  }
  else if(grepl(pattern = "Direct",x = nav_name[i]))
  {
    category=c(category,15)
  }
  else
  {
    category=c(category,16) 
  }
}

#attach the new category column to the available data
new_nav<-cbind(nav,category)
write.csv(x = new_nav,file = "E:/Aegis/Project Session Geetanjali/Data/generated/Bind Nav.csv")
###########################################################################################################################



#ranking

from="01-Jan-2016"
till="31-Mar-2016"
aa1=subset(new_s1,as.Date(Date, "%d-%b-%Y")==as.Date(from, "%d-%b-%Y"))
aa2=subset(new_s1,as.Date(Date, "%d-%b-%Y")==as.Date(till, "%d-%b-%Y"))

for(i in 1:nrow(aa1)){
  for(j in 1:nrow(aa2)){
    if(aa1$Scheme.Code[i]==aa2$Scheme.Code[j]){
      aa1$diff[i]=aa2$Net.Asset.Value[j]-aa1$Net.Asset.Value[i]
    }
  }
}

new_data_with_diff=na.omit(aa1)
ind111<-order(new_data_with_diff$diff,decreasing = TRUE)
rank1<-new_data_with_diff[ind111,]

for(i in 1:nrow(rank1)){
  rank1$rank[i]=i  
}

finalrank=rank1[,-3:-6]


#######################################################################################

#generation of data frames for different scheme codes which only having 254 observations
#and saved by their scheme.code names like '125453


aaa=unique(finalrank$Scheme.Code)
yyy=as.character(unique(finalrank$Scheme.Name))
list_254=c()
for(i in 1:length(aaa)){
  temp=as.data.frame(subset(new_s1,Scheme.Code==aaa[i]))
  if(nrow(temp)==254){
    assign(yyy[i], temp)
    list_254=c(list_254,yyy[i])
  }
}




