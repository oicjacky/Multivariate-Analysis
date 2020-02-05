# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#讀資料
setwd("C:/Users/User/Desktop/(去程)修改路名0514")#學校電腦
data<-read.csv("(去程)修改路名0514.csv",header = TRUE, sep = ",")
attach(data)
head(data)
data[,4]<-as.Date(data[,4])#把日期當作日期
data<-data[order(data[,4]),]#排序

x8165<-data[路線==8165,1:11]#整年份8165
x8166<-data[路線==8166,1:11]#整年份8166
x8161<-data[路線==8166,1:11]#整年份8161
x8163<-data[路線==8166,1:11]#整年份8163

m<-min(which(x8165[,4]=="2016/1/1"))
M<-max(which(x8165[,4]=="2016/1/20"))
x8165_1<-x8165[m:M,]#8165的第一個月
head(x8165_1)
table(x8165[,7])
length(which(table(x8165[,7])!=0))#知道站點數

##時間跟站點圖

