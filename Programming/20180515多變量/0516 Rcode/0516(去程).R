# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()
#讀資料
data<-read.csv("(去程)(原始資料).csv" ,header = TRUE, sep = ",")
data.noname<-read.csv("(去程)修改路名0514.csv" ,header = TRUE, sep = ",")
A<-data.noname[,-2]
comdata<-read.csv("(去程)替代路名對照.csv" ,header = TRUE, sep = ",")
head(data)
summary(data.noname[,-2])
##對照表
data[,4]<-as.Date(data[,4])#把日期當作日期
data<-data[order(data[,4]),]#排序

for(i in c(1:nrow(data))){
  if(data[i,1]==8161){
    data[i,1]=11
  }else if(data[i,1]==8163){
    data[i,1]=22
  }else if(data[i,1]==8165){
    data[i,1]=33
  }else{
    data[i,1]=44
  }
}
attach(data)
#四條路線代號
x11<-data[路線==11,]#整年份8161
x22<-data[路線==22,]#整年份8163
x33<-data[路線==33,]#整年份8165
x44<-data[路線==44,]#整年份8166

#8161十二個月
#####
x11_1<-x11[which(x11[,4]<="2016/1/31"),]
x11_2<-x11[which(x11[,4]>="2016/1/31" & x11[,4]<="2016/2/28"),]
x11_3<-x11[which(x11[,4]>="2016/2/28" & x11[,4]<="2016/3/31"),]
x11_4<-x11[which(x11[,4]>="2016/3/31" & x11[,4]<="2016/4/30"),]
x11_5<-x11[which(x11[,4]>="2016/4/30" & x11[,4]<="2016/5/31"),]
x11_6<-x11[which(x11[,4]>="2016/5/31" & x11[,4]<="2016/6/30"),]
x11_7<-x11[which(x11[,4]>="2016/6/30" & x11[,4]<="2016/7/31"),]
x11_8<-x11[which(x11[,4]>="2016/7/31" & x11[,4]<="2016/8/31"),]
x11_9<-x11[which(x11[,4]>="2016/8/31" & x11[,4]<="2016/9/30"),]
x11_10<-x11[which(x11[,4]>="2016/9/30" & x11[,4]<="2016/10/31"),]
x11_11<-x11[which(x11[,4]>="2016/10/31" & x11[,4]<="2016/11/30"),]
x11_12<-x11[which(x11[,4]>="2016/11/30" & x11[,4]<="2016/12/31"),]

#路線的站點名稱
table(x11[,7])
length(which(table(x44[,11])!=0))
which(table(x11[,7])==0)
n11<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "永隆","永隆分校","永興","永安農場","武陵","武麟山莊","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安",
       "大同農場","池上鄉公所","池上郵局","池上站","慶豐","富南國小","富里")

n22<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "永隆","永隆分校","永興","永安農場","武陵","武麟山莊","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安",
       "大同農場","池上鄉公所","池上郵局","池上站")

n33<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "瑞源","瑞源農會","瑞源教會","瑞興","瑞和","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","忠慶","新興","池上國中",
       "池上鄉公所","池上郵局","池上站")

n44<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "瑞源","瑞源農會","瑞源教會","瑞興","瑞和","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏",
       "陸安","大同農場","池上鄉公所","池上郵局","池上站")

#把站點排序
x11[,7]<-factor(x11[,7],levels=n11,ordered=T) ;length(which(is.na(x11[,7]==T)))
x22[,7]<-factor(x22[,7],levels=n22,ordered=T) ;length(which(is.na(x22[,7]==T)))
x33[,7]<-factor(x33[,7],levels=n33,ordered=T) ;length(which(is.na(x33[,7]==T)))
x44[,7]<-factor(x44[,7],levels=n44,ordered=T) ;length(which(is.na(x44[,7]==T)))


#上車整年份四條路線圖
#----------------
windows()
par(mfrow=c(2,2))
#路線11整年的圖
for(i in c(1:nrow(comdata))){       #替換掉站名
  for(j in c(1:length(n11))){
    if(comdata[i,1]==n11[j]){
      n11[j]=as.character(comdata[i,2])
    }
  }
}
plot(x=x11[,6]/60,                 # X軸的值
     y=x11[,7],                    # Y軸的值
     main="路線11,時間與站點",     # 圖片名稱
     xlab="時間(小時)",            # X軸名稱
     ylab="站點",                  # Y軸名稱
     yaxt='n')   
axis(2,c(1:length(n11)),n11)
#路線22整年的圖
for(i in c(1:nrow(comdata))){    #替換掉站名
  for(j in c(1:length(n22))){
    if(comdata[i,1]==n22[j]){
      n22[j]=as.character(comdata[i,2])
    }
  }
}
plot(x=x22[,6]/60,                 # X軸的值
     y=x22[,7],                    # Y軸的值
     main="路線22,時間與站點",     # 圖片名稱
     xlab="時間(小時)",            # X軸名稱
     ylab="站點",                  # Y軸名稱
     yaxt='n')   
axis(2,c(1:length(n22)),n22)

#路線33整年上車的圖
for(i in c(1:nrow(comdata))){     #替換掉站名
  for(j in c(1:length(n33))){
    if(comdata[i,1]==n33[j]){
      n33[j]=as.character(comdata[i,2])
    }
  }
}
plot(x=x33[,6]/60,                 # X軸的值
     y=x33[,7],                    # Y軸的值
     main="路線33,時間與站點",     # 圖片名稱
     xlab="時間(小時)",            # X軸名稱
     ylab="站點",                  # Y軸名稱
     yaxt='n')   
axis(2,c(1:length(n33)),n33)

#路線44整年的圖
for(i in c(1:nrow(comdata))){       #替換掉站名
  for(j in c(1:length(n44))){
    if(comdata[i,1]==n44[j]){
      n44[j]=as.character(comdata[i,2])
    }
  }
}
plot(x=x44[,6]/60,                  # X軸的值
     y=x44[,7],                     # Y軸的值
     main="路線44,時間與站點",      # 圖片名稱
     xlab="時間(小時)",             # X軸名稱
     ylab="站點",                   # Y軸名稱 
     yaxt='n')  
axis(2,c(1:length(n44)),n44)

#----------------
windows()
par(mfrow=c(2,2))
#路線11整年各站點使用量
barplot(table(x11[,7]),axisnames = T,
        names.arg = n11, cex.names = 0.5,
        ylim = c(0,2000),
        main="路線11,站點Barplot",      # 圖片名稱
        xlab="站點",                    # X軸名稱
        ylab="Frequence")               # Y軸名稱 

#路線22整年各站點使用量
barplot(table(x22[,7]),axisnames = T,
        names.arg = n22, cex.names = 0.5,
        ylim = c(0,5000),
        main="路線22,站點Barplot",      # 圖片名稱
        xlab="站點",                    # X軸名稱
        ylab="Frequence")               # Y軸名稱 

#路線33整年各站點使用量
barplot(table(x33[,7]),axisnames = T,
        names.arg = n33, cex.names = 0.5,
        ylim = c(0,2000),
        main="路線33,站點Barplot",      # 圖片名稱
        xlab="站點",                    # X軸名稱
        ylab="Frequence")               # Y軸名稱 

#路線44整年各站點使用量
barplot(table(x44[,7]),axisnames = T,
        names.arg = n44, cex.names = 0.5,
        ylim = c(0,2000),
        main="路線44,站點Barplot",      # 圖片名稱
        xlab="站點",                    # X軸名稱
        ylab="Frequence")               # Y軸名稱 
#-------------------


#把下車站點排序
x11[,11]<-factor(x11[,11],levels=n11,ordered=T) ;length(which(is.na(x11[,11]==T)))
x22[,11]<-factor(x22[,11],levels=n22,ordered=T) ;length(which(is.na(x22[,11]==T)))
x33[,11]<-factor(x33[,11],levels=n33,ordered=T) ;length(which(is.na(x33[,11]==T)))
x44[,11]<-factor(x44[,11],levels=n44,ordered=T) ;length(which(is.na(x44[,11]==T)))


#下車整年份四條路線圖
#--------------------
#路線11整年的圖
windows()
par(mfrow=c(2,2))
for(i in c(1:nrow(comdata))){       #替換掉站名
  for(j in c(1:length(n11))){
    if(comdata[i,1]==n11[j]){
      n11[j]=as.character(comdata[i,2])
    }
  }
}
plot(x=x11[,10]/60,            # X軸的值
     y=x11[,11],             # Y軸的值
     main="路線11,時間與站點",   # 圖片名稱
     xlab="時間(小時)",            # X軸名稱
     ylab="站點",
     yaxt='n') # Y軸名稱  
axis(2,c(1:length(n11)),n11)
#路線22整年的圖
for(i in c(1:nrow(comdata))){    #替換掉站名
  for(j in c(1:length(n22))){
    if(comdata[i,1]==n22[j]){
      n22[j]=as.character(comdata[i,2])
    }
  }
}
plot(x=x22[,10]/60,            # X軸的值
     y=x22[,11],             # Y軸的值
     main="路線22,時間與站點",   # 圖片名稱
     xlab="時間(小時)",            # X軸名稱
     ylab="站點",
     yaxt='n') # Y軸名稱  
axis(2,c(1:length(n22)),n22)

#路線33整年上車的圖
for(i in c(1:nrow(comdata))){     #替換掉站名
  for(j in c(1:length(n33))){
    if(comdata[i,1]==n33[j]){
      n33[j]=as.character(comdata[i,2])
    }
  }
}
plot(x=x33[,10]/60,            # X軸的值
     y=x33[,11],             # Y軸的值
     main="路線33,時間與站點",   # 圖片名稱
     xlab="時間(小時)",            # X軸名稱
     ylab="站點",
     yaxt='n') # Y軸名稱  
axis(2,c(1:length(n33)),n33)

#路線44整年的圖
for(i in c(1:nrow(comdata))){    #替換掉站名
  for(j in c(1:length(n44))){
    if(comdata[i,1]==n44[j]){
      n44[j]=as.character(comdata[i,2])
    }
  }
}
plot(x=x44[,10]/60,            # X軸的值
     y=x44[,11],             # Y軸的值
     main="路線44,時間與站點",   # 圖片名稱
     xlab="時間(小時)",            # X軸名稱
     ylab="站點",
     yaxt='n') # Y軸名稱  
axis(2,c(1:length(n44)),n44)

#----------------
windows()
par(mfrow=c(2,2))
#路線11整年各站點使用量
barplot(table(x11[,11]),axisnames = T,
        names.arg = n11, cex.names = 0.5,
        ylim = c(0,2000),
        main="路線11,站點Barplot",      # 圖片名稱
        xlab="站點",                    # X軸名稱
        ylab="Frequence")               # Y軸名稱 

#路線22整年各站點使用量
barplot(table(x22[,11]),axisnames = T,
        names.arg = n22, cex.names = 0.5,
        ylim = c(0,5000),
        main="路線22,站點Barplot",      # 圖片名稱
        xlab="站點",                    # X軸名稱
        ylab="Frequence")               # Y軸名稱 

#路線33整年各站點使用量
barplot(table(x33[,11]),axisnames = T,
        names.arg = n33, cex.names = 0.5,
        ylim = c(0,2000),
        main="路線33,站點Barplot",      # 圖片名稱
        xlab="站點",                    # X軸名稱
        ylab="Frequence")               # Y軸名稱 

#路線44整年各站點使用量
barplot(table(x44[,11]),axisnames = T,
        names.arg = n44, cex.names = 0.5,
        ylim = c(0,2000),
        main="路線44,站點Barplot",      # 圖片名稱
        xlab="站點",                    # X軸名稱
        ylab="Frequence")               # Y軸名稱 
#--------------------