data<-read.csv("(回程)8161-8163-8165-8166(0514).csv" ,header = TRUE, sep = ",")


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


#路線的站點名稱
table(x11[,7])
length(which(table(x44[,7])!=0))
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
