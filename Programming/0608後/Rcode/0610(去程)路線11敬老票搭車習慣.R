# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#讀資料
x11.go<-read.csv("(去程)路線11敬老票.csv" ,header = TRUE, sep = ",")

comdata<-read.csv("(去程)替代路名對照.csv" ,header = TRUE, sep = ",")

n11<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "永隆","永隆分校","永興","永安農場","武陵","武麟山莊","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安",
       "大同農場","池上鄉公所","池上郵局","池上站","慶豐","富南國小","富里")

l.n11<-length(n11)

which(x11.go[,7]==n11[1] & x11.go[,11]==n11[2])

#搭車習慣名稱
n.h<-c()
for(i in 1:71){
  for(j in (i+1):72){
    n.h<-c(n.h,paste(n11[i],n11[j]))
  }
}

#搭車習慣個數
l.n.h<-length(n.h)

x11.habit<-matrix(rep(0,l.n.h*l.n11),
                  nrow = l.n.h, 
                  ncol = l.n11,
                  dimnames = list(n.h,n11))
head(x11.habit)

k<-1
for(i in 1:(l.n11-1)){
  for(j in (i+1):l.n11){
    x11.habit[k,i]<-x11.habit[k,i]+length(which(x11.go[,7]==n11[i] & x11.go[,11]==n11[j]))
    x11.habit[k,j]<-x11.habit[k,j]+length(which(x11.go[,7]==n11[i] & x11.go[,11]==n11[j]))
    k<-k+1
  }
}
head(x11.habit)
sum(x11.habit)/2

n.x11.habit<-x11.habit[which(rowSums(x11.habit) != 0),]

head(n.x11.habit)
sum(n.x11.habit)/2

#將路線搭車習慣存成csv
# write.csv(n.x11.habit, file = "(去程)路線11老人搭車習慣.csv", row.names = F )
