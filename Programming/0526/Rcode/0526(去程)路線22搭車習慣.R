# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#讀資料
x22.go<-read.csv("(去程)路線22.csv" ,header = TRUE, sep = ",")

comdata<-read.csv("(去程)替代路名對照.csv" ,header = TRUE, sep = ",")

n22<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "永隆","永隆分校","永興","永安農場","武陵","武麟山莊","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安",
       "大同農場","池上鄉公所","池上郵局","池上站")

l.n22<-length(n22)

which(x22.go[,7]==n22[1] & x22.go[,22]==n22[2])

#搭車習慣名稱
n.h<-c()
for(i in 1:71){
  for(j in (i+1):72){
    n.h<-c(n.h,paste(n22[i],n22[j]))
  }
}

#搭車習慣個數
l.n.h<-length(n.h)

x22.habit<-matrix(rep(0,l.n.h*l.n22),
                  nrow = l.n.h, 
                  ncol = l.n22,
                  dimnames = list(n.h,n22))
head(x22.habit)

k<-1
for(i in 1:(l.n22-1)){
  for(j in (i+1):l.n22){
    x22.habit[k,i]<-x22.habit[k,i]+length(which(x22.go[,7]==n22[i] & x22.go[,11]==n22[j]))
    x22.habit[k,j]<-x22.habit[k,j]+length(which(x22.go[,7]==n22[i] & x22.go[,11]==n22[j]))
    k<-k+1
  }
}
head(x22.habit)
sum(x22.habit)/2

n.x22.habit<-x22.habit[which(rowSums(x22.habit) != 0),]

head(n.x22.habit)
sum(n.x22.habit)/2

#將路線搭車習慣存成csv
write.csv(n.x22.habit, file = "(去程)路線22搭車習慣.csv", row.names = T)
