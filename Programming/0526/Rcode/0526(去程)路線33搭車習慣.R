# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#讀資料
x33.go<-read.csv("(去程)路線33.csv" ,header = TRUE, sep = ",")

comdata<-read.csv("(去程)替代路名對照.csv" ,header = TRUE, sep = ",")

n33<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "瑞源","瑞源農會","瑞源教會","瑞興","瑞和","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","忠慶","新興","池上國中",
       "池上鄉公所","池上郵局","池上站")

l.n33<-length(n33)

which(x33.go[,7]==n33[1] & x33.go[,33]==n33[2])

#搭車習慣名稱
n.h<-c()
for(i in 1:71){
  for(j in (i+1):72){
    n.h<-c(n.h,paste(n33[i],n33[j]))
  }
}

#搭車習慣個數
l.n.h<-length(n.h)

x33.habit<-matrix(rep(0,l.n.h*l.n33),
                  nrow = l.n.h, 
                  ncol = l.n33,
                  dimnames = list(n.h,n33))
head(x33.habit)

k<-1
for(i in 1:(l.n33-1)){
  for(j in (i+1):l.n33){
    x33.habit[k,i]<-x33.habit[k,i]+length(which(x33.go[,7]==n33[i] & x33.go[,11]==n33[j]))
    x33.habit[k,j]<-x33.habit[k,j]+length(which(x33.go[,7]==n33[i] & x33.go[,11]==n33[j]))
    k<-k+1
  }
}
head(x33.habit)
sum(x33.habit)/2

n.x33.habit<-x33.habit[which(rowSums(x33.habit) != 0),]

head(n.x33.habit)
sum(n.x33.habit)/2

#將路線搭車習慣存成csv
write.csv(n.x33.habit, file = "(去程)路線33搭車習慣.csv", row.names = T)
