rm(list=ls())

#四條路線代號及資料
x11.go<-read.csv("(去程)路線11敬老票.csv" ,header = TRUE, sep = ",")#整年份8161

n11<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "永隆","永隆分校","永興","永安農場","武陵","武麟山莊","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安",
       "大同農場","池上鄉公所","池上郵局","池上站","慶豐","富南國小","富里")

x11.go
n11[which(n11==x11.go[1,7])]
which(n11==x11.go[1,7]) ;which(n11==x11.go[1,11])
which(n11==x11.go[2,7]) ;which(n11==x11.go[2,11])

#乘客行為模式整理(去)
N.go<-c()
for(i in 1:length(n11)){
  if(length(which(n11[i]==x11.go[,7])) !=0){
    a<-x11.go[which(n11[i]==x11.go[,7]),]               #固定上車
    a[,11]<-factor(a[,11], levels = n11, ordered = T)   #排序下車
    aa<-a[order(a[,11]),]
    
    N.go<-rbind(N.go,aa)
  }
}



#乘客使用站點的矩陣(去)
N<-matrix(0,nrow(N.go),72)
for(i in 1:nrow(N) ){
  N[i,][which(n11==N.go[i,7])]<-1   #上車使用放"1"
  N[i,][which(n11==N.go[i,11])]<-1  #下車使用放"1"
}
sum(N)/2
#將老人使用站點(去)存成csv
write.csv(N, file = "(去程)11老人使用站點.csv", row.names = F)


NN<-c()
count.row<-0
b<-1
for(j in 1:(ncol(N)-1)){
  
  for(i in b:nrow(N)){
    if( N[i,j]==1){
      count.row<-count.row+1
    }
  }
  if(count.row>=b){
    for(l in 1:(72-j)){
      a<-rep(0,72)
      
      for(m in b:count.row){
        if( N[m,(j+l)] ==1){ a<-a+N[m,] }
      }
      if(sum(a)!=0){ NN<-rbind(NN,a) }
    }
  }else{ count.row<-b}
  if(count.row<=8380){b<-count.row+1}else{break}
  print(sum(NN)/2)
}
sum(NN)/2



