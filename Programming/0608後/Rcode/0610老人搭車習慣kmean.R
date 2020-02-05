rm(list=ls())

qq=read.csv("(去程)路線11老人搭車習慣.csv")
qq[1:10,1:10]
comdata<-read.csv("(去程)替代路名對照.csv" ,header = TRUE, sep = ",")

n11<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "永隆","永隆分校","永興","永安農場","武陵","武麟山莊","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安",
       "大同農場","池上鄉公所","池上郵局","池上站","慶豐","富南國小","富里")

for(i in c(1:nrow(comdata))){       #替換掉站名
  for(j in c(1:length(n11))){
    if(comdata[i,1]==n11[j]){
      n11[j]=as.character(comdata[i,2])
    }
  }
}

colnames(qq)<-n11

pcaqq = prcomp(t(qq), center = T) ############# 6/1 revised
pcaqq$sdev

vars <- (pcaqq$sdev)^2
vars
props <- vars / sum(vars)    
props
cumulative.props <- cumsum(props)  # 累加前n個元素的值
cumulative.props

windows()
plot(cumulative.props,xlab = "PCA個數", ylab = "cumulative variance" )

top2.pca.eigenvector <- pcaqq$rotation[, 1:2]
top3.pca.eigenvector <- pcaqq$rotation[, 1:3]

top8.pca.eigenvector <- pcaqq$rotation[, 1:8]

PCAPt2=t(qq)%*%top2.pca.eigenvector ############# 6/1 revised
PCAPt3=t(qq)%*%top3.pca.eigenvector ############# 6/1 revised

PCAPt8=t(qq)%*%top8.pca.eigenvector ############# 6/1 revised


kms2<-kmeans(PCAPt2, 4)
kms3<-kmeans(PCAPt3, 4)

kms8<-kmeans(PCAPt8, 4)

kms3$cluster

kms8<-kmeans(PCAPt8, 3)


plot(PCAPt2, col = kms2$cluster )#, xlim = c(-10,50), ylim = c(-45,10))
points(kms2$centers, col = 1:4, pch = 1, cex = 30)
points(kms2$centers, col = 1:4, pch = 1, cex = 20)
points(kms2$centers, col = 1:4, pch = 1, cex = 10)
points(kms2$centers, col = 1:4, pch = 8, cex = 2)

points(PCAPt2[2,], col = "dark orange", pch = 9, cex = 8)
max(PCAPt2[,1])
max(PCAPt2[,2])

min(PCAPt2[,1])
min(PCAPt2[,2])

library(rgl)
plot3d(PCAPt3 , size = 10, type = "p")
plot3d(PCAPt3 , col = kms3$cluster, size = 3, type = "n")
text3d(PCAPt3, text = n11, col = kms3$cluster)

for(i in 1:4){
  pch3d(t(kms3$centers[i,]), col = i, radius = 100, lwd = 2)
}



which(kms3$cluster==1)
which(kms3$cluster==2)
which(kms3$cluster==3)
which(kms3$cluster==4)
which(kms3$cluster==5)
which(kms3$cluster==6)
which(kms3$cluster==7)

library(ggplot2)
windows()
barplot(colSums( qq )) # 每一站(年)使用次數
A<- data.frame(counts = colSums( qq )) 
ggplot(data=A , aes(x= row.names(A) ,y= counts )) + geom_bar(stat = "identity") + xlab("站點") + ylab("刷卡次數") +
  coord_flip()   ;rm(A)

# ^  
# |  0608 

