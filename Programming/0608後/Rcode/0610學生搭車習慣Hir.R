rm(list=ls())

qq=read.csv("(去程)路線11學生搭車習慣.csv")

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

top2.pca.eigenvector <- pcaqq$rotation[, 1:2]
top3.pca.eigenvector <- pcaqq$rotation[, 1:3]

top8.pca.eigenvector <- pcaqq$rotation[, 1:8]

PCAPt2=t(qq)%*%top2.pca.eigenvector ############# 6/1 revised
PCAPt3=t(qq)%*%top3.pca.eigenvector ############# 6/1 revised

PCAPt8=t(qq)%*%top8.pca.eigenvector ############# 6/1 revised


z_x11<-PCAPt8

#----------------將中文站名轉成英文站名----------------------
# comdata<-read.csv("(去程)替代路名對照.csv" ,header = TRUE, sep = ",")
# n11<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
#        "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
#        "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
#        "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
#        "永隆","永隆分校","永興","永安農場","武陵","武麟山莊","新豐","景豐","加拿","月眉國小",
#        "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安",
#        "大同農場","池上鄉公所","池上郵局","池上站","慶豐","富南國小","富里")
# for(i in c(1:nrow(comdata))){       #替換掉站名
#   for(j in c(1:length(n11))){
#     if(comdata[i,1]==n11[j]){
#       n11[j]=as.character(comdata[i,2])
#     }
#   }
# }
#--------------------將站點分群----------------------------
# row.names(w_x11)<-n11
# clusters <-hclust(dist(w_x11, method="euclidean"), method="single")
# plot(clusters, xlab="歐式距離")
# clusterCut <- cutree(clusters,4)
# which(clusterCut==4)
# table(clusterCut)
# #hclust(E.dist, method="single")   # 最近法
# #hclust(E.dist, method="complete") # 最遠法
# #hclust(E.dist, method="average")  # 平均法
# #hclust(E.dist, method="centroid") # 中心法
# #hclust(E.dist, method="ward.D2")  # 華德法
# 
# plot(w_x11[, 1], -w_x11[, 2], type = "n", xlab = "w_x11[,1]", ylab = "w_x11[,2]", main = "路線11站點", cex.axis = 1.2, 
#      cex.lab = 1.2, cex.main = 1.6, xlim = c(-0.4, 0.7), ylim = c(-0.4, 0.9))
# text(w_x11[, 1], -w_x11[, 2], n11, xpd = NA,col=clusterCut)
# abline(h = 0, v = 0, lwd = 1.2)
# 
# for (i in 1:72) {
#   mtext(n11[i], side = 1, line = 5 + i, at = -0.4)
#   mtext(toString(c(sprintf("%.3f", w_x11[i, 1]))), side = 1, line = 5 + i, at = 0)
#   mtext(toString(c(sprintf("%.3f", w_x11[i, 2]))), side = 1, line = 5 + i, at = 0.3)
# }


#--------------------將站點分群--------------------------------------------------------
clusters <-hclust(dist(z_x11, method="euclidean"), method="single")
windows()
plot(clusters, xlab="歐氏距離")
clusterCut <- cutree(clusters,4)

for(i in 1:4){
  print(which(clusterCut==i))
}

table(clusterCut)
