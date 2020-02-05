rm(list=ls())

qq=read.csv("(去程)路線11搭車習慣.csv")
qq<-qq[-1]
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



pcaqq = prcomp(t(qq)) ############# 6/1 revised
pcaqq

vars <- (pcaqq$sdev)^2
vars
props <- vars / sum(vars)    
props
cumulative.props <- cumsum(props)  # 累加前n個元素的值
cumulative.props

top2.pca.eigenvector <- pcaqq$rotation[, 1:2]
top3.pca.eigenvector <- pcaqq$rotation[, 1:3]

PCAPt2=t(qq)%*%top2.pca.eigenvector ############# 6/1 revised
PCAPt3=t(qq)%*%top3.pca.eigenvector ############# 6/1 revised

first.pca <- top3.pca.eigenvector[, 1]   #  第一主成份
second.pca <- top3.pca.eigenvector[, 2]  #  第二主成份
third.pca <- top3.pca.eigenvector[, 3]   #  第三主成份

which(first.pca==max(abs(first.pca)))   ########  6/1 找搭車模式

kms2<-kmeans(PCAPt2, 4)
kms3<-kmeans(PCAPt3, 4)

kms2$cluster


plot(PCAPt2, col = kms2$cluster , xlim = c(-300,1000), ylim = c(-700,400))
points(kms2$centers, col = 1:4, pch = 1, cex = 30)
points(kms2$centers, col = 1:4, pch = 1, cex = 20)
points(kms2$centers, col = 1:4, pch = 1, cex = 10)
points(kms2$centers, col = 1:4, pch = 8, cex = 2)

points(PCAPt2[2,], col = "dark orange", pch = 9, cex = 8)
max(PCAPt2[,1])
max(PCAPt2[,2])

min(PCAPt2[,1])
min(PCAPt2[,2])


plot3d(PCAPt3 , col = kms3$cluster, size = 10)
pch3d(kms3$centers, col = "dark orange", radius = 300)

# ^  
# |  0608 
##############################################################

##20180529 開始
# install.packages("scatterplot3d")
# install.packages("factoextra")
# library(scatterplot3d) 
# library(factoextra)
# install.packages(rgl)
# library(rgl)

#scatterplot3d(first.pca ,second.pca ,third.pca,main = "Basic 3D Scatter Plot")
scatterplot3d(PCAPt ,main = "Basic 3D Scatter Plot") 
plot3d(PCAPt ,main = "Basic 3D Scatter Plot") 



Kdata=dist(PCAPt)
cl <- kmeans(Kdata, centers=5)
require(factoextra)
fviz_cluster(cl,           # 分群結果
             data = Kdata,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")

cl




Kdata=dist(PCAPt)
cl <- kmeans(Kdata, centers=4)
require(factoextra)
fviz_cluster(cl,           # 分群結果
             data = Kdata,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")

cl

cl <- kmeans(Kdata, centers=2)
require(factoextra)
fviz_cluster(cl,           # 分群結果
             data = Kdata,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")


cl

cl <- kmeans(Kdata, centers=2)
require(factoextra)
fviz_cluster(cl,           # 分群結果
             data = Kdata,              # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")

cl



##20180529 結束

##Kcluster <- as.factor(cl)
scatterplot3d(PCAPt col = cl$cluster, main="k-means clusters")
plot3d(PCAPt, col = cl$cluster, main="k-means clusters")
points(cl$centers, col = 1:5, pch = 50)



##------------------- 6/1 test



first.pca[order(first.pca, decreasing=FALSE)]  
dotchart(first.pca[order(first.pca, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")   

second.pca[order(second.pca, decreasing=FALSE)]
dotchart(second.pca[order(second.pca, decreasing=FALSE)] ,  # 排序後的係數
         main="Loading Plot for PC2",                       # 主標題
         xlab="Variable Loadings",                          # x軸的標題
         col="blue") 





plot3d(pcaqq$scores[,1:3], col=iris$Species)

#biplot(pcaqq, choices=1:2)  
























#######另一個PCA的函數

##pc <- princomp(t(qq), cor=TRUE, scores=TRUE)
