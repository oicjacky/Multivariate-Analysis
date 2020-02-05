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



pcaqq = prcomp(qq[,-1])

pcaqq

vars <- (pcaqq$sdev)^2
vars
props <- vars / sum(vars)    
props
cumulative.props <- cumsum(props)  # 累加前n個元素的值
cumulative.props


top3.pca.eigenvector <- pcaqq$rotation[, 1:3]
top3.pca.eigenvector
first.pca <- top3.pca.eigenvector[, 1]   #  第一主成份
second.pca <- top3.pca.eigenvector[, 2]  #  第二主成份
third.pca <- top3.pca.eigenvector[, 3]   #  第三主成份

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




biplot(pcaqq, choices=1:2)  

warnings()
