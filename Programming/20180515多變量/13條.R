setwd("C:/Users/USER/Desktop/13條(含支線)")
data<-read.csv("(去程)61-63-65-66-67-68-68A-70-70A-70B-71-71A-71B.csv",header = T, sep = ",")
data2<-read.csv("(回程)61-63-65-66-67-68-68A-70-70A-70B-71-71A-71B.csv",header = T, sep = ",")
data<-data[,-12:-14]
data2<-data2[,-12:-14]

data<-data[-c(which(data$上車分鐘<=300)),]#刪除5點前的資料
data2<-data2[-c(which(data2$上車分鐘<=300)),]#刪除5點前的資料

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(ggplot2)
library(plotly)
library(epade)
library("scatterplot3d")
library(reshape2)
#來回分開票種數
p1<-qplot(as.character(data$票種), data = data, geom = "bar",fill = data$票種) + ggtitle("票種(去程)直方圖") + ylab("數量") + xlab("票種") + labs(fill = "交易票種") + geom_text(stat="count",aes(label=..count..),vjust=-0.2)
p2<-qplot(as.character(data2$票種), data = data2, geom = "bar",fill = data2$票種) + ggtitle("票種(回程)直方圖") + ylab("數量") + xlab("票種") + labs(fill = "交易票種") + geom_text(stat="count",aes(label=..count..),vjust=-0.2)
multiplot(p1, p2, cols=1)

#來回總票種數
#qplot(as.character(data$路線), data = data, geom = "bar",fill = data$票種) + ggtitle("路線-票種直方圖") + ylab("數量") + xlab("路線編號") + labs(fill = "交易票種") + geom_text(stat="count",aes(label=..count..),vjust=-1)
p1<-ggplot(data=data,aes(as.character(data$路線),fill=data$票種)) + geom_bar(position="dodge") + ggtitle("(去程)路線-票種直方圖") + ylab("數量") + xlab("路線") + labs(fill = "交易票種") + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
p2<-ggplot(data=data2,aes(as.character(data2$路線),fill=data2$票種)) + geom_bar(position="dodge") + ggtitle("(回程)路線-票種直方圖") + ylab("數量") + xlab("路線") + labs(fill = "交易票種") + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
multiplot(p1, p2, cols=1)
#分去程路線
L_8161<-subset(data,data$路線==8161)
L_8163<-subset(data,data$路線==8163)
L_8165<-subset(data,data$路線==8165)
L_8166<-subset(data,data$路線==8166)
L_8167<-subset(data,data$路線==8167)
L_8168<-subset(data,data$路線==8168)
L_8168A<-subset(data,data$路線=="8168A")
L_8170<-subset(data,data$路線==8170)
L_8170A<-subset(data,data$路線=="8170A")
L_8170B<-subset(data,data$路線=="8170B")
L_8171<-subset(data,data$路線==8171)
L_8171A<-subset(data,data$路線=="8171A")
L_8171B<-subset(data,data$路線=="8171B")

#分回程路線
L_8161B<-subset(data2,data2$路線==8161)
L_8163B<-subset(data2,data2$路線==8163)
L_8165B<-subset(data2,data2$路線==8165)
L_8166B<-subset(data2,data2$路線==8166)
L_8167B<-subset(data2,data2$路線==8167)
L_8168B<-subset(data2,data2$路線==8168)
L_8168A_B<-subset(data2,data2$路線=="8168A")
L_8170_B<-subset(data2,data2$路線==8170)
L_8170A_B<-subset(data2,data2$路線=="8170A")
L_8170B_B<-subset(data2,data2$路線=="8170B")
L_8171_B<-subset(data2,data2$路線==8171)
L_8171A_B<-subset(data2,data2$路線=="8171A")
L_8171B_B<-subset(data2,data2$路線=="8171B")

#去程站點排序
a1<-c("台東","中央市場","簡易法庭","署東醫院","變電所","舊縣議會","新生國小","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野","鹿野鄉公所","水源地","永隆","永隆分校","飛行夢工廠","永興","永安農場","武陵","武麟山莊","明野","新豐","景豐","加拿","月眉國小","月眉","崁頂","親水公園","關山","關山國小","關山慈濟","米國學校","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安","大同農場","池上鄉公所","池上郵局","池上站","慶豐","冷水","富南國小","堵港埔","富里")
a2<-c("台東","中央市場","簡易法庭","署東醫院","變電所","舊縣議會","新生國小","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野","鹿野鄉公所","水源地","永隆","永隆分校","飛行夢工廠","永興","永安農場","武陵","武麟山莊","明野","新豐","景豐","加拿","月眉國小","月眉","崁頂","親水公園","關山","關山國小","關山慈濟","米國學校","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安","大同農場","池上鄉公所","池上郵局","池上站","慶豐","富里")
a3<-c("台東","中央市場","簡易法庭","署東醫院","變電所","舊縣議會","新生國小","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野","鹿野鄉公所","水源地","瑞源","瑞源教會","瑞源農會","瑞興","瑞和","新豐","景豐","加拿","月眉國小","月眉","崁頂","親水公園","關山","關山國小","關山慈濟","米國學校","螢橋","德高","頂庄","忠慶","新興","池上國中","池上鄉公所","池上郵局","池上站","富里")
a4<-c("台東","中央市場","簡易法庭","署東醫院","變電所","舊縣議會","新生國小","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野","鹿野鄉公所","水源地","瑞源","瑞源教會","瑞源農會","瑞興","瑞和","新豐","景豐","加拿","月眉國小","月眉","崁頂","親水公園","關山","關山國小","關山慈濟","米國學校","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安","大同農場","池上鄉公所","池上郵局","池上站","富里")
a5<-c("台東","中央市場","簡易法庭","署東醫院","變電所","舊縣議會","新生國小","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","延平鄉公所","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野","鹿野鄉公所","水源地","永隆","永隆分校","飛行夢工廠","永興","永安農場","武陵","武麟山莊","明野","新豐","景豐","加拿","月眉國小","月眉","崁頂","親水公園","關山")
a6<-c("台東","中央市場","簡易法庭","署東醫院","變電所","舊縣議會","新生國小","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","延平鄉公所","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野","鹿野鄉公所","水源地","永昌部落","下鹿寮","中庄","上鹿寮","永康","永安","新豐")
a6_1<-c("台東","旅服中心","公教會館站","傳廣更生站","四維傳廣站","卑南入口站","台東新站","卑南文化公園","南王部落","明峰","原生植物園","初鹿牧場","四維","鹿鳴酒店","紅烏龍館","崑慈堂","鹿野","永昌","永安社區","鹿野遊客中心","鹿野高台")
a7<-c("台東","中央市場","簡易法庭","署東醫院","變電所","舊縣議會","新生國小","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野")
a7_1<-c("台東女中","台東中學","台東高商","舊縣議會","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野")
a7_2<-c("鼎東山線保養廠","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","龍岡","龍田國小","鹿野國中","鹿野")
a8<-c("台東","中央市場","簡易法庭","署東醫院","變電所","舊縣議會","新生國小","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿")
a8_1<-c("台東女中","台東中學","台東高商","舊縣議會","四維路口","更生路","馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿")
a8_2<-c("鼎東山線保養廠","卑南農會","卑南","卑南國小","南王","卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中","明峰","初鹿")
#回程站點排序
b1<-rev(a1)
b2<-rev(a2)
b3<-rev(a3)
b4<-rev(a4)
b5<-rev(a5)
b6<-rev(a6)
b6_1<-rev(a6_1)
b7<-rev(a7)
b7_1<-rev(a7_1)
b7_2<-rev(a7_2)
b8<-rev(a8)
b8_1<-rev(a8_1)
b8_2<-rev(a8_2)
#各線去程站點-分開票種直方圖(站點按照順序)
ggplot(L_8161, aes(factor(L_8161$下車站名,levels=a1), fill=L_8161$票種)) + geom_bar(position="dodge") + ggtitle("8161(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8161站點") + labs(fill = "8161交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8163, aes(factor(L_8163$下車站名,levels=a2), fill=L_8163$票種)) + geom_bar(position="dodge") + ggtitle("8163(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8163站點") + labs(fill = "8163交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8165, aes(factor(L_8165$下車站名,levels=a3), fill=L_8165$票種)) + geom_bar(position="dodge") + ggtitle("8165(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8165站點") + labs(fill = "8165交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8166, aes(factor(L_8166$下車站名,levels=a4), fill=L_8166$票種)) + geom_bar(position="dodge") + ggtitle("8166(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8166站點") + labs(fill = "8166交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8167, aes(factor(L_8167$下車站名,levels=a5), fill=L_8167$票種)) + geom_bar(position="dodge") + ggtitle("8167(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8167站點") + labs(fill = "8167交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8168, aes(factor(L_8168$下車站名,levels=a6), fill=L_8168$票種)) + geom_bar(position="dodge") + ggtitle("8168(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8168站點") + labs(fill = "8168交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8168A, aes(factor(L_8168A$下車站名,levels=a6_1), fill=L_8168A$票種)) + geom_bar(position="dodge") + ggtitle("8168A(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8168A站點") + labs(fill = "8168A交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8170, aes(factor(L_8170$下車站名,levels=a7), fill=L_8170$票種)) + geom_bar(position="dodge") + ggtitle("8170(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8170站點") + labs(fill = "8170交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8170A, aes(factor(L_8170A$下車站名,levels=a7_1), fill=L_8170A$票種)) + geom_bar(position="dodge") + ggtitle("8170A(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8170A站點") + labs(fill = "8170A交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8170B, aes(factor(L_8170B$下車站名,levels=a7_2), fill=L_8170B$票種)) + geom_bar(position="dodge") + ggtitle("8170B(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8170B站點") + labs(fill = "8170B交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8171, aes(factor(L_8171$下車站名,levels=a8), fill=L_8171$票種)) + geom_bar(position="dodge") + ggtitle("8171(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8171站點") + labs(fill = "8171交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8171A, aes(factor(L_8171A$下車站名,levels=a8_1), fill=L_8171A$票種)) + geom_bar(position="dodge") + ggtitle("8171A(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8171A站點") + labs(fill = "8171A交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8171B, aes(factor(L_8171B$上車站名,levels=a8_2), fill=L_8171B$票種)) + geom_bar(position="dodge") + ggtitle("8171B(去程)下車站點-票種直方圖") + ylab("數量") + xlab("8171B站點") + labs(fill = "8171B交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
#各線回程站點-分開票種直方圖(站點按照順序)
ggplot(L_8161B, aes(factor(L_8161B$上車站名,levels=b1), fill=L_8161B$票種)) + geom_bar(position="dodge") + ggtitle("8161(回程)站點-票種直方圖") + ylab("數量") + xlab("8161站點") + labs(fill = "8161交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8163B, aes(factor(L_8163B$上車站名,levels=b2), fill=L_8163B$票種)) + geom_bar(position="dodge") + ggtitle("8163(回程)站點-票種直方圖") + ylab("數量") + xlab("8163站點") + labs(fill = "8163交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8165B, aes(factor(L_8165B$上車站名,levels=b3), fill=L_8165B$票種)) + geom_bar(position="dodge") + ggtitle("8165(回程)站點-票種直方圖") + ylab("數量") + xlab("8165站點") + labs(fill = "8165交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8166B, aes(factor(L_8166B$上車站名,levels=b4), fill=L_8166B$票種)) + geom_bar(position="dodge") + ggtitle("8166(回程)站點-票種直方圖") + ylab("數量") + xlab("8166站點") + labs(fill = "8166交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8167B, aes(factor(L_8167B$上車站名,levels=b5), fill=L_8167B$票種)) + geom_bar(position="dodge") + ggtitle("8167(回程)站點-票種直方圖") + ylab("數量") + xlab("8167站點") + labs(fill = "8167交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8168B, aes(factor(L_8168B$上車站名,levels=b6), fill=L_8168B$票種)) + geom_bar(position="dodge") + ggtitle("8168(回程)站點-票種直方圖") + ylab("數量") + xlab("8168站點") + labs(fill = "8168交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8168A_B, aes(factor(L_8168A_B$上車站名,levels=b6_1), fill=L_8168A_B$票種)) + geom_bar(position="dodge") + ggtitle("8168A(回程)站點-票種直方圖") + ylab("數量") + xlab("8168A站點") + labs(fill = "8168A交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8170_B, aes(factor(L_8170_B$上車站名,levels=b7), fill=L_8170_B$票種)) + geom_bar(position="dodge") + ggtitle("8170(回程)站點-票種直方圖") + ylab("數量") + xlab("8170站點") + labs(fill = "8170交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8170A_B, aes(factor(L_8170A_B$上車站名,levels=b7_1), fill=L_8170A_B$票種)) + geom_bar(position="dodge") + ggtitle("8170A(回程)站點-票種直方圖") + ylab("數量") + xlab("8170A站點") + labs(fill = "8170A交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8170B_B, aes(factor(L_8170B_B$上車站名,levels=b7_2), fill=L_8170B_B$票種)) + geom_bar(position="dodge") + ggtitle("8170B(回程)站點-票種直方圖") + ylab("數量") + xlab("8170B站點") + labs(fill = "8170B交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8171_B, aes(factor(L_8171_B$上車站名,levels=b8), fill=L_8171_B$票種)) + geom_bar(position="dodge") + ggtitle("8171(回程)站點-票種直方圖") + ylab("數量") + xlab("8171站點") + labs(fill = "8171交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8171A_B, aes(factor(L_8171A_B$上車站名,levels=b8_1), fill=L_8171A_B$票種)) + geom_bar(position="dodge") + ggtitle("8171A(回程)站點-票種直方圖") + ylab("數量") + xlab("8171A站點") + labs(fill = "8171A交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
ggplot(L_8171B_B, aes(factor(L_8171B_B$上車站名,levels=b8_2), fill=L_8171B_B$票種)) + geom_bar(position="dodge") + ggtitle("8171B(回程)站點-票種直方圖") + ylab("數量") + xlab("8171B站點") + labs(fill = "8171B交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)

#各線去程站點-時間(分票種)
qplot(factor(L_8161$上車站名,levels=a1),L_8161$上車分鐘/60,data=L_8161,color=L_8161$票種) + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + ggtitle("8161(去程)站點-時間") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種")
qplot(factor(L_8163$上車站名,levels=a2),L_8163$上車分鐘/60,data=L_8163,color=L_8163$票種) + ggtitle("8163(去程)站點-時間") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8165$上車站名,levels=a3),L_8165$上車分鐘/60,data=L_8165,color=L_8165$票種) + ggtitle("8165(去程)站點-時間") + ylab("時間") + xlab("8165站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8166$上車站名,levels=a4),L_8166$上車分鐘/60,data=L_8166,color=L_8166$票種) + ggtitle("8166(去程)站點-時間") + ylab("時間") + xlab("8166站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8167$上車站名,levels=a5),L_8167$上車分鐘/60,data=L_8167,color=L_8167$票種) + ggtitle("8167(去程)站點-時間") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8168$上車站名,levels=a6),L_8168$上車分鐘/60,data=L_8168,color=L_8168$票種) + ggtitle("8168(去程)站點-時間") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8168A$上車站名,levels=a6_1),L_8168A$上車分鐘/60,data=L_8168A,color=L_8168A$票種) + ggtitle("8168A(去程)站點-時間") + ylab("時間") + xlab("8168A站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8170$上車站名,levels=a7),L_8170$上車分鐘/60,data=L_8170,color=L_8170$票種) + ggtitle("8170(去程)站點-時間") + ylab("時間") + xlab("8170站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8170A$上車站名,levels=a7_1),L_8170A$上車分鐘/60,data=L_8170A,color=L_8170A$票種) + ggtitle("8170A(去程)站點-時間") + ylab("時間") + xlab("8170A站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8170B$上車站名,levels=a7_2),L_8170B$上車分鐘/60,data=L_8170B,color=L_8170B$票種) + ggtitle("8170B(去程)站點-時間") + ylab("時間") + xlab("8170B站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8171$上車站名,levels=a8),L_8171$上車分鐘/60,data=L_8171,color=L_8171$票種) + ggtitle("8171(去程)站點-時間") + ylab("時間") + xlab("8171站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8171A$上車站名,levels=a8_1),L_8171A$上車分鐘/60,data=L_8171A,color=L_8171A$票種) + ggtitle("8171A(去程)站點-時間") + ylab("時間") + xlab("8171A站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8171B$上車站名,levels=a8_2),L_8171B$上車分鐘/60,data=L_8171B,color=L_8171B$票種) + ggtitle("8171B(去程)站點-時間") + ylab("時間") + xlab("8171B站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

#各線回程站點-時間(分票種)
qplot(factor(L_8161B$上車站名,levels=b1),L_8161B$上車分鐘/60,data=L_8161B,color=L_8161B$票種) + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9)) + ggtitle("8161(回程)站點-時間") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種")
qplot(factor(L_8163B$上車站名,levels=b2),L_8163B$上車分鐘/60,data=L_8163B,color=L_8163B$票種) + ggtitle("8163(回程)站點-時間") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8165B$上車站名,levels=b3),L_8165B$上車分鐘/60,data=L_8165B,color=L_8165B$票種) + ggtitle("8165(回程)站點-時間") + ylab("時間") + xlab("8165站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8166B$上車站名,levels=b4),L_8166B$上車分鐘/60,data=L_8166B,color=L_8166B$票種) + ggtitle("8166(回程)站點-時間") + ylab("時間") + xlab("8166站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8167B$上車站名,levels=b5),L_8167B$上車分鐘/60,data=L_8167B,color=L_8167B$票種) + ggtitle("8167(回程)站點-時間") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8168B$上車站名,levels=b6),L_8168B$上車分鐘/60,data=L_8168B,color=L_8168B$票種) + ggtitle("8168(回程)站點-時間") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8168A_B$上車站名,levels=b6_1),L_8168A_B$上車分鐘/60,data=L_8168A_B,color=L_8168A_B$票種) + ggtitle("8168A(回程)站點-時間") + ylab("時間") + xlab("8168A站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8170_B$上車站名,levels=b7),L_8170_B$上車分鐘/60,data=L_8170_B,color=L_8170_B$票種) + ggtitle("8170(回程)站點-時間") + ylab("時間") + xlab("8170站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8170A_B$上車站名,levels=b7_1),L_8170A_B$上車分鐘/60,data=L_8170A_B,color=L_8170A_B$票種) + ggtitle("8170A(回程)站點-時間") + ylab("時間") + xlab("8170A站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8170B_B$上車站名,levels=b7_2),L_8170B_B$上車分鐘/60,data=L_8170B_B,color=L_8170B_B$票種) + ggtitle("8170B(回程)站點-時間") + ylab("時間") + xlab("8170B站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8171_B$上車站名,levels=b8),L_8171_B$上車分鐘/60,data=L_8171_B,color=L_8171_B$票種) + ggtitle("8171(回程)站點-時間") + ylab("時間") + xlab("8171站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8171A_B$上車站名,levels=b8_1),L_8171A_B$上車分鐘/60,data=L_8171A_B,color=L_8171A_B$票種) + ggtitle("8171A(回程)站點-時間") + ylab("時間") + xlab("8171A站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
qplot(factor(L_8171B_B$上車站名,levels=b8_2),L_8171B_B$上車分鐘/60,data=L_8171B_B,color=L_8171B_B$票種) + ggtitle("8171B(回程)站點-時間") + ylab("時間") + xlab("8171B站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

#各線去程站點-時間(分票種)詳細!!!!!
G61_1<-L_8161[c(which(L_8161$上車分鐘/60>7.5)),]
G61_1<-G61_1[c(which(G61_1$上車分鐘/60<10)),]
#qplot(factor(G61_1$上車站名,levels=a1),G61_1$上車分鐘/60,data=G61_1,color=G61_1$票種) + ggtitle("8161(去程)站點-時間1/3") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G61_2<-L_8161[c(which(L_8161$上車分鐘/60>11)),]
G61_2<-G61_2[c(which(G61_2$上車分鐘/60<14.5)),]
#qplot(factor(G61_2$上車站名,levels=a1),G61_2$上車分鐘/60,data=G61_2,color=G61_2$票種) + ggtitle("8161(去程)站點-時間2/3") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G61_3<-L_8161[c(which(L_8161$上車分鐘/60>16.25)),]
G61_3<-G61_3[c(which(G61_3$上車分鐘/60<18.75)),]
#qplot(factor(G61_3$上車站名,levels=a1),G61_3$上車分鐘/60,data=G61_3,color=G61_3$票種) + ggtitle("8161(去程)站點-時間3/3") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

G63_1<-L_8163[c(which(L_8163$上車分鐘/60>=7.75)),]
G63_1<-G63_1[c(which(G63_1$上車分鐘/60<=12.25)),]
qplot(factor(G63_1$上車站名,levels=a2),G63_1$上車分鐘/60,data=G63_1,color=G63_1$票種) + ggtitle("8163(去程)站點-時間1/4") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G63_2<-L_8163[c(which(L_8163$上車分鐘/60>=14)),]
G63_2<-G63_2[c(which(G63_2$上車分鐘/60<=16.75)),]
qplot(factor(G63_2$上車站名,levels=a2),G63_2$上車分鐘/60,data=G63_2,color=G63_2$票種) + ggtitle("8163(去程)站點-時間2/4") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G63_3<-L_8163[c(which(L_8163$上車分鐘/60>17)),]
G63_3<-G63_3[c(which(G63_3$上車分鐘/60<=19)),]
qplot(factor(G63_3$上車站名,levels=a2),G63_3$上車分鐘/60,data=G63_3,color=G63_3$票種) + ggtitle("8163(去程)站點-時間3/4") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G63_4<-L_8163[c(which(L_8163$上車分鐘/60>=19.25)),]
G63_4<-G63_4[c(which(G63_4$上車分鐘/60<=22)),]
qplot(factor(G63_4$上車站名,levels=a2),G63_4$上車分鐘/60,data=G63_4,color=G63_4$票種) + ggtitle("8163(去程)站點-時間4/4") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

G65_1<-L_8165[c(which(L_8165$上車分鐘/60>=9)),]
G65_1<-G65_1[c(which(G65_1$上車分鐘/60<=11.5)),]
qplot(factor(G65_1$上車站名,levels=a3),G65_1$上車分鐘/60,data=G65_1,color=G65_1$票種) + ggtitle("8165(去程)站點-時間1/2") + ylab("時間") + xlab("8165站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G65_2<-L_8165[c(which(L_8165$上車分鐘/60>=14)),]
G65_2<-G65_2[c(which(G65_2$上車分鐘/60<=16)),]
qplot(factor(G65_2$上車站名,levels=a3),G65_2$上車分鐘/60,data=G65_2,color=G65_2$票種) + ggtitle("8165(去程)站點-時間2/2") + ylab("時間") + xlab("8165站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

G66_1<-L_8166[c(which(L_8166$上車分鐘/60>=6)),]
G66_1<-G66_1[c(which(G66_1$上車分鐘/60<=11)),]
qplot(factor(G66_1$上車站名,levels=a4),G66_1$上車分鐘/60,data=G66_1,color=G66_1$票種) + ggtitle("8166(去程)站點-時間1/3") + ylab("時間") + xlab("8166站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G66_2<-L_8166[c(which(L_8166$上車分鐘/60>=17.25)),]
G66_2<-G66_2[c(which(G66_2$上車分鐘/60<=21)),]
qplot(factor(G66_2$上車站名,levels=a4),G66_2$上車分鐘/60,data=G66_2,color=G66_2$票種) + ggtitle("8166(去程)站點-時間2/3") + ylab("時間") + xlab("8166站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G66_3<-L_8166[c(which(L_8166$上車分鐘/60>=22.5)),]
G66_3<-G66_3[c(which(G66_3$上車分鐘/60<=24)),]
qplot(factor(G66_3$上車站名,levels=a4),G66_3$上車分鐘/60,data=G66_3,color=G66_3$票種) + ggtitle("8166(去程)站點-時間3/3") + ylab("時間") + xlab("8166站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

#G67_1<-L_8167[c(which(L_8167$上車分鐘/60>0)),]
#G67_1<-G67_1[c(which(G67_1$上車分鐘/60<=2.5)),]
#qplot(factor(G67_1$上車站名,levels=a5),G67_1$上車分鐘/60,data=G67_1,color=G67_1$票種) + ggtitle("8167(去程)站點-時間1/6") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G67_2<-L_8167[c(which(L_8167$上車分鐘/60>5)),]
G67_2<-G67_2[c(which(G67_2$上車分鐘/60<=7.5)),]
qplot(factor(G67_2$上車站名,levels=a5),G67_2$上車分鐘/60,data=G67_2,color=G67_2$票種) + ggtitle("8167(去程)站點-時間2/6") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G67_3<-L_8167[c(which(L_8167$上車分鐘/60>7.5)),]
G67_3<-G67_3[c(which(G67_3$上車分鐘/60<=12.5)),]
qplot(factor(G67_3$上車站名,levels=a5),G67_3$上車分鐘/60,data=G67_3,color=G67_3$票種) + ggtitle("8167(去程)站點-時間3/6") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G67_4<-L_8167[c(which(L_8167$上車分鐘/60>12.5)),]
G67_4<-G67_4[c(which(G67_4$上車分鐘/60<=15)),]
qplot(factor(G67_4$上車站名,levels=a5),G67_4$上車分鐘/60,data=G67_4,color=G67_4$票種) + ggtitle("8167(去程)站點-時間4/6") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G67_5<-L_8167[c(which(L_8167$上車分鐘/60>15)),]
G67_5<-G67_5[c(which(G67_5$上車分鐘/60<=17)),]
qplot(factor(G67_5$上車站名,levels=a5),G67_5$上車分鐘/60,data=G67_5,color=G67_5$票種) + ggtitle("8167(去程)站點-時間5/6") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G67_6<-L_8167[c(which(L_8167$上車分鐘/60>19)),]
G67_6<-G67_6[c(which(G67_6$上車分鐘/60<=21.5)),]
qplot(factor(G67_6$上車站名,levels=a5),G67_6$上車分鐘/60,data=G67_6,color=G67_6$票種) + ggtitle("8167(去程)站點-時間6/6") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

G68_1<-L_8168[c(which(L_8168$上車分鐘/60>6)),]
G68_1<-G68_1[c(which(G68_1$上車分鐘/60<=7)),]
qplot(factor(G68_1$上車站名,levels=a6),G68_1$上車分鐘/60,data=G68_1,color=G68_1$票種) + ggtitle("8168(去程)站點-時間1/4") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G68_2<-L_8168[c(which(L_8168$上車分鐘/60>11.25)),]
G68_2<-G68_2[c(which(G68_2$上車分鐘/60<=14.75)),]
qplot(factor(G68_2$上車站名,levels=a6),G68_2$上車分鐘/60,data=G68_2,color=G68_2$票種) + ggtitle("8168(去程)站點-時間2/4") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G68_3<-L_8168[c(which(L_8168$上車分鐘/60>15)),]
G68_3<-G68_3[c(which(G68_3$上車分鐘/60<=17.5)),]
qplot(factor(G68_3$上車站名,levels=a6),G68_3$上車分鐘/60,data=G68_3,color=G68_3$票種) + ggtitle("8168(去程)站點-時間3/4") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
G68_4<-L_8168[c(which(L_8168$上車分鐘/60>20)),]
G68_4<-G68_4[c(which(G68_4$上車分鐘/60<=22.5)),]
qplot(factor(G68_4$上車站名,levels=a6),G68_4$上車分鐘/60,data=G68_4,color=G68_4$票種) + ggtitle("8168(去程)站點-時間4/4") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

#各線回程站點-時間(分票種)詳細!!!!!
 #B61_1<-L_8161B[c(which(L_8161B$上車分鐘/60>8)),]
 #B61_1<-B61_1[c(which(B61_1$上車分鐘/60<=9)),]
 #qplot(factor(B61_1$上車站名,levels=b1),B61_1$上車分鐘/60,data=B61_1,color=B61_1$票種) + ggtitle("8161(回程)站點-時間1/4") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B61_2<-L_8161B[c(which(L_8161B$上車分鐘/60>9.5)),]
B61_2<-B61_2[c(which(B61_2$上車分鐘/60<13)),]
qplot(factor(B61_2$上車站名,levels=b1),B61_2$上車分鐘/60,data=B61_2,color=B61_2$票種) + ggtitle("8161(回程)站點-時間2/4") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B61_3<-L_8161B[c(which(L_8161B$上車分鐘/60>16)),]
B61_3<-B61_3[c(which(B61_3$上車分鐘/60<17)),]
qplot(factor(B61_3$上車站名,levels=b1),B61_3$上車分鐘/60,data=B61_3,color=B61_3$票種) + ggtitle("8161(回程)站點-時間3/4") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B61_4<-L_8161B[c(which(L_8161B$上車分鐘/60>=18)),]
B61_4<-B61_4[c(which(B61_4$上車分鐘/60<=21)),]
qplot(factor(B61_4$上車站名,levels=b1),B61_4$上車分鐘/60,data=B61_4,color=B61_4$票種) + ggtitle("8161(回程)站點-時間4/4") + ylab("時間") + xlab("8161站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

B63_1<-L_8163B[c(which(L_8163B$上車分鐘/60>=5)),]
B63_1<-B63_1[c(which(B63_1$上車分鐘/60<=9)),]
qplot(factor(B63_1$上車站名,levels=b2),B63_1$上車分鐘/60,data=B63_1,color=B63_1$票種) + ggtitle("8163(回程)站點-時間1/3") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B63_2<-L_8163B[c(which(L_8163B$上車分鐘/60>=13.25)),]
B63_2<-B63_2[c(which(B63_2$上車分鐘/60<=16.5)),]
qplot(factor(B63_2$上車站名,levels=b2),B63_2$上車分鐘/60,data=B63_2,color=B63_2$票種) + ggtitle("8163(回程)站點-時間2/3") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B63_3<-L_8163B[c(which(L_8163B$上車分鐘/60>16.5)),]
B63_3<-B63_3[c(which(B63_3$上車分鐘/60<=19)),]
qplot(factor(B63_3$上車站名,levels=b2),B63_3$上車分鐘/60,data=B63_3,color=B63_3$票種) + ggtitle("8163(回程)站點-時間3/3") + ylab("時間") + xlab("8163站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

B65_1<-L_8165B[c(which(L_8165B$上車分鐘/60>=9)),]
B65_1<-B65_1[c(which(B65_1$上車分鐘/60<=10)),]
qplot(factor(B65_1$上車站名,levels=b3),B65_1$上車分鐘/60,data=B65_1,color=B65_1$票種) + ggtitle("8165(回程)站點-時間1/4") + ylab("時間") + xlab("8165站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B65_2<-L_8165B[c(which(L_8165B$上車分鐘/60>=11.5)),]
B65_2<-B65_2[c(which(B65_2$上車分鐘/60<=13.5)),]
qplot(factor(B65_2$上車站名,levels=b3),B65_2$上車分鐘/60,data=B65_2,color=B65_2$票種) + ggtitle("8165(回程)站點-時間2/4") + ylab("時間") + xlab("8165站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B65_3<-L_8165B[c(which(L_8165B$上車分鐘/60>=14.5)),]
B65_3<-B65_3[c(which(B65_3$上車分鐘/60<=16.5)),]
qplot(factor(B65_3$上車站名,levels=b3),B65_3$上車分鐘/60,data=B65_3,color=B65_3$票種) + ggtitle("8165(回程)站點-時間3/4") + ylab("時間") + xlab("8165站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B65_4<-L_8165B[c(which(L_8165B$上車分鐘/60>=17)),]
B65_4<-B65_4[c(which(B65_4$上車分鐘/60<=17.5)),]
qplot(factor(B65_4$上車站名,levels=b3),B65_4$上車分鐘/60,data=B65_4,color=B65_4$票種) + ggtitle("8165(回程)站點-時間4/4") + ylab("時間") + xlab("8165站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))


B66_1<-L_8166B[c(which(L_8166B$上車分鐘/60>=7.5)),]
B66_1<-B66_1[c(which(B66_1$上車分鐘/60<=10)),]
qplot(factor(B66_1$上車站名,levels=b4),B66_1$上車分鐘/60,data=B66_1,color=B66_1$票種) + ggtitle("8166(回程)站點-時間1/2") + ylab("時間") + xlab("8166站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B66_2<-L_8166B[c(which(L_8166B$上車分鐘/60>=17.5)),]
B66_2<-B66_2[c(which(B66_2$上車分鐘/60<=20)),]
qplot(factor(B66_2$上車站名,levels=b4),B66_2$上車分鐘/60,data=B66_2,color=B66_2$票種) + ggtitle("8166(回程)站點-時間2/2") + ylab("時間") + xlab("8166站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

#B67_1<-L_8167B[c(which(L_8167B$上車分鐘/60>2.5)),]
#B67_1<-B67_1[c(which(B67_1$上車分鐘/60<=5)),]
#qplot(factor(B67_1$上車站名,levels=b5),B67_1$上車分鐘/60,data=B67_1,color=B67_1$票種) + ggtitle("8167(回程)站點-時間1/4") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B67_2<-L_8167B[c(which(L_8167B$上車分鐘/60>6.5)),]
B67_2<-B67_2[c(which(B67_2$上車分鐘/60<=11.25)),]
qplot(factor(B67_2$上車站名,levels=b5),B67_2$上車分鐘/60,data=B67_2,color=B67_2$票種) + ggtitle("8167(回程)站點-時間2/4") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B67_3<-L_8167B[c(which(L_8167B$上車分鐘/60>15)),]
B67_3<-B67_3[c(which(B67_3$上車分鐘/60<=18)),]
qplot(factor(B67_3$上車站名,levels=b5),B67_3$上車分鐘/60,data=B67_3,color=B67_3$票種) + ggtitle("8167(回程)站點-時間3/4") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B67_4<-L_8167B[c(which(L_8167B$上車分鐘/60>20)),]
B67_4<-B67_4[c(which(B67_4$上車分鐘/60<=22.5)),]
qplot(factor(B67_4$上車站名,levels=b5),B67_4$上車分鐘/60,data=B67_4,color=B67_4$票種) + ggtitle("8167(回程)站點-時間4/4") + ylab("時間") + xlab("8167站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))

B68_1<-L_8168B[c(which(L_8168B$上車分鐘/60>5)),]
B68_1<-B68_1[c(which(B68_1$上車分鐘/60<=8)),]
qplot(factor(B68_1$上車站名,levels=b6),B68_1$上車分鐘/60,data=B68_1,color=B68_1$票種) + ggtitle("8168(回程)站點-時間1/3") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B68_2<-L_8168B[c(which(L_8168B$上車分鐘/60>12.5)),]
B68_2<-B68_2[c(which(B68_2$上車分鐘/60<=15)),]
qplot(factor(B68_2$上車站名,levels=b6),B68_2$上車分鐘/60,data=B68_2,color=B68_2$票種) + ggtitle("8168(回程)站點-時間2/3") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))
B68_3<-L_8168B[c(which(L_8168B$上車分鐘/60>16)),]
B68_3<-B68_3[c(which(B68_3$上車分鐘/60<=19.5)),]
qplot(factor(B68_3$上車站名,levels=b6),B68_3$上車分鐘/60,data=B68_3,color=B68_3$票種) + ggtitle("8168(回程)站點-時間3/3") + ylab("時間") + xlab("8168站點") + labs(fill = "交易票種") + theme(panel.background=element_rect(fill='transparent',color ="gray"), axis.text.x = element_text(angle = 70, hjust = 0.5, vjust = 0.5,color = "black",size=9))


par(las=2,cex=1.5)
tmp=dcast(L_8171B_B,factor(L_8171B_B$上車站名)~round(L_8171B_B$上車分鐘/60,1)) 
data=as.matrix(tmp[,-1])
rownames(data)=tmp[,1]
bar3d.ade(t(data), wall=6,main = "8171B回程",xlab = "站點",ylab = "count",xw=0.5,zw=0.3,axes=TRUE)

qplot(D$上車分鐘/60, data = D, geom = "bar",fill = D$上車站名) + ggtitle("8161池上郵局(回程)直方圖") + ylab("數量") + xlab("票種")

D<-L_8161B[c(which(L_8161B$上車站名=="池上郵局")),]


qplot(data$上車分鐘/60, data = data, geom = "histogram",fill = data$票種,binwidth = 0.01) + ggtitle("Histogram of boarding time") + ylab("count") + xlab("Boarding time") + labs(fill = "Card type") 
qplot(data$上車分鐘/60, data = data, geom = "density",color = data$票種) + ggtitle("Density of boarding time") + ylab("count") + xlab("Boarding time") + labs(color = "Card type") 
qplot(data$票種, data = data, geom = "bar",fill = data$票種) + ggtitle("Histogram of card") + ylab("count") + xlab("Card") + labs(fill = "Card type") + geom_text(position = position_dodge(width = 1), size = 3,stat="count",aes(label=..count..),vjust=-0.2)
qplot(data$上車分鐘/60, data = data, facets = data$路線 ~ .,geom = "histogram", binwidth = 0.1,fill = data$票種) + ggtitle("Histogram of route") + xlab("Boarding time") + ylab("count") + labs(fill = "Card type")
