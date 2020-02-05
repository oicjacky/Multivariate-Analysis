# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#讀資料
setwd("C:/Users/User/Dropbox/busticket/105整年(已洗清)")
data<-read.csv("105年平日.csv",header = TRUE, sep = ",")
attach(data)
head(data)
x<-data[路線==8129,1:11]#整年份8129
head(x)
x_1<-x[1:4340,]#8129的第一個月
head(x_1)

#用來檢查那些站點不存在-------------------------------------------
#which(x_1[,7]=="健康農莊")
#x_1[145,]
#M_x_1[145,]
#which(colSums(M_x_1)==0)
#-----------------------------------------------------------------


#第一列為站點,第一行為樣本點
M_x_1<-matrix(rep(0,325500), nrow = 4340, ncol = 75) 
v<-c("台東","台東X","新生站","新生站X","台東中學","台東中學X","金剛寺","金剛寺X","豐榮國小","豐榮國小X","東豐","東豐X",
     "一支線","一支線X","豐谷里","豐谷里X","豐里國小","豐里國小X","洽益米廠","洽益米廠X","豐里","豐里X","豐源國小","豐源國小X",
     "豐源","豐源X","大學路口","大學路口X","台東大學","台東大學X","開發隊A","開發隊AX","開發隊B","開發隊BX",
     "煙仔寮","煙仔寮X","知本火車站","知本火車站X","知本派出所","知本派出所X","知本國小","知本國小X","知本農場","知本農場X",
     "知本村","知本村X","新興里","新興里X","知本教會","知本教會X","崎仔頭","崎仔頭X","鎮樂","鎮樂X","溫泉國小","溫泉國小X","東遊季","東遊季X","知本溫泉","知本溫泉X",
     "白玉瀑布","白玉瀑布X","清覺寺","清覺寺X","泓泉飯店","泓泉飯店X","東台飯店","東台飯店X","內溫泉","內溫泉X","票種-敬老票","票種-學生票","票種-陪伴票","票種-一般票","孩童票")

#把y票種換成01向量
for(i in c(1:4340)){
  if(x_1[i,3]=="敬老票"){
    M_x_1[i,71]<-1
  }else if(x_1[i,3]=="學生票"){
    M_x_1[i,72]<-1    
  }else if(x_1[i,3]=="陪伴票"){
    M_x_1[i,73]<-1
  }else if(x_1[i,3]=="一般票"){
    M_x_1[i,74]<-1  
  }else{
    M_x_1[i,75]<-1    
  }
}
head(M_x_1)

odds <- seq(1,70, 2)#把上下站改成1
#把資料改成01形式,除了最後5col
idx<-c()
for(i in c(1:4340) ){
  for(j in odds){
    if(x_1[i,7]==v[j]&&x_1[i,11]==v[j]){
      idx<-i#把上下站點為同一站清掉
    }else if(x_1[i,7]==v[j]||x_1[i,11]==v[j]){
      M_x_1[i,j]=1
      M_x_1[i,j+1]=0      
    }else{
      M_x_1[i,j]=0
      M_x_1[i,j+1]=1
    }
  }
}
head(M_x_1)
idx
M_x_1<-M_x_1[-idx,]#整理資料

colnames(M_x_1)=c("台東","台東X","新生站","新生站X","台東中學","台東中學X","金剛寺","金剛寺X","豐榮國小","豐榮國小X"
                  ,"東豐","東豐X","一支線","一支線X","豐谷里","豐谷里X","豐里國小","豐里國小X","洽益米廠","洽益米廠X",
                    "豐里","豐里X","豐源國小","豐源國小X","豐源","豐源X","大學路口","大學路口X","台東大學","台東大學X"
                  ,"開發隊A","開發隊AX","開發隊B","開發隊BX","煙仔寮","煙仔寮X",
                  "知本火車站","知本火車站X","知本派出所","知本派出所X","知本國小","知本國小X","知本農場","知本農場X",
                    "知本村","知本村X","新興里","新興里X","知本教會","知本教會X","崎仔頭","崎仔頭X","鎮樂","鎮樂X","溫泉國小","溫泉國小X","東遊季","東遊季X","知本溫泉","知本溫泉X",
                    "白玉瀑布","白玉瀑布X","清覺寺","清覺寺X","泓泉飯店","泓泉飯店X","東台飯店","東台飯店X","內溫泉","內溫泉X","票種-敬老票","票種-學生票","票種-陪伴票","票種-一般票","孩童票")
head(M_x_1)#一月份原始資料矩陣
#--------------------------------------------------------------------------------------------------
x = M_x_1[,c(1:70)]
n = nrow(x)
p = ncol(x)

one = matrix(1, n, n)
h = diag(1, n, n) - one/n  # centering the matrix
a = x - matrix(apply(x, 2, mean), n, p, byrow = T)  # substracts mean
d = diag(1/sqrt(colSums(a^2)/n))

xs = h %*% as.matrix(x) %*% d  # standardized data
xs1 = xs/sqrt(n)
xs2 = t(xs1) %*% xs1

#
eig = eigen(xs2)  # spectral decomposition
lambda = eig$values
gamma = eig$vectors

w = gamma * (matrix(sqrt(lambda), nrow = nrow(gamma), ncol = ncol(gamma), byrow = T))  # coordinates of food
w = w[, 1:2]
w = round(w, 3)

z1 = xs1 %*% gamma  # coordinates of families
z2 = sqrt(n/p) * z1
z = z2[, 1:2]
z = round(z, 3)

namew = c("台東","台東X","新生站","新生站X","台東中學","台東中學X","金剛寺","金剛寺X","豐榮國小","豐榮國小X","東豐","東豐X",
          "一支線","一支線X","豐谷里","豐谷里X","豐里國小","豐里國小X","洽益米廠","洽益米廠X","豐里","豐里X","豐源國小","豐源國小X",
          "豐源","豐源X","大學路口","大學路口X","台東大學","台東大學X","開發隊A","開發隊AX","開發隊B","開發隊BX",
          "煙仔寮","煙仔寮X","知本火車站","知本火車站X","知本派出所","知本派出所X","知本國小","知本國小X","知本農場","知本農場X",
          "知本村","知本村X","新興里","新興里X","知本教會","知本教會X","崎仔頭","崎仔頭X","鎮樂","鎮樂X","溫泉國小","溫泉國小X","東遊季","東遊季X","知本溫泉","知本溫泉X",
          "白玉瀑布","白玉瀑布X","清覺寺","清覺寺X","泓泉飯店","泓泉飯店X","東台飯店","東台飯店X","內溫泉","內溫泉X")
namez = c(1:nrow(x))

par(mfrow = c(2, 2))
windows()
plot(w[, 1], -w[, 2], type = "n", xlab = "W[,1]", ylab = "W[,2]", main = "站點", cex.axis = 1.2, 
     cex.lab = 1.2, cex.main = 1.6, xlim = c(-1, 1), ylim = c(-0.8, 0.8))
text(w[, 1], -w[, 2], namew, xpd = NA)
abline(h = 0, v = 0, lwd = 1.2)

for (i in 1:ncol(x)) {
  mtext(namew[i], side = 1, line = 5 + i, at = -1)
  mtext(toString(c(sprintf("%.3f", w[i, 1]))), side = 1, line = 5 + i, at = -0.25)
  mtext(toString(c(sprintf("%.3f", w[i, 2]))), side = 1, line = 5 + i, at = 0.5)
}

plot(z[, 1], -z[, 2], type = "n", xlim = c(-0.6, 0.5), ylim = c(-0.5, 0.7), xlab = "Z[,1]", 
     ylab = "Z[,2]", main = "樣本點", cex.axis = 1.2, cex.lab = 0.8, cex.main = 0.8)
text(z[, 1], -z[, 2], namez, xpd = NA)
abline(h = 0, v = 0, lwd = 1.2)

for (i in 1:nrow(x)) {
  mtext(namez[i], side = 1, line = 5 + i, at = -0.5)
  mtext(toString(c(sprintf("%.3f", z[i, 1]))), side = 1, line = 5 + i, at = -0.25)
  mtext(toString(c(sprintf("%.3f", z[i, 2]))), side = 1, line = 5 + i, at = 0.25)
}

