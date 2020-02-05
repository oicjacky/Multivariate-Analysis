#把pca的座標存成檔案

# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

#讀資料
# setwd("C:/Users/User/Dropbox/busticket/蔡伊婷/去程/資料")
M_x11<-read.csv("(去程)11敬老票使用站點.csv" ,header = TRUE, sep = ",")
n11<-c("台東","新生站","中央市場","署東醫院","簡易法庭","變電所","新生國小","更生路","四維路口",
       "馬蘭加油站","公東高工","馬蘭榮家","卑南農會","卑南","卑南國小","南王",
       "卑南國中","十股","下賓朗","賓朗","種蓄場","美濃入口","東成","美濃","集貨場","煙草間","初鹿國中",
       "明峰","初鹿","龍過脈","嘉豐入口","舊鹿鳴","四維","湖底","龍田","光榮","鹿野國中","鹿野","鹿野鄉公所","水源地",
       "永隆","永隆分校","永興","永安農場","武陵","武麟山莊","新豐","景豐","加拿","月眉國小",
       "月眉","崁頂","親水公園","關山","關山慈濟","螢橋","德高","頂庄","北庄","山平路","海端","隴下","初來","錦屏","陸安",
       "大同農場","池上鄉公所","池上郵局","池上站","慶豐","富南國小","富里")

x = M_x11[,c(1:72)]
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

namew =n11
namez = c(1:nrow(x))

windows()
par(mfrow = c(2, 2))
windows()
plot(w[, 1], -w[, 2], type = "n", xlab = "W[,1]", ylab = "W[,2]", main = "路線11站點", cex.axis = 1.2, 
     cex.lab = 1.2, cex.main = 1.6, xlim = c(-0.4, 0.7), ylim = c(-0.4, 0.9))
text(w[, 1], -w[, 2], namew, xpd = NA)
abline(h = 0, v = 0, lwd = 1.2)

for (i in 1:ncol(x)) {
  mtext(namew[i], side = 1, line = 5 + i, at = -0.4)
  mtext(toString(c(sprintf("%.3f", w[i, 1]))), side = 1, line = 5 + i, at = 0)
  mtext(toString(c(sprintf("%.3f", w[i, 2]))), side = 1, line = 5 + i, at = 0.3)
}
windows()
plot(z[, 1], -z[, 2], type = "n", xlim = c(-0.3, 0.4), ylim = c(-0.3, 0.7), xlab = "Z[,1]", 
     ylab = "Z[,2]", main = "路線11樣本點", cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.6)
text(z[, 1], -z[, 2], namez, xpd = NA)
abline(h = 0, v = 0, lwd = 1.2)

for (i in 1:nrow(x)) {
  mtext(namez[i], side = 1, line = 5 + i, at = -0.1)
  mtext(toString(c(sprintf("%.3f", z[i, 1]))), side = 1, line = 5 + i, at = 0)
  mtext(toString(c(sprintf("%.3f", z[i, 2]))), side = 1, line = 5 + i, at = 0.15)
}

write.csv(w, file = "w_x11_pca.csv", row.names = F)#路線11站點pca座標
write.csv(z, file = "z_x11_pca.csv", row.names = F)#路線11乘客點pca座標