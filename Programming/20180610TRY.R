
#----------路線11pca-------------------------------
x = read.csv("(去程)路線11轉換01矩陣.csv")
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
gamma[,1:2]
w = gamma * (matrix(sqrt(lambda), nrow = nrow(gamma), ncol = ncol(gamma), byrow = T))  # coordinates of food
w = w[, 1:2]
w = round(w, 3)

z1 = xs1 %*% gamma  # coordinates of families
z1[,1:2]
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
#----------路線22pca-------------------------------
x = M_x22[,c(1:69)]
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

namew =n22
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
#----------路線22pca-------------------------------
x = M_x22[,c(1:69)]

pcaqq = prcomp(x ,center = T ,scale. = T)

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
#----------路線33pca-------------------------------
x = cbind(M_x33[,c(1:57)],M_x33[,c(59:64)]) 
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

namew =n33
namez = c(1:nrow(x))


windows()
par(mfrow = c(2, 2))

#windows()
plot(w[, 1], -w[, 2], type = "n", xlab = "W[,1]", ylab = "W[,2]", main = "路線33站點", cex.axis = 1.2, 
     cex.lab = 1.2, cex.main = 1.6, xlim = c(-0.4, 0.6), ylim = c(-0.6, 0.6))
text(w[, 1], -w[, 2], namew, xpd = NA)
abline(h = 0, v = 0, lwd = 1.2)

for (i in 1:ncol(x)) {
  mtext(namew[i], side = 1, line = 5 + i, at = -0.4)
  mtext(toString(c(sprintf("%.3f", w[i, 1]))), side = 1, line = 5 + i, at = 0)
  mtext(toString(c(sprintf("%.3f", w[i, 2]))), side = 1, line = 5 + i, at = 0.3)
}

#windows()
plot(z[, 1], -z[, 2], type = "n", xlim = c(-0.5, 0.4), ylim = c(-0.7, 0.5), xlab = "Z[,1]", 
     ylab = "Z[,2]", main = "路線33樣本點", cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.6)
text(z[, 1], -z[, 2], namez, xpd = NA)
abline(h = 0, v = 0, lwd = 1.2)

for (i in 1:nrow(x)) {
  mtext(namez[i], side = 1, line = 5 + i, at = -0.1)
  mtext(toString(c(sprintf("%.3f", z[i, 1]))), side = 1, line = 5 + i, at = 0)
  mtext(toString(c(sprintf("%.3f", z[i, 2]))), side = 1, line = 5 + i, at = 0.15)
}
#----------路線44pca-------------------------------
x = cbind(M_x44[,c(1:57)],M_x44[,c(59:60)], M_x44[,c(62:68)]) 
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

namew =n44
namez = c(1:nrow(x))

windows()
par(mfrow = c(2, 2))

#windows()
plot(x = w[, 1], y = -w[, 2], type = "n", xlab = "W[,1]", ylab = "W[,2]", main = "路線44站點", cex.axis = 1.2, 
     cex.lab = 1.2, cex.main = 1.6, xlim = c(-0.6, 0.7), ylim = c(-0.7, 0.4))
text(w[, 1], -w[, 2], namew, xpd = NA)
abline(h = 0, v = 0, lwd = 1.2)

for (i in 1:ncol(x)) {
  mtext(namew[i], side = 1, line = 5 + i, at = -0.25)
  mtext(toString(c(sprintf("%.3f", w[i, 1]))), side = 1, line = 5 + i, at = 0)
  mtext(toString(c(sprintf("%.3f", w[i, 2]))), side = 1, line = 5 + i, at = 0.25)
}

#windows()
plot(z[, 1], -z[, 2], type = "n", xlim = c(-0.2, 0.5), ylim = c(-0.7, 0.35), xlab = "Z[,1]", 
     ylab = "Z[,2]", main = "路線44樣本點", cex.axis = 1.2, cex.lab = 1.2, cex.main = 1.6)
text(z[, 1], -z[, 2], namez, xpd = NA)
abline(h = 0, v = 0, lwd = 1.2)

for (i in 1:nrow(x)) {
  mtext(namez[i], side = 1, line = 5 + i, at = -0.2)
  mtext(toString(c(sprintf("%.3f", z[i, 1]))), side = 1, line = 5 + i, at = 0)
  mtext(toString(c(sprintf("%.3f", z[i, 2]))), side = 1, line = 5 + i, at = 0.2)
}
#----------------------------------------------------------------
#標顏色

#plot(x = w[, 1], y = -w[, 2], type = "n", xlab = "W[,1]", ylab = "W[,2]", main = "路線44站點", cex.axis = 1.2, 
 #    cex.lab = 1.2, cex.main = 1.6, xlim = c(-0.4, 0.6), ylim = c(-1, 0.5))
#text(w[1:24, 1], -w[1:24, 2], namew, xpd = NA, col = '2')
#text(w[25:48, 1], -w[25:48, 2], namew, xpd = NA, col = '3')
#text(w[49:72, 1], -w[49:72, 2], namew, xpd = NA, col = '4')
#abline(h = 0, v = 0, lwd = 1.2)
