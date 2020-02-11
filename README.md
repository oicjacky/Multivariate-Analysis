# Multivariate Analysis: Bus Route Data

我們收集了幾條**偏鄉地區公車路線的電子票證資料**。探討傳統公車在偏鄉地區，由於地廣人稀造成之**搭乘需求分散問題**。

並且，考慮「**小黃公車**」(結合[跳蛙公車](https://leapfroggingbus.tw/about_us)與計程車之優點)配合**專車**的可能性，
進一步改善傳統公車的**空車率**與**服務效率**。

---

# Objective

## 路線整併、區間直達車
  * 各路線重要站點-使用率高
  
  * 不同客群-老人
  
    > 不能廢除
  
  * 不同客群-學生
  
    > 學生專車




---

# Data

![Data](https://github.com/oicjacky/Multivariate-Analysis/blob/master/data.PNG)


- 路線: 11(上車54站,下車71站)、22(上車51站,下車68站)、33(上車50站,下車60站)、44(上車51站,下車62站)

- 票種: 敬老票、學生票、一般票(類別)

- 上下車時間: 時間轉換為分鐘數(e.g. 09:08→60*9+8=548)

- 上下車站名: 各個站點的名稱(類別) 

- 去程與回程資料分開

- 資料來源:東部運輸中心(105年搭乘公車刷卡紀錄)

---

# EDA

**路線上下車站使用量(整年)**

![使用量](https://github.com/oicjacky/Multivariate-Analysis/blob/master/data03.png)


- 使用量高的集中於某幾個站點上

- [詳見 MVA Part I report bus data analysis (0517).pdf](https://github.com/oicjacky/Multivariate-Analysis/blob/master/0611MVA%20Part%20II%20Presentation/MVA%20Part%20I%20report%20bus%20data%20analysis%20(0517).pdf)

---

# Method

1. PCA(Principal Component Analysis)
    - 這裡用來將原本高維度的資料進行降維，但盡量減少在降維過程中遺失的重要訊息。

2. Hierarchical
    - 我們使用聚合式階層分群法(agglomerative hierarchical clustering)。利用歐氏距離作為測量相似性(measure of similarity)。

3. K-means
    - K-means為一種分群方法，使用資料的距離作為分群依據。在此篇使用的距離為歐氏距離。

- [詳見 MVA Part III Report.pdf](https://github.com/oicjacky/Multivariate-Analysis/blob/master/0617MVA%20Part%20III%20Report%20Writing/MVA%20Part%20III%20Report.pdf)

---

# Conclusion

經由分析結果，發現**GS、HP、CL2、JP**這四個站對**學生**是重要的，究其原因，我們利用網路地圖搜尋這四個站點附近的建築，發現這四個站點附近確實都有國中、小，此外，對於**老人**較重要的五個站點，**SCS、GS、CM、MF、LYTO**，我們也發現，這些站點附近較多功能性建築，例如:醫院、市場、公務機關…等。
針對這些重要站點，我們希望規劃出**學生族群專車**，以及**敬老族群的「小黃公車」**，期望能使公車的使用率以及效率達到最大。


