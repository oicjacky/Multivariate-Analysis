# Multivariate Analysis: Bus Route Data

我們收集了幾條**偏鄉地區公車路線的電子票證資料**。探討傳統公車在偏鄉地區，由於地廣人稀造成之**搭乘需求分散問題**。

並且，考慮「**小黃公車**」(結合[跳蛙公車](https://leapfroggingbus.tw/about_us)與計程車之優點)配合**專車**的可能性，
進一步改善傳統公車的**空車率**與**服務效率**。

---

# Data

![Data](https://github.com/oicjacky/Multivariate-Analysis/blob/master/data.PNG)


- 路線: 11(上車54站,下車71站)、22(上車51站,下車68站)、33(上車50站,下車60站)、44(上車51站,下車62站)

- 票種: 敬老票、學生票、一般票(類別)

- 上下車時間: 時間轉換為分鐘數(e.g. 09:08→60*9+8=548)

- 上下車站名: 各個站點的名稱(類別) 

- 去程與回程資料分開

- 資料來源:東部運輸中心(105年搭乘公車刷卡紀錄)

## EDA

**路線上下車站使用量(整年)**

![使用量](https://github.com/oicjacky/Multivariate-Analysis/blob/master/data03.png)

**路線上下車站與時間(整年)**

![班次](https://github.com/oicjacky/Multivariate-Analysis/blob/master/data02.png)

---

