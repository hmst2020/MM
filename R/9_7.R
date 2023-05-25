library(fpp2)
sales<-c(8.7444,10.53,10.99,11.97,12.74,12.83,14.69,15.30, # 銷售額
         16.11,16.31,16.46,17.67,19.65,18.86,19.93,20.51)  
dmedia<- c(2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)    # 數位媒體費用
pmedia<-c(2,3,4,5,2,3,4,5,2,3,4,5,2,3,4,5)     # 平面媒體費用
df<-data.frame(         # 建構data frame資料物件
  sales=sales,              # sales欄: 銷售額資料
  dmedia=dmedia,            # dmedia欄: 數位媒體費用
  pmedia=pmedia)            # pmedia欄: 平面媒體費用
library(GGally)
ggpairs(df)             # 繪製各變數之間相關係數及分布圖

d.ts<-ts(df,start=1,frequency=1)   # 多變數的時間序列(mts)
print(fit <- tslm(   # 各期別以線性迴歸擬合
  formula=sales ~ dmedia + pmedia, # 時間序列物件欄位(應變數、自變數)
  data=d.ts))            # 資料依據之時間序列物件

df[,"Residuals"]<-as.numeric(   # 將擬合物件ts轉為數字項量增加至df物件
  residuals(fit)) 
p1 <- ggplot(data=df,    # 產生預測變數(數位媒體費用)與殘差散佈圖
             aes(x=dmedia, y=Residuals)) +  
  geom_point()
p2 <- ggplot(data=df,    # 產生預測變數(平面媒體費用)與殘差散佈圖
             aes(x=pmedia, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, ncol=2)  # 將比較圖排列繪於一處


library(dplyr)
g<-cbind(            # 建構多變數的時間序列物件(mts)
  Fitted=fitted(fit),   #  擬合值
  Residuals=residuals(fit))  %>%    #  殘差
  as.data.frame() %>%     #  mts物件轉成vector欄位的data frame
  ggplot(                 # 使用data frame物件繪製散佈圖
    aes(x=Fitted,         # 散佈圖x、y軸對應data frame資料欄位
        y=Residuals)) + 
  geom_point() +          # 繪製資料點
  xlab('擬合值') + ylab('誤差值(殘差)') +  # x、y軸的標籤
  ggtitle('誤差值散佈型態')                # 圖標題
print(g)             # 列出圖物件

checkresiduals(  # 迴歸模型的殘差(誤差項)檢定
  fit,              # 線性迴歸模型
  test='BG',        # 自相關檢定法
  #lag=NULL,        # 使用預設遲延期數進行檢定
  lag.max=10)       # 繪出最多遲延期數

library(lmtest)     # 載入線性測試套件
dwtest(             # DW 檢定函式
  formula = fit,              # 擬合模型(tslm)物件
  alternative = "two.sided")  # 對立假設為雙尾 
################# end of 9_7.R #################

