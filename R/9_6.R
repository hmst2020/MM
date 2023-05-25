library(fpp2)
sales<-c(220,230,205,125,155,145,225,195,175,220,170,240) # 銷售額
adv<- c(350,395,305,170,180,180,375,240,230,335,220,405)  # 廣告支出
df<-data.frame(   # 建構data frame資料物件
  sales=sales,    # sales欄銷售額資料
  adv=adv)        # adv欄廣告支出資料
cor(df$sales,df$adv)  # 銷售額與廣告支出之相關係數

print(LM<-lm(           # 建構線性迴歸模型
  formula=sales ~ adv,      # 自變數與應變數公式
  data=df))                 # 模型資料依據
Acf(LM$residuals)       # Breusch-Godfrey test(BG test) 自相關測試
checkresiduals(LM)      # Breusch-Godfrey test(BG test) 殘差聯合測試
ggplot(data=df,         # 繪製觀察值分布與迴歸線
       aes(x=adv, y=sales)) +  # x軸與y軸在df物件之資料欄
  ylab("銷售金額(萬元)") +     # y軸文字標籤
  xlab("廣告支出(百元)") +     # x軸文字標籤
  geom_point() +           # 觀察值以點狀呈現
  geom_abline(             # 附加斜線於其上     
    intercept=LM$coefficients[1],   # 截距
    slope=LM$coefficients[2]) +     # 斜率
  scale_x_continuous(      # x軸之尺規標示
    limits=c(min(adv),max(adv)),    # 尺規上下限
    breaks=seq(min(adv),max(adv),by=20))  # 尺規分隔間距為20

d.ts<-ts(     # 建構時間序列物件ts
  data=df,       # 本例data frame物件資料
  start=c(1),    # 資料之起始期別
  frequency=1)   # 每期別1筆
print(fit <- tslm(   # 各期別以線性迴歸擬合
  formula=sales ~ adv,   # 時間序列物件欄位(應變數、自變數)
  data=d.ts))            # 資料依據之時間序列物件

summary(fcst<-forecast(      # 以擬合模型預測接續3期
  fit,                  # 擬合模型物件
  newdata=data.frame(   # 預測期的假設自變數資料
    period=c(13,14,15), # 預測期
    adv=c(500,600,700)))) # 自變數資料(廣告支出)
g<-autoplot(d.ts[,'sales'],series="實際觀察值") +
  autolayer(fcst$fitted,series="擬合線") +
  autolayer(fcst,series="預測線") +
  ylab("銷售金額")+
  xlab("期別")+
  ggtitle("各期實際銷售與預測(線性迴歸)")
print(g)             # 繪出圖形

print(res<-(d.ts[,'sales']-fcst$fitted))  # 殘差計算及列印
sqrt(mean(res^2,na.rm=TRUE)) # RMSE
mean(abs(res),na.rm=TRUE)    # MAE
mean(res,na.rm=TRUE)         # 殘差平均值
################# end of 9_6.R #################