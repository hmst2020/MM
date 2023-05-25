library(fpp2)
load(file='data/T102.rds')         # 載入本例資料物件
print(df<-data.frame(              # 建構data frame 物件
  period=1:12,                        # period欄位表示期別
  demand=data))                       # 資料向量物件

fit1 <- lm(                  # 對時間序列資料進行線性迴歸
  formula=demand ~ period,       # 迴歸依據自變數與應變數
  data=df)                       # 資料物件
summary(fit1)                # 擬合物件總攬

fit2 <- tslm(                # 對時間序列資料進行線性迴歸
  formula=d.ts ~ trend)          # 時間序列資料物件與虛擬變數
summary(fit2)                # 擬合物件總攬

print(fcst<-forecast(              # 以擬合資料進行預測
  fit2,                          # 擬合物件
  newdata=data.frame(period=c(13,14,15)) # 預測3 期(13~15)
))
g<-autoplot(d.ts,series="實際觀察值") +
  autolayer(fcst,series="預測線") +
  autolayer(fitted(fit2),series="擬和線") +
  ylab("市場需求") +
  xlab("期別") + 
  ggtitle("各期實際需求與預測(時間序列線性迴歸)")
print(g)  # 繪出圖形
################# end of 9_5.R #################