load(file='data/T102.rds')    # 載入本例資料物件
library(fpp2)
alpha<-0.3          # 平滑參數值
print(fcst <- ses(  # 簡單指數平滑模式預測
  d.ts,             # 本例表10-1的時間序列物件
  alpha=alpha,      # 指定alpha值
  h=3,              # 預測期數
  initial='simple'))   # 簡單初始模式
# summary(fcst)  # 預測物件總攬
fcst$fitted    # 列出擬合結果
print(sigma<-sqrt(sum(      # 標準差(standard deviation)
  fcst$residuals^2)/length(fcst$residuals)))
alpha*d.ts[12]+(1-alpha)*fcst$fitted[12] # 預測第1期的點預測
fcst$mean[1]+qnorm(0.90, mean = 0, sd = 1)*sigma # 預測第1期的 Hi 80
fcst$mean[1]+qnorm(0.10, mean = 0, sd = 1)*sigma # 預測第1期的 Lo 80
g<-autoplot(d.ts,series="觀察值") +
  autolayer(fcst,series="預測線") +
  autolayer(fcst$fitted,series="擬合線") +
  ylab("市場需求")+
  xlab("期別")+
  ggtitle("各期實際需求、擬合及預測(簡單指數平滑)")
print(g)  # 繪出圖形

print(res<-(d.ts-fcst$fitted))  # 殘差計算及列印
sqrt(mean(res^2,na.rm=TRUE)) # RMSE
mean(abs(res),na.rm=TRUE)    # MAE
mean(res,na.rm=TRUE)         # 殘差平均值
############### end of 9_3.R ############
