data<-c(1600,2200,2000,1600,2500,3500,   # 本例資料(vector 物件)
        3300,3200,3900,4700,4300,4400)
d.ts<-ts(          # 以vector 物件建立時間序列物件
  data,            # 本例表10-1的vector 物件
  start=1,        # 資料之起始期別
  frequency=1)    # 每期別1筆
save(data,d.ts,   # 將物件存檔
     file='data/T102.rds')  # 檔案放置路徑

library(fpp2)  # 載入fpp2套件
autoplot(d.ts,series="觀察值")+ # 繪出本例各期實際需求圖
  labs(y="市場需求",x="期別",title='各期實際需求(觀察值)')

library(smooth)
sma.3<-sma(        # 擬合、預測及繪圖函式
  data,            # 時間序列樣本(觀察值)向量物件
  order=4,         # 移動平均期數
  h=3,             # 為往後3期預測
  interval='parametric',  # 預測區間
  silent=FALSE)    # 繪出擬合與預測結果
as.vector(sma.3$fitted)      # 擬合結果以向量物件印出
sma.3$forecast               # 預測結果

library(TTR)   # 載入TTR套件
print(sma.4<-SMA(data,n=4))  # 計算每4期移動平均並列印
print(fit<-           # 每期預測(擬合)值等於前4期移動平均
      c(NA,sma.4[1:length(sma.4)-1]))

n<-4     # 移動平均期數
h<-3     # 預測期數
orign<-length(d.ts)  # 預測起始期別
print(d<-c(data[(orign-n+1):orign],rep(NA,h))) # 據以計算預測值
for (t in 1:h){  # 計算並填入各預測值取代NA
  d[n+t]<-mean(d[t:(t+n-1)])
}
print(fc<-tail(d, n=h)) # 依預測期數取出預測結果
fc.ts<-ts(              # 將預測結果向量建構時間序列物件
  fc,start=orign+1,frequency=1) 
fit.ts<-ts(              # 將擬合結果向量建構時間序列物件
  fit,start=1,frequency=1) 
autoplot(d.ts,series="觀察值")+        # 繪出擬合與預測線圖
  autolayer(fit.ts,series="擬合線") +
  autolayer(fc.ts,series="預測") +
  labs(y="市場需求",x="期別",title='各期實際需求與')

print(res<-(d.ts-fit.ts))  # 殘差計算及列印
sqrt(mean(res^2,na.rm=TRUE)) # RMSE
mean(abs(res),na.rm=TRUE)    # MAE
mean(res,na.rm=TRUE)         # 殘差平均值
############### end of 9_1.R ############