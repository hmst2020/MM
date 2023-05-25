load(file='data/T102.rds')    # 載入本例資料物件
weight<-c(0.1,0.2,0.3,0.4)    # 4期的加權配比
library(fpp2)  # 載入fpp2套件
library(TTR)    # 載入TTR套件
print(wma.4<-WMA(     # 計算每4期加權移動平均並列印
  data,               # 本例表10-1的vector 物件
  n=4,                # 取4期(含當期)予以加權
  wts=weight))        # n期的加權配比
print(fit<-           # 每期預測(擬合)值等於前4期加權移動平均
        c(NA,wma.4[1:length(wma.4)-1]))

n<-4     # 移動平均期數
h<-3     # 預測期數
orign<-length(d.ts)  # 預測起始期別
print(d<-c(data[(orign-n+1):orign],rep(NA,h))) # 據以計算預測值
for (t in 1:h){  # 計算並填入NA各預測值
  d[n+t]<-weighted.mean(d[t:(t+n-1)])  # 權重平均數計算
}
print(fc<-tail(d, n=h)) # 依預測期數取出預測結果
fc.ts<-ts(              # 以預測結果向量物件建構時間序列物件
  fc,start=orign+1,frequency=1) 
fit.ts<-ts(              # 以擬合結果向量物件建構時間序列物件
  fit,start=1,frequency=1) 
autoplot(d.ts,series="觀察值")+        # 繪出擬合與預測線圖
  autolayer(fit.ts,series="擬合線") +
  autolayer(fc.ts,series="預測") +
  labs(y="市場需求",x="期別",title='各期實際需求與預測(加權移動平均)')

print(res<-(d.ts-fit.ts))  # 殘差計算及列印
sqrt(mean(res^2,na.rm=TRUE)) # RMSE
mean(abs(res),na.rm=TRUE)    # MAE
mean(res,na.rm=TRUE)         # 殘差平均值
############### end of 9_2.R ############