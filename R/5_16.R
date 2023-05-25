Ex<-220         # 母體平均數
sigma<-15       # 母體標準差
N<-10000      # 母體大小
set.seed(123)   # 設置可重現隨機取樣
smpl<-rnorm(N,mean=Ex,sd=sigma)  # 產生隨機母體
mean(smpl)              # 驗證母體平均數
sd(smpl)*sqrt((N-1)/N)  # 驗證母體標準差

hist(                       # 母體的常態分佈圖(直方圖)
  x=smpl,                   # 母體
  col ="#00800060")         # 塗色
abline(v=Ex,col='red',lty=2) # 於母體平均數處標示垂直虛線

n<-c(10,30,100)      # 抽樣數
k<-2000     # 抽樣批次
par(mfcol=c(1,3))      # 設置繪圖排列(一列三行)
for (i in n){
  x<-c()      # 樣本平均數初始直
  for (j in 1:k){   # 每批抽樣100的樣本平均數
    x[j]<-mean(sample(smpl,i,replace=FALSE))
  }
  h<-hist(       # 樣本平均數的分布圖(列印)
    x=x,               # k批次樣本平均數
    xlim=c(200,240),
    col ="#00800060") # 塗色
  abline(v=220,col='red',lty=2) # 依題意標示平均值220處垂直虛線
}
sum(h$density[which(h$mids<=217)])  # 計算直方圖條圖面積機率近似值
par(mfcol=c(1,1))      # 重置繪圖排列為預設值(一列一行)

pnorm(217,mean=Ex,sd=sigma/sqrt(100))    # 計算累積機率
pnorm(                 # 計算累積機率(標準常態分布)
  (217-Ex)/sigma*sqrt(100),
  mean=0,sd=1)    
############### end of 5_16.R ############