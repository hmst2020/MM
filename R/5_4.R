range(x)    # 全距
sd(x)       # 樣本標準差
var(x)      # 樣本變異數

print(h<-hist(  # 用R內建的hist函式繪圖
  x,    # 給予觀測資料
  breaks=seq(0,24,3), # 組距依據
  main= "觀測值分布(直方圖)", # 圖標題
  xlab= "觀察值",     # x軸標籤
  ylab= "觀測值個數", # y軸標籤
  xlim=c(0,25),       # 指定x軸的範圍
  ylim=c(0,10)        # 指定y軸的範圍
))
abline(v=mean(x),col='red',lty=2)
abline(v=mean(x)-sd(x)/2,col='blue',lty=2)
abline(v=mean(x)+sd(x)/2,col='blue',lty=2)
par(new=TRUE)    # 疊加另一圖層
plot(
  x=h$mids,
  y=h$counts,
  type='l',
  xlab= "",     # x軸標籤
  ylab= "", # y軸標籤
  xlim=c(0,25),       # 指定x軸的範圍
  ylim=c(0,10)        # 指定y軸的範圍
)
############### end of 5_4.R ############
