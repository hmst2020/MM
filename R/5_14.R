dx<-0.1           # 各樣本微小距離
x<-seq(0,300,dx)    # 樣本空間
distr<-dnorm(x, mean=160, sd=30) # 使用內建的函式計算機率密度分布
plot(       # 列印機率密度函式圖形
  x=x,      # x軸(樣本空間)
  y=distr,  # y軸(機率分配)
  type='l', # 點與線
  main='常態分配機率密度分布',  # 圖標題
  xlab='反應時間(微秒)',      # x軸標籤
  xaxt='n',       # 抑制x軸尺度繪出
  xlim=c(0,300),            # 指定x軸的範圍
  ylab='機率')          # y軸標籤
axis(side = 1, at=seq(0,300,20))
abline(v=100,col='blue',lty=2) # 於期望值處標示垂直虛線
abline(v=180,col='blue',lty=2) # 於期望值處標示垂直虛線
polygon(           # 繪出填色之多邊形
  x=c(100,x[which(x>=100 & x<=180)],180),     # 多邊形x範圍
  y=c(0,distr[which(x>=100& x<=180)],0),      # 多邊形y範圍
  col='#00800060') # 多邊形填入之顏色與透明度 (最後二碼)

sum(distr[which(x>=100& x<=180)]*dx)  # 計算累積機率(近似平滑)

pnorm(180,mean=160,sd=30)-     # 計算累積機率
  pnorm(100,mean=160,sd=30)
############### end of 5_14.R ############