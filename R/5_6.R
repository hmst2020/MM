####frequency pologon ###########
b<-c(6, 14, 22, 30, 38, 46, 54)   # 各組邊界
x<-sapply(seq(1,length(b)-1,1),   # 求各組中心點
          function(x) (b[x]+b[x+1])/2)
x<-c(min(b)-4,x,max(b)+4) # 加上首尾兩端之延伸(平分組內距之一半)
y<-c(0.2,  0.3,  0.25, 0.15, 0.07, 0.03) # 各組相對頻率
plot(              # 繪出多邊形之各點及連線
  x=x,             # x軸(中心點)向量資料
  y=c(0,y,0),      # y軸各值向量
  xlab= "銷售額",    # x軸標籤
  ylab= "相對頻率",  # y軸標籤
  xaxt='n',        # 不標示尺度      
  type="b")        # 點與線
axis(side=1,       # 標示x軸尺度
     at=c(min(x),b,max(x)),  # 尺度位置
     labels=c(min(x),b,max(x)))  # 尺度文字(標籤)
par(new=TRUE)    # 疊加另一圖層
library(actuar)
gdata<- actuar::grouped.data(  # 建構群組化資料(左開右閉)
  Group=b,      # 群組界線(含最小、最大)
  Frequency=y)  # 每組對應頻次
h<-graphics::hist(   # 產生直方圖物件並繪出
  x=gdata,           # 群組資料
  main="頻率多邊圖與直方圖", # 圖標題
  yaxt='n',          # 抑制y軸尺度繪出
  xaxt='n',          # 抑制x軸尺度繪出
  ylab='',           # 無y軸名稱標籤
  xlab='',           # 無x軸名稱標籤
  xlim=range(x))     # 指定x軸的範圍
polygon(           # 繪出填色之多邊形
  x=x,             # 多邊形x範圍
  y=c(0,y,0),      # 多邊形y範圍
  col='#00800060') # 多邊形填入之顏色與透明度(最後二碼)
#### ogive curve ###########
library(purrr)
plot(              # 繪出肩形曲線
  x=b,             # 群組邊界值(對應累計值)
  y=c(0,accumulate(y,`+`)),   # 累計相對頻率(從0開始到1)
  main='肩形曲線 - 相對頻率',          # 圖標題
  xlab='銷售金額',            # x軸標籤
  ylab='相對頻率累計',        # y軸標籤
  xlim=c(0,60),               # 指定x軸的範圍
  xaxt='n',                   # 抑制x軸尺度繪出
  type='b',                   # 繪製點與線
  col='#123456')              # 點與線顏色
axis(side=1,    # 標示x軸尺度
     at=seq(0,60,10),      # 尺度值位置
     labels=seq(0,60,10))  # 尺度文字(標籤)

par(new=TRUE)      # 疊加另一圖層
plot(              # 繪出肩形曲線
  x=b,             # 群組邊界值(對應累計值)
  y=c(rev(cumsum(rev(y))),0),   # 累計相對頻率(從1開始到0)
  xlim=c(0,60),               # 指定x軸的範圍
  xaxt='n',                   # 抑制x軸尺度繪出
  yaxt='n',                   # 抑制y軸尺度繪出
  ylab='',                    # 無y軸名稱標籤
  xlab='',                    # 無x軸名稱標籤
  type='b',                   # 繪製點與線
  col='#654321')              # 點與線顏色
############### end of 5_6.R ############