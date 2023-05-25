############方法一 ##########
library(actuar)
print(gdata<- actuar::grouped.data(  # 建構群組化資料並列印
  Group=c(0,100,200,300,400,500,600),  # 群組界線(含最小、最大)
  Frequency=c( 30,38, 50, 31, 22, 13), # 每組對應頻次
  right = FALSE))    # 右邊界不是閉區間
h<-graphics::hist(   # 產生直方圖物件並繪出
  x=gdata,           # 群組資料
  main= "消費值頻率(直方圖)", # 圖標題
  freq=TRUE,
  xlab= "消費值",    # x軸標籤
  ylab= "頻率次數")   # y軸標籤

library("HistogramTools")
PlotRelativeFrequency(  # 繪出相對頻率分布圖
  x=h,                   
  main= "消費值相對頻率(直方圖)", # 圖標題
  xlab= "消費值",     # x軸標籤
  ylab= "相對頻率",   # y軸標籤
  ylim=c(0,ceiling(max(h$density)*1000)/10))  # 指定y軸的範圍
############方法二 ##########
bar<-barplot(    # 繪製長條圖
  height=h$counts/sum(h$counts), # 高度值(相對頻率)
  col='white',   # 條圖填入顏色
  space=0,       # 條圖間隔
  main= "消費值相對頻率(直方圖)", # 圖標題
  xlab= "消費值",     # x軸標籤
  ylab= "相對頻率",    # y軸標籤
  ylim=c(0,ceiling(max(h$density)*1000)/10)  # 指定y軸的範圍
)
axis(1,at=c(0,bar+0.5),labels=eval(parse(text=h$xname)))  # x軸標記
############### end of 5_5.R ############