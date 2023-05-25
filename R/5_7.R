cdata<-data.frame(  # 各分類之觀察值
  item=c('服務生粗魯','服務緩慢','冷餐點','餐桌狹窄','氣氛不佳'),
  freq=c(12,42,5,20,10))
x<-cdata[rev(order(cdata$freq)),]  # 由大至小依次數排序
dev.off()  # 重置繪圖參數至預設值
par(mar=c(1, 1, 1, 1))  # 設定繪圖邊界共四個邊(底、左、上、右)
library(RColorBrewer)
cp <- brewer.pal(nrow(cdata), "Set3")  # 取出調色盤5種顏色
pcent<- paste0(round(100*x$freq/sum(x$freq), 1),'%') # 佔比
pie(           # 繪製圓餅圖
  x = x$freq,  # 給予抱怨次數資料
  labels = pcent,   # 抱怨佔比(%)
  border='white',   # 邊界線顏色白色
  col=cp,           # 使用的調色盤
  radius=0.8,       # 圓餅的縮放比例
  main="")          # 不印圖標題
legend(         # 列印圖例
  x='topright', # 圖例位置
  y=x$item,     # 圖例項目文字
  cex = 0.8,    # 文字大小
  fill = cp)    # 圖例各顏色


dev.off()  # 重置繪圖參數至預設值
par(mar=c(5, 6, 4.1, 2.1))  # 設定繪圖邊界共四個邊(底、左、上、右)
barplot(                # 產生長條圖
  height=x$freq,        # 給予繪圖高度資料 
  names.arg=x$item,     # 分析類別名稱
  horiz=TRUE,           # 橫式長條圖
  main='客戶抱怨統計',  # 圖標題
  ylab='',              # 無y軸名稱標籤
  xlab= "次數",         # x軸標籤
  las=1,                # 座標軸文字方向 (0~3)，讀者可自行嘗試
  cex.names=0.7)        # 座標軸文字大小
title(              # 額外加入軸標籤文字
  ylab= "抱怨項目", # y軸標籤
  line=4.5,         # 與圖的邊界距離
  cex.lab=1.2)      # 標籤文字大小
############### end of 5_7.R ############
