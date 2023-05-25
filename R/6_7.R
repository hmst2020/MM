###### 1 美分複利的第30天(次方)#############
0.01*(1+1)^30/1000000 - 3   # 每天翻倍與一次領的差異(百萬)
#######1 美分複利30天曲線圖(等比級數和)#######
y1.f <- function(y,rt,p){   # 自訂一函式計算每天翻倍本利和
  # 等比級數和的公式(換算以millions回傳)
  p*(1-rt^y)/(1-rt)/1000000 # y: 第幾日 rt: 公比 p:本金
}
prnspl <- 0.01                     # 一美分本金(美元)
title <- '1美分起每日翻倍之未來値' # 圖表標題
xy <- data.frame(x = c(0,30),y=c(0,0)) # 繪圖x、y 軸範圍
x.label <- '天'                        # x軸標籤
y.label <- '本利和(millions)'          # y軸標籤
lgnd.title <- ''                       # 圖例標題
rt= 2.0                                # 等比級數之公比(每日翻倍)
colors <- c('#FF2345','#000000')       # 利率顏色對應
library(ggplot2)                  # 載入ggplot2 函式庫
p<-ggplot(
  data=xy,                        # 指定繪圖資料來源
  mapping=aes(x,y)                # x、y軸在引數data 的對應行
)+
  ggtitle(title)+                 # 圖標題
  xlim(0,30)                      # 畫出x軸尺度的範圍，本例為30天
p <- p +
  labs(x=x.label,y=y.label)+      # 給予xy軸標籤
  theme(                        # 主題設定
    axis.title.x = # x軸標籤的顏色、大小、字體粗細
      element_text(color = "black", size = 12, face = "bold"),
    axis.title.y = # x軸標籤的顏色、大小、字體粗細
      element_text(color = "blue", size = 12, face = "bold")
  )+
  scale_colour_manual(     # 圖例依線圖顏色對應標示
    name=lgnd.title,       # 圖例名稱
    values =colors)+       # 圖例顏色
  stat_function(  # 使用stat_function將x軸的值及args的引數值算出y值
    fun = y1.f,   # 呼叫y1.f函式計算出y軸值
    n = 1000,     # 線圖依x軸計算y軸值之內插點數
    args = list(  # 呼叫y1.f函式時指定除第一個引數外，其它的引數
      p=prnspl,
      rt = rt),
    aes( 
      colour = '一美分未來値'))+  # 圖例線圖的文字標示
  stat_function(  # 同上
    fun = function(x) (3),
    n = 1000,
    aes(colour = '3百萬')) 
print(p) # 顯示圖形
############# end of 6_7.R##########