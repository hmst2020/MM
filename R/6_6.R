######## a.	20年期表格 ##############
y1.f <- function(y,rt,p){   # 自訂一函式計算複利率本利和
  p*(1+rt)^y                # y: 第幾年 rt: 利率 p:本金
}
prnspl <- 100                  # 本金(principal)
y=c(0:20)                      # 投資各年數
rts= c(0.05,0.10,0.15)         # 各年利率
f.data <- data.frame(       # 表格資料依投資年數各一列(row)
  year=y)
for (i in 1:length(rts)){   # 表格資料依年利率組各產生一行(column)
  # 每行資料由本金、利率與投資年數由函式y1.f分別得出
  c.data=c()                # 初始每年資料
  for (j in 1:length(y)){   # 計算每年複利下各本利和
    c.data[j] <- round(y1.f(y[j],rts[i],prnspl),digits=0)
  }
  f.data <- cbind(    # 使用cbind將產生的行資料c.data併入f.data
    f.data, c.data)  
}
c.names <- c(NULL)          # 欄位名稱初始值
for (i in 1:length(rts)){   # 依利率給予新的欄位名稱
  c.names[i] <- paste0('年利率',rts[i]*100,'%')
}
colnames(f.data) <- c('投資年數',c.names) # 賦予f.data新的欄位名稱
library(writexl)
write_xlsx(
  x=f.data,                     # data.frame 物件資料
  path = './data/out1.xlsx',    # 檔案名及位置
  col_names = TRUE,             # 含欄位名稱
  format_headers = TRUE)         # 欄位名稱粗體 
######## b.	20年期本利和曲線 ##############
y1.f <- function(y,rt,p){      # 自訂一函式計算複利率本利和
  p*(1+rt)^y                   # y: 第幾年 rt: 利率 p:本金
}
# 宣告本例相關常數
prnspl <- 100                             # 本金(principal)
title <- "$100 複利率下每年底累計本利和"  # 圖表表題
xy <- data.frame(x = c(1,20),y=c(0,5000)) # 繪圖x、y 軸範圍
x.label <- '年'              # x軸標籤
y.label <- '累計金額(百萬)'  # y軸標籤
lgnd.title <- '年利率'       # 圖例標題
rts= c(0.05,0.10,0.15)       # 各年利率
sizes= c(0.2,0.7,1.2)        # 線條粗細對應
colors= c('#FF2345','#34FF45','#AD34AE') # 各利率線圖顏色順序對應
library(ggplot2)             # 載入ggplot2 函式庫
j<- 1
p<-ggplot(                    # 建立ggplot 繪圖物件
  data=xy,                    # 指定繪圖資料來源
  mapping=aes(x=x,y=y)        # x、y軸在引數data 的對應行
)+
  ggtitle(title)+               # 圖標題
  xlab(x.label)+ylab(y.label)+  # 給予xy軸標籤
  theme(                        # 主題設定
    axis.title.x = # x軸標籤的顏色、大小、字體粗細
      element_text(color = "black", size = 12, face = "bold"),
    axis.title.y = # x軸標籤的顏色、大小、字體粗細
      element_text(color = "blue", size = 12, face = "bold")
  )+
  xlim(0,20)+            # 畫出x軸的範圍，本例為20年
  scale_colour_manual(     # 圖例依線圖顏色對應標示
    name=lgnd.title,       # 圖例名稱
    values =colors)        # 圖例顏色
# 宣告ggplot 疊加線圖函式
s.f <- function(s,rt,p,size){   # 自訂一ggplot 疊加線圖函式
  s<- s+
    stat_function(  # 使用stat_function將線圖疊加於plot 物件p上
      fun = y1.f,   # 呼叫y1.f函式計算出y軸值
      n = 1000, # 線圖依x軸計算y軸值之內插點數，本例為拋物曲線此數字影響平滑程度
      args =    # 呼叫y1.f函式時指定除第一個引數外，其它的引數
        list(p=p,rt = rt), # 給予p(本金)及rt(利率)引數
      mapping=aes(   # 線圖顏色及圖例不同線圖的文字標示
        colour = as.character(rt)), 
      size = size  # 線圖粗細
    )
  return(s)
}
for (i in 1:length(rts)){  # 利用迴圈繪出疊加曲線圖
  p <-s.f(    # 使用自訂函式將繪圖物件疊加各線條
    p,        # ggplot繪圖物件
    rts[i],   # 利率對應
    prnspl,   # 本金
    sizes[i]  # 線圖粗細
  )
}
print(p)      # 顯示圖形
############# end of 6_6.R##########