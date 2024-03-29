(A <- matrix(   # 產生矩陣物件A，並印出
  c(5,3,        # 需求方程式各係數
    52,-30),    # 供給方程式各係數
  nrow = 2,     # 依序排兩列之矩陣
  byrow=TRUE))  # 依列排滿換列之順序
(B <- matrix(   # 同上
  c(30,-45),    # 矩陣代表式AX=B 之rhs(right hand side)
  ncol=1))      # 依序排一行之矩陣
print(inverse.A <-solve(A)) # 用solve函式解A反矩陣並印出
(X <- inverse.A %*% B)      # A反矩陣與B矩陣相乘算出X

### 5x +3p -30=0       需求線(x為需求數)
### 52x-30p+45=0       供給線(x為供給數)
x<-1:10           # 需求數(千個)
d<-(30-5*x)/3     # 需求線上單價
png(file="data/saving_plot1.png")
plot(                  # 使用內建函式plot 繪製本例之需求線
  x=x,                 # x軸為需求數
  y=d,                 # y軸單價
  type='l',            # 指定繪線圖
  xlab="供需數(千個)", # x軸標籤
  ylab="單價",         # y軸標籤 
  xlim=c(1,7),         # x軸限制刻度
  ylim=c(1,8),         # y軸限制刻度
  col='blue',          # 線圖顏色
  main="需求線與供給線的平衡點") # 圖標題

(s<-(52*x+45)/30)      # 供給線上單價
lines(                 # 繪製本例之供給線(疊加其上)
  x=x,                 # x軸為需求數
  y=s,                 # y軸單價
  col="red",           # 線之顏色
  lwd=2)                # 線之粗細
points(                # 繪製本例供需平衡點X
  x=X[1,1],            # x軸為需求數
  y=X[2,1])            # 單價
legend(                  # 繪製圖例
  5,                     # 左上角在x軸位置
  7,                     # 左上角在y軸位置
  c("需求線","供給線"),  # 圖例說明
  lwd=c(2,2),            # 圖例線寬
  col=c("blue","red"),   # 圖例顏色
  y.intersp=1)            # 圖例高度
text(                       # 將plot繪出的圖疊加文字
  x=X[1,1],                 # 文字對應x軸位置 
  y=X[2,1],                 # 文字對應y軸位置
  adj=c(-0.2,0.2),          # 文字橫向往右調整0.2，縱向往下調整0.2
  paste0(                   # paste將參數轉換為字符串，並將其連接
    '(',X[1],',',round(X[2],6),')'))  # 文字內容
dev.off()
#################end of 1_1.R####################