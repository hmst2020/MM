obj<-'20*x*y+10*(2*x*64/(x*y)+2*y*64/(x*y))'  # 本例目標成本函數
print(ex<-parse(text=obj))  # 解析函數傳回該函數的expression物件

library(Deriv)
print(dx<-Deriv(ex,c('x')))   # 對x微分傳回導函數
print(dy<-Deriv(ex,'y'))   # 對y微分傳回導函數

coeif<-rbind(   # 對數化簡的ln(x)、ln(y)係數矩陣
  c(2,1),        # 第一式ln(x)、ln(y)係數
  c(1,2)         # 第二式ln(x)、ln(y)係數
)
print(matx<-solve(   # 解線性函數向量ln(x)、ln(y)
  a=coeif,       #  a矩陣
  b=c(log(128)-log(2),log(128)-log(2))    # b 向量
))
print(xy<-exp(matx))   # 向量ln(x)、ln(y)反函數運算


# D=fxx(x0,y0)fyy(x0,y0)−(fxy(x0,y0))2
# If  D>0  and  fxx(x0,y0)>0 , then f has a local minimum at  (x0,y0) .
# If  D>0  and  fxx(x0,y0)<0 , then f has a local maximum at  (x0,y0) .
# If  D<0 , then  f  has a saddle point at  (x0,y0) .
# If  D=0 , then the test is inconclusive.
x<-xy[1]   # 目標函數的x 解
y<-xy[2]   # 目標函數的y 解
print(dx2<-Deriv(dx,'x'))   # x二階偏導函數
print(dy2<-Deriv(dy,'y'))   # y二階偏導函數
print(dxy<-Deriv(dx,'y'))   # 將x一階導函數再對y微分得二階導函數
fxx<-eval(dx2)     # x二階偏導函數值
fyy<-eval(dy2)     # y二階偏導函數值
fxy<-eval(dxy)     # xy二階偏導函數值
print(D<- fxx*fyy-fxy^2)   # 判斷是否有極值
print(fxx)                 # 判斷是否有極小值

print(c<-eval(ex)) # 目標函數值(極小值)
print(64/x/y)   # 水族箱高度

X<-2:6     # 繪製x軸範圍
Y<-2:6     # 繪製y軸範圍
fc <- function(x,y) {      # 自訂函式計算目標函數值
  eval(ex)
}
print(C<-outer(  # 依x、y軸範圍計算z軸(成本)，構成矩陣
  X=setNames(X,X),   # 列表示長的範圍
  Y=setNames(Y,Y),   # 列表示寬的範圍
  fc))     # 面積函數

par(mar=c(2,0,0,0))        # 調整環境參數(圖邊界)
res<-persp(x=X, y=Y, z=C,  # 繪製目標函數分布圖(3D)
           col='#ABBC9A',  # 顏色
           theta = 30,     # 水平左轉(順時鐘)30度
           phi = -10,      # 垂直向後傾斜10度
           zlim=c(500,1500),   # z軸(成本)座標範圍
           ticktype='detailed', # 各軸刻度依陣列資料明細標示
           zlab='c    \n\n',    # z軸標籤
           cex.lab = 1.5)       # 各軸標籤文字放大1.5倍
library(grDevices)
points(trans3d(x,y,c,           # 疊加繪出極值以實心圓點標示
               pmat = res),
       col='red',               
       pch = 16,                # 實心圓點
       cex=1.5)
lines(trans3d(x=X,y=y,c,        # 疊加繪出極值處x偏微分切線
              pmat = res),
      col='red')
lines(trans3d(x=x,y=Y,c,        # 疊加繪出極值處y偏微分切線
              pmat = res),
      col='red')
############### end of 4_10.R ###########