library(Deriv)
f<-function(x,y) x*y    # 目標函式(面積函數)
X<-0:10                 # x(長)範圍
Y<-0:10                 # y(寬)範圍
A = outer(X, Y,f)       # 依x、y軸範圍計算z軸(面積)，構成矩陣 
par(mar=c(2,0,0,0))        # 調整環境參數(圖邊界)
res<-persp(x=X, y=Y, z=A,  # 繪製目標函數分布圖(3D)
           col='#ABBC9A',  # 顏色
           theta = 60,     # 水平左轉(順時鐘)60度
           phi = -10,      # 垂直向後傾斜10度
           zlim=c(0,120),   # z軸(面積)座標範圍
           ticktype='detailed', # 各軸刻度依陣列資料明細標示
           zlab='面積    \n\n',    # z軸標籤
           cex.lab = 1.5)       # 各軸標籤文字放大1.5倍
lines(trans3d(              # 疊加限制條件在3D上的線圖
           x=X,y=10-X,z=0,   
           pmat = res),
      col='red')
lines(trans3d(              # 限制式在目標函數曲面上之投影
           x=X,
           y=10-X,
           z=f(X,10-X),     # 限制條件下的z座標值(面積)
           pmat = res),
      col='red')

g<-function(x,y) x+y-10     # 限制條件函數
print(fxy<-Deriv(f,c('x','y')))  # 目標函數對x、y偏微
print(gxy<-Deriv(g))             # 限制函數對x、y偏微

# y=lambda*1
# x=lambda*1
# x+y-10=0
print(S<-solve(       # 解線性方程組，求x、y、lambda 的解
  a=rbind(            # 係數矩陣
    setNames(c(0,1,-1),c('x','y','lambda')), # 給予係數項名稱
    c(1,0,-1),
    c(1,1,0)
  ),
  b=c(0,0,10)         # 常數項向量(rhs)
))

print(z<-unname(f(S['x'],S['y'])))   # 目標函數極值
points(trans3d(     # 疊加繪出極值以實心圓點標示
       x=S['x'],y=S['y'],z=z, 
       pmat = res),
    col='red',               
    pch = 16,                # 實心圓點
    cex=1.2)                 # 標示點放大倍數

############### end of 4_11.R ###########