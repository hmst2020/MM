library(Ryacas)
N<-function(p) -78*p^2+655*p-1125    # 市場總需求量
X<-function(p,h) h*N(p)              # 銷售量
C<-function(x) 50+1.5*x+8*x^0.75     # 銷售成本
P<-function(p,h) p*X(p,h)-C(X(p,h))  # 總利潤

df<-data.frame()      # 初始一環境變數(data frame 物件)
Pf<-function(p,h){     # 計算利潤函數、填入df物件各欄
  Pph<-P(p,h)   # 總利潤
  df<<-rbind(   # 於環境變數df上新增p與h組合的紀錄
    df,         # 在df物件上
    data.frame(   # 建構一筆新的row
      h=h,        # 市場占有率
      p=p,        # 價格
      N=N(p),     # 市場總需求量
      X=X(p,h),   # 波音707銷售量
      C=C(X(p,h)),# 成本
      Profit=Pph  # 總利潤
    ))
  return (Pph)
} 
p<-seq(4.5,5.5,0.01)     # 定價範圍
h<-seq(0.25,1,0.1)       # 市場占有率
head(M<-outer( # 依p、h軸範圍計算P軸(利潤)，構成矩陣
  setNames(p,c(p)),   # 列表示定價範圍
  setNames(h,c(h)),   # 行表示市占率範圍
  Pf),6)     # 列印最前6筆
tail(M,6)     # 列印最後6筆

par(mar=c(2,0,0,0))        # 調整環境參數(圖邊界)
res<-persp(x=p, y=h, z=M,  # 繪製目標函數分布圖(3D)
           col='#ABBC9A',  # 顏色
           theta = 40,     # 水平左轉(順時鐘)40度
           phi = -2,      # 垂直向後傾斜2度
           zlim=c(min(M)-50,max(M)+50),   # z軸(利潤)座標範圍
           ticktype='detailed', # 各軸刻度依陣列資料明細標示
           xlab='價格',    # x軸標籤
           ylab='市佔率',  # y軸標籤
           zlab='利潤\n\n',# z軸標籤
           cex.lab = 1)       # 各軸標籤文字放大倍數


library(data.table)
print(as.data.table(df),3)   # df 的前後3筆
df[which(df$Profit>0 & df$Profit<0.05),] # 找出損益平衡點
df[which(df$Profit==max(df$Profit)),] # 找出總利潤最高點

library(Deriv)
print(Pp<-Deriv(P,c('p'))) # 求目標函數對p(價格)的偏導數

SS<-function(x) {  # 自訂函式求以知h值， p的最佳解使目標利潤最大
  ss<-uniroot(    # p的偏導數求根
    f=Pp,     # 目標函數對p變數的一階偏導數
    interval=c(min(p),max(p)),    # p參數之起迄值
    h=x       # Pp函式的h參數值
  )
  return (ss$root)
}
f<-function(h){  # 自訂一函式針對不同的市場占比，產生最適價格表
  df<-data.frame()  # 初始一局部變數(local variable)
  for (x in h){
    px<-SS(x)    # 最佳定價
    df<-rbind(   # 於局部變數df上新增p與h組合的紀錄
      df,         # 在df物件上
      data.frame(   # 建構一筆新的row
        h=x,        # 市場占有率
        p=px,        # 價格
        N=N(px),     # 市場總需求量
        X=X(px,x),   # 波音707銷售量
        C=C(X(px,x)),# 成本
        Profit=P(px,x)  # 總利潤
      ))
  }
  return (df) 
}
print(df2<-f(h))   # 列印在各種不同市場占有率下，最適價格表

print(f(c(0.5,1.0))) # 列印在0.5及1.0市場占有率下，最適價格表

points(trans3d(     # 繪出各市場占比(h)下最適價格(p)各點
  x=df2$p, y = df2$h, z =P(df2$p,df2$h),
  pmat = res),
  col='red',               
  pch = 16,         # 實心圓點
  cex=1.1)  
lines (trans3d(     # 繪出p與h使利潤最大的趨勢線
  x=df2$p, y = df2$h, z =P(df2$p,df2$h), 
  pmat = res), 
  col = 'red')

############### end of 4_16.R ###########