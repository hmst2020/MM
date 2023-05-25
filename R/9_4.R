library(fpp2)
load(file='data/T102.rds')         # 載入本例資料物件
F2<-function(FIT1,alpha,A1){       # 自訂函式，平均平滑公式(9.4.2)
  return (FIT1+alpha*(A1-FIT1))
}
T2<-function(T1=0,beta,F2,FIT1){   # 自訂函式，趨勢平滑公式(9.4.3)
  return (T1+beta*(F2-FIT1))
}
FIT2<-function(F2,T2){             # 自訂函式，合計平滑公式(9.4.1)
  return (F2+T2)
}
alpha<-0.3        # 指定平均平滑參數值
beta<-0.4         # 指定趨勢平滑參數值
initFt=1600       # 初始值(平均平滑值)
initTt=600        # 初始值(趨勢平滑值)
df<-data.frame( # 此物件用於紀錄每期計算結果，初值即第一期
  At=data[1:1],   # 初值(實際需求、觀測值)
  Ft=initFt,      # 初值(平均值)
  Tt=initTt,      # 初值(趨勢值)
  FITt=initFt+initTt)    # 初值(合計值)
for (t in 2:length(data)){  # 依據初值(第1期)，自第2期起依據公式計算
  Ft<-F2(                   # 公式 9.4.2
    FIT1=df[t-1,'FITt'],    # 前期合計
    alpha=alpha,            # 指定alpha引數
    A1=data[t-1])           # 前期觀測值
  Tt<-T2(                   # 公式 9.4.3
    T1=df[t-1,'Tt'],        # 前期趨勢平滑值
    beta=beta,              # 指定beta引數
    F2=Ft,                  # 本期平均平滑值
    FIT1=df[t-1,'FITt'])    # 前期合計
  FITt<-FIT2(F2=Ft,T2=Tt)   # 合計本期擬合值(公式 9.4.1)
  df[t,'At']<-data[t]       # 紀錄第t期觀測值
  df[t,'Ft']<-Ft            # 紀錄第t期平均平滑值
  df[t,'Tt']<-Tt            # 紀錄第t期趨勢平滑值
  df[t,'FITt']<-FITt        # 紀錄第t期擬合值
}
print(df)        # 列印擬合結果

h<-3     # 預測期數
orign<-length(d.ts)  # 預測起始期別
fcst.df<-tail(df,1)  # 樣本最後一期
for (t in 2:(h+1)){
  Ft<-F2(                   # 公式 9.4.2
    FIT1=fcst.df[t-1,'FITt'],    # 前期合計
    alpha=alpha,                 # 指定alpha引數
    A1=fcst.df[t-1,'At'])        # 前期觀測值
  Tt<-T2(                   # 公式 9.4.3
    T1=fcst.df[t-1,'Tt'],        # 前期趨勢平滑值
    beta=beta,                   # 指定beta引數
    F2=Ft,                       # 本期平均平滑值
    FIT1=fcst.df[t-1,'FITt'])    # 前期合計
  FITt<-FIT2(F2=Ft,T2=Tt)   # 合計本期擬合值(公式 9.4.1)
  fcst.df[t,'Ft']<-Ft            # 紀錄第t期平均平滑值
  fcst.df[t,'Tt']<-Tt            # 紀錄第t期趨勢平滑值
  fcst.df[t,'FITt']<-FITt        # 紀錄第t期擬合值
  fcst.df[t,'At']<-fcst.df[t,'FITt']   # 紀錄第t期觀測值(同擬合值)
}
print(fc.ts<-ts(              # 以預測結果向量物件建構時間序列物件
  tail(fcst.df,h)[,'FITt'],start=orign+1,frequency=1))

dfts<-ts(df, start=1, frequency=1)      # 擬合結果建構時間序列物件
g<-autoplot(dfts[,'At'],series="實際觀察值") +
  autolayer(dfts[,'Ft'],series="平均平滑線") +
  autolayer(dfts[,'Tt'],series="趨勢平滑線") +
  autolayer(dfts[,'FITt'],series="擬合線") +
  autolayer(fc.ts,series="點預測線") +
  ylab("市場需求")+
  xlab("期別")+
  ggtitle("各期實際需求與預測(趨勢調整指數平滑)")
print(g)

print(res<-(dfts[,'At']-dfts[,'FITt']))  # 殘差計算及列印
sqrt(mean(res^2,na.rm=TRUE)) # RMSE
mean(abs(res),na.rm=TRUE)    # MAE
mean(res,na.rm=TRUE)         # 殘差平均值

fcst <- holt(     # Holt's 趨勢預測
  d.ts,           # 本例表9-1的時間序列物件
  h=h,            # 預測期數
  initial='simple',  # 簡單初始模式
  alpha=0.3,         # 指定alpha值(平均平滑參數值)
  beta=0.4           # 指定beta值(趨勢平滑參數值)
)
summary(fcst)        # 預測物件總攬

print(fcst$fitted)   # 列出擬合結果
print(sigma<-sqrt(sum(  # 標準差(standard deviation)
  fcst$residuals^2)/length(fcst$residuals)))
fcst$mean[1]+qnorm(0.90, mean = 0, sd = 1)*sigma # 預測第1期的 Hi 80
fcst$mean[1]+qnorm(0.10, mean = 0, sd = 1)*sigma # 預測第1期的 Lo 80
g<-autoplot(d.ts,series="實際觀察值") +
  autolayer(fcst$fitted,series="擬合線") +
  autolayer(fcst,series="預測線") +
  ylab("市場需求")+
  xlab("期別")+
  ggtitle("各期實際需求與預測(Holt's 趨勢預測法)")
print(g)

print(res<-(d.ts-fcst$fitted))  # 殘差計算及列印
sqrt(mean(res^2,na.rm=TRUE)) # RMSE
mean(abs(res),na.rm=TRUE)    # MAE
mean(res,na.rm=TRUE)         # 殘差平均值
################# end of 9_4.R #################