library(fpp2)
library(olsrr)
x1<-c(63,59,59,54,59,54,60,61,59,59,       # 血塊分數
      63,62,59,54,62,59,58,53,61,59,
      60,59,58,68,58,59,53,64,57,59,
      58,58,51,56,57,58,58,54,65,60,
      53,60,56,57,58,55,61,60,60,65) 
x2<-c(67,36,46,73,71,82,91,50,87,88,       # 體能指標
      63,79,89,50,70,58,53,90,23,70,
      61,62,53,82,55,82,69,44,63,78,
      59,53,79,20,65,52,50,23,93,59,
      81,37,76,93,72,86,83,92,60,83)
x3<-c(95,55,47,63,74,75,65,28,94,70,       # 肝功能分數
      50,51,14,26,57,69,59,37,51,68,
      81,56,68,72,40,43,48,10,56,74,
      53,80,68,98,59,70,55,80,70,60,
      74,66,86,82,60,84,31,25,67,55)
x4<-c(69,34,40,48,47,46,67,39,64,64,       # 酸素檢定分數
      58,46,53,36,60,53,51,30,42,58,
      52,59,46,84,50,48,26,47,59,57,
      49,48,42,51,46,39,39,33,94,51,
      35,53,54,66,51,72,40,32,45,55)
x5<-c(70,42,47,44,61,47,73,53,75,68,       # 體重
      68,75,72,40,68,50,46,48,47,58,
      56,55,46,94,48,63,40,59,51,62,
      52,48,39,42,51,47,46,41,95,57,
      46,45,52,62,55,53,72,75,56,89)
y<- c(2986, 950, 950,1459,1969,1975,2506, 722,  # 手術後存活時間
      3524,2509,1766,2048,1042,  19,2038,1792,
      1290,1534, 803,2063,2312,1597,1848,3118,
      834,1830, 819, 596,1359,2386,1349,1866,
      1378,1396,1649,1627,1139, 879,2928,1663,
      1908,1423,2444,2715,1699,2440,1432,1441,
      1947,2451)
df<-data.frame(   # 建構data frame資料物件
  y=y,            # 存活時間
  x1=x1,          # x1資料
  x2=x2,          # x2資料
  x3=x3,          # x3資料
  x4=x4,          # x4資料
  x5=x5)          # x5資料
full_model <- lm(          # 對data frame進行線性迴歸
  formula=y ~ .,           # 迴歸依據所有自變數與應變數
  data=df)                 # 資料物件
summary(full_model)    # 全變數模型彙總
X<-as.matrix(cbind(1,df[,2:6]))   # matrix LSP 公式 的X
Xt<-t(X)
y<-as.matrix(df[,1])
Beta<-solve(Xt%*%X)%*%(Xt%*%y);Beta     # matrix LSP 公式 的 beta
########## 方法一 所有迴歸式比較選取#######################
n<-length(y)      # 觀察樣本數
nvar<-5           # 自變數個數
ttl<-2^nvar       # predictor組合總數
mdls<-data.frame(   # predictor各組合計算指標各紀錄(給予初始值)
  x1=rep(0,ttl),
  x2=rep(0,ttl),
  x3=rep(0,ttl),
  x4=rep(0,ttl),
  x5=rep(0,ttl),
  pn=rep(0,ttl),    # predictor 個數
  CV=rep(NA,ttl),           
  AdjR2=rep(NA,ttl),
  R2=rep(NA,ttl),
  MSE=rep(NA,ttl),
  AIC=rep(NA,ttl),
  AICc=rep(NA,ttl),
  Cp=rep(NA,ttl)
)
vars<-paste0('x',seq(1,nvar))   # 變數名稱向量
p<- nvar+1     # 係數個數(常數項+變數項)
sigma2<-sum(full_model$residuals^2)/(n-p)   # 全變數的變異數

dfnr<-0    # mdls 列號初始值
for (i in 0:nvar){    # 預測因子數的所有組合
  mtx<-combn(nvar,i)  # 因子數的組合矩陣
  for (j in 1:ncol(mtx)){     # 每一組合為mdls一列(筆)，逐列處理
    dfnr<-dfnr+1              # 每一列的列號
    col<-mtx[,j]              # 組合的變數(x1~x5)代號(1~5)
    if (length(col)==0){      # 無自變數(預測因子)的線性迴歸
      f<-as.formula(paste('y','~','NULL'))
      #mdls[dfnr,'formula']<-paste('y','~','NULL')
    }else{                    # 有自變數(預測因子)的線性迴歸
      for (k in 1:length(col)){    # 標記組合
        mdls[dfnr,col[k]]<-1
      }
      mdls[dfnr,'pn']<-length(col)   # predictor 個數
      f<-as.formula(                 # 迴歸公式物件
        paste('y','~',paste(vars[col],collapse='+')))
      #mdls[dfnr,'formula']<-  deparse(f)     # 迴歸公式文字
    }
    fit <- lm(          # 對data frame進行線性迴歸
      formula=f,           # 迴歸依據自變數與應變數
      data=df)             # 資料物件
    SSE<-sum(fit$residuals^2)   # 殘差平方和(sum of squared errors)
    q<-length(col)+1    # 預測因子(predictor)+常數項(intercept)個數
    cv.value<-CV(fit)   # 交叉驗證擬合模型
    acc<-accuracy(fit)  # 準確性指標
    mdls[dfnr,'CV']<-cv.value['CV']   # 交叉驗證誤差估計值
    AIC<-log(SSE/n)*n+2*(q)  # 赤池訊息準則
    mdls[dfnr,'AIC']<-AIC      # 紀錄赤池訊息準則
    #mdls[dfnr,'AIC']<-round(cv.value['AIC'],2) # 赤池訊息準則
    mdls[dfnr,'AICc']<-round(cv.value['AICc'],2) # 修正赤池訊息準則
    mdls[dfnr,'AdjR2']<-round(cv.value['AdjR2'],2) # 調整後判定係數
    Cp<-SSE/sigma2-(n-2*q)   # Mallow's Cp
    #Cp<-ols_mallows_cp(fit, full_model)# olsrr package Mallow's Cp
    mdls[dfnr,'Cp']<-round(Cp,2)      # 紀錄Mallow's Cp
    
    MSE<-SSE/(n-q)           # 平均平方誤差(mean squared error)
    mdls[dfnr,'MSE']<-MSE    # 紀錄MSE
    r2<-1-sum(fit$residuals^2)/sum((df$y-mean(df$y))^2) # R2 判定係數
    mdls[dfnr,'R2']<-round(r2,4)   # 紀錄判定係數
  }
}
head(res<-   # 列出最前幾筆
mdls[order(  # 依模型指標排序    
  mdls$pn,     # 模型預測因子數升冪
  -mdls$AdjR2, # 調整後判定係數降冪
  mdls$MSE),],n=10)  # 平均平方誤差升冪
tail(res)      # 列出最後幾筆
head(res2<-    # 列出最前幾筆
mdls[order(
  -mdls$AdjR2, # 調整後判定係數降冪
  mdls$AICc,   # 修正赤池訊息準則升冪
  mdls$MSE),],6)  # 平均平方誤差升冪
tail(res2)     # 列出最後幾筆
########## 方法二 逐步迴歸選取#######################
##### 往前 ###########
library(olsrr)
ols_step_both_p(full_model)
##### 往後 ###########
kk<-ols_step_backward_aic(full_model,progress=TRUE)  # 殘差服從獨立常態分布
################# end of 9_8.R #################
