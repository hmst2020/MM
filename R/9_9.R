library(fpp2)
demand<-c(1600,2200,2000,1600,2500,3500,    # 各期實際需求
        3300,3200,3900,4700,4300,4400)
fitted<- c(1523,1810,2097,2383,2670,2957,   # 各期擬合(預測)
        3243,3530,3817,4103,4390,4677)
print(residuals<-demand-fitted)         # 各期殘差
RSFE<-sum(residuals)                    # 預測誤差移動總和
MAD<- mean(abs(residuals))              # 平均絕對誤差(MAD)
MAPE<-mean(abs(residuals/demand))*100   # 平均絕對百分比誤差
MSE<- mean(residuals^2)                 # 平均平方誤差
print(res<-setNames(                    # 列印結果
  c(MAD,MSE,MAPE,RSFE,RSFE/MAD),
  c('MAD','MSE','MAPE','RSFE','RSFE/MAD')
))

model<-list(              # 彙整lm類別物件所需資料
  fitted.values=fitted,                             # 擬合(預測)值
  residuals=(demand-fitted),                      # 殘差
  terms=as.formula('demand ~ NULL'),   # 迴歸公式
  model=data.frame(demand=demand))  # 迴歸公式內各資料值(實際需求)
fit<-structure(model, class = "lm")   # 創造lm類別物件
accuracy(fit)         # 衡量擬合(預測)準確性之各項指標
################# end of 9_9.R #################