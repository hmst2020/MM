T <-cbind(          # 建構遞移矩陣物件
  c(0.97,0.03),     
  c(0.06,0.94))
dimnames(T) <- list( # 對矩陣給予列名、行名
  c('都市','郊區'),c('都市','郊區'))
print(T)           # 列印遞移矩陣
(x0<-c(0.65,0.35)) # 初始人口分布狀態
(x1 <- T %*% x0)   # 求第一年後分布狀態
############### end of 8_1.R ############