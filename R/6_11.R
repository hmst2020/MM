P<- 120000  #  貸款金額(現值)
i<- 0.0045  #  每期利率
n <- 360    #  期數
(S <- P*((1+i)^n))     # 求複利下之終值
factor <- sum(         # 加總向量元素
  (1+i)^(0:(n-1)))     # 將0 至 n-1 期的複利加總
(R<-S/factor)     # 以 S 求算每期償還金額
############# end of 6_11.R##########