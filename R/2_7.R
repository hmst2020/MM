A <- matrix(            # 建立上述矩陣A
  c(0,1,2,3,4,5,        # 自變數x(年度)
    1,1,1,1,1,1),       # 常數項b(截距)
  nrow = 6,
  byrow=FALSE)
colnames(A)<-c('m','b') # 為A的行命名
print(A)                # 列印 A矩陣
Y <- matrix(            # 建立上述單行矩陣Y
  c(2.91,3.23,3.42,3.63,3.85,4.08), # 每年費用
  nrow = 6, 
  byrow=TRUE)
print(Y)                # 列印Y矩陣
(AT <- t(A))            # 求矩陣A的轉置(transpose)矩陣，即上述的AT
(C <- AT%*%A)           # 求上述 CX=D 式其中的C
(D <- AT%*%Y)           # 求上述 CX=D 式其中的D
solve(a=C,b=D)          # 使用solve解矩陣等式C %*% X = D 其中的X
############### end of 2_7.R #######################