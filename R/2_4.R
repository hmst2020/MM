A <- matrix(        # 使用內建函式建構3x3矩陣物件
  data= c(
    -0.4,0.5, -0.1,  # 方程組第2式
    -0.4,-0.4,0.7,   # 方程組第3式
    1,1,1),          # 設其三人工資相同
  nrow = 3, 
  byrow=TRUE,
  dimnames=list(     # 以list物件賦予列名、行名
    c('Ben','John','Tim'),
    c('Ben','John','Tim')
  ))
print(A)             # 印出投入產出矩陣
(B <-matrix(
  c(0,0,9000),       # 設其三人工資合計9000
  ncol = 1))
solve(A,B)           # 本例結果(三人分別工資)
############### end of 2_4.R #######################