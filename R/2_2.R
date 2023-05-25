A <- matrix(        # 使用內建函式建構3x3矩陣物件
  c(2,1,1, 
    3,2,1,
    2,1,2),
  nrow = 3, 
  byrow=TRUE)
print(A)            # 列印矩陣A
(solve(             # 使用solve解矩陣等式a %*% x = b 其中的x
  a=A,
  b=diag(3)         # b參數 即為反矩陣定義中的I
))
solve(a=A)          # 省略b參數即代表解反矩陣
############## end of 2_2.R################