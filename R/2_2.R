(A <- rbind(c(2,1,1),c(3,2,1),c(2,1,2)))  # 使用rbind組合成3x3矩陣
(solve(             # 使用solve解矩陣等式a %*% x = b 其中的x
  a=A,              # 矩陣A
  b=diag(3)))       # b參數 即為反矩陣定義中的I
solve(a=A)          # 省略b參數即代表解反矩陣
############## end of 2_2.R################