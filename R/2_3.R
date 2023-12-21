############ 轉碼加密(encode) ##################
target.str <- "SEPTEMBER IS OKAY" # 加密目標字串
(A <- rbind(        # 使用內建函式rbind建構3x3的加密矩陣物件
  c(1,0,0), 
  c(3,1,5),
  c(-2,0,1)))
(dim.A <- ncol(A))   # A矩陣行數
(str.a <- gsub(      # 去除字串中空白字元
  pattern=" ", replacement="",  #以空字串代替空白字元
  x= target.str))    # 字串對象
(sec.pos <- seq(       # dim.A個字母一組位置
  1,                   # 啟始位置
  nchar(str.a),        # str.a 字串長度(byte數)
  by=dim.A))             # 每隔dim.A個數
(str.vector <-  # 將原自串str.a依dim.A個字元切割
    sapply(      # 使用內建sapply函式執行自訂之匿名函式function(pos)
      X=sec.pos, # 依上述sec.pos切割位置function(pos)的傳入參數
      FUN=function(pos) {     # 匿名函式
        substr(str.a, pos, pos+dim.A-1) # 擷取str.a dim.A個字元
      }
    ))
encode.f<- function(data){ # 自訂轉碼加密函式 參數data為目標字串
  B<- matrix(              # 目標字串之對應數字 dim.A x 1矩陣
    data= match(   # 使用match函式傳回各字母對應之位置數字
      x= unlist(   # 轉換list物件為vector物件
        strsplit(  # 將傳入的字串物件分離各字母，回傳list的結果
          data,    # 目標字串
          split="" # 空字串表示無分隔符號地分割字母
        )
      ), 
      table=LETTERS       # R內建大寫字母vector物件
    ),
    nrow = dim.A,         #  同A列數
    ncol=1,               # 1行
    byrow=TRUE
  )
  return (A %*% B)   # 本例加密法
}
encode.f('SEP')      # 測試函式對SEP字串處理結果
(res<-sapply(     # 將每個切割的字組經加密函式encode.f進行加密
  X=str.vector,
  FUN=encode.f))
result<-as.vector(res) # 將加密的矩陣數字轉為向量物件
############ 解碼(decode) ##################
(inverse.A <- solve(A))    # 上述A 之反矩陣
decode.f<- function(B){  # 宣告自訂解碼函式 參數B為加密vector
  LETTERS[(inverse.A %*%B)[,1]]  # 反矩陣與B相乘結果對應大寫字母
}
(dec.m<-apply(               # 對matrix依序交予FUN指定函式處理
  X=res,
  MARGIN=2,                 # 依res的行序處理
  FUN=function(col){        # 處理每行向量的解碼
    decode.f(col)           # 回傳解碼向量
  }
))
paste(                      # 將dec.m矩陣轉為字串向量後去空白
  as.vector(dec.m),
  collapse='')  
############### end of 2_3.R #######################