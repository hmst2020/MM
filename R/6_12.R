P<- 50000  #  貸款金額(現值)
i<- 0.08  #  每期利率
n <- 6    #  年底償還期數

(S<- P*(1+i)^n)   #計算第5年後複利下的終值
(R<- S/sum((1+i)^(1:5)))  # 計算每年第1~5年底償還金額

c0<-0;c1<-NA;c2<-NA;c3<-NA;c4<-P  # 償還表中自左至右各欄初始值
df<-data.frame()                  # 表中各列與行資料物件初始值
df<-rbind(df,c(c0,c1,c2,c3,c4))   # 給與第0期資料
c2<-R                             # 每期攤(付)款金額
for (j in 1:5){                   # 依每年迴圈共5次
  c1<-c4*i                        # 利息攤還
  c3<-R - c1                      # 本金償還
  c4<-c4 - c3                     # 尚未償還本金
  df<-rbind(df,c(j,c1,c2,c3,c4))  # 計算結果加入df最後一列
}
colnames(df)<-c(                  # 賦予df 新的欄位名稱
  '期數','利息攤還','付款金額','本金償還','尚未償還本金')
library(writexl)                # 載入函式庫
write_xlsx(
  x=round(df,2),                # data.frame 物件取小數2位資料
  path = './data/out2.xlsx',    # 檔案名及位置
  col_names = TRUE,             # 含欄位名稱
  format_headers = TRUE)        # 欄位名稱粗體 

############# end of 6_12.R##########