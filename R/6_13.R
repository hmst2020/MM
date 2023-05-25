S <- 30000        #  2年後終值
i<- 0.10/4        #  每期利率
n <- 2*4          #  期數
(R<- S/sum((1+i)^(0:(n-1))))  # 每季存款金額

c0<-1;c1<-NA;c2<-0;c3<-R;c4<-R    # 存款表中自左至右各欄初始值
df<-data.frame()                  # 表中各列與行資料物件初始值
c1<-R                             # 每季存款金額
for (j in 1:n){                   # 依每季迴圈共8次
  df<-rbind(df,c(c0,c1,c2,c3,c4)) # 計算結果加入df最後一列
  c0<-c0+1                        # 期數
  c2<-c4*i                        # 利息收入
  c3<-c1+c2                       # 本期基金增額
  c4<-c4+c3                       # 基金累計總額
}
colnames(df)<-c(                  # 賦予df 新的欄位名稱
  '期數','存款金額','利息收入','本期基金增額','基金累計總額')
library(writexl)                # 載入函式庫
write_xlsx(
  x=round(df,2),                # data.frame 物件取小數2位資料
  path = './data/out3.xlsx',    # 檔案名及位置
  col_names = TRUE,             # 含欄位名稱
  format_headers = TRUE)        # 欄位名稱粗體

############# end of 6_13.R##########