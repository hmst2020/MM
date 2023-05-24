# 主題
資料科學的良器: R語言 在管理數學領域的應用
# 目錄: 
/R 程式

/data 實例資料檔以及程式輸出之Excel檔案，請參閱書籍實例說明

# 介紹
第1章  線性函數與線性方程組:直線與線性函數

       第1節  直線的交點(Intersection of Straight Line)
       
          [實例一]市場均衡下求均衡數量與價格
          
          [實例二]生產排程(production scheduling)
          
          [實例三]求線性方程組的解
  
       第2節  最小平方法(The Method of Least Squares)
  
          [實例四]健康照護費用迴歸線與預測。
  
第2章  矩陣

       第1節  矩陣定義與基本運算

          [實例一]Acrosonic公司五月時的藍芽喇叭生產資料之表示及彙總
          
       第2節  矩陣應用於密碼學(Cryptography)
       
          [實例二]求A的反矩陣(inverse of the matrix)
          
          [實例三]文字加、解密
          
       第3節  矩陣應用於經濟學: Leontief模式 
       
          [實例四]使用封閉型Leontief模式決定相關收入
          
          [實例五]另一種應用為使用開放型Leontief模式，滿足未來的生產量
          
          [實例六]消費者需求滿足及生產投入
          
       第4節  矩陣應用於最小平方法
       
          [實例七]以矩陣求解美國健康照護費用的線性函數
          
第3章  向量空間與線性轉換
       
       第1節  向量與向量空間
       
       第2節  線性獨立與基底
       
          [實例一]在 2 × 2 矩陣的向量空間R^2中的線性組合
          
          [實例二]決定向量(4,-1)是否為向量(2,3)及 (3,1)的線性組合 ?
          
          [實例三]描述以下R^2 子集合的展成(span)
          
          [實例四]X_1=(1,1,1) ，X_2=(1,2,3)，X_3=(0,1,0)說明其是否為線性獨立?
          
          [實例五]證明下列集合S_1= {(2,3),(5,8),(1,2)}為線性相依。
          
          [實例六]非標準基底B={(1,0),(1,2)}下，一座標為(3,2)的向量x，在R2標準基底下其座標向量為何 ?
          
          [實例七]在R^3空間裡，若要檢查X_1= (1,1,1) ，X_2= (1,2,3)， X_3= (0,1,0)是否造成R^3  ? 
          
          [實例八]首先驗證向量的柯西不等式
          
          [實例九]在R^5中u= (-1,2,0,1,3)，v = (2,1,3,1,1)，驗證 Cauchy- Schwarz不等式成立，並求u與v 夾角的餘弦。
          
          [實例十]向量u = (3, 2, − 1, 4)，v = (1, − 1, 1, 0)是否正交 ?
          
          [實例十一]三個 R^3的向量分別為u=(1,0,1)，v=(1,1,0)及w=   (-1,1,1)，子空間S1=span {u,v}，S2=span{w}，S1及S2是否互為正交子空間 ?
          
          [實例十二]為某藥物隨著時間的經過，在人體中殘留的濃度變化，1.使用最小平方法找出其迴歸線，2. 估計在第5小時時的濃度。
 
       第3節  線性轉換(Linear transformation)
       
          [實例十三]定義對一個向量v=(v1,v2)的線性轉換T(v1,v2)=(v1-v2,v1+2v2) 是二維空間T: R^2→R^2的線性轉換，則1.求v=(-1,2) 的像w ? 2.求v=(0,0) 的像w ? 3.當w=(-1,11)時，其原像v ?

          [實例十四]T: R^3→R^3的線性轉換T(x1,x2,x3)=(2x1+x2-x3, -x1+3x2-2x3, 3x2+4x3) 1. 求T(1,0,0)、T(0,1,0)、T(0,0,1)的像 ? 2.以轉換矩陣表示此線性轉換 ?

          [實例十五]T: R^3→R^2的線性轉換T(x,y,z)=(x-2y, 2x+y)，試找出標準線性轉換矩陣 ?
          
          [實例十六]T: R^2→R^2的線性轉換T(x1, x2)=( x1+ x2, 2 x1-x2)，定義域的基底B={b1,b2}={(1,2),(-1,1)}，對應域的基底C={c1,c2}={(1,0),(1,1)} 試找出轉移矩陣(transition matrix) 
          
          [實例十七]T: R^2→R^2的線性轉換T(x1, x2)=( 11x1+x2, -10x1+x2)，基底C={c1,c2}={(-1,2),(2,-2)}，基底B={b1,b2}={(-3,2),(4,-2)}，設有一向量對於C基底座標為(-3,1)，對於下列B 基底的轉換矩陣A
          
          
