# 主題
AI時代的管理數學：使用R語言實作
# 目錄: 
/R 程式

/data 實例資料檔以及程式輸出之Excel檔案，請參閱書籍實例說明

# 介紹
第1章  截彎取直：線性函數與線性方程組

       第1節  直線的交點(Intersection of Straight Line)
       
          [實例一]市場均衡下求均衡數量與價格
          
          [實例二]生產排程(production scheduling)
          
          [實例三]求線性方程組的解
  
       第2節  最小平方法(The Method of Least Squares)
  
          [實例四]健康照護費用迴歸線與預測。
  
第2章  資料呈現的語言：矩陣

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
          
第3章  Google搜尋是如何運作：向量空間與線性轉換
       
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
          
       第4節  特徵值與特徵向量(Eigenvalues and eigenvectors)
       
          [實例十八]求下列矩陣A的特徵值
          
          [實例十九]求下列矩陣A的特徵值與特徵向量
          
          [實例二十]設已知一線性轉換矩陣為一對稱矩陣1. 特徵值及其特徵向量 2. 此矩陣可否對角化

          [實例二十一]已知某種兔子成長增殖的模型下1. 目前年齡數量分布一年後將如何 2.其各年齡層的分布比例多久後將穩定

          [實例二十二]網頁排序演算法(PageRank algorithm)
          
第4章  資源有限條件的極值問題：極佳化方法
      
       第1節  微分及其應用
       
          [實例一]總獲利 P 元與每週產量x(以1,000公斤為單位)的關係為 P =  250x - 5x^2 試求獲利對於產量的變化率為何 ?
          
          [實例二]一傢俱製造商已確知生產桌子的邊際成本經常是增加的，公司決定在邊際成本達 110元時，就停止桌子的生產。假設桌子的成本函數為 C(x) = 0.01x^2 + 80x + 100 該公司在生產多少桌子後，會停止桌子的生產?

          [實例三]一位農夫想要用總長為2400英尺籬笆，沿著一直線的河岸圍出，一塊矩形區域，而且靠河那邊不需要籬笆。試問如何才能維持最大的面積?
          
          [實例四]某公司的資產A (以百萬元計) 隨著時間t (以年計) 而增加。假設其關係為A(t) = 5t^2+100，0≤t≤5 (1).試問最後三年資產的平均成長率為若干 ? (2).在t = 2(第二年底)時，資產的成長率為若干? 又其相對於A的成長百分比為若干?
          
       第2節  偏微分及全微分
       
          [實例五]假設銷售型號A10的藍牙耳機x個，以及型號A20的藍牙耳機y個的收益為 R(x , y) = 100x +150y -0.03x^2 -0.02y^2 元 求當銷售型號A10 的藍牙耳機 50個，以及型號A20 的藍牙耳機40 個時，收益對型號A10之銷售個數的變化率
          
          [實例六]設某大汽車廠生產小客車與卡車的成本函數為 C =0.12x^2+0.04 y^2 +0.04xy +320 x + 80y +30 小客車與卡車的邊際成本

       第3節  函數的極值
          
          [實例七]設f (x) = x^3 + 3x^2 – 9x – 9，試求f 的相對極大值與相對極小值
          
          [實例八]某廠商所製造產品的需求函數為q = 90 – 2p其中q為產量，p為產品的單位價格 。若廠商的成本函數為C=q^3- 39.5 q^(2 )+ 120q+ 125，試問廠商應生產多少單位可使利潤最大以及最小?

          [實例九]一儲存盒製造商計畫生產一批頂部開口的盒子，底部為正方形。每個盒子的體積為100立方呎，底部材料的成本為每平方呎8元，側邊的材料則為每平方呎5元，求使材料成本最低的盒子尺寸
          
          [實例十]某水族箱製造廠要製造可盛64立方呎水的矩形大水族箱，若底座材料的成本為每立方呎20元，側邊材料的成本為每立方呎10元。求使材料成本最低的尺寸
          
       第4節  拉氏乘數( Lagrange multiplier)   
          
          [實例十一]若利用一根20公尺長的繩子圍成一長方形。試問長與寬應各為多少時，長方形的面積會最大?
          
          [實例十二]假設某公司生產的產品，其銷售額 s與電視廣告時間(分鐘) t及報紙廣告篇幅(行)n的關係為s = f (t,n ) = t^3n若公司廣告預算為400,000元，電視廣告每分鐘費用為40,000元，而報紙廣告每行為400元，試問該公司應如何選擇廣告媒體以使銷售量最大?
          
          [實例十三]中美不銹鋼廠每年產能f 為直接人力，和機器設備資金y的函數，且f (x, y) = 5√(x∙y)，假設直接人工每人每日成本為4單位，工廠每年分配於直接人工薪水及機器設備資金總金額，固定為60,000單位，一年以300工作天計，求應使用多少直接人數及設備資金，可使產能最大，且λ意義為何?
          
          [實例十四]某農場種植三種作物，當種植1000單位的x、y、z作物，其利潤可以p(x、y、z)= 4x + 8y + 6z 模型來表示。其產量限制式為 x^2 + 4y^2 + 2z^2  ≤800，試求出該企業最大利潤。並以 x^2 + 4y^2 + 2z^2  ≤801限制式，重新計算該問題。以計算結果，詮釋λ的意義
          
       第5節  極佳化方法的應用
       
          [實例十五]某百貨公司專櫃經銷之防刮手機保護膜，每年的需求量估計為10,000個，若每次訂購成本為90元，防刮手機保護膜的單價為200元，且年倉儲成本約為商品價值的10%，試問經濟訂購量為若干? 訂單間隔時間(time between order, TBO) ?
          
          [實例十六]波音公司生產的噴射客機的定價
          
第5章  COVID-19陽性、偽陽性議題：機率與統計

       第1節  敘述性統計

          [實例一]一家大型百貨公司收集旗下售貨員的銷售業績，下面是20位售貨員的銷售業績: 9,6,12,10,13,15,16,14,14,16,17,16,24,21,22,18,19,18,20,17，找出此一資料集的第50個百分位數，以及第90個百分位數
          
          [實例二]旗下售貨員的銷售業績，求下四分位數、中四分位數、上四分位數及四分位數間距(IQR)
          
          [實例三]旗下售貨員的銷售業績，求觀察值的平均數及眾數
          
          [實例四]旗下售貨員的銷售業績，求觀察值的全距、變異數及標準差
          
          [實例五]一家電的老闆紀錄大減價最後一天，有184位來店顧客的全部消費，資料被分割成如下數群組: 0美元到小於100美元，100美元到小於200美元，依此類推，最後是500美元到小於600美元。各組及其頻率顯示如表 5 3，求其直方圖，分別以絕對頻率(absolute frequency)與相對頻率(relative frequency)表示
          
          [實例六]一家披薩店每週的銷售業績相對頻率，如下表 5 4，以千元計，分別繪製頻率多邊形、肩形圖
          
          [實例七]餐廳的經理關心顧客抱怨
                  
       第2節  機率
       
          [實例八]班上30位同學中有2/3 是男生，80% 男生是手機重度使用者(heavy user)，40% 女生是手機重度使用者。則隨機挑選一人，其為手機重度使用者的機率為何?
          
          [實例九]假設運動競賽的興奮劑(doping)篩選測試宣稱有「95%正確」，表示95%的興奮劑服用者和95%的未服用者分類正確。假設每50名運動員中有1名確實在任何時候都服用興奮劑。如果某運動員的檢測呈陽性，那麼他們真正服用興奮劑的機率是多少?
          
          [實例十]醫生在為病人診斷的時候，基本上，就是觀察病人的一些症狀(symptoms)，然後試圖找出所患的疾病(disease)。但是經常不同的疾病，卻具有部分相同的症狀，因此醫生必須在幾個可能的疾病中，決定一個最符合症狀的疾病。假設中醫臨床上發現d_1、d_2、d_3等3種疾病(disease)，可能產生下面一種或兩種以上症狀(symptoms):多汗、噁心欲嘔、眩暈、耳鳴。若10,000個病歷中患有3種疾病的患者，如下表 5 6。求一個罹患有三高疾病、梅尼爾氏症、過敏性鼻炎的患者，如果有S_1 、S_2 、S_3 、S_4等任意一個或兩個以上的症狀時，患有d_1、d_2、d_3等3種疾病(disease)的機率各是多少?
       
          [實例十一]假定調查的100封郵件，其中70封是垃圾郵件，剩下的30封是一般郵件，接著，再此70封垃圾郵件之中，有40封含有「グラビア」字樣。另外，30封的平常郵件中，10封含有「グラビア」字樣
          
       第3節  隨機變數、常態分配及抽樣分配
       
          [實例十二]一個機器人投籃10次，每次進球機率固定為0.4，而且任何兩次投籃的結果是統計獨立，求進球數的平均數與變異數
          
          [實例十三]史上第一個卜瓦松(Poisson)分配應用:著名的普魯士軍隊(Prussian Army)遭馬踢致死的例子
          
          [實例十四]電子公路收費站(Electronic Turnpike Fare)。假設車內電子儀器對收費站訊號的反應時間，是一個平均160微秒(microseconds)，標準差30微秒的常態隨機變數。該儀器對訉號的反應時間介於100至180微秒的機率是多少？
          
          [實例十五]電腦微處理器半導體內的雜質(impurities)濃度，是一個平均數127 ppm( parts per million)，標準差22的常態隨機變數。能被客戶接受的半導體，其雜質濃度必須低於150ppm。請問有多少比率的半導體可以被接受?
          
          [實例十六]Mercury公司製造出一個2.4公升V6快艇引擎。該公司的工程師相信這種引擎的平均馬力是220匹，而標準差是15匹馬力。一位有意購買的買主，想抽樣100個引擎，則樣本平均X ̅ 比217匹馬力小的機率是多少?
       
第6章  時間的價值：單利、複利的年金;分期償還及償債基金       
       
       第1節  單利、複利
       
          [實例一]投資2000 元於10 年期的信託基金，已知該基金以單利計算，且年利率 6%。試問 10 年結束時的本利和若干？
          
          [實例二]Jane花了9850元購買一張為期26週，到期值 10,000元的美國國庫債券(T-Bill)，試問其投資回收率若干？
          
          [實例三]依下面的情況，試問1000元的本金存放3年後的本利和若干？已知年利率 8%，且(a)一年複利一次 (compounded annually)；(b)半年複利一次(compounded semiannually)；(c)一季複利一次(compounded quarterly)；(d)一個月複利一次(compounded monthly)及 (e)一天複利一次(compounded daily)
          
          [實例四]依下面的情況，試問1000元的本金存放 3 年後的本利和若干？已知年利率8%，且(a)一天複利一次(假設一年是365天)與(b)連續複利
                          
       第2節  單利、複利的進階應用
       
          [實例五]100 元的本金，試分別以2%, 4%,…18%單利年利率，求出20年期本利和曲線
          
          [實例六]100 元的本金，試分別以5%，10%，15%複利年利率，求出20年期表格，以及本利和曲線
          
          [實例七]神奇的一美分幣(The Magic Penny) 兩個選項讓你選，你會選擇那一個？1. 現在馬上獲得300萬美元現金。2.	現在獲得一美分，但是價值每天翻倍，連續翻31天
          
       第3節  年金 (Annuity)
       
          [實例八]一12 月期的普通年金，每期於月底付款100元，年利率12%，每月複利一次，試問年金的終值？
          
          [實例九]大學學費儲蓄計畫(Saving for an university Education)
          
          [實例十]一普通年金共24期，每月付款100元，年利率3%，每月複利一次，試問其現值？
          
          [實例十一]房貸付款(Home Mortgage Payment)  李先生向銀行貸款12萬元購買房子。銀行收取的利息以年利率5.4%計算，於每月月底計息，且李先生同意以30年期的分期付款還清銀行貸款。試問李先生每月月底應償還多少錢？
          
          [實例十二]分期償還表(Amortization schedule)
          
          [實例十三]五金行的經營者Alan設立了一個償債基金，打算2年後添購一部卡車，卡車預定的購買價為3萬元。已知投資的基金帳戶可有10% 的年利率，每季複利一次。若以定額的方式存款，問Alan (a)每季應存入多少元？(b)列出償債基金的報表
          
第7章  最少的資源與滿足最佳效益：線性規劃

       第1節  極大化問題(maximization problem)
          
          [實例一]生產問題，解利潤極大化問題
          
          [實例二]生產創造收益問題
          
          [實例三]生產排程( production planning) 
          
          [實例四]單形法的三度空間幾何解釋
          
          [實例五]無界限解的線性規劃問題          
          
       第2節  極小化問題(minimization problem)
          
          [實例六]營養問題(A Nutrition problem)
          
          [實例七]倉庫問題 (warehouse problem)，即求解成本極小問題
          
          [實例八]郵局在一周的不同天，要求不同數量的全職員工，每天所需的全職員工數量，如表8-6。工會規定，每位全職員工必須連續工作五天，然後有兩天休息。例如，週一至週五工作的員工必須在周六和周日休息。郵局希望只使用全職員工來滿足其日常需求，制定 LP ，郵局可以最大限度地減少必須雇用的全職員工的數量
          
          [實例九]進一步探討維他命議題: 有7種維他命劑的營養問題
       
第8章  AI中的隨機與穩態過程：馬可夫鏈
          [實例一]都市與郊區間的人口流動(Urban-Suburban population Flow)
          
          [實例二]延續[實例一]，試問兩年後居住於都市的人口比例有多少？三年後呢？
          
          [實例三]十年後呢？
          
          [實例四]計程車的移動區域(Taxi movement between zones)
          
          [實例五]承[實例四]計程車的移動區域。在[實例四]的例題中，我們找出描述計程車移動區域的遞移矩陣T，並知T為正規隨機矩陣。求計程車長時間之後在三個區域的分布情形
          
          [實例六]女性的教育狀況(Educational Status of Women)

第9章  AI 的前沿應用：預測

       第1節  定性的預測方法
       
       第2節  時間序列預測方法
       
          [實例一]假設Dr. WU面膜，過去一年市場需求資料如下表 9 1。使用四期移動平均來計算第5期預測
          
          [實例二]假設Dr.WU 面膜，過去一年市場需求資料如表 9 1。使用四期移動加權移動平均來計算第5期預測。權數分別為0.4、0.3、0.2、0.1，指派給最近、第2接近、第3和第4接近的期別
          
          [實例三]假設Dr.WU 面膜，過去一年市場需求資料如表 9 1。使用指數平滑法計算第3期的預測。假設第2期的預測值為1,600，利用平滑常數 α= 0.3
       
          [實例四]假設Dr.WU 面膜，使用趨勢調整指數平滑法來計算第4期的預測值。假設針對該序列第2期之平滑平均值為1,600，平滑趨勢為300，使用α=0.3 和 β=0.4
          
          [實例五]假設Dr.WU 公司所生產的面膜，其需求如表 9 1所示:1. 趨勢線為何?  2. 第13期的預測為何?          
          
       第3節  關聯性預測 - 簡單廻歸模式
       
          [實例六]過去六個月之銷售與數位廣告金額如下，試決定銷售與數位廣告的線性關係
       
       第4節  關聯性預測 - 複廻歸模式
       
          [實例七]某一市場研究人員了解兩種推廣費用 - 數位媒體費用(X_1)和平面媒體費用(X_2)- 對食品銷售額(Y)的影響，經選擇十六個地點進行實驗，這十六個地點的市場潛量和目標市場的代表性是相似的。下表列舉各地點有關推廣費用和銷售的資料
          
          [實例八](存活時間)某心臟科醫師研究心手術後病患，其存活時間y(單位 : 天)與病人手術前的身體狀況，如血塊分數(x_1)、體能指標(x_2)、肝功能分數(x_3)、酸素檢定分數(x_4)、體重(x_5)的關係，收集50位開刀病人資料
       
       第5節  判定係數與相關係數
       
       第6節  顯著性檢定
       
       第7節  預測準確性
       
          [實例九]荷史公司12個月期的需求和預測。試計算MAD、MSE、MAPE 和追踪信號。假設該追蹤訊號的管制界限為±3，我們對其預測品質有何結論?
       
