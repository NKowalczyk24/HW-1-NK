# Read Su_raw_matrix.txt into su variable
su <- read.delim("Su_raw_matrix.txt")

# Su is now a data frame

#Using mean function to find mean of Liver_2.CEL column and print value
mean_su_liver2 <- mean(su$Liver_2.CEL)

print(mean_su_liver2)
# [1] 241.8246

#Using sd function to find standard deviation of Liver_2.CEL column and print value
sd_su_liver2 <- sd(su$Liver_2.CEL)

print(sd_su_liver2)
# [1] 1133.352

#Use colMeans to find the averages of the columns and print value 
column_averages <- colMeans(su)

print(column_averages)
# Brain_1.CEL       Brain_2.CEL Fetal_brain_1.CEL Fetal_brain_2.CEL Fetal_liver_1.CEL 
#  204.9763          315.0924          198.3439      267.6551          209.8722 
# Fetal_liver_2.CEL       Liver_1.CEL       Liver_2.CEL 
#   399.1482               160.8558          241.8246 

#Use colSums to find the total values of the columns and print value
column_sums <- colSums(su)

print(column_sums)
# Brain_1.CEL       Brain_2.CEL Fetal_brain_1.CEL Fetal_brain_2.CEL Fetal_liver_1.CEL 
#   2588031           3978357      2504290           3379413           2649846 
# Fetal_liver_2.CEL       Liver_1.CEL       Liver_2.CEL 
#    5039645                2030966           3053278 

#Assign values to mean and sigma and use rnorm to generate 10000 numbers
mean_val = 0

sigma_val = 0.2

n = 10000

random_numbers = rnorm(n, mean = mean_val, sd = sigma_val)

#Display a histogram for the above values
hist(random_numbers, main = paste("Histogram of rnorm with mean =", mean_val, "and sigma =", sigma_val), 
     xlab = "Value", ylab = "Frequency", col = "blue", breaks = 50, xlim = c(-5, 5))

#Assign values to mean and sigma and use rnorm to generate 10000 numbers
mean_val = 0

sigma_val = 0.5

n = 10000

random_numbers = rnorm(n, mean = mean_val, sd = sigma_val)

#Display a histogram for the above values
hist(random_numbers, main = paste("Histogram of rnorm with mean =", mean_val, "and sigma =", sigma_val), 
     xlab = "Value", ylab = "Frequency", col = "blue", breaks = 50, xlim = c(-5, 5))

#Use "dat" dataframe and ggplot2 to observe plot functions
library(ggplot2)

dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                  rating = c(rnorm(200),rnorm(200, mean=.8)))

# Overlaid histograms
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

# Interleaved histograms
ggplot(dat, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, position="dodge")

# Density plots
ggplot(dat, aes(x=rating, colour=cond)) + geom_density()

# Density plots with semitransparent fill
ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)

# Read diabetes_train.csv into diabetes variable
diabetes <- read.csv("diabetes_train.csv")

library(ggplot2)

# Overlaid histograms
ggplot(diabetes, aes(x=mass, fill=class)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

# Interleaved histograms
ggplot(diabetes, aes(x=mass, fill=class)) + geom_histogram(binwidth=.5, position="dodge")

# Density plots
ggplot(diabetes, aes(x=mass, colour=class)) + geom_density()

# Density plots with semitransparent fill
ggplot(diabetes, aes(x=mass, fill=class)) + geom_density(alpha=.3)

# Read titanic.csv into passengers variable
passengers <- read.csv("titanic.csv")

library(tidyr)

# The following will drop any rows missing values and provide a statistical summary
passengers %>% drop_na() %>% summary() 

#Results
#       X          PassengerId       Survived      Pclass 
# Min.   :  0.0   Min.   :  1.0   Min.   :0.0000   ?: 37  
# 1st Qu.:221.2   1st Qu.:222.2   1st Qu.:0.0000   1:174  
# Median :444.0   Median :445.0   Median :0.0000   2:162  
# Mean   :447.6   Mean   :448.6   Mean   :0.4062   3:341  
# 3rd Qu.:676.8   3rd Qu.:677.8   3rd Qu.:1.0000          
# Max.   :890.0   Max.   :891.0   Max.   :1.0000          

#                                   Name         Sex           Age            SibSp       
# Abbing, Mr. Anthony                  :  1   female:261   Min.   : 0.42   Min.   :0.0000  
# Abbott, Mr. Rossmore Edward          :  1   male  :453   1st Qu.:20.12   1st Qu.:0.0000  
# Abbott, Mrs. Stanton (Rosa Hunt)     :  1                Median :28.00   Median :0.0000  
# Abelson, Mr. Samuel                  :  1                Mean   :29.70   Mean   :0.5126  
# Abelson, Mrs. Samuel (Hannah Wizosky):  1                3rd Qu.:38.00   3rd Qu.:1.0000  
# Adahl, Mr. Mauritz Nils Martin       :  1                Max.   :80.00   Max.   :5.0000  
# (Other)                              :708                                                
#     Parch                 Ticket         Fare                Cabin     Embarked
# Min.   :0.0000   347082      :  7   Min.   :  0.00              :529    :  2   
# 1st Qu.:0.0000   3101295     :  6   1st Qu.:  8.05   B96 B98    :  4   C:130   
# Median :0.0000   347088      :  6   Median : 15.74   C23 C25 C27:  4   Q: 28   
# Mean   :0.4314   CA 2144     :  6   Mean   : 34.69   G6         :  4   S:554   
# 3rd Qu.:1.0000   382652      :  5   3rd Qu.: 33.38   C22 C26    :  3           
# Max.   :6.0000   S.O.C. 14879:  5   Max.   :512.33   D          :  3           
#                  (Other)     :679                    (Other)    :167

library(tidyr)
library(dplyr)

# The following will only produce data where the value of 'Sex' is male
passengers %>% filter(Sex == "male")

#Results
#X PassengerId Survived Pclass                               Name  Sex   Age SibSp Parch
#1    0           1        0      3            Braund, Mr. Owen Harris male 22.00     1     
#2    4           5        0      3           Allen, Mr. William Henry male 35.00     0     
#3    5           6        0      3                   Moran, Mr. James male    NA     0     
#4    6           7        0      1            McCarthy, Mr. Timothy J male 54.00     0     
#5    7           8        0      3     Palsson, Master. Gosta Leonard male  2.00     3     
#6   12          13        0      3     Saundercock, Mr. William Henry male 20.00     0     
#7   13          14        0      3        Andersson, Mr. Anders Johan male 39.00     1     
#8   16          17        0      3               Rice, Master. Eugene male  2.00     4     
#9   17          18        1      2       Williams, Mr. Charles Eugene male    NA     0     
#10  20          21        0      2               Fynney, Mr. Joseph J male 35.00     0     
#11  21          22        1      2              Beesley, Mr. Lawrence male 34.00     0     
#12  23          24        1      1       Sloper, Mr. William Thompson male 28.00     0     
#13  26          27        0      3            Emir, Mr. Farred Chehab male    NA     0     
#14  27          28        0      1     Fortune, Mr. Charles Alexander male 19.00     3     
#15  29          30        0      3                Todoroff, Mr. Lalio male    NA     0     
#16  30          31        0      1           Uruchurtu, Don. Manuel E male 40.00     0     
#17  33          34        0      2              Wheadon, Mr. Edward H male 66.00     0     
#18  34          35        0      1            Meyer, Mr. Edgar Joseph male 28.00     1     
#19  35          36        0      1     Holverson, Mr. Alexander Oskar male 42.00     1     
#20  36          37        1      3                   Mamee, Mr. Hanna male    NA     0     
#21  37          38        0      ?           Cann, Mr. Ernest Charles male 21.00     0     
#22  42          43        0      3                Kraeff, Mr. Theodor male    NA     0     
#23  45          46        0      3           Rogers, Mr. William John male    NA     0     
#24  46          47        0      3                  Lennon, Mr. Denis male    NA     1     
#25  48          49        0      3                Samaan, Mr. Youssef male    NA     2     
#26  50          51        0      3         Panula, Master. Juha Niilo male  7.00     4     
#27  51          52        0      3       Nosworthy, Mr. Richard Cater male 21.00     0     
#28  54          55        0      1     Ostby, Mr. Engelhart Cornelius male 65.00     0     
#29  55          56        1      1                  Woolner, Mr. Hugh male    NA     0     
#30  57          58        0      3                Novel, Mr. Mansouer male 28.50     0     
#31  59          60        0      3 Goodwin, Master. William Frederick male 11.00     5     
#32  60          61        0      3              Sirayanian, Mr. Orsen male 22.00     0     
#33  62          63        0      1        Harris, Mr. Henry Birkhardt male 45.00     1     
#34  63          64        0      3              Skoog, Master. Harald male  4.00     3     
#35  64          65        0      1              Stewart, Mr. Albert A male    NA     0     
#36  65          66        1      3           Moubarek, Master. Gerios male    NA     1     
#37  67          68        0      3           Crease, Mr. Ernest James male 19.00     0     
#38  69          70        0      3                  Kink, Mr. Vincenz male 26.00     2     
#39  70          71        0      2         Jenkin, Mr. Stephen Curnow male 32.00     0     
#40  72          73        0      2               Hood, Mr. Ambrose Jr male 21.00     0     
#41  73          74        0      3        Chronopoulos, Mr. Apostolos male 26.00     1     
#42  74          75        1      3                      Bing, Mr. Lee male 32.00     0     
#43  75          76        0      3            Moen, Mr. Sigurd Hansen male 25.00     0     
#44  76          77        0      3                  Staneff, Mr. Ivan male    NA     0     
#45  77          78        0      3           Moutal, Mr. Rahamin Haim male    NA     0     
#46  78          79        1      2      Caldwell, Master. Alden Gates male  0.83     0     
#47  80          81        0      ?               Waelens, Mr. Achille male 22.00     0     
#48  81          82        1      3        Sheerlinck, Mr. Jan Baptist male 29.00     0     
#49  83          84        0      1            Carrau, Mr. Francisco M male 28.00     0     
#50  86          87        0      3             Ford, Mr. William Neal male 16.00     1     
#51  87          88        0      3      Slocovski, Mr. Selman Francis male    NA     0     
#52  89          90        0      3             Celotti, Mr. Francesco male 24.00     0     
#53  90          91        0      3               Christmann, Mr. Emil male 29.00     0     
#54  91          92        0      3         Andreasson, Mr. Paul Edvin male 20.00     0     
#55  92          93        0      1        Chaffee, Mr. Herbert Fuller male 46.00     1     
#56  93          94        0      3            Dean, Mr. Bertram Frank male 26.00     1     
#57  94          95        0      3                  Coxon, Mr. Daniel male 59.00     0     
#58  95          96        0      3        Shorney, Mr. Charles Joseph male    NA     0     
#59  96          97        0      1          Goldschmidt, Mr. George B male 71.00     0     
#60  97          98        1      1    Greenfield, Mr. William Bertram male 23.00     0     
#61  99         100        0      2                  Kantor, Mr. Sinai male 34.00     1     
#62 101         102        0      3   Petroff, Mr. Pastcho ("Pentcho") male    NA     0     
#63 102         103        0      1          White, Mr. Richard Frasar male 21.00     0     
#64 103         104        0      3         Johansson, Mr. Gustaf Joel male 33.00     0     
#65 104         105        0      3     Gustafsson, Mr. Anders Vilhelm male 37.00     2     
#66 105         106        0      3              Mionoff, Mr. Stoytcho male 28.00     0     
#67 107         108        1      3             Moss, Mr. Albert Johan male    NA     0     
#68 108         109        0      3                    Rekic, Mr. Tido male 38.00     0     
#69 110         111        0      1     Porter, Mr. Walter Chamberlain male 47.00     0     
#70 112         113        0      3             Barton, Mr. David John male 22.00     0     
#71 115         116        0      3              Pekoniemi, Mr. Edvard male 21.00     0     
#72 116         117        0      3               Connors, Mr. Patrick male 70.50     0     
#73 117         118        0      2    Turpin, Mr. William John Robert male 29.00     1     
#74 118         119        0      1           Baxter, Mr. Quigg Edmond male 24.00     0     
#75 120         121        0      2        Hickman, Mr. Stanley George male 21.00     2     
#76 121         122        0      3         Moore, Mr. Leonard Charles male    NA     0     
#              Ticket     Fare       Cabin Embarked
#1          A/5 21171   7.2500                    S
#2             373450   8.0500                    S
#3             330877   8.4583                    Q
#4              17463  51.8625         E46        S
#5             349909  21.0750                    S
#6          A/5. 2151   8.0500                    S
#7             347082  31.2750                    S
#8             382652  29.1250                    Q
#9             244373  13.0000                    S
#10            239865  26.0000                    S
#11            248698  13.0000         D56        S
#12            113788  35.5000          A6        S
#13              2631   7.2250                    C
#14             19950 263.0000 C23 C25 C27        S
#15            349216   7.8958                    S
#16          PC 17601  27.7208                    C
#17        C.A. 24579  10.5000                    S
#18          PC 17604  82.1708                    C
#19            113789  52.0000                    S
#20              2677   7.2292                    C
#21        A./5. 2152   8.0500                    S
#22            349253   7.8958                    C
#23   S.C./A.4. 23567   8.0500                    S
#24            370371  15.5000                    Q
#25              2662  21.6792                    C
#26           3101295  39.6875                    S
#27        A/4. 39886   7.8000                    S
#28            113509  61.9792         B30        C
#29             19947  35.5000         C52        S
#30              2697   7.2292                    C
#31           CA 2144  46.9000                    S
#32              2669   7.2292                    C
#33             36973  83.4750         C83        S
#34            347088  27.9000                    S
#35          PC 17605  27.7208                    C
#36              2661  15.2458                    C
#37         S.P. 3464   8.1583                    S
#38            315151   8.6625                    S
#39        C.A. 33111  10.5000                    S
#40      S.O.C. 14879  73.5000                    S
#41              2680  14.4542                    C
#42              1601  56.4958                    S
#43            348123   7.6500       F G73        S
#44            349208   7.8958                    S
#45            374746   8.0500                    S
#46            248738  29.0000                    S
#47            345767   9.0000                    S
#48            345779   9.5000                    S
#49            113059  47.1000                    S
#50        W./C. 6608  34.3750                    S
#51   SOTON/OQ 392086   8.0500                    S
#52            343275   8.0500                    S
#53            343276   8.0500                    S
#54            347466   7.8542                    S
#55       W.E.P. 5734  61.1750         E31        S
#56         C.A. 2315  20.5750                    S
#57            364500   7.2500                    S
#58            374910   8.0500                    S
#59          PC 17754  34.6542          A5        C
#60          PC 17759  63.3583     D10 D12        C
#61            244367  26.0000                    S
#62            349215   7.8958                    S
#63             35281  77.2875         D26        S
#64              7540   8.6542                    S
#65           3101276   7.9250                    S
#66            349207   7.8958                    S
#67            312991   7.7750                    S
#68            349249   7.8958                    S
#69            110465  52.0000        C110        S
#70            324669   8.0500                    S
#71 STON/O 2. 3101294   7.9250                    S
#72            370369   7.7500                    Q
#73             11668  21.0000                    S
#74          PC 17558 247.5208     B58 B60        C
#75      S.O.C. 14879  73.5000                    S
#76         A4. 54510   8.0500                    S
# [ reached 'max' / getOption("max.print") -- omitted 501 rows ]

library(tidyr)
library(dplyr)

#The following sorts the data frame from highest to lowest 'Fare' value
passengers %>% arrange(desc(Fare))

#Results
#X PassengerId Survived Pclass
#1  258         259        1      1
#2  679         680        1      1
#3  737         738        1      1
#4   27          28        0      1
#5   88          89        1      1
#6  341         342        1      1
#7  438         439        0      1
#8  311         312        1      1
#9  742         743        1      1
#10 118         119        0      1
#11 299         300        1      1
#12 380         381        1      ?
#13 557         558        0      1
#14 700         701        1      1
#15 716         717        1      1
#16 527         528        0      1
#17 377         378        0      1
#18 689         690        1      1
#19 730         731        1      ?
#20 779         780        1      1
#21 318         319        1      1
#22 856         857        1      1
#23 268         269        1      1
#24 332         333        0      1
#25 609         610        1      1
#26 297         298        0      1
#27 305         306        1      1
#28 498         499        0      1
#29 708         709        1      1
#30  31          32        1      1
#31 195         196        1      1
#32 269         270        1      1
#33 325         326        1      1
#34 373         374        0      1
#35 319         320        1      1
#36 337         338        1      1
#37 334         335        1      1
#38 660         661        1      1
#39 390         391        1      ?
#40 435         436        1      1
#41 763         764        1      1
#42 802         803        1      1
#43 215         216        1      1
#44 393         394        1      1
#45 659         660        0      1
#46 306         307        1      1
#47 550         551        1      1
#48 581         582        1      1
#49 698         699        0      1
#50 307         308        1      1
#51 505         506        0      1
#52 537         538        1      1
#53 544         545        0      1
#54 520         521        1      1
#55 820         821        1      1
#56 291         292        1      1
#57 484         485        1      1
#58 224         225        1      1
#59 245         246        0      1
#60 412         413        1      ?
#61 486         487        1      1
#62 453         454        1      1
#63 849         850        1      1
#64 257         258        1      1
#65 504         505        1      1
#66 759         760        1      ?
#67  62          63        0      1
#68 230         231        1      1
#69 310         311        1      1
#70 835         836        1      1
#71 879         880        1      1
#72  34          35        0      1
#73 375         376        1      1
#74 445         446        1      1
#75  61          62        1      ?
#76 829         830        1      1
#                                                                                 Name    Sex
#1                                                                    Ward, Miss. Anna female
#2                                                  Cardeza, Mr. Thomas Drake Martinez   male
#3                                                              Lesurer, Mr. Gustave J   male
#4                                                      Fortune, Mr. Charles Alexander   male
#5                                                          Fortune, Miss. Mabel Helen female
#6                                                      Fortune, Miss. Alice Elizabeth female
#7                                                                   Fortune, Mr. Mark   male
#8                                                          Ryerson, Miss. Emily Borie female
#9                                               Ryerson, Miss. Susan Parker "Suzette" female
#10                                                           Baxter, Mr. Quigg Edmond   male
#11                                    Baxter, Mrs. James (Helene DeLaudeniere Chaput) female
#12                                                              Bidois, Miss. Rosalie female
#13                                                                Robbins, Mr. Victor   male
#14                                  Astor, Mrs. John Jacob (Madeleine Talmadge Force) female
#15                                                      Endres, Miss. Caroline Louise female
#16                                                                 Farthing, Mr. John   male
#17                                                          Widener, Mr. Harry Elkins   male
#18                                                  Madill, Miss. Georgette Alexandra female
#19                                                      Allen, Miss. Elisabeth Walton female
#20                              Robert, Mrs. Edward Scott (Elisabeth Walton McMillan) female
#21                                                           Wick, Miss. Mary Natalie female
#22                                         Wick, Mrs. George Dennick (Mary Hitchcock) female
#23                                      Graham, Mrs. William Thompson (Edith Junkins) female
#24                                                          Graham, Mr. George Edward   male
#25                                                          Shutes, Miss. Elizabeth W female
#26                                                       Allison, Miss. Helen Loraine female
#27                                                     Allison, Master. Hudson Trevor   male
#28                                    Allison, Mrs. Hudson J C (Bessie Waldo Daniels) female
#29                                                               Cleaver, Miss. Alice female
#30                                     Spencer, Mrs. William Augustus (Marie Eugenie) female
#31                                                               Lurette, Miss. Elise female
#32                                                             Bissette, Miss. Amelia female
#33                                                           Young, Miss. Marie Grice female
#34                                                                Ringhini, Mr. Sante   male
#35                           Spedden, Mrs. Frederic Oakley (Margaretta Corning Stone) female
#36                                                    Burns, Miss. Elizabeth Margaret female
#37                                 Frauenthal, Mrs. Henry William (Clara Heinsheimer) female
#38                                                      Frauenthal, Dr. Henry William   male
#39                                                         Carter, Mr. William Ernest   male
#40                                                          Carter, Miss. Lucile Polk female
#41                                          Carter, Mrs. William Ernest (Lucile Polk) female
#42                                                Carter, Master. William Thornton II   male
#43                                                            Newell, Miss. Madeleine female
#44                                                             Newell, Miss. Marjorie female
#45                                                         Newell, Mr. Arthur Webster   male
#46                                                            Fleming, Miss. Margaret female
#47                                                        Thayer, Mr. John Borland Jr   male
#48                               Thayer, Mrs. John Borland (Marian Longstreth Morris) female
#49                                                           Thayer, Mr. John Borland   male
#50 Penasco y Castellana, Mrs. Victor de Satode (Maria Josefa Perez de Soto y Vallejo) female
#51                                         Penasco y Castellana, Mr. Victor de Satode   male
#52                                                                LeRoy, Miss. Bertha female
#53                                                         Douglas, Mr. Walter Donald   male
#54                                                              Perreault, Miss. Anne female
#55                                 Hays, Mrs. Charles Melville (Clara Jennings Gregg) female
#56                                            Bishop, Mrs. Dickinson H (Helen Walton) female
#57                                                            Bishop, Mr. Dickinson H   male
#58                                                       Hoyt, Mr. Frederick Maxfield   male
#59                                                        Minahan, Dr. William Edward   male
#60                                                             Minahan, Miss. Daisy E female
#61                                    Hoyt, Mrs. Frederick Maxfield (Jane Anne Forby) female
#62                                                           Goldenberg, Mr. Samuel L   male
#63                                       Goldenberg, Mrs. Samuel L (Edwiga Grabowska) female
#64                                                               Cherry, Miss. Gladys female
#65                                                              Maioni, Miss. Roberta female
#66                           Rothes, the Countess. of (Lucy Noel Martha Dyer-Edwards) female
#67                                                        Harris, Mr. Henry Birkhardt   male
#68                                       Harris, Mrs. Henry Birkhardt (Irene Wallach) female
#69                                                     Hays, Miss. Margaret Bechstein female
#70                                                        Compton, Miss. Sara Rebecca female
#71                                      Potter, Mrs. Thomas Jr (Lily Alexenia Wilson) female
#72                                                            Meyer, Mr. Edgar Joseph   male
#73                                              Meyer, Mrs. Edgar Joseph (Leila Saks) female
#74                                                          Dodge, Master. Washington   male
#75                                                                Icard, Miss. Amelie female
#76                                          Stone, Mrs. George Nelson (Martha Evelyn) female
#     Age SibSp Parch   Ticket     Fare           Cabin Embarked
#1  35.00     0     0 PC 17755 512.3292                        C
#2  36.00     0     1 PC 17755 512.3292     B51 B53 B55        C
#3  35.00     0     0 PC 17755 512.3292            B101        C
#4  19.00     3     2    19950 263.0000     C23 C25 C27        S
#5  23.00     3     2    19950 263.0000     C23 C25 C27        S
#6  24.00     3     2    19950 263.0000     C23 C25 C27        S
#7  64.00     1     4    19950 263.0000     C23 C25 C27        S
#8  18.00     2     2 PC 17608 262.3750 B57 B59 B63 B66        C
#9  21.00     2     2 PC 17608 262.3750 B57 B59 B63 B66        C
#10 24.00     0     1 PC 17558 247.5208         B58 B60        C
#11 50.00     0     1 PC 17558 247.5208         B58 B60        C
#12 42.00     0     0 PC 17757 227.5250                        C
#13    NA     0     0 PC 17757 227.5250                        C
#14 18.00     1     0 PC 17757 227.5250         C62 C64        C
#15 38.00     0     0 PC 17757 227.5250             C45        C
#16    NA     0     0 PC 17483 221.7792             C95        S
#17 27.00     0     2   113503 211.5000             C82        C
#18 15.00     0     1    24160 211.3375              B5        S
#19 29.00     0     0    24160 211.3375              B5        S
#20 43.00     0     1    24160 211.3375              B3        S
#21 31.00     0     2    36928 164.8667              C7        S
#22 45.00     1     1    36928 164.8667                        S
#23 58.00     0     1 PC 17582 153.4625            C125        S
#24 38.00     0     1 PC 17582 153.4625             C91        S
#25 40.00     0     0 PC 17582 153.4625            C125        S
#26  2.00     1     2   113781 151.5500         C22 C26        S
#27  0.92     1     2   113781 151.5500         C22 C26        S
#28 25.00     1     2   113781 151.5500         C22 C26        S
#29 22.00     0     0   113781 151.5500                        S
#30    NA     1     0 PC 17569 146.5208             B78        C
#31 58.00     0     0 PC 17569 146.5208             B80        C
#32 35.00     0     0 PC 17760 135.6333             C99        S
#33 36.00     0     0 PC 17760 135.6333             C32        C
#34 22.00     0     0 PC 17760 135.6333                        C
#35 40.00     1     1    16966 134.5000             E34        C
#36 41.00     0     0    16966 134.5000             E40        C
#37    NA     1     0 PC 17611 133.6500                        S
#38 50.00     2     0 PC 17611 133.6500                        S
#39 36.00     1     2   113760 120.0000         B96 B98        S
#40 14.00     1     2   113760 120.0000         B96 B98        S
#41 36.00     1     2   113760 120.0000         B96 B98        S
#42 11.00     1     2   113760 120.0000         B96 B98        S
#43 31.00     1     0    35273 113.2750             D36        C
#44 23.00     1     0    35273 113.2750             D36        C
#45 58.00     0     2    35273 113.2750             D48        C
#46    NA     0     0    17421 110.8833                        C
#47 17.00     0     2    17421 110.8833             C70        C
#48 39.00     1     1    17421 110.8833             C68        C
#49 49.00     1     1    17421 110.8833             C68        C
#50 17.00     1     0 PC 17758 108.9000             C65        C
#51 18.00     1     0 PC 17758 108.9000             C65        C
#52 30.00     0     0 PC 17761 106.4250                        C
#53 50.00     1     0 PC 17761 106.4250             C86        C
#54 30.00     0     0    12749  93.5000             B73        S
#55 52.00     1     1    12749  93.5000             B69        S
#56 19.00     1     0    11967  91.0792             B49        C
#57 25.00     1     0    11967  91.0792             B49        C
#58 38.00     1     0    19943  90.0000             C93        S
#59 44.00     2     0    19928  90.0000             C78        Q
#60 33.00     1     0    19928  90.0000             C78        Q
#61 35.00     1     0    19943  90.0000             C93        S
#62 49.00     1     0    17453  89.1042             C92        C
#63    NA     1     0    17453  89.1042             C92        C
#64 30.00     0     0   110152  86.5000             B77        S
#65 16.00     0     0   110152  86.5000             B79        S
#66 33.00     0     0   110152  86.5000             B77        S
#67 45.00     1     0    36973  83.4750             C83        S
#68 35.00     1     0    36973  83.4750             C83        S
#69 24.00     0     0    11767  83.1583             C54        C
#70 39.00     1     1 PC 17756  83.1583             E49        C
#71 56.00     0     1    11767  83.1583             C50        C
#72 28.00     1     0 PC 17604  82.1708                        C
#73    NA     1     0 PC 17604  82.1708                        C
#74  4.00     0     2    33638  81.8583             A34        S
#75 38.00     0     0   113572  80.0000             B28         
#76 62.00     0     0   113572  80.0000             B28         
# [ reached 'max' / getOption("max.print") -- omitted 815 rows ]

library(tidyr)
library(dplyr)

#The following adds a new column to the data frame called "FamSize" which is the sum of "Parch" and "SibSp" columns, calculating the family size for each passenger
passengers %>% mutate(FamSize = Parch + SibSp)

#Results
#X PassengerId Survived Pclass                                                      Name
#1   0           1        0      3                                   Braund, Mr. Owen Harris
#2   1           2        1      1       Cumings, Mrs. John Bradley (Florence Briggs Thayer)
#3   2           3        1      3                                    Heikkinen, Miss. Laina
#4   3           4        1      1              Futrelle, Mrs. Jacques Heath (Lily May Peel)
#5   4           5        0      3                                  Allen, Mr. William Henry
#6   5           6        0      3                                          Moran, Mr. James
#7   6           7        0      1                                   McCarthy, Mr. Timothy J
#8   7           8        0      3                            Palsson, Master. Gosta Leonard
#9   8           9        1      3         Johnson, Mrs. Oscar W (Elisabeth Vilhelmina Berg)
#10  9          10        1      2                       Nasser, Mrs. Nicholas (Adele Achem)
#11 10          11        1      3                           Sandstrom, Miss. Marguerite Rut
#12 11          12        1      1                                  Bonnell, Miss. Elizabeth
#13 12          13        0      3                            Saundercock, Mr. William Henry
#14 13          14        0      3                               Andersson, Mr. Anders Johan
#15 14          15        0      3                      Vestrom, Miss. Hulda Amanda Adolfina
#16 15          16        1      2                          Hewlett, Mrs. (Mary D Kingcome) 
#17 16          17        0      3                                      Rice, Master. Eugene
#18 17          18        1      2                              Williams, Mr. Charles Eugene
#19 18          19        0      3   Vander Planke, Mrs. Julius (Emelia Maria Vandemoortele)
#20 19          20        1      3                                   Masselmani, Mrs. Fatima
#21 20          21        0      2                                      Fynney, Mr. Joseph J
#22 21          22        1      2                                     Beesley, Mr. Lawrence
#23 22          23        1      3                               McGowan, Miss. Anna "Annie"
#24 23          24        1      1                              Sloper, Mr. William Thompson
#25 24          25        0      3                             Palsson, Miss. Torborg Danira
#26 25          26        1      ? Asplund, Mrs. Carl Oscar (Selma Augusta Emilia Johansson)
#27 26          27        0      3                                   Emir, Mr. Farred Chehab
#28 27          28        0      1                            Fortune, Mr. Charles Alexander
#29 28          29        1      3                             O'Dwyer, Miss. Ellen "Nellie"
#30 29          30        0      3                                       Todoroff, Mr. Lalio
#31 30          31        0      1                                  Uruchurtu, Don. Manuel E
#32 31          32        1      1            Spencer, Mrs. William Augustus (Marie Eugenie)
#33 32          33        1      3                                  Glynn, Miss. Mary Agatha
#34 33          34        0      2                                     Wheadon, Mr. Edward H
#35 34          35        0      1                                   Meyer, Mr. Edgar Joseph
#36 35          36        0      1                            Holverson, Mr. Alexander Oskar
#37 36          37        1      3                                          Mamee, Mr. Hanna
#38 37          38        0      ?                                  Cann, Mr. Ernest Charles
#39 38          39        0      3                        Vander Planke, Miss. Augusta Maria
#40 39          40        1      3                               Nicola-Yarred, Miss. Jamila
#41 40          41        0      3            Ahlin, Mrs. Johan (Johanna Persdotter Larsson)
#42 41          42        0      2  Turpin, Mrs. William John Robert (Dorothy Ann Wonnacott)
#43 42          43        0      3                                       Kraeff, Mr. Theodor
#44 43          44        1      2                  Laroche, Miss. Simonne Marie Anne Andree
#45 44          45        1      3                             Devaney, Miss. Margaret Delia
#46 45          46        0      3                                  Rogers, Mr. William John
#47 46          47        0      3                                         Lennon, Mr. Denis
#48 47          48        1      3                                 O'Driscoll, Miss. Bridget
#49 48          49        0      3                                       Samaan, Mr. Youssef
#50 49          50        0      3             Arnold-Franchi, Mrs. Josef (Josefine Franchi)
#51 50          51        0      3                                Panula, Master. Juha Niilo
#52 51          52        0      3                              Nosworthy, Mr. Richard Cater
#53 52          53        1      1                  Harper, Mrs. Henry Sleeper (Myna Haxtun)
#54 53          54        1      2        Faunthorpe, Mrs. Lizzie (Elizabeth Anne Wilkinson)
#55 54          55        0      1                            Ostby, Mr. Engelhart Cornelius
#56 55          56        1      1                                         Woolner, Mr. Hugh
#57 56          57        1      2                                         Rugg, Miss. Emily
#58 57          58        0      3                                       Novel, Mr. Mansouer
#59 58          59        1      2                              West, Miss. Constance Mirium
#60 59          60        0      3                        Goodwin, Master. William Frederick
#61 60          61        0      3                                     Sirayanian, Mr. Orsen
#62 61          62        1      ?                                       Icard, Miss. Amelie
#63 62          63        0      1                               Harris, Mr. Henry Birkhardt
#64 63          64        0      3                                     Skoog, Master. Harald
#65 64          65        0      1                                     Stewart, Mr. Albert A
#66 65          66        1      3                                  Moubarek, Master. Gerios
#67 66          67        1      2                              Nye, Mrs. (Elizabeth Ramell)
#68 67          68        0      3                                  Crease, Mr. Ernest James
#69 68          69        1      3                           Andersson, Miss. Erna Alexandra
#70 69          70        0      3                                         Kink, Mr. Vincenz
#71 70          71        0      2                                Jenkin, Mr. Stephen Curnow
#      Sex  Age SibSp Parch           Ticket     Fare       Cabin Embarked FamSize
#1    male 22.0     1     0        A/5 21171   7.2500                    S       1
#2  female 38.0     1     0         PC 17599  71.2833         C85        C       1
#3  female 26.0     0     0 STON/O2. 3101282   7.9250                    S       0
#4  female 35.0     1     0           113803  53.1000        C123        S       1
#5    male 35.0     0     0           373450   8.0500                    S       0
#6    male   NA     0     0           330877   8.4583                    Q       0
#7    male 54.0     0     0            17463  51.8625         E46        S       0
#8    male  2.0     3     1           349909  21.0750                    S       4
#9  female 27.0     0     2           347742  11.1333                    S       2
#10 female 14.0     1     0           237736  30.0708                    C       1
#11 female  4.0     1     1          PP 9549  16.7000          G6        S       2
#12 female 58.0     0     0           113783  26.5500        C103        S       0
#13   male 20.0     0     0        A/5. 2151   8.0500                    S       0
#14   male 39.0     1     5           347082  31.2750                    S       6
#15 female 14.0     0     0           350406   7.8542                    S       0
#16 female 55.0     0     0           248706  16.0000                    S       0
#17   male  2.0     4     1           382652  29.1250                    Q       5
#18   male   NA     0     0           244373  13.0000                    S       0
#19 female 31.0     1     0           345763  18.0000                    S       1
#20 female   NA     0     0             2649   7.2250                    C       0
#21   male 35.0     0     0           239865  26.0000                    S       0
#22   male 34.0     0     0           248698  13.0000         D56        S       0
#23 female 15.0     0     0           330923   8.0292                    Q       0
#24   male 28.0     0     0           113788  35.5000          A6        S       0
#25 female  8.0     3     1           349909  21.0750                    S       4
#26 female 38.0     1     5           347077  31.3875                    S       6
#27   male   NA     0     0             2631   7.2250                    C       0
#28   male 19.0     3     2            19950 263.0000 C23 C25 C27        S       5
#29 female   NA     0     0           330959   7.8792                    Q       0
#30   male   NA     0     0           349216   7.8958                    S       0
#31   male 40.0     0     0         PC 17601  27.7208                    C       0
#32 female   NA     1     0         PC 17569 146.5208         B78        C       1
#33 female   NA     0     0           335677   7.7500                    Q       0
#34   male 66.0     0     0       C.A. 24579  10.5000                    S       0
#35   male 28.0     1     0         PC 17604  82.1708                    C       1
#36   male 42.0     1     0           113789  52.0000                    S       1
#37   male   NA     0     0             2677   7.2292                    C       0
#38   male 21.0     0     0       A./5. 2152   8.0500                    S       0
#39 female 18.0     2     0           345764  18.0000                    S       2
#40 female 14.0     1     0             2651  11.2417                    C       1
#41 female 40.0     1     0             7546   9.4750                    S       1
#42 female 27.0     1     0            11668  21.0000                    S       1
#43   male   NA     0     0           349253   7.8958                    C       0
#44 female  3.0     1     2    SC/Paris 2123  41.5792                    C       3
#45 female 19.0     0     0           330958   7.8792                    Q       0
#46   male   NA     0     0  S.C./A.4. 23567   8.0500                    S       0
#47   male   NA     1     0           370371  15.5000                    Q       1
#48 female   NA     0     0            14311   7.7500                    Q       0
#49   male   NA     2     0             2662  21.6792                    C       2
#50 female 18.0     1     0           349237  17.8000                    S       1
#51   male  7.0     4     1          3101295  39.6875                    S       5
#52   male 21.0     0     0       A/4. 39886   7.8000                    S       0
#53 female 49.0     1     0         PC 17572  76.7292         D33        C       1
#54 female 29.0     1     0             2926  26.0000                    S       1
#55   male 65.0     0     1           113509  61.9792         B30        C       1
#56   male   NA     0     0            19947  35.5000         C52        S       0
#57 female 21.0     0     0       C.A. 31026  10.5000                    S       0
#58   male 28.5     0     0             2697   7.2292                    C       0
#59 female  5.0     1     2       C.A. 34651  27.7500                    S       3
#60   male 11.0     5     2          CA 2144  46.9000                    S       7
#61   male 22.0     0     0             2669   7.2292                    C       0
#62 female 38.0     0     0           113572  80.0000         B28                0
#63   male 45.0     1     0            36973  83.4750         C83        S       1
#64   male  4.0     3     2           347088  27.9000                    S       5
#65   male   NA     0     0         PC 17605  27.7208                    C       0
#66   male   NA     1     1             2661  15.2458                    C       2
#67 female 29.0     0     0       C.A. 29395  10.5000         F33        S       0
#68   male 19.0     0     0        S.P. 3464   8.1583                    S       0
#69 female 17.0     4     2          3101281   7.9250                    S       6
#70   male 26.0     2     0           315151   8.6625                    S       2
#71   male 32.0     0     0       C.A. 33111  10.5000                    S       0
# [ reached 'max' / getOption("max.print") -- omitted 820 rows ]

library(tidyr)
library(dplyr)

#The following will list summary statistics for average fare and total number of survivors separately for each sex category
passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))

#Results
# A tibble: 2 x 3
#  Sex    meanFare numSurv
#  <fct>     <dbl>   <int>
#1 female     44.5     233
#2 male       25.5     109

library(tidyr)
library(dplyr)

#Read diabetes_train.csv into diabetes variable
diabetes <- read.csv("diabetes_train.csv")

#Calculate 10th, 30th, 50th, 60th percentiles of skin attribute of diabetes data
percentiles <- quantile(diabetes$skin, probs = c(0.10, 0.30, 0.50, 0.60))


print(percentiles)
#10% 30% 50% 60% 
#0   10  23  27