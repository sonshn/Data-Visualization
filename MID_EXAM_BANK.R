# (데이터 셋을 로딩)my_bank에 BankChurners.csv(데이터 파일) 저장하기
# 아래에 진행한 모든 분석은 my_bank$를 바탕으로 진행됩니다.
my_bank <- read.table(file="BankChurners.csv", header = TRUE, sep=',',stringsAsFactors = FALSE)

# (전반적인 데이터셋 내용 및 구조 탐색)
my_bank
class(my_bank)
str(my_bank) # 분석결과, 10127 objects and 23개의 변수를 가진 구조로 되어 있다.
# 데이터 셋의 내용은 고객들의 ID, 나이, 성별, 학력, 결혼여부 등의 기본 정보와
# 수입, 카드등급, 총/지난 1년간 거래 금액 등 금융 정보로 이루어져 있다.

# (전반적인 데이터셋 시각화)
plot(my_bank)#plot 명령어만 사용할 경우 plot이 너무 크기 때문에 error 메세지 발생하거나 plot을 그리는데 시간이 지나치게 오래 걸림림
round(cor(my_bank),3)

temp<-cor(my_bank)
round(temp,3)

temp<-cor(my_bank, use = 'complete.obs')
round(temp,2)# 각 변수들에 대한 상관계수 값은 정상적으로 콘솔창에 출력되는 것을 확인

# 3. 개별 변수 요약과 집계
# [데이터셋 중 범주형 변수를 2개 선정해 특성요약과 시각화]
# 업무 목적: 은행과 계약을 잠정적으로 중단한 사람들의 발길을 다시 되돌리기 위해 특정 계층을 겨냥한 금융 상품을 가상으로 만든다는 것을 가정
# 최근, 저출산 문제가 심각한 사회문제로 대두되어, 주부를 비롯한 가정을 이룬 사람들의 기본 정보 및 금융정보를 파악해
# 은행 측에서는 아래 분석한 데이터를 활용해 금융 상품을 만들어 고객들을 다시 은행으로 유치할 수 있게하는 것이 제가 생각한 최종 업무 목적입니다.
str(my_bank$Gender)
my_bank$Gender_f <- factor(my_bank$Gender, labels = c('M','F'))
summary(my_bank$Gender_f)
Hmisc::describe(my_bank$Gender_f)
# 우선 각 고객들의 성별을 파악하기 위해 범주형 변수인 Gender 변수를 불러와서 factor형으로 바꿈
# 분석결과 : 남성-5358명, 여성-4769명
# 아래에 prop함수를 사용해서 비율로 변환할 겨우 각각 52.9%와 47.1%의 비율임
# 이탈한 고객들 중에서는 남성이 여성보다 더 많은 것을 알 수 있음
Gender_freq <- table(my_bank$Gender)
Gender_f_freq<-table(my_bank$Gender_f)
Gender_df <-as.data.frame(Gender_f_freq)
Gender_prop <-prop.table(Gender_freq)
Gender_f_prop <-prop.table(Gender_f_freq)
Gender_f_prop

# 저출산을 해결하기 위해 가장 중요한 것은 미래에 자녀들을 부양할 때의 부담을 최대한 줄여 안정적인 출산을 유도하는 것이라 판단하여,
# 부양해야 할 가족 수를 저장한 범주형 변수인 Dependent_count를 불러와서 factor형으로 바꿔서 분석을 진행하였습니다.
# 즉, 가족 구성원 수가 많을 수록 가정의 금융적인 부담이 크다고 판단했고, 그 정도에 따라 일반적인 가정의 출산율에 영향을 줄 수 있다고 생각하였습니다.
str(my_bank$Dependent_count)
my_bank$Dependent_count_f <- factor(my_bank$Dependent_count, labels = c('0','1','2','3','4','5'))
summary(my_bank$Dependent_count_f) #부양 가족수가 6명 이상인 경우는 없음
#summary를 통해서 나온 응답자수의 합 = 총 응답자수의 합
Hmisc::describe(my_bank$Dependent_count_f)
# 분석결과 : 부양 가족수가 없는 경우 - 904명(8.9%), 부양 가족수가 1명인 경우 - 1838명(18.1%), 부양 가족 수가 2명인 경우 - 2655명(26.2%)
# 부양 가족 수가 3명인 경우 - 2732명(27%), 부양 가족 수가 4명인 경우 - 1574명(15.5%), 부양 가족 수가 5명인 경우 - 424명(4.2%)
# 저는 부양 가족 수가 일반적인 가정의 자녀 수와도 관련이 있다고 판단하였기 때문에 일반적인 
# 분석결과 다자녀가정(자녀 수가 3명 이상인 가정)에 해당하는 가정, 즉 부양 가족 수가 3명이 넘어가는 가정이 50%에 가깝고,
# 부양 가족 수가 2명인 경우까지 확대하면 전체의 70%가 넘는 고객들의 부양 가족 수가 2명 이상이기 때문에
# 만약 은행 측에서 부양 가족 수가 많은 가정을 대상으로 한 금융 상품을 개발한다면 거래를 중단한 고객들 중 상당수에게
# 다시 관심을 얻을 수 있을 것이라고 판단하였고, 의미 있는 분석이라고 생각하여 분석을 진행하였습니다.

Dependent_count_freq <- table(my_bank$Dependent_count)
Dependent_count_f_freq<- table(my_bank$Dependent_count_f)
Dependent_count_prop <- prop.table(Dependent_count_freq)
Dependent_count_f_prop <- prop.table(Dependent_count_f_freq)
Dependent_count_f_prop
#여기서도 prop 함수를 사용하여 분석을 진행하였습니다. 
par(mfrow=c(2,2))#plot은 한 화면에 2*2=4 총 4개가 나옵니다.

# 성별을 나타내는 변수인 Gender 변수는 앞에서 만든 Gender_f_prop에 저장하여 남자는 하늘색, 여자는 핑크색으로 표시가 된
# 막대 그래프를 생성하였습니다.
barplot(Gender_f_prop, main = "BankChurners의 성별 분석", col=c('M'='SkyBlue','F'='Pink'))

# 부양 가족 수를 나타내는 변수인 Dependent_count 변수는 앞에서 만든 Dependent_Count_f_prop에 저장하여
# 0명인 경우 오렌지색, 1명인 경우 연두색, 2명인 경우 하늘색, 3명인 경우 핑크색, 4명인 경우 보라색, 5명인 경우 갈색으로
# 막대가 그려지도록 막대 그래프를 생성하였습니다.
barplot(Dependent_count_f_prop, main = "부양 가족 수", col=c('0'= 'orange','1' = 'lightgreen','2'='skyblue','3'='pink','4'='purple','5'='brown'))

# [데이터셋 중 연속형 변수를 2개 선정해 특성요약과 시각화]
# 은행에서 고객들의 발길을 돌리기 위한 금융 상품을 만든다면 우선 고객들이 얼마나 자주 오는지를 파악해야 한다고 판단하여
# 지난 1년간 고객들이 총 거래한 건수를 저장한 변수인 Total_Trans_Ct에 대하여 아래와 같이 특성을 요약(summary)하고 분석을 진행하였습니다.
str(my_bank$Total_Trans_Ct)
my_bank$Total_Trans_Ct_f <- factor(my_bank$Total_Trans_Ct)
summary(my_bank$Total_Trans_Ct)

# 은행이 만들게끔 유도한 금융상품이 '저출산'을 해결하기 위한 상품이기 때문에, 상대적으로 3-40대 고객들의 비율을 파악하는 것이 중요하다고
# 판단하여 고객들의 나이를 분석하기로 하였습니다. 이를 위해 고객들의 나이를 저장한 변수인 Customer_Age를 사용합니다.
# 강의자료에서는 나이가 '연령대'로 분류되어 범주형 변수이지만, 제가 가진 BankChruners.csv 에서는 나이가 연령대가 아닌, 40, 58과 같이
# 몇 '살'인지로 나와있어 구글 검색 결과 이런 경우는 연속형 변수가 맞다고 판단하여 이 부분에 분석을 진행하였습니다.
# 앞과 같은 과정으로 Customer_Age도 분석을 진행하여 아래와 같이 특성을 요약(summary)하고 분석을 진행하였습니다.
str(my_bank$Customer_Age)
my_bank$Customer_Age <- factor(my_bank$Customer_Age)
summary(my_bank$Customer_Age)

par(mfrow=c(2,2))

# 지난 1년간 총 거래 건수인 Total_Trans_Ct를 강의자료처럼 plot을 사용하여 시각화를 하려했더니, 분석하기도 힘들고, 보기 힘든 형태로
# 그래프가 생성되었습니다.
plot(my_bank$Total_Trans_Ct, main="지난 1년간 총 거래 횟수")
abline(h=seq(from=1, to=200, by=20), col="gray",lty=2)
abline(v=seq(from=1, to=10000, by=1000), col="gray",lty=2)
# abline으로 각 도수의 결과가 구분이 잘 되지 않았기 때문에 boxplot을 사용하여 도수들의 결과의 분포를 파악하는 방향으로 분석을 진행하여
# boxplot으로 시각화를 진행하였습니다.
boxplot(my_bank$Total_Trans_Ct, main="지난 1년간 총 거래 횟수(BOXPLOT)", ylab = "지난 1년간 총 거래 횟수")
# 분석결과 거래를 중단한 대부분의 고객들의 지난 1년간 총 거래 횟수는 60여번 인 것을 알 수 있었습니다.

# 고객들의 나이를 저장한 변수인 Customer_Age의 plot를 그려서 전체적인 고객들의 나이 분포를 히스토그램으로 확인하였습니다.
# 이 때 분석 및 가독성을 용이하게 하기 위해 응답자수를 나타내는 y축은 100명씩 끊어서 오렌지색으로 line을 만들었고
# 나이를 나타내는 x축은 5살씩 끊어서 하늘색으로 line을 만들었습니다.
plot(my_bank$Customer_Age, main = "Churners들의 나이 분포",xlab = "나이", ylab = "명")
abline(h=seq(from=0, to=600, by=100), col="orange",lty=2)
abline(v=seq(from=0, to=100, by=5), col="skyblue",lty=2)
# 분석결과 거래를 중단한 고객들의 대부분은 40대에 몰려있는 것을 확인할 수 있었습니다.
# 위 두 가지 분석결과를 진행한 결과 3-40대를 메인 타겟으로 삼아야 한다는 것은 맞는 판단임을 확인할 수 있었습니다.

# 4. 다차원변수 요약과 집계
# [3장에서 다룬 범주형 변수 2개간 특성요약과 시각화]
# 제가 고른 것은 성별(Gender)에 따른 부양 가족 수(Dependent_count) 교차 빈도 분석입니다
# 메인 타겟을 더 좁히기 위해(ex. 주부) 아래 분석을 진행하였습니다.
table(my_bank$Gender_f, my_bank$Dependent_count_f)
gd_dp_freq <- table(my_bank$Gender_f,my_bank$Dependent_count_f)
gd_dp_freq # 성별과 부양 가족 수의 교차 분석 내용은 gd_dp_freq에 저장하였습니다

prop.table(gd_dp_freq,1)
gd_dp_prop <- prop.table(gd_dp_freq,1)
gd_dp_prop
addmargins(round(gd_dp_prop,3),2)
# 바로 윗 줄 addmarigins의 결과를 바탕으로 분석을 진행한 결과 남자의 경우 부양 가족 수가 2명과 3명인 경우가 각각 25.6%와 26.4%로 대부분을 차지하고
# 여성의 경우도 부양 가족 수가 2명과 3명인 경우가 2명과 3명인 경우가 각각 26.6%과 27.6%인 경우가 대부분을 차지합니다.
# 이 떄 round함수 때문에 약간의 sum 오차가 발생하기는 합니다.
gd_dp_result <- round(gd_dp_prop,3)*100
gd_dp_result
addmargins(gd_dp_result,2)

par(mfrow=c(2,2))
barplot(gd_dp_freq,
        main="부양 가족 수에 따른\n 성별 분포 비교",
        xlab = "부양 가족 수(Dependent_count)", ylab = "응답자수",beside = TRUE,
        col=c("blue","red"), legend = rownames(gd_dp_freq))
# 가로축에 부양 가족 수, 세로 축에 응답자(고객)수를 배치하여 남성과 여성으로 나누어 그래프가 시각화를 시켰습니다.
# 남성은 M(파랑), 여성은 F(빨강)으로 나옵니다.

# [3장에서 다룬 연속형 변수 2개간 특성요약과 시각화]

#var(my_bank$Total_Trans_Ct, my_bank$Total_Trans_Amt)
#cor(my_bank$Total_Trans_Ct, my_bank$Total_Trans_Amt, method = 'spearman')
#cor(my_bank$Total_Trans_Ct, my_bank$Total_Trans_Amt, method = 'pearson')

library(psych)
psych::describe(all(duplicated(my_bank$Customer_Age)[-1]))
psych::describe(my_bank$Total_Trans_Ct)
var(my_bank$Customer_Age[-1], my_bank$Total_Trans_Ct)
# 앞에서 다룬 Customer_Age는 거래를 중단한 고객들의 나이, Total_Trans_Ct는 거래를 중단한 고객들이 지난 1년간 거래를 한 횟수입니다.
# 따라서 이 두 변수 간에 특성요약과 시각화를 진행하였습니다. 이 경우 Customer_Age는 error문구가 출력되어 all(duplicated)를 사용하였습니다.

par(mfrow=c(2,2))
# 한 화면에 plot은 최대 4개까지만 출력됩니다.

plot(my_bank$Total_Trans_Ct ~ my_bank$Customer_Age, data = my_bank, pch =19,
     main = "Churners의 나이에 따른 총 거래 횟수의 관련성",
     xlab = "Churners의 나이", ylab = "지난 1년간 총 거래 건수")
# 세로 축은 Churners의 지난 1년간 총 거래 건수, 가로 축은 Churners의 나이입니다.
# 따라서 위 정보들을 연관시켜 "Churners의 나이에 따른 총 거래 횟수의 관련성"이라 그래프의 이름을 정했습니다.
# 분석결과 대체로 나이가 젊을수록 지난 1년간 총 거래건수가 많았고, 44세(40대) 전후로도 총 거래 건수가 많아 새로운 금융상품을 만드는데 활용할 수 있을 것 같습니다.

# (3장에서 다룬 범주형변수와 연속형변수 중 1개 관계를 선정해 특성요약과 시각화)
# 부양 가족 수와 지난 1년간 총 거래 건수 사이에 관계를 선정해 특성요약과 시각화를 진행하려 했습니다.
my_bank$Dependent_count_f<-factor(my_bank$Dependent_count, levels = c(1,2,3,4,5,6),
                                  labels = c('0','1','2','3','4','5'))
Hmisc::describe(my_bank$Dependent_count_f)
Hmisc::describe(my_bank$Total_Trans_Ct)

aggregate(formula = Total_Trans_Ct ~ Dependent_count,data=my_bank, FUN=mean, na.rm = TRUE)
aggregate(Total_Trans_Ct ~ Dependent_count_f,my_bank,mean,na.rm=TRUE, trim=0.05)
aggregate(Total_Trans_Ct ~ Dependent_count_f,my_bank,sd,na.rm=TRUE)

#install.packages('magrittr')
#library(magrittr)
library(dplyr)

my %>% 
  group_by(Dependent_count_f) %>%
  dplyr::summarize(Avg = mean(Total_Trans_Ct), SD = sd(Total_Trans_Ct)) %>%
  arrange(desc(Avg))
# 이 부분에서 에러가 발생하였습니다.

# [데이터셋 중 변수 리코딩 작업 2개 실시]
# 제가 첫번째로 리코딩한 변수는 1년 수입입니다.
# 상대적으로 1년 수입이 적을수록 새로운 금융상품을 만들 때 효과(거래를 중단한 고객이 다시 돌아올 확률)가 더 클 것이라고 생각했기 때문입니다. 
table(my_bank$Income_Category)
# 1년 수입을 저장하는 변수인 Income_Category를 불러와서 각 단계별로, 수입이 적은 순서부터 1~5로 리코딩을 진행하였고, unknown은 6에 배치하였습니다.
my_bank$Income_Category_n1[my_bank$Income_Category == 'Less than $40K'] <- 1 ;
my_bank$Income_Category_n1[my_bank$Income_Category == '$40K - $60K'] <- 2 ;
my_bank$Income_Category_n1[my_bank$Income_Category == '$60K - $80K'] <- 3 ;
my_bank$Income_Category_n1[my_bank$Income_Category == '$80K - $120K'] <- 4 ;
my_bank$Income_Category_n1[my_bank$Income_Category == '$120K +'] <- 5 ;
my_bank$Income_Category_n1[my_bank$Income_Category == 'Unknown'] <- 6 ;

summary(my_bank$Income_Category_n1) #리코딩한 결과는 Income_Category_n1에 저장되고, 특성요약은 summary를 통하여 나타냈습니다.
# Mean = 2.745인 것을 보아 실제로 소득이 낮은 쪽의 고객이 더 많을 것을 알 수 있었습니다.
# 실제로 1년 수입이 4만달러보다 적은 고객수가 3561명으로 제일 많았고, 4만달러 이상 6만 달러 미만이 1790명,
# 8만 달러 이상 12만 달러 미만이 1535명, 4만 달러 이상 8만 달러 미만이 1402명, 12만 달러 이상이 727명, Unknown이 1112명이었습니다.
# 따라서 저소득층을 메인 타겍으로 한 새로운 금융상품을 만들 경우 효과가 클 것이라고 판단하였습니다.

# 제가 두번째로 리코딩한 변수는 결혼여부입니다.
# 아무래도 결혼을 한 상태이거나 이혼을 한 상태가 상품의 의도(저출산 해결)에 더 맞다고 생각했기 때문입니다.
table(my_bank$Marital_Status)
# 결혼 여부를 저장하는 변수인 Marital_Status를 불러와서 Married/Single/Divorced/Unknown(각각 기혼, 싱글, 이혼, 알 수 없음)을 각각 1~4로 리코딩을 진행하였습니다.
my_bank$Marital_Status_n1[my_bank$Marital_Status == 'Married'] <-1;
my_bank$Marital_Status_n1[my_bank$Marital_Status == 'Single'] <-2;
my_bank$Marital_Status_n1[my_bank$Marital_Status == 'Divorced'] <-3;
my_bank$Marital_Status_n1[my_bank$Marital_Status == 'Unknown'] <-4;

summary(my_bank$Marital_Status_n1) #리코딩한 결과는 Maritial_Status_n1에 저장되고, 특성요약은 summary를 통하여 나타냈습니다.
# Mean = 1.759인 것을 보아 싱글이거나, 이혼한 사람보다 결혼한 사람의 비율이 더 높다는 것을 알 수 있습니다.
# 실제로 기혼이 4687명 싱글이 3943명 이혼이 748명, Unknown이 749명이었고, 이혼한 사람들 중에서 부양해야 할 자녀가 있다고 가정할 경우
# 기혼+이혼의 비율이 50%를 넘어가, 앞에서 리코딩한 변수과 연관지으면 은행이 저소득 + 자녀가 있는 사람들을 대상으로 새로운 금융 상품을 만들어도
# 큰 효과를 볼 수 있을 것이라고 생각합니다.

# 데이터셋 중 요약변수 2개 만들기
Interact_last12 <- c('Months_Inactive_12_mon', 'Contacts_Count_12_mon')
Interact_last12
# 거래를 중단한 고객을 기준으로 '지난 1년간 얼마나 자주 은행을 이용했나'에 대한 정보를 알아내기 위해
# Interact_last12라는 변수를 만들었습니다. 이 변수는 '고객이 지난 1년동안 은행을 이용하지 않은 달(개월)수'를 나타내는 변수인
# Months_Inactive_12_mon과 지난 1년간 접촉한 정도를 저장한 변수인 Contacts_Count_12_mon을 사용하여 만들었습니다.

money_per_contact <- c('Total_Amt_Chng_Q4_Q1', 'Total_Ct_Chng_Q4_Q1')
money_per_contact
# 거래를 중단한 고객이 왜 거래를 중단하였는가에 대한 근본적인 원인을 알아내 그에 맞추어 금융상품을 만들도록 유도하기 위해
# 일단은 거래 빈도 및 금액에 초점을 맞추기 위해 money_per_contact라는 변수를 만들었습니다.
# 이 변수를 만들기 위해 사용한 기존의 데이터셋 변수는 'Total_Amt_Chng_Q4_Q1', 'Total_Ct_Chng_Q4_Q1'로
# 각각 1분기 총 거래 금액 대비 4분기 총 거래 금액과 1분기 대비 4분기 총 거래 건수입니다.
# 제 경험으로 각 고객들의 거래 금액과 빈도로 "이 고객이 얼마나 은행을 자주, 많이 이용하나"라고 판단할 수 있다고 생각하여
# 이에 대한 요약변수를 만들기로 하였습니다.

my_bank$Interact_last12_rate <- apply(my_bank[Interact_last12],1,mean)
my_bank$Interact_last12_rate
Hmisc::describe(my_bank$Interact_last12_rate)
# 앞에서 만든 Interact_last12를 바탕으로 Interact_last12_rate라는 요약변수를 apply함수와 mean를 활용하여 생성하였으며,
# Hmisc::describe를 이용해 특성을 요약하였습니다.

my_bank$money_per_contact_rate <- apply(my_bank[money_per_contact],1,mean)
my_bank$money_per_contact_rate
Hmisc::describe(my_bank$money_per_contact_rate)
# 앞에서 만든 money_per_contact를 바탕으로 money_per_contact_rate라는 요약변수를 apply함수와 mean를 활용하여 생성하였으며,
# Hmisc::describe를 이용해 특성을 요약하였습니다

# 데이터셋 중 파생변수 2개 만들기

# 5장에서 만든 리코딩변수/요약변수/파생변수 중 범주형 변수 2개간 특성요약과 시각화


# 5장에서 만든 리코딩변수/요약변수/파생변수 중 연속형 변수 2개간 특성요약과 시각화
library(psych)
psych::describe(my_bank$Interact_last12_rate)
psych::describe(my_bank$money_per_contact_rate)
# 앞에서 만든 Interact_last12_rate 요약 변수와 money_per_contact_rate 요약 변수 간의 특성요약과 시각화를 진행하였습니다.
# 특성요약을 위해 psych::describe 함수를 사용하였습니다. 각 rate의 최댓값은 각각 6과 3.32입니다.
# 그 외 median, trimmed, skew등의 정보가 콘솔창에 출력되며, 앞과 마찬가지로 이를 통해 특성을 파악할 수 있습니다.
var(my_bank$Interact_last12_rate, my_bank$money_per_contact_rate)
cor(my_bank$Interact_last12_rate, my_bank$money_per_contact_rate, method = 'spearman')
cor(my_bank$Interact_last12_rate, my_bank$money_per_contact_rate, method = 'pearson')
# 추가적으로 var, spearman, pearson으로 분석을 진행하였습니다.

par(mfrow=c(2,2))

plot(Interact_last12_rate ~ money_per_contact_rate, data = my_bank, pch =19,
     main = "최근 1년간 고객이 은행을 이용/이용하지 않은\n 정도에 따른 한 4분기의 거래 규모",
     xlab = "1분기 대비 4분기의 거래 규모", ylab = "최근 1년간 고객이 은행을 이용/이용하지 않은 정도")
# 따라서 두 요약변수간에 시각화를 진행하였습니다. 가로축과 세로축은 업무 적용방안과 앞에서 요약변수를 만들었을 때의 의도와 맞게
# 가로축은 1분기 대비 4분기의 거래 규모, 세로축은 최근 1년간 고객이 은행을 이용한/하지않은 정도입니다.
# 분석결과, 4분기의 거래 규모가 적을 수록, 은행 이용 정도도 낮아지는 것을 알 수 있었습니다.
# 따라서 두 데이터는 대체로 비례관계를 띄는 것을 확인할 수 있었습니다.

# 5장에서 만든 리코딩변수/요약변수/파생변수 중 1개 관계를 선정해 특성요약과 시각화
