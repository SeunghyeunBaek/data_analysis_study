# 다음 물음을 답하는 간략한 코드 (파이썬이 아닌R), 계산 결과와 설명을 제시하시오.
# 크레딧 카드 사용 내역 데이터 data_credit.csv를 사용하여 다음의 순서로 탐색적 분석 EDA를 진행하시오.

# 데이터 불러오기
data = fread("data/data_credit.csv",header =  T, stringsAsFactors = F)
summary(data) # 3362796 x 8

# 100만개개만 샘플링
set.seed(1)
idx <- sample(1:nrow(data), 1000000)
data_samp <-data[idx,]

# 1 데이터 클리닝 & 결측치 처리
# installments의 99%가 결측치다
null_table = sapply(data_samp, function(x) mean(is.na(x)))
null_table

# insatallment의 na를 모두 1로 대치(일시불)
summary(data_samp$installments)
data$installments[is.na(data_samp$installments)]<-1
sapply(data_samp, function(x) mean(is.na(x))) # ???? ?? ?????? ????

# 2. 일변량 분석 & 시각화
cols = colnames(data_samp)

# 2.1. store_id
# 상점 개수 확인 : 1775개 상점
n_stores = length(unique(data_samp$store_id))

# q1. 상점에 따라 거래횟수에 차이가 있는가?
# 상점에 따라 거래횟수에 차이가 존재하는 것으로 보인다.
hist(data_samp$store_id,breaks = 1775/30)

# 2.2. date : Year - Month - Date
# q2. 날짜에 따라 거래횟수가 차이나는가?
data_samp$date_converted <- as.Date(data_samp$date) # charactor to date
range(data_samp$date_converted) #20160801 ~ 20180731

# year, month column ????
data_samp$year <-year(data_samp$date_converted)
data_samp$month <-month(data_samp$date_converted)

hist(data_samp$date_converted,breaks = 365) # day/2 : 거래횟수는 증가하는 추세
barplot(table(data_samp$month)) # month : 1,2월 / 3월-7월 / 8월- 12월 구간별로 거래횟수 차이가 나타난다.
barplot(table(data_samp$year)) # 2017년 거래횟수가 가장 많다.

# 2.3. time
# q3. 시간대에 따라 거래횟수 차이가 존재하는가?
# 아침(06-11) : 중가
# 점심(12-14) : 12시에 최댓값, 이후 감소
# 오후(15-17) : 증가 or 일정
# 저녁(18-20) : 19시에 최댓값, 이후 감소
# 밤, 새벽(21-05) : 감소
data_samp$hours <-sapply(data_samp$time, function(time) format(strptime(time,"%H:%M:%S"),'%H')) # hours column 추가?
barplot(table(data_samp$hours))

# 2.4. card_id
# card_id 형태 확인 - 10자리
# q.6 거래량에 따라 카드 아이디를 분류할 수 있는가
n_card_id = length(unique(data_samp$card_id)) # 922522 종류

# 2.5. amount
# q.5 거래 금액에 따른 거래량 차이가 있는가
# log amount는 정규 분포와 비슷한 형태다
hist(log(data_samp$amount+1))

summary(data_samp$amount) # 음수(환불 데이터 존재)
# 환불데이터 분포
amount_neg = data_samp$amount[which(data_samp$amount<0)]
hist(abs(amount_neg))
summary(abs(amount_neg)) # 75% 가 450 이하, max = 135980
# 환불 거래 데이터 비율
ratio_pos_neg = length(amount_neg)/nrow(data_samp) # 1%

# 2.6. day of week, holiday
# q.6 휴일과 주말에 더 결제횟수가 많은가?
# 주말/주중 은 큰 차이가 없어 보인다. 금/토가 약간 많다.
# 휴일에 일 평균 거래횟수가 더 적다.

barplot(table(data_samp$days_of_week))

n_not_holyday = length(unique(data_samp$date[which(data_samp$holyday == 0)])) # 휴일아 아닌 날 : 693
n_holyday = length(unique(data_samp$date[which(data_samp$holyday == 1)])) # 흎일: 37
mean_not_holyday = length(data_samp$date[which(data_samp$holyday == 0)]) / n_not_holyday # 평일 일 평균 거래 횟수 : 1379
mean_holyday = length(data_samp$date[which(data_samp$holyday == 1)]) / n_holyday # 휴일 일 평균 거래 횟수 : 1198

# 2.7. Installments
hist(data_samp$installments) # 결측치를 1로 채워서, 1이 가장 많다.(99%)

cols = colnames(data_samp)

#3. 이변량 분석 & 시각화

# 3.1. store_id vs amount
# 매장별 총 매출 : 대체적으로 패턴이 없으나 외상치가 존재한다.
amount_store = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$store_id))
plot(amount_store)
summary(amount_store)

plot(amount_store[which(amount_store$x<94881),]) # 3Q 이하만 출력
plot(amount_store[which(amount_store$x>94881),]) # 3Q 이상

# 3.2. date vs amount
# 월별 총 매출 : 12-2 / 3-7 / 8-11 3개 구간으로 나눌수 있다.
amount_month = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$month))
plot(amount_month)
abline(v = c(2,7,11), col = 'red')

# 연간 총 매출 : 2017 > 2018 > 2016
amount_year = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$year))
plot(amount_year)

# 3.3. time vs amount
# 시간대별 총 매출 : 0-5/6-11/12-16/17-20/21-23 총 5개 구간으로 나눌 수 있다.
amount_time = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$hours))
plot(amount_time)
abline(v = c(5,11,16,20), col = 'red')

# 3.4. days_of_week vs amount
# 월 - 목 / 금,토 / 일 3개 구간으로 나눌 수 있다.  일요일의 총 매출이 가장 적다.
amount_dow = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$days_of_week))
plot(amount_dow)

