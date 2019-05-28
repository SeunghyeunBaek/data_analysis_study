# 다음 물음을 답하는 간략한 코드 (파이썬이 아닌R), 계산 결과와 설명을 제시하시오.
# 직방의 아파트가격 데이터 example_zigbang.csv를 사용하여 다음의 순서로 탐색적 분석 EDA를 진행하시오.

# 1. 데이터 불러오기
library(data.table)
data = fread('data/example_zigbang.csv',header = T, stringsAsFactors = F)
summary(data)
data = data[which(data$city==1),] # 서울 데이터만 사용
                
# 2. 결측치 확인, 데이터 클리닝
na_table = sapply(data, function(col) mean(is.na(col)))
na_table[which(na_table>0)]

# 2.1. total_parking_capacity_in_site
# 주차대수는 세대수와 관련있을 것이다. cor = 0.90
household_parking = data[,c('total_household_count_in_sites','total_parking_capacity_in_site')]
cor(household_parking,use="complete.obs",method = 'pearson') # cor = 0.91
# scatter.smooth(household_parking)

# 선형회귀로 대치
train = household_parking[which(is.na(household_parking$total_parking_capacity_in_site)==F)]
test =  household_parking[-train,]
lm_mod = lm(total_parking_capacity_in_site~total_household_count_in_sites, data = train)
y_pred = predict(lm_mod,test)

data[which(is.na(data$total_parking_capacity_in_site)==T),]$total_parking_capacity_in_site = y_pred
scatter.smooth(data[,c('total_parking_capacity_in_site','total_household_count_in_sites')])
mean(is.na(data$total_parking_capacity_in_site))

# 2.2. tallest_building_in_site, lowest_building_in_site
# 둘다 15층이 제일 많음, 15층으로 대치
sort(table(data$tallest_building_in_sites))
sort(table(data$lowest_building_in_sites))

data[which(is.na(data$tallest_building_in_sites)==T)]$tallest_building_in_sites = 15
data[which(is.na(data$lowest_building_in_sites)==T)]$lowest_building_in_sites = 15

# 2.3. room_count, bathroom_count
# 평균으로 대치
# install.packages('imputeTS')
tmp = colMeans(data[,c('room_count','bathroom_count')],na.rm = T)
data[which(is.na(data$room_count)==T)]$room_count = tmp[1]
data[which(is.na(data$bathroom_count)==T)]$bathroom_count = tmp[2]

na_table = sapply(data, function(col) mean(is.na(col)))
na_table

# 3. EDA, 변수 변환
# 3.1. transaction_year_month -> year, month
range(data$transaction_year_month) # 날짜 범위 확인 : 200601 - 201810
data$year = sapply(data$transaction_year_month, function(date) as.numeric(substring(toString(date),1,4)))
data$month = sapply(data$transaction_year_month, function(date) as.numeric(substring(toString(date),5,6)))

# 3.2. transaction_date 변수 클리닝
# 1-10, 11-20 : 1
# 21-28, 21-29, 21-30, 21-31 : 2
table(data$transaction_date)
date_price = aggregate(data$transaction_real_price, FUN = 'mean', by = list(data$transaction_date))
colnames(date_price)<-c('date','price')
ggplot(data=date_price, aes(x= date, y= as.integer(price)))+geom_bar(stat = "identity")
labelDate <- function(date){
  result = 1
  if (date == '1-10' | date == '11-20'){result = 0}
  return(result)
}
data$transaction_date <- sapply(data$transaction_date,labelDate)


# 3.3. transaction_real_price 로그변환
hist(as.integer(data$transaction_real_price))
data$log_price <- log(data$transaction_real_price)
hist(data$log_price)

# 3.4. 세대별 주차대수에 해당하는 파생변수 만들기
# 세대별 주차대수 = 주차대수 / 세대수
data$parking_per_household  = data$total_parking_capacity_in_site/data$total_household_count_in_sites
hist(data$parking_per_household)

# 4. 설명 변수와 종속변수의 통계적 특성을 한 개씩 요약, 탐색, 시각화 한다. 
cols = colnames(data)

# 4.1. 종속변수 : log_price : 대략 정규분포를 따른다
hist(data$log_price)


# 4.2. 설명변수
#
# 지역 정보 : city, latitude, longitude, address_by_law. 
#
# 실내, 설비 정보 :heat_type,heat_fuel,room_id,room_count,bathroom_count,front_door_structure
#
# 면적 정보 : exclusive_use_area, floor, supply_area
#
# 실외 정보 : apartment_id, total_parking_capacity_in_site,
# total_household_count_in_sites,apartment_building_count_in_sites,parking_per_household
# tallest_building_in_sites, lowest_building_in_sites, total_household_count_of_area_type,
# parking_per_household
# 
# 날짜 정보 : transaction_date, transaction_year_month, year_of_completion, year, month

# 4.2.1. 지역정보
# address_by_law : 법정코드 10자리 : 시도(2)+ 시군구(3) + 읍면동(3)+ 리(2)
data$gu <- sapply(data$address_by_law, function(address) substr(as.character.integer64(address),3,5))
# 다른 구에 중복된 동 코드가 있음, 동은 구+동으로 구분해야함
data$dong <- sapply(data$address_by_law, function(address) substr(as.character.integer64(address),3,8))

# 구에 따른 실거래가 평균
# 평균 실거래가 상위 3개구
# 710(송파구), 680(강남구),650(서초구)
length(unique(data$gu)) # 서울시 25개 자치구
gu_price = aggregate(data$transaction_real_price, by = list(data$gu), FUN = 'mean')
colnames(gu_price)<-c('gu','price')
ggplot(data=gu_price, aes(x=gu,y=as.integer.integer64(price)))+geom_bar(stat = 'identity')

# 동에 따른 실거래가 평균
# 실거래가 평균 상위 5개 
# 680110(강남구 압구정동), 170127(용산구 용산동), 170131(용산구 한남동), 170116(용산구 청암동),140122(중구 회현동)
length(unique(data$dong)) # 264개동
dong_price = aggregate(data$transaction_real_price, by = list(data$dong), FUN = 'mean')
colnames(dong_price)<-c('dong','price')
ggplot(data=dong_price, aes(x=dong,y=as.integer.integer64(price)))+geom_bar(stat = 'identity')
dong_price[order(dong_price$price, decreasing = T),]

# 4.2.2. 실내, 설비정보
# room_count, 방 3개짜리가 많음
barplot(table(data$room_count))

# bathroom_count, 1개, 2개가 많음
barplot(table(data$bathroom_count))

# 4.2.3. 면적정보
# exclusive_area vs supply_area : corr = 0.98
# 공급면적(supply_area) = 전용면적(exclusive_area) + 주거공용면적(복도, 엘리베이터)
cor(data$exclusive_use_area, data$supply_area, method = 'pearson')
hist(data$exclusive_use_area) # 최빈값 : 50-60, 80-90
hist(data$supply_area) # 최빈값 : 100-110

# 주거공용면적
data$residential_area = data$supply_area-data$exclusive_use_area
hist(data$residential_area) # 최빈값 : 20-30

# 공급면적과 전용면적의 평균으로 area 변수 설정
data$area = (data$supply_area+data$exclusive_use_area)/2

# area vs transcation_real_price
# 면적이 커질수록 실거래가도 높아지는 추세( cor = 0.63)
# 270-300(80-90평) 구간에서 실거래가가 가장 높은 지점 존재(9억이상)

plot(data$area, data$transaction_real_price)
cor(data$area, data$transaction_real_price, method = 'pearson') # 0.63

# floor
hist(data$floor) # 최빈값 : 0-5층
# floor vs transaction_real_price 
# 층수가 높을수록 실거래가가 증가하는 추세.
# 0-5 층건물 중 가격이 높은 건물이 있다. 재건축을 기다리는 아파트로 보인다.
floor_price = aggregate(data$transaction_real_price, FUN = 'mean' , by = list(data$floor))
plot(floor_price)
# 0-5층인데, 15억 이상인,준공 1990년 이전
# 650107(서초구반포동), 680110(강남구압구정동), 680106(강남구대치동)
# 재건축을 앞둔 아파트의 조건을 컬럼으로 만들어야 할듯
expensive_area = data[which( (data$transaction_real_price>1500000000)&(data$floor<5)&((data$year_of_completion<1990)) ),]$dong
tmp = sort(table(expensive_area),decreasing = T)
barplot(tmp)

# 4.2.4. 실외정보
# apartment_id
hist(data$apartment_id) # 최빈값 : 0-200번대
# total_household_count_in_sites
hist(data$total_household_count_in_sites) # 최빈값 : 0-1000세대
# apartment_building_count_in_sites
hist(data$apartment_building_count_in_sites) # 최빈값 : 0-15 동
# parking_per_household
hist(data$parking_per_household) # 대부분 2 이하

# 4.2.5. 날짜정보
# month : 3월에 이사를 많이가고, 11-2월은 이사를 적게 간다. 나머지는 비슷함
barplot(table(data$month))

# year vs transaction_real_price : 2007년 주춤하다가 매년 오르는 추세다.
year_price = aggregate(data$transaction_real_price, FUN = 'mean', by = list(data$year))
plot(year_price)

# year_of_completion
hist(data$year_of_completion) # 2000년대를 중심으로 많이 지었다.
# year_of_completion vs transaction_real_price : 
# 70년대 중반에 지어진 건물 중 실거래가가 높은 건물이 있다. 옛날 아파트인데 재건축을 기다리는 것으로 보인다.
yc_price = aggregate(data$transaction_real_price, FUN = 'mean', by = list(data$year_of_completion))
colnames(yc_price) = c('yc','price')
ggplot(data = yc_price, aes(x=yc,y=as.integer.integer64(price)))+geom_bar(stat = 'identity')

# 5. 결론(종속변수에 영향을 많이 주는 설명변수)
# 지역정보
#   구에따라 실거래가의 차이가 많이 나는 것으로 보인다. 강남3구(송파구,강남구,서초구)의 평균 실거래가가 가장 높다.
#   그중에서도  압구정동, 청담동, 용산동, 한남동, 회현동의 평균 실거래가가 높게 나타났다.
# 
# 면적정보
#   면적이 커질수록 실거래가가 증가하는 추세다.  80-100평에서 최대치가 존재하며, 30평 구간에도 높은 가격대가 보인다.
#   재건축을 기다리는 아파트로 보인다.
#   0-5층 건물이 가장 많다. 층수가 높을수록 실거래가가 증가하는 추세다. 다만 0-5층 건물도 실거래가가 높은 데이터가 있다.
#   15억이상, 0-5층, 1990년 이전 준공 데이터의 위치를 확인하니 반포동, 압구정동, 대치동으로 확인됐다. 
# 
# 날짜정보
#   80 - 90 년대까지 실거래가 평균이 감소한다.80-90년대 베이비붐으로 인구가 증가하면서 양산형 아파트를 많이 지었는데, 지금은 가격이 떨어진 것으로 보인다.
#   90년대 이후부터는 준공년도가 최근일수록 실거래가가 증가한다.
#   3월에 이사를 많이가고, 11-2월은 이사를 적게 간다. 나머지 월은 비슷하다.
