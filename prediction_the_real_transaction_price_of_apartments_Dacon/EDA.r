# ���� ������ ���ϴ� ������ �ڵ� (���̽��� �ƴ�R), ��� ����� ������ �����Ͻÿ�.
# ������ ����Ʈ���� ������ example_zigbang.csv�� ����Ͽ� ������ ������ Ž���� �м� EDA�� �����Ͻÿ�.

# 1. ������ �ҷ�����
library(data.table)
data = fread('data/example_zigbang.csv',header = T, stringsAsFactors = F)
summary(data)
data = data[which(data$city==1),] # ���� �����͸� ���
                
# 2. ����ġ Ȯ��, ������ Ŭ����
na_table = sapply(data, function(col) mean(is.na(col)))
na_table[which(na_table>0)]

# 2.1. total_parking_capacity_in_site
# ��������� ������� �������� ���̴�. cor = 0.90
household_parking = data[,c('total_household_count_in_sites','total_parking_capacity_in_site')]
cor(household_parking,use="complete.obs",method = 'pearson') # cor = 0.91
# scatter.smooth(household_parking)

# ����ȸ�ͷ� ��ġ
train = household_parking[which(is.na(household_parking$total_parking_capacity_in_site)==F)]
test =  household_parking[-train,]
lm_mod = lm(total_parking_capacity_in_site~total_household_count_in_sites, data = train)
y_pred = predict(lm_mod,test)

data[which(is.na(data$total_parking_capacity_in_site)==T),]$total_parking_capacity_in_site = y_pred
scatter.smooth(data[,c('total_parking_capacity_in_site','total_household_count_in_sites')])
mean(is.na(data$total_parking_capacity_in_site))

# 2.2. tallest_building_in_site, lowest_building_in_site
# �Ѵ� 15���� ���� ����, 15������ ��ġ
sort(table(data$tallest_building_in_sites))
sort(table(data$lowest_building_in_sites))

data[which(is.na(data$tallest_building_in_sites)==T)]$tallest_building_in_sites = 15
data[which(is.na(data$lowest_building_in_sites)==T)]$lowest_building_in_sites = 15

# 2.3. room_count, bathroom_count
# ������� ��ġ
# install.packages('imputeTS')
tmp = colMeans(data[,c('room_count','bathroom_count')],na.rm = T)
data[which(is.na(data$room_count)==T)]$room_count = tmp[1]
data[which(is.na(data$bathroom_count)==T)]$bathroom_count = tmp[2]

na_table = sapply(data, function(col) mean(is.na(col)))
na_table

# 3. EDA, ���� ��ȯ
# 3.1. transaction_year_month -> year, month
range(data$transaction_year_month) # ��¥ ���� Ȯ�� : 200601 - 201810
data$year = sapply(data$transaction_year_month, function(date) as.numeric(substring(toString(date),1,4)))
data$month = sapply(data$transaction_year_month, function(date) as.numeric(substring(toString(date),5,6)))

# 3.2. transaction_date ���� Ŭ����
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


# 3.3. transaction_real_price �α׺�ȯ
hist(as.integer(data$transaction_real_price))
data$log_price <- log(data$transaction_real_price)
hist(data$log_price)

# 3.4. ���뺰 ��������� �ش��ϴ� �Ļ����� �����
# ���뺰 ������� = ������� / �����
data$parking_per_household  = data$total_parking_capacity_in_site/data$total_household_count_in_sites
hist(data$parking_per_household)

# 4. ���� ������ ���Ӻ����� ����� Ư���� �� ���� ���, Ž��, �ð�ȭ �Ѵ�. 
cols = colnames(data)

# 4.1. ���Ӻ��� : log_price : �뷫 ���Ժ����� ������
hist(data$log_price)


# 4.2. ������
#
# ���� ���� : city, latitude, longitude, address_by_law. 
#
# �ǳ�, ���� ���� :heat_type,heat_fuel,room_id,room_count,bathroom_count,front_door_structure
#
# ���� ���� : exclusive_use_area, floor, supply_area
#
# �ǿ� ���� : apartment_id, total_parking_capacity_in_site,
# total_household_count_in_sites,apartment_building_count_in_sites,parking_per_household
# tallest_building_in_sites, lowest_building_in_sites, total_household_count_of_area_type,
# parking_per_household
# 
# ��¥ ���� : transaction_date, transaction_year_month, year_of_completion, year, month

# 4.2.1. ��������
# address_by_law : �����ڵ� 10�ڸ� : �õ�(2)+ �ñ���(3) + ���鵿(3)+ ��(2)
data$gu <- sapply(data$address_by_law, function(address) substr(as.character.integer64(address),3,5))
# �ٸ� ���� �ߺ��� �� �ڵ尡 ����, ���� ��+������ �����ؾ���
data$dong <- sapply(data$address_by_law, function(address) substr(as.character.integer64(address),3,8))

# ���� ���� �ǰŷ��� ���
# ��� �ǰŷ��� ���� 3����
# 710(���ı�), 680(������),650(���ʱ�)
length(unique(data$gu)) # ����� 25�� ��ġ��
gu_price = aggregate(data$transaction_real_price, by = list(data$gu), FUN = 'mean')
colnames(gu_price)<-c('gu','price')
ggplot(data=gu_price, aes(x=gu,y=as.integer.integer64(price)))+geom_bar(stat = 'identity')

# ���� ���� �ǰŷ��� ���
# �ǰŷ��� ��� ���� 5�� 
# 680110(������ �б�����), 170127(��걸 ��굿), 170131(��걸 �ѳ���), 170116(��걸 û�ϵ�),140122(�߱� ȸ����)
length(unique(data$dong)) # 264����
dong_price = aggregate(data$transaction_real_price, by = list(data$dong), FUN = 'mean')
colnames(dong_price)<-c('dong','price')
ggplot(data=dong_price, aes(x=dong,y=as.integer.integer64(price)))+geom_bar(stat = 'identity')
dong_price[order(dong_price$price, decreasing = T),]

# 4.2.2. �ǳ�, ��������
# room_count, �� 3��¥���� ����
barplot(table(data$room_count))

# bathroom_count, 1��, 2���� ����
barplot(table(data$bathroom_count))

# 4.2.3. ��������
# exclusive_area vs supply_area : corr = 0.98
# ���޸���(supply_area) = �������(exclusive_area) + �ְŰ������(����, ����������)
cor(data$exclusive_use_area, data$supply_area, method = 'pearson')
hist(data$exclusive_use_area) # �ֺ� : 50-60, 80-90
hist(data$supply_area) # �ֺ� : 100-110

# �ְŰ������
data$residential_area = data$supply_area-data$exclusive_use_area
hist(data$residential_area) # �ֺ� : 20-30

# ���޸����� ��������� ������� area ���� ����
data$area = (data$supply_area+data$exclusive_use_area)/2

# area vs transcation_real_price
# ������ Ŀ������ �ǰŷ����� �������� �߼�( cor = 0.63)
# 270-300(80-90��) �������� �ǰŷ����� ���� ���� ���� ����(9���̻�)

plot(data$area, data$transaction_real_price)
cor(data$area, data$transaction_real_price, method = 'pearson') # 0.63

# floor
hist(data$floor) # �ֺ� : 0-5��
# floor vs transaction_real_price 
# ������ �������� �ǰŷ����� �����ϴ� �߼�.
# 0-5 ���ǹ� �� ������ ���� �ǹ��� �ִ�. ������� ��ٸ��� ����Ʈ�� ���δ�.
floor_price = aggregate(data$transaction_real_price, FUN = 'mean' , by = list(data$floor))
plot(floor_price)
# 0-5���ε�, 15�� �̻���,�ذ� 1990�� ����
# 650107(���ʱ�������), 680110(�������б�����), 680106(��������ġ��)
# ������� �յ� ����Ʈ�� ������ �÷����� ������ �ҵ�
expensive_area = data[which( (data$transaction_real_price>1500000000)&(data$floor<5)&((data$year_of_completion<1990)) ),]$dong
tmp = sort(table(expensive_area),decreasing = T)
barplot(tmp)

# 4.2.4. �ǿ�����
# apartment_id
hist(data$apartment_id) # �ֺ� : 0-200����
# total_household_count_in_sites
hist(data$total_household_count_in_sites) # �ֺ� : 0-1000����
# apartment_building_count_in_sites
hist(data$apartment_building_count_in_sites) # �ֺ� : 0-15 ��
# parking_per_household
hist(data$parking_per_household) # ��κ� 2 ����

# 4.2.5. ��¥����
# month : 3���� �̻縦 ���̰���, 11-2���� �̻縦 ���� ����. �������� �����
barplot(table(data$month))

# year vs transaction_real_price : 2007�� �����ϴٰ� �ų� ������ �߼���.
year_price = aggregate(data$transaction_real_price, FUN = 'mean', by = list(data$year))
plot(year_price)

# year_of_completion
hist(data$year_of_completion) # 2000��븦 �߽����� ���� ������.
# year_of_completion vs transaction_real_price : 
# 70��� �߹ݿ� ������ �ǹ� �� �ǰŷ����� ���� �ǹ��� �ִ�. ���� ����Ʈ�ε� ������� ��ٸ��� ������ ���δ�.
yc_price = aggregate(data$transaction_real_price, FUN = 'mean', by = list(data$year_of_completion))
colnames(yc_price) = c('yc','price')
ggplot(data = yc_price, aes(x=yc,y=as.integer.integer64(price)))+geom_bar(stat = 'identity')

# 5. ���(���Ӻ����� ������ ���� �ִ� ������)
# ��������
#   �������� �ǰŷ����� ���̰� ���� ���� ������ ���δ�. ����3��(���ı�,������,���ʱ�)�� ��� �ǰŷ����� ���� ����.
#   ���߿�����  �б�����, û�㵿, ��굿, �ѳ���, ȸ������ ��� �ǰŷ����� ���� ��Ÿ����.
# 
# ��������
#   ������ Ŀ������ �ǰŷ����� �����ϴ� �߼���.  80-100�򿡼� �ִ�ġ�� �����ϸ�, 30�� �������� ���� ���ݴ밡 ���δ�.
#   ������� ��ٸ��� ����Ʈ�� ���δ�.
#   0-5�� �ǹ��� ���� ����. ������ �������� �ǰŷ����� �����ϴ� �߼���. �ٸ� 0-5�� �ǹ��� �ǰŷ����� ���� �����Ͱ� �ִ�.
#   15���̻�, 0-5��, 1990�� ���� �ذ� �������� ��ġ�� Ȯ���ϴ� ������, �б�����, ��ġ������ Ȯ�εƴ�. 
# 
# ��¥����
#   80 - 90 ������ �ǰŷ��� ����� �����Ѵ�.80-90��� ���̺������ �α��� �����ϸ鼭 ����� ����Ʈ�� ���� �����µ�, ������ ������ ������ ������ ���δ�.
#   90��� ���ĺ��ʹ� �ذ��⵵�� �ֱ��ϼ��� �ǰŷ����� �����Ѵ�.
#   3���� �̻縦 ���̰���, 11-2���� �̻縦 ���� ����. ������ ���� ����ϴ�.
