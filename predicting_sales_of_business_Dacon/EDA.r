# ���� ������ ���ϴ� ������ �ڵ� (���̽��� �ƴ�R), ��� ����� ������ �����Ͻÿ�.
# ũ���� ī�� ��� ���� ������ data_credit.csv�� ����Ͽ� ������ ������ Ž���� �м� EDA�� �����Ͻÿ�.

# ������ �ҷ�����
data = fread("data/data_credit.csv",header =  T, stringsAsFactors = F)
summary(data) # 3362796 x 8

# 100�������� ���ø�
set.seed(1)
idx <- sample(1:nrow(data), 1000000)
data_samp <-data[idx,]

# 1 ������ Ŭ���� & ����ġ ó��
# installments�� 99%�� ����ġ��
null_table = sapply(data_samp, function(x) mean(is.na(x)))
null_table

# insatallment�� na�� ��� 1�� ��ġ(�Ͻú�)
summary(data_samp$installments)
data$installments[is.na(data_samp$installments)]<-1
sapply(data_samp, function(x) mean(is.na(x))) # ???? ?? ?????? ????

# 2. �Ϻ��� �м� & �ð�ȭ
cols = colnames(data_samp)

# 2.1. store_id
# ���� ���� Ȯ�� : 1775�� ����
n_stores = length(unique(data_samp$store_id))

# q1. ������ ���� �ŷ�Ƚ���� ���̰� �ִ°�?
# ������ ���� �ŷ�Ƚ���� ���̰� �����ϴ� ������ ���δ�.
hist(data_samp$store_id,breaks = 1775/30)

# 2.2. date : Year - Month - Date
# q2. ��¥�� ���� �ŷ�Ƚ���� ���̳��°�?
data_samp$date_converted <- as.Date(data_samp$date) # charactor to date
range(data_samp$date_converted) #20160801 ~ 20180731

# year, month column ????
data_samp$year <-year(data_samp$date_converted)
data_samp$month <-month(data_samp$date_converted)

hist(data_samp$date_converted,breaks = 365) # day/2 : �ŷ�Ƚ���� �����ϴ� �߼�
barplot(table(data_samp$month)) # month : 1,2�� / 3��-7�� / 8��- 12�� �������� �ŷ�Ƚ�� ���̰� ��Ÿ����.
barplot(table(data_samp$year)) # 2017�� �ŷ�Ƚ���� ���� ����.

# 2.3. time
# q3. �ð��뿡 ���� �ŷ�Ƚ�� ���̰� �����ϴ°�?
# ��ħ(06-11) : �߰�
# ����(12-14) : 12�ÿ� �ִ�, ���� ����
# ����(15-17) : ���� or ����
# ����(18-20) : 19�ÿ� �ִ�, ���� ����
# ��, ����(21-05) : ����
data_samp$hours <-sapply(data_samp$time, function(time) format(strptime(time,"%H:%M:%S"),'%H')) # hours column �߰�?
barplot(table(data_samp$hours))

# 2.4. card_id
# card_id ���� Ȯ�� - 10�ڸ�
# q.6 �ŷ����� ���� ī�� ���̵� �з��� �� �ִ°�
n_card_id = length(unique(data_samp$card_id)) # 922522 ����

# 2.5. amount
# q.5 �ŷ� �ݾ׿� ���� �ŷ��� ���̰� �ִ°�
# log amount�� ���� ������ ����� ���´�
hist(log(data_samp$amount+1))

summary(data_samp$amount) # ����(ȯ�� ������ ����)
# ȯ�ҵ����� ����
amount_neg = data_samp$amount[which(data_samp$amount<0)]
hist(abs(amount_neg))
summary(abs(amount_neg)) # 75% �� 450 ����, max = 135980
# ȯ�� �ŷ� ������ ����
ratio_pos_neg = length(amount_neg)/nrow(data_samp) # 1%

# 2.6. day of week, holiday
# q.6 ���ϰ� �ָ��� �� ����Ƚ���� ������?
# �ָ�/���� �� ū ���̰� ���� ���δ�. ��/�䰡 �ణ ����.
# ���Ͽ� �� ��� �ŷ�Ƚ���� �� ����.

barplot(table(data_samp$days_of_week))

n_not_holyday = length(unique(data_samp$date[which(data_samp$holyday == 0)])) # ���Ͼ� �ƴ� �� : 693
n_holyday = length(unique(data_samp$date[which(data_samp$holyday == 1)])) # �q��: 37
mean_not_holyday = length(data_samp$date[which(data_samp$holyday == 0)]) / n_not_holyday # ���� �� ��� �ŷ� Ƚ�� : 1379
mean_holyday = length(data_samp$date[which(data_samp$holyday == 1)]) / n_holyday # ���� �� ��� �ŷ� Ƚ�� : 1198

# 2.7. Installments
hist(data_samp$installments) # ����ġ�� 1�� ä����, 1�� ���� ����.(99%)

cols = colnames(data_samp)

#3. �̺��� �м� & �ð�ȭ

# 3.1. store_id vs amount
# ���庰 �� ���� : ��ü������ ������ ������ �ܻ�ġ�� �����Ѵ�.
amount_store = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$store_id))
plot(amount_store)
summary(amount_store)

plot(amount_store[which(amount_store$x<94881),]) # 3Q ���ϸ� ���
plot(amount_store[which(amount_store$x>94881),]) # 3Q �̻�

# 3.2. date vs amount
# ���� �� ���� : 12-2 / 3-7 / 8-11 3�� �������� ������ �ִ�.
amount_month = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$month))
plot(amount_month)
abline(v = c(2,7,11), col = 'red')

# ���� �� ���� : 2017 > 2018 > 2016
amount_year = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$year))
plot(amount_year)

# 3.3. time vs amount
# �ð��뺰 �� ���� : 0-5/6-11/12-16/17-20/21-23 �� 5�� �������� ���� �� �ִ�.
amount_time = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$hours))
plot(amount_time)
abline(v = c(5,11,16,20), col = 'red')

# 3.4. days_of_week vs amount
# �� - �� / ��,�� / �� 3�� �������� ���� �� �ִ�.  �Ͽ����� �� ������ ���� ����.
amount_dow = aggregate(data_samp$amount, FUN = 'sum', by = list(data_samp$days_of_week))
plot(amount_dow)

