dat_vacant <- read.csv('./data/서울교통공사_지하상가 임대정보_20230331.csv',
                       fileEncoding = 'CP949')

## 결측치 처리하기
str(dat_vacant)
#dat_vacant[is.na(dat_vacant)] <- 0

#library(zoo)
#dat_vacant$면적.제곱미터. <- na.approx(dat_vacant$면적.제곱미터.)

summary(dat_vacant$면적.제곱미터.)
summary(dat_vacant$월임대료)

# 결측치를 평균으로 대체하기
dat_vacant$면적.제곱미터.[is.na(dat_vacant$면적.제곱미터.)] <- mean(dat_vacant$면적.제곱미터., na.rm = T)
dat_vacant$월임대료[is.na(dat_vacant$월임대료)] <- mean(dat_vacant$월임대료, na.rm = T)


## 역별 공실률
library(dplyr)

result_train <- dat_vacant %>%
  group_by(`역명`) %>%
  summarise(VacantRatio = round(sum(`상가유형` == '공실') / n(), 2),
            rental_fee = sum(`월임대료`),
            store_area = round(sum(`면적.제곱미터.`), 2))


## 노선별 공실률
result_subway <- dat_vacant %>%
  group_by(`호선`) %>%
  summarise(VacantRatio = round(sum(`상가유형` == '공실') / n(), 2),
            rental_fee = sum(`월임대료`),
            store_area = round(sum(`면적.제곱미터.`), 2))


## 노선별 지하철 공실률 막대 그래프
library(ggplot2)
library(extrafont)
ggplot(result_subway, aes(x = `호선`, y = VacantRatio)) +
  geom_bar(stat='identity', fill = "skyblue", colour = 'black') +
  labs(x = "", y = "") +
  theme_bw(base_family = 'KoPubWorldDotum Medium') +
  ggtitle('노선별 공실률 막대 그래프') +
  theme(plot.title = element_text(hjust=0.5, size=20),
        axis.text.x = element_text(size=15),
        axis.text.y = element_text(size=15))


## 역별 공실률 막대 그래프
subway_vacant <- merge(result_train, dat_vacant[,c('호선', '역명')], by='역명')
a_rows <- duplicated(subway_vacant)
subway_vacant <- subset(subway_vacant, !a_rows)
row.names(subway_vacant) <- NULL
subway_vacant <- subway_vacant[-224,]
row.names(subway_vacant) <- NULL


# 공실률이 높은 역 확인하기
subway_vacant2 <- subway_vacant %>%
  arrange(desc(VacantRatio))

# 공실률 상위 10개 역 확인하기
head(subway_vacant2, 10)


## 6호선이 공실률 가장 높음 -> 상위 10개 중 3개 역이 6호선


# 공실률이 0이 아닌 데이터들만 추출하기
subway_vacant_not_0 <- subway_vacant2[subway_vacant2$VacantRatio != 0, ]

subway_vacant_not_0_2 <- head(subway_vacant_not_0, 35)

# 시각화
library(RColorBrewer)

pal <- brewer.pal(8, 'Set3')

ggplot(arrange(subway_vacant_not_0_2, desc(VacantRatio)),
       aes(x = reorder(`역명`, -VacantRatio), y = VacantRatio, fill = `호선`)) +
  geom_bar(stat='identity', colour = 'black', position = 'dodge') +
  labs(x = "", y = "", fill='노선명') +
  scale_fill_manual(values=c("#0052A4", "#3B9F37", "#FF851B", "#3165A8",
                             "#703E8C", "#904D23", "#5B692E", "#C82363")) +
  theme_bw(base_family = 'KoPubWorldDotum Medium') +
  ggtitle('역별 공실률 막대 그래프') +
  theme(plot.title = element_text(hjust=0.5, size=30),
        axis.text.x = element_text(angle=90, size=20),
        axis.text.y = element_text(size=15))



## 공실률과 타 변수 간의 관계

# 공실률이 0이 넘는 데이터만들 대상으로 진행
cor(subway_vacant_not_0$VacantRatio, subway_vacant_not_0$rental_fee)
cor(subway_vacant_not_0$VacantRatio, subway_vacant_not_0$store_area)

# 두 변수 모두 p-value가 0.05보다 작아 통계적으로 유의함
  # 임대료와 상가면적은 공실률에 영향을 미침(반비례 관계)
summary(lm(VacantRatio ~ rental_fee, data=subway_vacant_not_0))
summary(lm(VacantRatio ~ store_area, data=subway_vacant_not_0))


# 회귀분석 시각화

# 임대료
ggplot(data=subway_vacant_not_0, aes(x=rental_fee, y=VacantRatio)) +
  geom_point(size=3, fill='skyblue', shape=21) +
  theme_bw(base_family = 'KoPubWorldDotum Medium') +
  scale_x_continuous(labels = scales::comma_format(scale = 0.0001)) +
  ggtitle('임대료와 공실률 간의 관계') +
  labs(x='임대료(단위:1만원)', y='공실률') +
  theme(plot.title = element_text(hjust = 0.5, size=20,
                                  family = 'KoPubWorldDotum Bold'),
        axis.title = element_text(size=15)) +
  geom_smooth(method = lm, se = F)


# 상가면적
ggplot(data=subway_vacant_not_0, aes(x=store_area, y=VacantRatio)) +
  geom_point(size=3, fill='skyblue', shape=21) +
  theme_bw(base_family = 'KoPubWorldDotum Medium') +
  ggtitle('상가면적과 공실률 간의 관계') +
  labs(x='상가면적(m²)', y='공실률') +
  theme(plot.title = element_text(hjust = 0.5, size=20,
                                  family = 'KoPubWorldDotum Bold'),
        axis.title = element_text(size=15)) +
  geom_smooth(method = lm, se = F)



