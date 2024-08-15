### 연령별 연관분석

sort(unique(float_pop_8$나이))

# 10대
float_pop_8_age10 <- float_pop_8[float_pop_8$나이 %in% c(0, 10, 15), ]

trans_age10 <- float_pop_8_age10[float_pop_8_age10$이동유형 != 'HW' & float_pop_8_age10$이동유형 != 'WH', c(11,12)]
write.csv(file = './data/8시 10대 유동인구 연관분석.csv', trans_age10, row.names = F)

trans_age10_2 <- read.transactions('./data/8시 10대 유동인구 연관분석.csv', header = T,
                                   rm.duplicates = T, sep = ',')

age10_rules <- apriori(trans_age10_2, list(support = 0.005, confidence = 0.01, 
                                           minlen = 2, maxlen = 15))

inspect(sort(age10_rules, by = "lift")[1:10])

rule.list = as.data.frame(inspect(age10_rules))

colnames(rule.list)[2] = "Direction"

rule.list_2 = rule.list %>% arrange(-lift)

RULE = rule.list_2 %>%
  mutate(Rules = paste0(lhs, Direction, rhs))

LIST = list()

for(k in 1:nrow(RULE)){
  LIST[[k]] = sort(unlist(strsplit(RULE$Rules[k], "=>")))
}

print(LIST)

D_LIST = duplicated(LIST)
RULE2 = RULE[D_LIST, ]
head(RULE2, 10)


# 20대
float_pop_8_age20 <- float_pop_8[float_pop_8$나이 %in% c(20, 25), ]
trans_age20 <- float_pop_8_age20[float_pop_8_age20$이동유형 != 'HW' & float_pop_8_age20$이동유형 != 'WH', c(11,12)]
write.csv(file = './data/8시 20대 유동인구 연관분석.csv', trans_age20, row.names = F)

trans_age20_2 <- read.transactions('./data/8시 20대 유동인구 연관분석.csv', header = T,
                                   rm.duplicates = T, sep = ',')

age20_rules <- apriori(trans_age20_2, list(support = 0.005, confidence = 0.01, 
                                           minlen = 2, maxlen = 15))

inspect(sort(age20_rules, by = "lift")[1:10])

rule.list = as.data.frame(inspect(age20_rules))

colnames(rule.list)[2] = "Direction"

rule.list_2 = rule.list %>% arrange(-lift)

RULE = rule.list_2 %>%
  mutate(Rules = paste0(lhs, Direction, rhs))

LIST = list()

for(k in 1:nrow(RULE)){
  LIST[[k]] = sort(unlist(strsplit(RULE$Rules[k], "=>")))
}

print(LIST)

D_LIST = duplicated(LIST)
RULE2 = RULE[D_LIST, ]
head(RULE2, 10)


# 30대
float_pop_8_age30 <- float_pop_8[float_pop_8$나이 %in% c(30, 35), ]
trans_age30 <- float_pop_8_age30[float_pop_8_age30$이동유형 != 'HW' & float_pop_8_age30$이동유형 != 'WH', c(11,12)]
write.csv(file = './data/8시 30대 유동인구 연관분석.csv', trans_age30, row.names = F)

trans_age30_2 <- read.transactions('./data/8시 30대 유동인구 연관분석.csv', header = T,
                                   rm.duplicates = T, sep = ',')

age30_rules <- apriori(trans_age30_2, list(support = 0.005, confidence = 0.01, 
                                           minlen = 2, maxlen = 15))

inspect(sort(age30_rules, by = "lift")[1:10])

rule.list = as.data.frame(inspect(age30_rules))

colnames(rule.list)[2] = "Direction"

rule.list_2 = rule.list %>% arrange(-lift)

RULE = rule.list_2 %>%
  mutate(Rules = paste0(lhs, Direction, rhs))

LIST = list()

for(k in 1:nrow(RULE)){
  LIST[[k]] = sort(unlist(strsplit(RULE$Rules[k], "=>")))
}

print(LIST)

D_LIST = duplicated(LIST)
RULE2 = RULE[D_LIST, ]
head(RULE2, 10)


# 40대
float_pop_8_age40 <- float_pop_8[float_pop_8$나이 %in% c(40, 45), ]
trans_age40 <- float_pop_8_age40[float_pop_8_age40$이동유형 != 'HW' & float_pop_8_age40$이동유형 != 'WH', c(11,12)]
write.csv(file = './data/8시 40대 유동인구 연관분석.csv', trans_age40, row.names = F)

trans_age40_2 <- read.transactions('./data/8시 40대 유동인구 연관분석.csv', header = T,
                                   rm.duplicates = T, sep = ',')

age40_rules <- apriori(trans_age40_2, list(support = 0.005, confidence = 0.01, 
                                           minlen = 2, maxlen = 15))

inspect(sort(age40_rules, by = "lift")[1:10])

rule.list = as.data.frame(inspect(age40_rules))

colnames(rule.list)[2] = "Direction"

rule.list_2 = rule.list %>% arrange(-lift)

RULE = rule.list_2 %>%
  mutate(Rules = paste0(lhs, Direction, rhs))

LIST = list()

for(k in 1:nrow(RULE)){
  LIST[[k]] = sort(unlist(strsplit(RULE$Rules[k], "=>")))
}

print(LIST)

D_LIST = duplicated(LIST)
RULE2 = RULE[D_LIST, ]
head(RULE2, 10)


# 50대
float_pop_8_age50 <- float_pop_8[float_pop_8$나이 %in% c(50, 55), ]
trans_age50 <- float_pop_8_age50[float_pop_8_age50$이동유형 != 'HW' & float_pop_8_age50$이동유형 != 'WH', c(11,12)]
write.csv(file = './data/8시 50대 유동인구 연관분석.csv', trans_age50, row.names = F)

trans_age50_2 <- read.transactions('./data/8시 50대 유동인구 연관분석.csv', header = T,
                                   rm.duplicates = T, sep = ',')

age50_rules <- apriori(trans_age50_2, list(support = 0.005, confidence = 0.01, 
                                           minlen = 2, maxlen = 15))

inspect(sort(age50_rules, by = "lift")[1:10])

rule.list = as.data.frame(inspect(age50_rules))

colnames(rule.list)[2] = "Direction"

rule.list_2 = rule.list %>% arrange(-lift)

RULE = rule.list_2 %>%
  mutate(Rules = paste0(lhs, Direction, rhs))

LIST = list()

for(k in 1:nrow(RULE)){
  LIST[[k]] = sort(unlist(strsplit(RULE$Rules[k], "=>")))
}

print(LIST)

D_LIST = duplicated(LIST)
RULE2 = RULE[D_LIST, ]
head(RULE2, 10)


# 60대 이상
float_pop_8_age60 <- float_pop_8[float_pop_8$나이 %in% c(60, 65, 70, 75, 80), ]
trans_age60 <- float_pop_8_age60[float_pop_8_age60$이동유형 != 'HW' & float_pop_8_age60$이동유형 != 'WH', c(11,12)]
write.csv(file = './data/8시 60대 유동인구 연관분석.csv', trans_age60, row.names = F)

trans_age60_2 <- read.transactions('./data/8시 60대 유동인구 연관분석.csv', header = T,
                                   rm.duplicates = T, sep = ',')

age60_rules <- apriori(trans_age60_2, list(support = 0.005, confidence = 0.01, 
                                           minlen = 2, maxlen = 15))

inspect(sort(age60_rules, by = "lift")[1:10])

rule.list = as.data.frame(inspect(age60_rules))

colnames(rule.list)[2] = "Direction"

rule.list_2 = rule.list %>% arrange(-lift)

RULE = rule.list_2 %>%
  mutate(Rules = paste0(lhs, Direction, rhs))

LIST = list()

for(k in 1:nrow(RULE)){
  LIST[[k]] = sort(unlist(strsplit(RULE$Rules[k], "=>")))
}

print(LIST)

D_LIST = duplicated(LIST)
RULE2 = RULE[D_LIST, ]
head(RULE2, 10)
