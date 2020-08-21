# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q2
# Purpose:      Imputation
# programmer:   Zhe Liu
# date:         2020-08-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Imputing inside existing provinces ----
raw.imp <- raw.total %>% 
  # filter(!(province %in% c('福建'))) %>% 
  mutate(quarter = stri_sub(quarter, 5, 6), 
         month = stri_sub(date, 5, 6))

# quarterly date continuity
date.continuity <- raw.imp %>% 
  distinct(province, city, district, pchc, market, year, quarter, month) %>% 
  count(province, city, district, pchc, market, year, quarter) %>% 
  pivot_wider(names_from = year, 
              values_from = n, 
              values_fill = 0) %>% 
  mutate(cnt_min = pmin(`2019`, `2020`), 
         cnt_max = pmax(`2019`, `2020`))

# city molecule yearly growth
city.growth <- date.continuity %>% 
  filter(cnt_min >= 2) %>% 
  inner_join(raw.imp, 
             by = c("quarter", "province", "city", "district", "pchc", "market")) %>% 
  group_by(province, city, year, quarter, market, atc3, molecule) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year, 
              values_from = sales, 
              values_fill = 0) %>% 
  mutate(growth = `2020` / `2019`,
         growth = if_else(is.na(growth) | growth < 0.1 | growth > 10, 
                          1, 
                          growth)) %>% 
  select(quarter, province, city, market, atc3, molecule, growth)

# imputing
imputing.data <- date.continuity %>% 
  filter(cnt_max >= 2) %>% 
  left_join(raw.imp, 
            by = c("quarter", "province", "city", "district", "pchc", "market")) %>% 
  group_by(year, quarter, month, province, city, district, pchc, market, atc3, 
           molecule, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year, 
              values_from = sales, 
              values_fill = -1) %>% 
  left_join(city.growth, 
            by = c("quarter", "province", "city", "market", "atc3", "molecule")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         flag_2019 = if_else(`2019` == -1, 1, 0),
         flag_2020 = if_else(`2020` == -1, 1, 0),
         sales_2019 = if_else(flag_2019 == 1, `2020` / growth, `2019`),
         sales_2020 = if_else(flag_2020 == 1, `2019` * growth, `2020`)) %>% 
  pivot_longer(flag_2019:sales_2020, 
               names_to = 'type', 
               values_to = 'value') %>% 
  separate(type, c("type", "year"), sep = "_") %>% 
  select(year, quarter, month, province, city, district, pchc, market, atc3, 
         molecule, packid, type, value) %>% 
  pivot_wider(names_from = type, 
              values_from = value) %>% 
  mutate(date = stri_paste(year, month),
         quarter = stri_paste(year, quarter)) %>% 
  select(year, date, quarter, province, city, district, pchc, market, atc3, 
         molecule, packid, sales_imp = sales, flag)

# joint
imputed.data <- raw.total %>% 
  full_join(imputing.data, 
            by = c("year", "date", "quarter", "province", "city", "district", 
                   "pchc", "market", "atc3", "molecule", "packid")) %>% 
  mutate(sales = if_else(is.na(sales), sales_imp, sales),
         flag1 = if_else(is.na(flag), 0, flag)) %>% 
  select(year, date, quarter, province, city, district, pchc, market, atc3, 
         molecule, packid, sales, flag1)

# imputation result
imp.total <- imputed.data

write_feather(imp.total, '03_Outputs/02_Servier_CHC_Imputation.feather')


##---- Imputing outside existing provinces ----
# model data
model.data <- raw.history %>% 
  filter(city %in% target.city, year %in% c('2018', '2019')) %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, atc3, 
           molecule, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# sample data
model.sample <- model.data %>% 
  filter(province %in% c("安徽", "福建", "江苏", "浙江")) %>% 
  mutate(flag = if_else(province %in% c("福建"), 0, 1))

# summarising pack ID by PCHC
sample.pchc <- model.sample %>% 
  group_by(province, city, district, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# model
model.set <- sample.pchc %>% 
  pivot_wider(names_from = date, 
              values_from = sales)
model.set[is.na(model.set)] <- 0

train.set <- model.set[model.set$flag == 1, ]
test.set <- model.set[model.set$flag == 0, ]

train.set.tmp <- train.set[, 5:29]
test.set.tmp <- test.set[, 5:29]

knn.model <- kknn(flag ~ ., train = train.set.tmp, test = test.set.tmp, 
                  k = 3, scale = TRUE)

# weightage extraction 1
model.indice <- as.data.frame(knn.model$C) %>% 
  lapply(function(x) {
    train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  bind_cols(test.set[, 1:4]) %>% 
  pivot_longer(pchc_1:pchc_3, 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

model.weight <- as.data.frame(knn.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
         pchc_1 = pchc_1 / weight_sum,
         pchc_2 = pchc_2 / weight_sum,
         pchc_3 = pchc_3 / weight_sum) %>% 
  bind_cols(test.set[, 1:4]) %>% 
  select(-weight_sum) %>% 
  pivot_longer(pchc_1:pchc_3, 
               names_to = 'knn_level', 
               values_to = 'knn_weight')

# ah, bj, js - 1910~1912
# ah.bj.js <- raw.total %>% 
#   filter(province == "安徽") %>% 
#   bind_rows(total.in.imp) %>% 
#   filter(province %in% c("安徽", "北京", "江苏"), date %in% c("201910","201911","201912"))

# sample growth
sample.sales <- raw.total %>% 
  filter(province %in% c("安徽", "江苏", "浙江"), 
         quarter %in% c("2019Q1", "2019Q2", "2020Q1", '2020Q2')) %>% 
  group_by(knn_pchc = pchc, molecule, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

sample.growth <- model.indice %>% 
  left_join(model.weight, 
            by = c("province", "city", "district", "pchc", "knn_level")) %>% 
  inner_join(sample.sales, by = c("knn_pchc")) %>% 
  group_by(pchc, molecule, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = quarter, 
              values_from = sales) %>% 
  mutate(growth_2019Q1 = `2020Q1` / `2019Q1`, 
         growth_2019Q2 = `2020Q2` / `2019Q2`) %>% 
  select(pchc, molecule, growth_2019Q1, growth_2019Q2) %>% 
  pivot_longer(cols = starts_with('growth'), 
               names_to = 'quarter', 
               values_to = 'growth', 
               values_drop_na = TRUE) %>% 
  mutate(quarter = stri_sub(quarter, -6, -1))

# imputation
imp.total <- raw.total %>% 
  filter(quarter %in% c("2019Q1", "2019Q2"),
         province %in% c("福建")) %>% 
  left_join(sample.growth, by = c("quarter", "pchc", "molecule")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(sales = sales * growth,
         date = gsub("2019", "2020", date),
         quarter = gsub("2019", "2020", quarter),
         year = "2020")


















