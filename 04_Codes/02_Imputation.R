# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q2
# Purpose:      Imputation
# programmer:   Zhe Liu
# date:         2020-08-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Imputing inside existing provinces ----
raw.imp <- raw.total %>% 
  filter(city %in% target.city) %>% 
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


# QC
chk <- imp.total %>% 
  group_by(city, market, quarter) %>% 
  summarise(sales = sum(sales)) %>% 
  ungroup() %>% 
  arrange(city, market, quarter)


