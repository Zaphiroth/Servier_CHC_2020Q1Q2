# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q2
# Purpose:      Projection
# programmer:   Zhe Liu
# date:         2020-08-18
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
# hospital universe
hospital.universe.raw <- pchc.universe %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            hospital = first(na.omit(`单位名称`)), 
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup()

hospital.universe <- raw.total %>% 
  distinct(province, city, district, pchc) %>% 
  bind_rows(hospital.universe.raw) %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district)), 
            est = first(na.omit(est))) %>% 
  ungroup()

# city segment
segment <- read_xlsx("02_Inputs/seg_45cities.xlsx") %>% 
  mutate(seg_city = if_else(city == "上海", paste0(city, district), city)) %>% 
  select(seg_city, seg = seg_up)

# sample range
sample.pick <- read_xlsx("02_Inputs/历史数据样本范围_13Cities.xlsx", sheet = 2)

# argument
sd.argument <- hospital.universe %>% 
  filter(province == "山东") %>% 
  # group_by(pchc) %>% 
  # summarise(province = first(na.omit(`省`)),
  #           city = first(na.omit(`地级市`)),
  #           est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  # ungroup() %>% 
  filter(est > 0) %>% 
  select(province, city, pchc, est)

proj.argument <- read_xlsx("02_Inputs/7省自变量.xlsx") %>% 
  filter(Est_DrugIncome_RMB > 0) %>% 
  distinct(province = `省`, city = `地级市`, pchc = PCHC_Code, 
           est = Est_DrugIncome_RMB) %>% 
  bind_rows(sd.argument) %>% 
  mutate(panel = if_else(pchc %in% raw.total$pchc, 1, 0),
         panel = if_else(is.na(est), 0, panel),
         panel_all = panel) %>% 
  left_join(hospital.universe[, c('province', 'city', 'district', 'pchc')], 
            by = c("province", "city", "pchc")) %>% 
  mutate(seg_city = if_else(city == "上海", paste0(city, district), city),
         seg_city = trimws(seg_city)) %>% 
  distinct(province, city, district, seg_city, pchc, est, panel, panel_all)

# outlier
outlier <- read_xlsx("02_Inputs/outlier.xlsx") %>% 
  unlist()

# projection flag
proj.flag <- read_xlsx("02_Inputs/ot2.xlsx") %>% 
  select(pchc = mapping, market = mkt) %>% 
  distinct() %>% 
  mutate(flag_ot = 1)

# projection factor
proj.factor <- read_xlsx("02_Inputs/factor4.xlsx") %>% 
  select("market" = "mkt", "seg", "factor")


##---- Projection without Fuzhou & Shanghai ----
# projection data
proj.raw <- imp.total %>% 
  filter(!(city %in% c("上海", "福州")), 
         quarter %in% c("2020Q1", "2020Q2"))

# quarter sales
proj.quarter <- proj.raw %>% 
  group_by(quarter, province, city, district, pchc, market, 
           atc3, molecule, packid) %>% 
  summarise(panel_sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# universe set
universe.set <- merge(distinct(proj.argument, province, city, district, seg_city, pchc), 
                      distinct(proj.raw, quarter)) %>% 
  left_join(distinct(proj.raw, province, city, district, 
                     market, atc3, molecule, packid), 
            by = c('province', 'city', 'district')) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  left_join(proj.argument, 
            by = c("province", "city", "district", "seg_city", "pchc")) %>% 
  left_join(proj.flag, by = c("pchc", "market")) %>% 
  mutate(panel = if_else(pchc %in% outlier, 0, panel),
         panel = if_else(!is.na(flag_ot), 0, panel),
         panel = if_else(!(pchc %in% sample.pick$mapping), 0, panel)) %>% 
  left_join(segment, by = "seg_city") %>% 
  mutate(seg = if_else(is.na(seg), 1, seg))

# filtered set
filtered.set <- universe.set %>% 
  filter(panel == 1) %>% 
  left_join(proj.quarter, 
            by = c("province", "city", "district", "pchc", "quarter", 
                   "market", "atc3", "molecule", "packid")) %>% 
  mutate(panel_sales = if_else(is.na(panel_sales), 0, panel_sales))

# projection parameter
proj.parm <- data.table(filtered.set)[, {
  ux <- mean(est)
  uy <- mean(panel_sales)
  slope <- uy / ux
  intercept <- 0
  predict_sales <- slope * est
  spearman_cor <- cor(panel_sales, predict_sales, method = "spearman")
  list(slope = slope, intercept = intercept, spearman_cor = spearman_cor)
}, by = list(city, quarter, packid, market, seg)]

# QC
chk <- universe.set %>% 
  inner_join(proj.quarter, by = c("quarter", "province", "city", "district", 
                                  "pchc", "market", "atc3", "molecule", "packid")) %>% 
  mutate(panel_sales = if_else(is.na(panel_sales), 0, panel_sales))

sum(chk$panel_sales, na.rm = TRUE) <= sum(proj.quarter$panel_sales, na.rm = TRUE)
# TRUE: no multiple match

# projection result
proj.most <- universe.set %>% 
  left_join(proj.quarter, by = c("province", "city", "district", "pchc", "quarter", 
                                 "market", "molecule", "atc3", "packid")) %>% 
  mutate(panel_sales = if_else(is.na(panel_sales), 0, panel_sales)) %>% 
  left_join(proj.parm, by = c("quarter", "city", "market", "packid", "seg")) %>% 
  mutate(predict_sales = est * slope + intercept,
         predict_sales = if_else(predict_sales < 0, 0, predict_sales)) %>% 
  left_join(proj.factor, by = c("market", "seg")) %>% 
  mutate(final_sales = if_else(panel_all == 0, predict_sales * factor, panel_sales)) %>% 
  filter(final_sales > 0) %>% 
  mutate(year = stri_sub(quarter, 1, 4)) %>% 
  select(year, quarter, province, city, district, pchc, market, atc3, molecule, 
         packid, sales = final_sales, panel)


##---- Projection of Fuzhou ----
# projection data
proj.raw.fz <- imp.total %>% 
  filter(city == '福州', 
         quarter %in% c("2020Q1", "2020Q2")) %>% 
  left_join(hospital.universe, 
            by = c('province', 'city', 'district', 'pchc'))

# hospital count
hospital.count <- hospital.universe %>% 
  count(province, city, district)

# quarter sales
proj.quarter.fz <- proj.raw.fz %>% 
  group_by(year, quarter, province, city, district, market, atc3, 
           molecule, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(hospital.count, by = c('province', 'city', 'district')) %>% 
  mutate(final_sales = sales / n)

# projection
hospital.diff <- setdiff(hospital.universe$pchc[hospital.universe$city == '福州'], 
                         unique(proj.raw.fz$pchc))

proj.fz1 <- hospital.universe %>% 
  filter(pchc %in% hospital.diff) %>% 
  select(province, city, district, pchc) %>% 
  left_join(proj.quarter.fz, by = c('province', 'city', 'district')) %>% 
  mutate(panel = 0)

proj.fz2 <- proj.raw.fz %>% 
  group_by(year, quarter, province, city, district, pchc, market, atc3, 
           molecule, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(panel = 1, 
         final_sales = sales)

# projection result
proj.fz <- bind_rows(proj.fz1, proj.fz2) %>% 
  filter(final_sales > 0) %>% 
  select(year, quarter, province, city, district, pchc, market, atc3, molecule, 
         packid, sales = final_sales, panel)


##---- Result ----
proj.total <- bind_rows(proj.most, proj.fz) %>% 
  group_by(year, quarter, province, city, pchc, market, 
           atc3, molecule, packid, panel) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write_feather(proj.total, "03_Outputs/03_Servier_CHC_Projection.feather")


