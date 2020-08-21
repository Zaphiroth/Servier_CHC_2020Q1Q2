# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q2
# Purpose:      Projection of Shanghai CHC
# programmer:   Zhe Liu
# date:         2020-08-18
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Sample ----
# sample
chc.history <- read.xlsx("06_Deliveries/CHC_MAX_16Q420Q1_0622_m_add_raw_sales_value.xlsx", 
                             sheet = 2, check.names = FALSE)

sh.bj.sample <- chc.history %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0)) %>% 
  filter(Channel == "CHC", 
         Province %in% c("上海", "北京"), 
         stri_sub(Date, 1, 4) %in% c("2017", "2018", "2019"), 
         !is.na(Pack_ID), 
         Sales > 0) %>% 
  group_by(quarter = Date, market = MKT, atc3 = ATC3, 
           molecule = Molecule_Desc, packid = Pack_ID) %>% 
  summarise(province = first(na.omit(Province)),
            city = first(na.omit(City)),
            sales = sum(Sales, na.rm = TRUE)) %>% 
  ungroup()

sh.19q2 <- chc.history %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0)) %>% 
  filter(Channel == "CHC", 
         Province == "上海", 
         Date == "2019Q2", 
         !is.na(Pack_ID), 
         Sales > 0) %>% 
  group_by(quarter = Date, market = MKT, atc3 = ATC3, 
           molecule = Molecule_Desc, packid = Pack_ID) %>% 
  summarise(province = first(na.omit(Province)),
            city = first(na.omit(City)),
            units = sum(Units, na.rm = TRUE),
            sales = sum(Sales, na.rm = TRUE)) %>% 
  ungroup()

# pack ID existing & missing
sh.exist.pack <- sh.19q2$packid[which(sh.19q2$packid %in% sh.bj.sample$packid)]
sh.miss.pack <- sh.19q2$packid[which(!(sh.19q2$packid %in% sh.bj.sample$packid))]

# growth of existing pack
## 去掉growth太大的pack
growth.exist <- raw.total %>% 
  filter(city == "北京", quarter %in% c("2019Q2", "2020Q2")) %>% 
  filter(packid %in% unique(sh.19q2$packid)) %>% 
  mutate(province = "上海",
         city = "上海") %>% 
  group_by(quarter, city, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(quarter, sales, fill = 0) %>% 
  mutate(growth_1920q2 = `2020Q2` / `2019Q2`) %>% 
  select(city, packid, growth_1920q2) %>% 
  mutate(growth_1920q2 = if_else(is.na(growth_1920q2) | 
                                   is.infinite(growth_1920q2), 
                                 1, 
                                 growth_1920q2)) %>% 
  filter(growth_1920q2 <= 1.5, growth_1920q2 > 0)


##---- K-nn model ----
# ims sales
ims.raw <- fread("02_Inputs/cn_IMS_Sales_Fdata_201912_1.txt", stringsAsFactors = FALSE)

ims.sales <- ims.raw %>% 
  mutate(date = gsub("M", "", Period_Code),
         packid = stri_pad_left(Pack_ID, 7, 0)) %>% 
  filter(Geography_id == "CHT", date >= "201701") %>% 
  group_by(date, packid) %>% 
  summarise(sales = sum(LC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(date, sales, fill = 0) %>% 
  mutate(train_flag = if_else(packid %in% growth.exist$packid, 1, 0))

# k-nn model
train.sh <- ims.sales[ims.sales$train_flag == 1, ]
test.sh <- ims.sales[ims.sales$train_flag == 0, ]

train.sh.tmp <- select(train.sh, -packid)
test.sh.tmp <- select(test.sh, -packid)

sh.model <- kknn(train_flag ~ ., train = train.sh.tmp, test = test.sh.tmp, 
                 k = 3, scale = TRUE)

sh.indice <- as.data.frame(sh.model$C) %>% 
  lapply(function(x) {
    train.sh$packid[x]
  }) %>% 
  as.data.frame(col.names = c("pack_1", "pack_2", "pack_3")) %>% 
  bind_cols(test.sh[, c("packid")]) %>% 
  setDT() %>% 
  melt(id.vars = "packid", variable.name = "knn_level", value.name = "knn_pack")

sh.weight <- as.data.frame(sh.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c("pack_1", "pack_2", "pack_3")) %>% 
  mutate(weight_sum = pack_1 + pack_2 + pack_3,
         pack_1 = pack_1 / weight_sum,
         pack_2 = pack_2 / weight_sum,
         pack_3 = pack_3 / weight_sum) %>% 
  bind_cols(test.sh[, c("packid")]) %>% 
  select(-weight_sum) %>% 
  setDT() %>% 
  melt(id.vars = "packid", variable.name = "knn_level", value.name = "knn_weight")

# weighted growth
weight.growth <- sh.indice %>% 
  left_join(sh.weight, by = c("packid", "knn_level")) %>% 
  left_join(growth.exist, by = c("knn_pack" = "packid")) %>% 
  group_by(city, packid) %>% 
  summarise(growth_1920q2 = sum(growth_1920q2 * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(city, packid, growth_1920q2)

# growth
surplus <- setdiff(sh.19q2$packid[!(sh.19q2$packid %in% growth.exist$packid)], 
                   ims.sales$packid)

surplus.growth <- data.frame(city = "上海",
                             packid = surplus) %>% 
  mutate(growth_1920q2 = 1)

sh.growth <- bind_rows(merge(growth.exist, 0),
                       merge(weight.growth, 1),
                       merge(surplus.growth, 2)) %>% 
  rename("flag" = "y")


##---- Result ----
proj.sh2 <- sh.19q2 %>% 
  left_join(sh.growth, by = c("city", "packid")) %>% 
  mutate(sales = sales * growth_1920q2,
         units = units * growth_1920q2,
         price = sales / units,
         quarter = "2020Q2",
         year = "2020") %>% 
  filter(sales > 0) %>% 
  select(year, quarter, province, city, market, atc3, molecule, packid, 
         units, sales, price)

write.xlsx(proj.sh2, "03_Outputs/06_Seriver_CHC_Shanghai_2020Q2.xlsx")





