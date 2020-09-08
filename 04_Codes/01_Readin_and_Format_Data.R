# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q2
# Purpose:      Readin Raw Data
# programmer:   Zhe Liu
# Date:         2020-08-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
# PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20200814.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

# IMS pack
ims.pack <- fread("02_Inputs/pfc与ims数据对应_20200824.csv") %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0),
         atc3 = stri_sub(ATC4_Code, 1, 4),
         atc2 = stri_sub(ATC4_Code, 1, 3)) %>% 
  distinct(packid, atc3, atc2, molecule_desc = Molecule_Desc)

# market definition
market.def <- read_xlsx("02_Inputs/Market_Definition_20200824.xlsx") %>% 
  distinct(molecule = Molecule_Desc, market = TA) %>% 
  right_join(ims.pack, by = c('molecule' = "molecule_desc")) %>% 
  filter(!is.na(market))

# target city
target.prov <- c("北京", "福建", "广东", "江苏", "上海", "浙江", "安徽", "山东")
target.city <- c("北京", "常州", "福州", "广州", "杭州", "南京", 
                 "宁波", "泉州", "厦门", "上海", "苏州", "温州", 
                 "无锡", "济南", "徐州", "合肥", "绍兴", "青岛")


##---- Formatting raw data ----
# history
raw.history.gz <- read_feather("02_Inputs/data/Servier_guangzhou_171819_final.feather") %>% 
  distinct(year = as.character(Year), 
           date = as.character(PERIOD), 
           quarter = QUARTER, 
           province, 
           city, 
           district, 
           pchc = PCHC, 
           hospital = hosp_name, 
           packid = stri_pad_left(PFC, 7, 0), 
           price = VALUE / UNIT, 
           units = UNIT, 
           sales = VALUE) %>% 
  left_join(pchc.mapping3, by = c("province", "city", "hospital")) %>% 
  mutate(district = if_else(is.na(district.x), district.y, district.x), 
         pchc = if_else(is.na(pchc.x), pchc.y, pchc.x)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(market.def, by = "packid") %>% 
  select(year, date, quarter, province, city, district, pchc, market, atc3, 
         molecule, packid, price, units, sales)

raw.history <- read_feather("02_Inputs/data/Servier_CHC_Total_Raw_2017-2019.feather") %>% 
  filter(province != '广东') %>% 
  mutate(price = sales / units, 
         molecule = molecule_desc, 
         pchc = if_else(pchc == 'PCHC09264', 'PCHC04241', pchc)) %>% 
  left_join(pchc.mapping4, by = 'pchc') %>% 
  mutate(province = if_else(is.na(province.x), province.y, province.x), 
         city = if_else(is.na(city.x), city.y, city.x)) %>% 
  select(year, date, quarter, province, city, district, pchc, market, atc3, 
         molecule, packid, price, units, sales) %>% 
  bind_rows(raw.history.gz) %>% 
  group_by(pchc) %>% 
  mutate(province = first(na.omit(province)), 
         city = first(na.omit(city)), 
         district = first(na.omit(district))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, atc3, 
           molecule, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# Guangzhou
raw.gz1 <- read.csv('02_Inputs/data/gzs 20q1.csv') %>% 
  mutate(date = gsub('[-]', '0', period), 
         quarter_m = stri_sub(date, 5, 6)) %>% 
  distinct(year = stri_sub(date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           date, 
           province = '广东', 
           city = '广州', 
           hospital = name, 
           packid = stri_pad_left(pfc, 7, 0), 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital')) %>% 
  left_join(market.def, by = "packid") %>% 
  select(year, date, quarter, province, city, district, pchc, market, atc3, 
         molecule, packid, price, units, sales)

raw.gz2 <- read.xlsx('02_Inputs/data/2jd_date.xlsx') %>% 
  mutate(quarter_m = stri_sub(date, 5, 6)) %>% 
  distinct(year = stri_sub(date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           date = stri_sub(date, 1, 6), 
           province = '广东', 
           city = '广州', 
           hospital = name, 
           packid = stri_pad_left(pfc, 7, 0), 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital')) %>% 
  left_join(market.def, by = "packid") %>% 
  select(year, date, quarter, province, city, district, pchc, market, atc3, 
         molecule, packid, price, units, sales)

# servier
raw.ah <- read_xlsx('02_Inputs/data/Servier_ah_CHC_2020Q1Q2.xlsx')
raw.bj <- read_xlsx('02_Inputs/data/Servier_bj_CHC_2020Q1Q2.xlsx')
raw.js <- read_xlsx('02_Inputs/data/Servier_js_CHC_2020Q1Q2.xlsx')
raw.zj <- read_xlsx('02_Inputs/data/Servier_zj_CHC_2020Q1Q2.xlsx')
raw.fjsd <- read_xlsx('02_Inputs/data/Servier_fjsd_CHC_2020Q1Q2(predicted by all_raw_data_packid_Servier_171819_CHC_m_v4).xlsx')

raw.total <- bind_rows(raw.js, raw.zj) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month)) %>% 
  bind_rows(raw.ah, raw.bj, raw.fjsd) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc4 = ATC4_Code, 
           nfc = NFC123_Code, 
           product = Prd_desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = Value / Price, 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  left_join(market.def, by = "packid") %>% 
  bind_rows(raw.gz1, raw.gz2, raw.history) %>% 
  filter(!is.na(pchc), !is.na(market), pchc != '#N/A', units > 0, sales > 0) %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2020Q1', '2020Q2')) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  group_by(pchc) %>% 
  mutate(province = first(na.omit(province)), 
         city = first(na.omit(city)), 
         district = first(na.omit(district))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, atc3, 
           molecule, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units)

write_feather(raw.total, '03_Outputs/01_Servier_CHC_Raw.feather')

# QC
chk <- raw.total %>% 
  group_by(city, market, quarter, packid) %>% 
  summarise(sales = sum(sales), 
            units = sum(units)) %>% 
  ungroup() %>% 
  arrange(city, market, quarter, packid)

