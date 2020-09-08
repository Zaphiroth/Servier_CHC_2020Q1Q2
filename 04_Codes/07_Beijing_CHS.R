# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q2
# Purpose:      Projection of Beijing CHS
# programmer:   Zhe Liu
# date:         2020-08-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin ----
# pack info
chs.pack <- read_xlsx("02_Inputs/ims.mapping1904.xlsx") %>% 
  distinct(packid = stri_pad_left(Pack_Id, 7, 0), pack_desc = Pck_Desc, 
           corp_desc = Corp_Desc, pack_size = PckSize_Desc)

# pack.mapping.19 <- read.xlsx("02_Inputs/all_raw_data_m_19q4_ahbjjs_20200204-匹配完成.xlsx", sheet = 2) %>% 
#   select(`匹配名`, `剂型`, `规格`, `包装`, `生产企业`, packid = `七位产品编码`) %>% 
#   distinct() %>% 
#   filter(!is.na(`匹配名`), !is.na(packid)) %>% 
#   mutate(packid = stri_pad_left(packid, 7, 0),
#          `规格` = tolower(`规格`))

# product name
product.name <- fread("02_Inputs/pfc与ims数据对应_20200824.csv") %>% 
  distinct(packid = stri_pad_left(Pack_Id, 7, 0), product = `商品名`)


##---- Beijing CHS projection ----
# CHS raw
raw.bj.total <- read.csv('02_Inputs/data/ahbjjszj2020Q1Q2_packid_moleinfo.csv')

raw.bj.chs <- raw.bj.total %>% 
  filter(Quarter %in% c('2020Q1', '2020Q2'), Province == '北京市', 
         grepl("服务站", Hospital_Name)) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = Value / Price, 
           sales = Value) %>% 
  left_join(market.def, by = "packid") %>% 
  filter(!is.na(market)) %>% 
  group_by(year, quarter, province, city, market, atc3, molecule, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(product.name, by = "packid") %>% 
  left_join(chs.pack, by = "packid") %>% 
  mutate(dosage_units = pack_size * units,
         atc2 = stri_sub(atc3, 1, 3),
         market = if_else(atc2 %in% c("C07", "C08"), "IHD", market), 
         packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid), 
         corp_desc = if_else(product == "GLUCOPHAGE", "MERCK GROUP", corp_desc),
         corp_desc = if_else(product == 'ONGLYZA', 'ASTRAZENECA GROUP', corp_desc)) %>% 
  filter(!(market == "IHD" & product == "CORLENTOR")) %>% 
  filter(!(market == "OAD" & product == "TANG LIN"))

# adjust
chs.ihd.adj <- raw.bj.chs %>% 
  filter(atc2 %in% c("C07", "C08")) %>% 
  mutate(factor = if_else(atc2 == "C07", 0.25, 0.1),
         sales = sales * factor,
         units = units * factor,
         dosage_units = dosage_units * factor,
         market = "IHD") %>% 
  select(-factor)

chs.htn.adj <- raw.bj.chs %>% 
  filter(atc2 %in% c("C07", "C08")) %>% 
  mutate(factor = if_else(atc2 == "C07", 0.75, 0.9),
         sales = sales * factor,
         units = units * factor,
         dosage_units = dosage_units * factor,
         market = "HTN") %>% 
  select(-factor)


##---- Result ----
bj.chs <- raw.bj.chs %>% 
  filter(!(atc2 %in% c("C05", "C07", "C08")), 
         stri_sub(pack_desc, 1, 4) %in% c("CAP ", "TAB ", "PILL")) %>% 
  bind_rows(chs.ihd.adj, chs.htn.adj) %>% 
  mutate(channel = "CHS",
         year = "2020") %>% 
  select(province, city, year, quarter, market, atc3, molecule, packid, 
         pack_desc, product, corp_desc, channel, units, dosage_units, sales)

write.xlsx(bj.chs, '03_Outputs/07_Servier_CHS_Beijing.xlsx')





