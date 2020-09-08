# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q2
# Purpose:      Summary
# programmer:   Zhe Liu
# date:         2020-08-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


#---- CHC ----
# product name
product.name <- fread("02_Inputs/pfc与ims数据对应_20200824.csv") %>% 
  distinct(packid = stri_pad_left(Pack_Id, 7, 0), product = `商品名`)

# flag
flag.raw <- read.xlsx("02_Inputs/13城市的招标flag_zs_flag.xlsx")

flag <- flag.raw %>% 
  filter(!is.na(`是否是13城市`)) %>% 
  distinct(province = `省`, city = `地级市`, pchc = PCHC_Code, zs_flag)

proj.zs <- proj.adj %>% 
  left_join(flag, by = c("province", "city", "pchc")) %>% 
  mutate(zs_flag = if_else(zs_flag != 1, 0, zs_flag)) %>% 
  filter(!(city %in% c("北京", "上海") & zs_flag != 1)) %>% 
  filter(quarter == '2020Q2')

# corporation
corp.ref <- fread("02_Inputs/cn_corp_ref_201912_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct()

# pack
pack.ref <- fread("02_Inputs/cn_prod_ref_201912_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct() %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0))

corp.pack <- pack.ref %>% 
  select("Pack_Id", "Pck_Desc", "Corp_ID") %>% 
  distinct() %>% 
  left_join(corp.ref, by = "Corp_ID") %>% 
  mutate(Corp_Desc = if_else(Corp_Desc == "LUYE GROUP", "LVYE GROUP", Corp_Desc)) %>% 
  select(packid = Pack_Id, pack_desc = Pck_Desc, corp_desc = Corp_Desc)

# pack size
pack.size <- pack.ref %>% 
  distinct(packid = Pack_Id, pack_size = PckSize_Desc)

# join
chc.part <- bind_rows(proj.zs, proj.sh2) %>% 
  left_join(product.name, by = "packid") %>% 
  left_join(corp.pack, by = "packid") %>% 
  left_join(pack.size, by = "packid") %>% 
  filter(stri_sub(pack_desc, 1, 4) %in% c("CAP ", "TAB ", "PILL")) %>% 
  mutate(dosage_units = pack_size * units,
         channel = "CHC") %>% 
  group_by(province, city, year, quarter, market, atc3, molecule, packid, 
           pack_desc, product, corp_desc, channel) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            dosage_units = sum(dosage_units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(sales = if_else(sales < 0, 0, sales),
         units = if_else(units < 0, 0, units),
         sales = if_else(units == 0, 0, sales),
         units = if_else(sales == 0, 0, units))

# A10S
a10s <- proj.adj %>% 
  filter(atc3 == "A10S") %>% 
  left_join(product.name, by = "packid") %>% 
  left_join(corp.pack, by = "packid") %>% 
  left_join(pack.size, by = "packid") %>% 
  filter(stri_sub(pack_desc, 1, 4) %in% c("CAP ", "TAB ", "PILL")) %>% 
  mutate(dosage_units = pack_size * units,
         channel = "CHC") %>% 
  group_by(province, city, year, quarter, market, atc3, molecule, packid, 
           pack_desc, product, corp_desc, channel) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            dosage_units = sum(dosage_units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# total CHC
total.chc <- bind_rows(chc.part, a10s, bj.chs) %>% 
  select(Pack_ID = packid, Channel = channel, Province = province, City = city, 
         Date = quarter, ATC3 = atc3, MKT = market, Molecule_Desc = molecule, 
         Prod_Desc = product, Pck_Desc = pack_desc, Corp_Desc = corp_desc, 
         Sales = sales, Units = units, DosageUnits = dosage_units)

write.xlsx(total.chc, '03_Outputs/08_Servier_CHC_2020Q1Q2.xlsx')


##---- Add flag ----
# readin 4+7 flag
capital.47 <- read_xlsx("02_Inputs/4+7+省会名单.xlsx") %>% 
  filter(`类别` %in% "4+7城市") %>% 
  mutate(City = gsub("市", "", `城市`)) %>% 
  select("City", "是否是4+7城市" = "类别")

# product bid
prod.bid <- read_xlsx("02_Inputs/Displayname Mapping.xlsx", sheet = 1) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0),
         Pack_ID_5 = stri_sub(Pack_ID, 1, 5),
         `Display Name3 CN` = if_else(`Display Name3 CN` %in% c("中标品规", "非中标品规"), 
                                      "仿制", 
                                      `Display Name3 CN`),
         `Display Name3 CN` = gsub("-2|-1", "", `Display Name3 CN`),
         `Display Name2 CN` = gsub("-2|-1", "", `Display Name2 CN`)) %>% 
  distinct(name1 = `Display Name1 CN`,
           name2 = `Display Name2 CN`,
           name3 = `Display Name3 CN`,
           Pack_ID_5)

# join
chc.bid <- total.chc %>% 
  left_join(capital.47, by = "City") %>% 
  mutate(Pack_ID_5 = stri_sub(Pack_ID, 1, 5)) %>% 
  left_join(prod.bid, by = "Pack_ID_5")

# corporation, ATC3
corp.atc3 <- read_xlsx("02_Inputs/产品性质_chpa 08.23(1).xlsx", sheet = 1)

corp.type <- distinct(corp.atc3, Corp_Desc, Mnf_Type = `厂家性质`)

atc3.cn <- distinct(corp.atc3, ATC3 = ATC3_Code, `ATC3中文分类` = `类别`)

molecule.cn <- distinct(corp.atc3, Molecule_Desc, Molecule_CN = `分子`)

corp.add <- read_xlsx("02_Inputs/Corp_Info_20200908.xlsx") %>% 
  group_by(Corp_Desc) %>% 
  arrange(MnfType_Desc) %>% 
  summarise(Mnf_Type1 = first(MnfType_Desc)) %>% 
  ungroup() %>% 
  mutate(Mnf_Type1 = if_else(is.na(Mnf_Type1), "Local", Mnf_Type1),
         Mnf_Type1 = if_else(Mnf_Type1 %in% c("Imported", "Joint Venture"), "MNC", Mnf_Type1))

# molecule bid
molecule.bid <- distinct(chc.bid, Molecule_Desc, name1) %>% 
  filter(name1 == "4+7分子") %>% 
  right_join(molecule.cn, by = "Molecule_Desc") %>% 
  mutate(name1 = if_else(Molecule_CN %in% c("赖诺普利", "卡托普利"), "4+7分子", name1)) %>% 
  select(Molecule_Desc, name1)

# join
chc.flag <- chc.bid %>% 
  select(-name1) %>% 
  left_join(molecule.bid, by = "Molecule_Desc") %>% 
  left_join(corp.type, by = "Corp_Desc") %>% 
  left_join(corp.add, by = "Corp_Desc") %>% 
  left_join(atc3.cn, by = "ATC3") %>% 
  mutate(name3 = trimws(name3),
         Mnf_Type = if_else(is.na(Mnf_Type), Mnf_Type1, Mnf_Type),
         Mnf_Type = trimws(Mnf_Type),
         name3 = ifelse(is.na(name3) & Mnf_Type == "MNC", "原研", 
                        ifelse(is.na(name3) & Mnf_Type == "Local", "仿制", 
                               name3)),
         name2 = ifelse(name2 == "非中标产品", NA, name2)) %>% 
  rename("是否进入带量采购" = "name1",
         "是否是中标品种" = "name2",
         "是否是原研" = "name3",
         "是否是MNC" = "Mnf_Type") %>% 
  select(-Pack_ID_5, -Mnf_Type1) %>% 
  filter(!grepl("AMP", Pck_Desc))


##---- Add new column ----
packid.profile.raw <- read_xlsx("02_Inputs/packid_prod_20181112.xlsx")

packid.profile <- packid.profile.raw %>% 
  mutate(Prod_ID = substr(stri_pad_left(Pack_Id, 7, "0"), 1, 5)) %>% 
  distinct(Pack_Id, Prod_ID, ATC4_Code, ims_product_cn)

prod.profile <- packid.profile %>%
  select(Prod_ID, ATC4_Code, ims_product_cn) %>%
  distinct()

city.en <- read.xlsx("02_Inputs/CityEN.xlsx")

chc.add <- chc.flag %>% 
  mutate(first_num_position = stri_locate_first(Pck_Desc, regex = "\\d")[,1],
         last_space_position = stri_locate_last(Pck_Desc, regex = "\\s")[,1],
         Package = str_squish(substr(Pck_Desc, 1, first_num_position - 1)),
         Dosage = str_squish(substr(Pck_Desc, first_num_position, 
                                    last_space_position - 1)),
         Quantity = as.integer(str_squish(substr(Pck_Desc, last_space_position, 
                                                 nchar(Pck_Desc)))),
         Pack_ID = as.integer(Pack_ID)) %>% 
  left_join(packid.profile, by = c("Pack_ID" = "Pack_Id")) %>% 
  mutate(Prod_ID = substr(stri_pad_left(Pack_ID, 7, "0"), 1, 5)) %>% 
  left_join(prod.profile, by = c("Prod_ID")) %>% 
  mutate(ATC4_Code.x = ifelse(is.na(ATC4_Code.x) & !is.na(Pack_ID),
                              ATC4_Code.y, ATC4_Code.x),
         ims_product_cn.x = ifelse(is.na(ims_product_cn.x) & !is.na(Pack_ID),
                                   ims_product_cn.y, ims_product_cn.x)) %>% 
  rename(ATC4 = ATC4_Code.x, Prod_CN_Name = ims_product_cn.x) %>% 
  select(-c(Prod_ID, ATC4_Code.y, ims_product_cn.y)) %>% 
  mutate(
    Pack_ID = stri_pad_left(Pack_ID, 7, 0),
    TherapeuticClsII = case_when(
      ATC3 == "A10H" ~ "SULPHONYLUREA",
      ATC3 == "A10J" ~      "BIGUANIDE",
      ATC3 == "A10K" ~     "GLITAZONE",
      ATC3 == "A10L" ~     "AGIs",
      ATC3 == "A10M" ~    "METAGLINIDE",
      ATC3 == "A10N" ~    "DPP-IV",
      ATC3 == "A10P" ~     "SGLT2",
      ATC3 == "A10S" ~     "GLP-1",
      ATC3 == "A10X" ~     "OTHERS",
      ATC3 == "C02A" ~     "ANTI-HTN",
      ATC3 == "C02B" ~     "ANTI-HTN",
      ATC3 == "C02C" ~     "ANTI-HTN",
      ATC3 == "C03A" ~     "DIURETICS",
      ATC3 == "C07A" ~     "BB",
      ATC3 == "C08A" ~     "CCB",
      ATC3 == "C08B" ~     "CCB",
      ATC3 == "C09A" ~     "ACEi PLAIN",
      ATC3 == "C09C" ~     "ARB PLAIN",
      ATC3 == "C07A" ~     "BB",
      ATC3 == "C08A" ~     "CCB",
      ATC3 == "C01E" ~     "NITRITES",
      Molecule_Desc == "TRIMETAZIDINE" ~   "TMZ",
      ATC3 == "C01D" & Molecule_Desc != "TRIMETAZIDINE" ~  "OTHERS",
      ATC4 %in% c("C09B3", "C09D3") ~ "A+C FDC",
      ATC4 %in% c("C09B1", "C09D1") ~ "A+D FDC",
      !is.na(ATC4) ~ "OTHERS",
      TRUE ~ NA_character_
    )
  ) %>% 
  select(Pack_ID, 
         Channel, 
         Province, 
         City,  
         Date, 
         ATC3, 
         MKT, Molecule_Desc,
         Prod_Desc, Pck_Desc, Corp_Desc, Sales,  Units, DosageUnits,
         ATC4, TherapeuticClsII, Prod_CN_Name,  Package, Dosage, Quantity, 
         `是否是4+7城市`, 是否进入带量采购, 是否是原研, 是否是中标品种,
         是否是MNC, ATC3中文分类) %>% 
  left_join(city.en, by = "City") %>% 
  mutate(`Period-MAT` = NA_character_, 
         TherapeuticClsII = case_when(
           MKT == "HTN" & ATC3 == "C09A" ~ "RAASi Plain",
           MKT == "HTN" & ATC3 == "C09C" ~ "RAASi Plain",
           MKT == "HTN" & ATC3 == "C09B" ~ "RAASi FDC",
           MKT == "HTN" & ATC3 == "C09D" ~ "RAASi FDC",
           MKT == "HTN" & ATC3 == "C02A" ~ "ANTI-HTN",
           MKT == "HTN" & ATC3 == "C02B" ~ "ANTI-HTN",
           MKT == "HTN" & ATC3 == "C02C" ~ "ANTI-HTN",
           MKT == "HTN" & ATC3 == "C03A" ~ "DIURETICS",
           MKT == "HTN" & ATC3 == "C07A" ~ "BB",
           MKT == "HTN" & ATC3 == "C08A" ~ "CCB",
           MKT == "HTN" & ATC3 == "C08B" ~ "CCB",
           MKT == "OAD" & ATC3 == "A10H" ~ "SULPHONYLUREA",
           MKT == "OAD" & ATC3 == "A10J" ~ "BIGUANIDE",
           MKT == "OAD" & ATC3 == "A10K" ~ "GLITAZONE",
           MKT == "OAD" & ATC3 == "A10L" ~ "AG Is",
           MKT == "OAD" & ATC3 == "A10M" ~ "METAGLINIDE",
           MKT == "OAD" & ATC3 == "A10N" ~ "DPP-IV",
           MKT == "OAD" & ATC3 == "A10P" ~ "SGLT2",
           MKT == "OAD" & ATC3 == "A10S" ~ "GLP-1",
           MKT == "OAD" & ATC3 == "A10X" ~ "OTHERS",
           MKT == "IHD" & ATC3 == "C07A" ~ "BB",
           MKT == "IHD" & ATC3 == "C08A" ~ "CCB",
           MKT == "IHD" & ATC3 == "C01E" ~ "NITRITES",
           MKT == "IHD" & ATC3 == "C01D" & Molecule_Desc == "TRIMETAZIDINE" ~ "TMZ",
           MKT == "IHD" & ATC3 == "C01D" & Molecule_Desc != "TRIMETAZIDINE"~ "OTHERS",
           TRUE ~ NA_character_
         ),
         Sales = round(Sales, 2),
         Units = round(Units),
         DosageUnits = round(DosageUnits)) %>% 
  filter(Molecule_Desc != "ENALAPRIL+FOLIC ACID") %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, 
         `Period-MAT`, `CITY-EN`, TherapeuticClsII, Prod_CN_Name,  Package, 
         Dosage, Quantity, `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, 
         `是否是中标品种`, `是否是MNC`, `ATC3中文分类`)


##---- Result ----
chc.history <- read.xlsx("06_Deliveries/CHC_MAX_16Q420Q1_0622_m_add_raw_sales_value.xlsx", 
                         sheet = 2, check.names = FALSE)

# DPP IV
chc.dpp4 <- chc.history %>% 
  filter(Channel == 'CHC', City %in% c('福州', '泉州', '厦门', '济南', '青岛'), 
         TherapeuticClsII == 'DPP-IV', Date %in% c('2019Q1', '2019Q2')) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0), 
         price = Sales / Units, 
         Sales = case_when(
           Pack_ID == '5890602' & Date == '2019Q1' ~ Sales * 1.207828, 
           Pack_ID == '5890602' & Date == '2019Q2' ~ Sales * 1.174208, 
           Pack_ID == '4268602' & Date == '2019Q1' ~ Sales * 1.361853, 
           Pack_ID == '4268602' & Date == '2019Q2' ~ Sales * 1.37669, 
           Pack_ID == '4268604' & Date == '2019Q1' ~ Sales * 1.245801, 
           Pack_ID == '4268604' & Date == '2019Q2' ~ Sales * 1.25994, 
           Pack_ID == '5739702' & Date == '2019Q1' ~ Sales * 1.600415, 
           Pack_ID == '5739702' & Date == '2019Q2' ~ Sales * 1.388511, 
           Pack_ID == '5518904' & Date == '2019Q1' ~ Sales * 1.283041, 
           Pack_ID == '5518904' & Date == '2019Q2' ~ Sales * 1.580584, 
           TRUE ~ NaN
         )) %>% 
  filter(!is.na(Sales)) %>% 
  mutate(Units = Sales / price, 
         DosageUnits = Units * Quantity, 
         Sales = round(Sales, 2), 
         Units = round(Units), 
         DosageUnits = round(DosageUnits), 
         Date = gsub('2019', '2020', Date))

# update 2020Q1 & 2020Q2
# chc.result <- chc.history %>% 
#   filter(stri_sub(Date, 1, 4) %in% c('2016', '2017', '2018', '2019')) %>% 
#   mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0), 
#          Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '06470', 
#                            stri_paste('64895', stri_sub(Pack_ID, 6, 7)), 
#                            Pack_ID), 
#          Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '47775', 
#                            stri_paste('58906', stri_sub(Pack_ID, 6, 7)), 
#                            Pack_ID), 
#          Corp_Desc = if_else(Prod_Desc == "GLUCOPHAGE", "MERCK GROUP", Corp_Desc),
#          Corp_Desc = if_else(Prod_Desc == 'ONGLYZA', 'ASTRAZENECA GROUP', Corp_Desc)) %>% 
#   bind_rows(chc.add) %>% 
#   filter(Sales > 0, Units > 0, DosageUnits > 0, 
#          !(MKT %in% c("HTN", "IHD") & 
#              !(stri_sub(Package, 1, 3) %in% c("CAP", "TAB", "PIL")))) %>% 
#   filter(!(Channel == 'CHC' & 
#              City %in% c('福州', '泉州', '厦门', '济南', '青岛') & 
#              Pack_ID %in% c('5890602', '4268602', '4268604', '5739702', '5518904') & 
#              Date %in% c('2020Q1', '2020Q2'))) %>% 
#   bind_rows(chc.dpp4) %>% 
#   mutate(`Period-MAT` = case_when(
#            Date %in% c('2020Q2', '2020Q1', '2019Q4', '2019Q3') ~ 'MAT20Q2', 
#            Date %in% c('2019Q2', '2019Q1', '2018Q4', '2018Q3') ~ 'MAT19Q2', 
#            Date %in% c('2018Q2', '2018Q1', '2017Q4', '2017Q3') ~ 'MAT18Q2', 
#            TRUE ~ NA_character_
#          )) %>% 
#   group_by(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
#            Prod_Desc, Corp_Desc, `Period-MAT`, `CITY-EN`, 
#            TherapeuticClsII, Prod_CN_Name,  Package, Quantity, 
#            `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
#            `是否是MNC`, `ATC3中文分类`) %>% 
#   summarise(Pck_Desc = last(Pck_Desc), 
#             Dosage = last(Dosage), 
#             Sales = round(sum(Sales, na.rm = TRUE), 2),
#             Units = round(sum(Units, na.rm = TRUE)),
#             DosageUnits = round(sum(DosageUnits, na.rm = TRUE))) %>% 
#   ungroup() %>% 
#   mutate(Sales_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ Sales / 0.75,
#                                substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ Sales / 0.25,
#                                substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ Sales / 0.9,
#                                substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ Sales / 0.1,
#                                TRUE ~ Sales),
#          Units_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ Units / 0.75,
#                                substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ Units / 0.25,
#                                substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ Units / 0.9,
#                                substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ Units / 0.1,
#                                TRUE ~ Units),
#          DosageUnits_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ DosageUnits / 0.75,
#                                      substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ DosageUnits / 0.25,
#                                      substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ DosageUnits / 0.9,
#                                      substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ DosageUnits / 0.1,
#                                      TRUE ~ DosageUnits)) %>%
#   select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
#          Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, 
#          `Period-MAT`, `CITY-EN`, TherapeuticClsII, Prod_CN_Name,  Package, 
#          Dosage, Quantity, `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, 
#          `是否是中标品种`, `是否是MNC`, `ATC3中文分类`, Sales_raw, Units_raw, 
#          DosageUnits_raw) %>% 
#   arrange(Channel, Date, Province, City, MKT, Pack_ID)
# 
# write.xlsx(chc.result, "03_Outputs/08_Servier_CHC_2016Q4_2020Q2.xlsx")

# update 2020Q2
chc.result1 <- chc.history %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0), 
         Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '06470', 
                           stri_paste('64895', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID), 
         Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '47775', 
                           stri_paste('58906', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID), 
         Corp_Desc = if_else(Prod_Desc == "GLUCOPHAGE", "MERCK GROUP", Corp_Desc),
         Corp_Desc = if_else(Prod_Desc == 'ONGLYZA', 'ASTRAZENECA GROUP', Corp_Desc)) %>% 
  bind_rows(chc.add[chc.add$Date == '2020Q2', ]) %>% 
  filter(Sales > 0, Units > 0, DosageUnits > 0, 
         !(MKT %in% c("HTN", "IHD") & 
             !(stri_sub(Package, 1, 3) %in% c("CAP", "TAB", "PIL")))) %>% 
  filter(!(Channel == 'CHC' & 
             City %in% c('福州', '泉州', '厦门', '济南', '青岛') & 
             Pack_ID %in% c('5890602', '4268602', '4268604', '5739702', '5518904') & 
             Date %in% c('2020Q2'))) %>% 
  bind_rows(chc.dpp4[chc.dpp4$Date == '2020Q2', ]) %>% 
  mutate(`Period-MAT` = case_when(
           Date %in% c('2020Q2', '2020Q1', '2019Q4', '2019Q3') ~ 'MAT20Q2', 
           Date %in% c('2019Q2', '2019Q1', '2018Q4', '2018Q3') ~ 'MAT19Q2', 
           Date %in% c('2018Q2', '2018Q1', '2017Q4', '2017Q3') ~ 'MAT18Q2', 
           TRUE ~ NA_character_
         )) %>% 
  mutate(first_num_position = stri_locate_first(Pck_Desc, regex = "\\d")[,1],
         last_space_position = stri_locate_last(Pck_Desc, regex = "\\s")[,1],
         Dosage = if_else(is.na(Dosage), 
                          str_squish(substr(Pck_Desc, 
                                            first_num_position, 
                                            last_space_position - 1)), 
                          Dosage)) %>% 
  group_by(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
           Prod_Desc, Corp_Desc, `Period-MAT`, `CITY-EN`, 
           TherapeuticClsII, Prod_CN_Name,  Package, Quantity, 
           `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
           `是否是MNC`, `ATC3中文分类`) %>% 
  summarise(Pck_Desc = last(Pck_Desc), 
            Dosage = last(Dosage), 
            Sales = round(sum(Sales, na.rm = TRUE), 2),
            Units = round(sum(Units, na.rm = TRUE)),
            DosageUnits = round(sum(DosageUnits, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(Sales_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ Sales / 0.75,
                               substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ Sales / 0.25,
                               substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ Sales / 0.9,
                               substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ Sales / 0.1,
                               TRUE ~ Sales),
         Units_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ Units / 0.75,
                               substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ Units / 0.25,
                               substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ Units / 0.9,
                               substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ Units / 0.1,
                               TRUE ~ Units),
         DosageUnits_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ DosageUnits / 0.75,
                                     substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ DosageUnits / 0.25,
                                     substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ DosageUnits / 0.9,
                                     substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ DosageUnits / 0.1,
                                     TRUE ~ DosageUnits)) %>%
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, 
         `Period-MAT`, `CITY-EN`, TherapeuticClsII, Prod_CN_Name,  Package, 
         Dosage, Quantity, `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, 
         `是否是中标品种`, `是否是MNC`, `ATC3中文分类`, Sales_raw, Units_raw, 
         DosageUnits_raw) %>% 
  arrange(Channel, Date, Province, City, MKT, Pack_ID)

write.xlsx(chc.result1, "03_Outputs/08_Servier_CHC_2016Q4_2020Q2_m.xlsx")

# QC
chk <- chc.result1 %>% 
  add_count(Channel, Date, City, MKT, Pack_ID) %>% 
  filter(n > 1)



