# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q4
# Purpose:      Readin Raw Data
# programmer:   Zhe Liu
# Date:         2021-03-03
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
## PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20210303.xlsx", sheet = "PCHC")

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

## molecule
ims_prod_ref <- fread("02_Inputs/cn_prod_ref_201912_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0")) %>% 
  select(Pack_Id, NFC123_Code)

ims.mol.raw <- read.xlsx("02_Inputs/ims_chpa_to20Q4.xlsx", startRow = 3, cols = 1:21)

ims.mol1 <- ims.mol.raw[, 1:21] %>% 
  distinct() %>% 
  filter(!is.na(Pack_Id)) %>% 
  left_join(ims_prod_ref, by = "Pack_Id") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, MNF_TYPE, MnfType_Desc, 
         Mnf_Desc, ATC4_Code, NFC123_Code, Prd_desc, Pck_Desc, 
         Molecule_Desc)

ims_prod_ref <- fread("02_Inputs/cn_prod_ref_201912_1.txt") %>% 
  setDF() %>% 
  mutate(Pack_Id = str_pad(Pack_Id, 7, "left", pad = "0"))

ims_mol_lkp_ref <- fread("02_Inputs/cn_mol_lkp_201912_1.txt") %>%
  setDF() %>%
  arrange(Pack_ID, Molecule_ID) %>%
  mutate(Pack_ID  = str_pad(Pack_ID , 7, "left", pad = "0"))

ims_mol_ref <- fread("02_Inputs/cn_mol_ref_201912_1.txt")

ims_corp_ref <- fread("02_Inputs/cn_corp_ref_201912_1.txt")

ims.mol2 <- ims_mol_lkp_ref %>%
  left_join(ims_mol_ref, by = c("Molecule_ID" = "Molecule_Id")) %>%
  arrange(Pack_ID, Molecule_Desc) %>%
  group_by(Pack_ID) %>%
  summarise(Molecule_Desc = paste(Molecule_Desc, collapse = "+")) %>%
  ungroup() %>%
  left_join(ims_prod_ref, by = c("Pack_ID" = "Pack_Id")) %>%
  left_join(ims_corp_ref, by = "Corp_ID") %>% 
  select(packid = Pack_ID, Corp_ID, Corp_Desc, ATC4_Code, NFC123_Code,
         Prd_desc, Pck_Desc, Molecule_Desc)

ims.mol <- ims.mol2 %>% 
  filter(!(packid %in% ims.mol1$packid)) %>% 
  mutate(Corp_ID = stri_pad_left(Corp_ID, 4, 0)) %>% 
  bind_rows(ims.mol1)

## target city
kTargetCity <- c("北京", "上海", "杭州", "广州", "南京", "苏州", "宁波", 
                 "福州", "无锡", "温州", "济南", "青岛", "金华", "常州", 
                 "徐州", "台州", "厦门")


##---- Formatting raw data ----
## history
history.az <- read_feather("02_Inputs/data/01_AZ_CHC_Total_Raw.feather") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

history.pfizer <- read_feather("02_Inputs/data/01_Pfizer_CHC_Total_Raw.feather") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

history.servier <- read_feather("02_Inputs/data/01_Servier_CHC_Total_Raw.feather") %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

## Beijing
raw.bj1 <- read_csv('02_Inputs/data/【法柏】20Q4北京交付表修-2103_Formatted_packid_moleinfo.csv', 
                    locale = locale(encoding = 'GB18030'))

raw.bj <- raw.bj1 %>% 
  filter(Quarter == '2020Q4') %>% 
  distinct(project = Project, 
           year = as.character(Year), 
           quarter = Quarter, 
           date = gsub('/', '', Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital'))

## Guangzhou
raw.gz1 <- read.xlsx('02_Inputs/data/gz_广东省_2020Q4_packid_moleinfo.xlsx')

raw.gz <- raw.gz1 %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = '广东', 
           city = '广州', 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital'))

## Fujian
raw.servier.pfizer.fj1 <- read_csv('02_Inputs/data/Servier_Pfizer_fj20Q3Q4_packid_moleinfo.csv', 
                                   locale = locale(encoding = 'GB18030'))

raw.servier.pfizer.fj <- raw.servier.pfizer.fj1 %>% 
  filter(Quarter == '2020Q4', 
         !is.na(packcode)) %>% 
  distinct(project = Project, 
           year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital'))

## Servier & Pfizer
raw.servier.pfizer1 <- read_csv('02_Inputs/data/Servier_Pfizer_ahbjjssd20Q4_zj20Q3Q4_packid_moleinfo.csv', 
                                locale = locale(encoding = 'GB18030'))

raw.servier.pfizer <- raw.servier.pfizer1 %>% 
  filter(!(Province %in% c('北京市'))) %>% 
  distinct(project = Project, 
           year = as.character(Year), 
           quarter = Quarter, 
           date = gsub('/', '', Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital'))

## Servier
raw.servier <- bind_rows(raw.servier.pfizer, raw.bj, raw.servier.pfizer.fj) %>% 
  filter(project == 'Servier') %>% 
  bind_rows(raw.gz, history.servier) %>% 
  filter(quarter %in% c('2019Q4', '2020Q4'), 
         !is.na(pchc), !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  select(year, date, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, 
         corp = Corp_Desc, packid, units, sales)

write.xlsx(raw.servier, '03_Outputs/01_Raw_Servier.xlsx')

## Pfizer
raw.pfizer <- bind_rows(raw.servier.pfizer, raw.bj, raw.servier.pfizer.fj) %>% 
  filter(project == 'Pfizer') %>% 
  bind_rows(raw.gz, history.pfizer) %>% 
  filter(quarter %in% c('2019Q4', '2020Q4'), 
         !is.na(pchc), !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  select(year, date, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, 
         corp = Corp_Desc, packid, units, sales)

write.xlsx(raw.pfizer, '03_Outputs/01_Raw_Pfizer.xlsx')

## AZ Fujian
raw.az.fj1 <- read_csv('02_Inputs/data/AZ_fj_20Q4_packid_moleinfo(predicted by AZ_fj_2018_packid_moleinfo_v2).csv', 
                       locale = locale(encoding = 'GB18030'))

raw.az.fj <- raw.az.fj1 %>% 
  filter(Quarter == '2020Q4', 
         !is.na(packcode)) %>% 
  distinct(project = Project, 
           year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital'))

## AZ
raw.az1 <- read_csv('02_Inputs/data/AZ_ahbjjssdzj_20Q4_packid_moleinfo.csv', 
                    locale = locale(encoding = 'GB18030'))

raw.az <- raw.az1 %>% 
  filter(!(Province %in% c('北京市')), 
         Quarter == '2020Q4', 
         !is.na(packcode)) %>% 
  distinct(project = Project, 
           year = as.character(Year), 
           quarter = Quarter, 
           date = gsub('/', '', Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == '市辖区', '北京', gsub('市', '', City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  bind_rows(raw.bj, raw.az.fj) %>% 
  filter(project == 'AZ') %>% 
  bind_rows(raw.gz, history.az) %>% 
  filter(quarter %in% c('2019Q4', '2020Q4'), 
         !is.na(pchc), 
         !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  left_join(ims.mol, by = 'packid') %>% 
  select(year, date, quarter, province, city, district, pchc, atc4 = ATC4_Code, 
         nfc = NFC123_Code, molecule = Molecule_Desc, product = Prd_desc, 
         corp = Corp_Desc, packid, units, sales)

write.xlsx(raw.az, '03_Outputs/01_Raw_AZ.xlsx')

## check
# chk <- raw.data %>% 
#   filter(is.na(pchc), 
#          grepl('中心', hospital), 
#          grepl('社区', hospital), 
#          !grepl('卫生院|卫生室|卫生站|服务站|社区站|医院', hospital)) %>% 
#   distinct(province, city, district, hospital) %>% 
#   arrange(province, city, district, hospital)

##---- TA ----
## market definition
market.mapping <- read.xlsx("02_Inputs/Market_Def_2020_CHC_0909.xlsx", 
                            sheet = "MKT DEF")[, -1] %>% 
  select(`小市场`, `大市场`, `购买方式`, flag_mkt = flag) %>% 
  distinct() %>% 
  group_by(`小市场`) %>% 
  mutate(`购买方式` = paste(unique(`购买方式`), collapse = "+")) %>% 
  ungroup()

market.cndrug <- read.xlsx("02_Inputs/Market_Def_2020_CHC_0909.xlsx", 
                           sheet = "XZK-其他降脂中药")

source('04_Codes/function/AZMarketDef.R', encoding = 'UTF-8')
raw.total <- MarketDef(raw.az = raw.az, 
                       raw.servier = raw.servier, 
                       raw.pfizer = raw.pfizer, 
                       market.mapping = market.mapping, 
                       market.cndrug = market.cndrug)

write.xlsx(raw.total, '03_Outputs/01_AZ_CHC_Raw_Total.xlsx')

## check
chk <- raw.total %>% 
  add_count(date, pchc, packid, flag_mkt) %>% 
  filter(n > 1)
