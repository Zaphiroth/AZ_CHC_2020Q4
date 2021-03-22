# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ CHC
# Purpose:      Market definition
# programmer:   Zhe Liu
# Date:         2021-03-04
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


MarketDef <- function(raw.az, 
                      raw.servier, 
                      raw.pfizer, 
                      market.mapping, 
                      market.cndrug) {
  
  ## GI
  gi.1 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        molecule %in% c("ESOMEPRAZOLE", "OMEPRAZOLE", "PANTOPRAZOLE", 
                        "LANSOPRAZOLE", "RABEPRAZOLE") ~ 1, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  gi.2 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        product %in% c("YI LI AN           LZB") ~ 2, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  gi.3 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        stri_sub(atc4, 1, 4) == "A06A" & 
          stri_sub(nfc, 1, 1) %in% c("A", "B", "D") ~ 3, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  raw.gi <- bind_rows(gi.1, gi.2, gi.3) %>% 
    mutate(TA = 'GI') %>% 
    group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
             molecule, product, corp, packid, flag_mkt) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(market.mapping, by = 'flag_mkt')
  
  ## RE
  re.5 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        product %in% c("SYMBICORT TURBUHAL AZN", "SERETIDE           GSK", 
                       "FOSTER             C5I", "RELVAR             GSK") ~ 5, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  re.6 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        product == "BRICANYL           AZM" & 
          molecule == "TERBUTALINE" & 
          stri_sub(packid, 1, 5) == "14018" & 
          stri_sub(nfc, 1, 2) == "RG" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "SU SHUN            SU9" & 
          molecule == "TERBUTALINE" & 
          stri_sub(packid, 1, 5) == "16352" & 
          stri_sub(nfc, 1, 2) == "FM" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "SU SHUN            SU9" & 
          molecule == "TERBUTALINE" & 
          stri_sub(packid, 1, 5) == "16352" & 
          stri_sub(nfc, 1, 2) == "FQ" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "SALBUTAMOL SULFATE SSZ" & 
          molecule == "SALBUTAMOL" & 
          stri_sub(packid, 1, 5) == "56285" & 
          stri_sub(nfc, 1, 2) == "RG" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "VENTOLIN           GSK" & 
          molecule == "SALBUTAMOL" & 
          stri_sub(packid, 1, 5) == "02003" & 
          stri_sub(nfc, 1, 2) == "RG" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "SALBUTAMOL         JO-" & 
          molecule == "SALBUTAMOL" & 
          stri_sub(packid, 1, 5) == "01734" & 
          stri_sub(nfc, 1, 2) == "FM" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "SALBUTAMOL  SULFAT SHT" & 
          molecule == "SALBUTAMOL" & 
          stri_sub(packid, 1, 5) == "52133" & 
          stri_sub(nfc, 1, 2) == "FQ" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "DA FEN KE CHUANG   SZA" & 
          molecule == "SALBUTAMOL" & 
          stri_sub(packid, 1, 5) == "36434" & 
          stri_sub(nfc, 1, 2) == "RG" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "SALBUTAMOL  SULFAT SFU" & 
          molecule == "SALBUTAMOL" & 
          stri_sub(packid, 1, 5) == "55281" & 
          stri_sub(nfc, 1, 2) == "FQ" & 
          stri_sub(atc4, 1, 4) == "R03A" ~ 6, 
        product == "ATROVENT           B.I" & 
          molecule == "IPRATROPIUM BROMIDE" & 
          stri_sub(packid, 1, 5) == "04354" & 
          stri_sub(nfc, 1, 2) == "RG" & 
          stri_sub(atc4, 1, 4) == "R03K" ~ 6, 
        product == "COMBIVENT          B.I" & 
          molecule == "IPRATROPIUM BROMIDE+SALBUTAMOL" & 
          stri_sub(packid, 1, 5) == "07319" & 
          stri_sub(nfc, 1, 2) == "RG" & 
          stri_sub(atc4, 1, 4) == "R03L" ~ 6, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  re.7 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        stri_sub(atc4, 1, 4) == "R05C" & nfc != "ABD" ~ 7, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  re.8 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        stri_sub(atc4, 1, 3) == "R03" ~ 8, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  raw.re <- bind_rows(re.5, re.6, re.7, re.8) %>% 
    mutate(TA = 'RE') %>% 
    group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
             molecule, product, corp, packid, flag_mkt) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(market.mapping, by = 'flag_mkt')
  
  ## CV
  cv.9 <- raw.servier %>% 
    mutate(
      flag_mkt = case_when(
        stri_sub(atc4, 1, 4) == "C07A" & 
          stri_sub(nfc, 1, 1) %in% c("A", "B") ~ 9, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.10 <- raw.servier %>% 
    mutate(
      flag_mkt = case_when(
        molecule == "IVABRADINE" ~ 10, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.11 <- raw.servier %>% 
    mutate(
      flag_mkt = case_when(
        stri_sub(atc4, 1, 4) == "C08A" ~ 11, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.12 <- raw.servier %>% 
    mutate(
      flag_mkt = case_when(
        product == "EXFORGE            NVR" ~ 12, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.13 <- raw.pfizer %>% 
    mutate(
      flag_mkt = case_when(
        product == "CADUET             PFZ" ~ 13, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0) %>% 
    bind_rows(raw.servier) %>% 
    mutate(
      flag_mkt = case_when(
        !is.na(flag_mkt) ~ flag_mkt, 
        stri_sub(atc4, 1, 3) %in% c("C03", "C07", "C08", "C09") ~ 13, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.14 <- raw.pfizer %>% 
    mutate(
      flag_mkt = case_when(
        atc4 == "C10A1" ~ 14,
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.15 <- raw.pfizer %>% 
    mutate(
      flag_mkt = case_when(
        product %in% c("CADUET             PFZ", "VYTORIN            MSD", 
                       "EZETROL            SG7") ~ 15, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.16 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        atc4 == "B01C2" ~ 16, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.17 <- raw.pfizer %>% 
    mutate(
      flag_mkt = case_when(
        molecule %in% c("ATORVASTATIN", "ROSUVASTATIN", "SIMVASTATIN", 
                        "PITAVASTATIN", "PRAVASTATIN", "FLUVASTATIN", 
                        "EZETIMIBE", "LOVASTATIN", "EZETIMIBE+SIMVASTATIN") ~ 17, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.18 <- raw.pfizer %>% 
    mutate(
      flag_mkt = case_when(
        product == "CADUET             PFZ" ~ 18, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0) %>% 
    bind_rows(raw.az) %>% 
    mutate(
      flag_mkt = case_when(
        !is.na(flag_mkt) ~ flag_mkt, 
        product %in% c("XUE ZHI KANG       BWX", "ZHI BI TAI         DJP", 
                       "JIANG ZHI TONG MAI YYK") ~ 18,
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  cv.19 <- raw.az %>% 
    filter(stri_sub(packid, 1, 5) %in% market.cndrug$PROD_COD) %>% 
    mutate(flag_mkt = 19)
  
  raw.cv <- bind_rows(cv.9, cv.10, cv.11, cv.12, cv.13, cv.14, 
                      cv.15, cv.16, cv.17, cv.18, cv.19) %>% 
    mutate(TA = 'CV') %>% 
    group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
             molecule, product, corp, packid, flag_mkt) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(market.mapping, by = 'flag_mkt')
  
  ## DM
  dm.20 <- raw.servier %>% 
    mutate(
      flag_mkt = case_when(
        stri_sub(atc4, 1, 4) %in% c("A10L", "A10H", "A10M", "A10J", 
                                    "A10K", "A10P", "A10S", "A10N") ~ 20, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  dm.21 <- raw.servier %>% 
    mutate(
      flag_mkt = case_when(
        product %in% c("EUCREAS            NVR", "KOMBIGLYZE XR      AZN", 
                       "TRAJENTA DUO       B.I", "ONGLYZA            AZN", 
                       "JANUVIA            MSD", "GALVUS             NVR", 
                       "TRAJENTA           B.I", "NESINA             TAK", 
                       "JANUMET            MSD") ~ 21, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  dm.22 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        product %in% c("INVOKANA           MCK") ~ 22, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0) %>% 
    bind_rows(raw.servier) %>% 
    mutate(
      flag_mkt = case_when(
        !is.na(flag_mkt) ~ flag_mkt, 
        product %in% c("FORXIGA            AZN", "JARDIANCE          B.I") ~ 22, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  raw.dm <- bind_rows(dm.20, dm.21, dm.22) %>% 
    mutate(TA = 'DM') %>% 
    group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
             molecule, product, corp, packid, flag_mkt) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(market.mapping, by = 'flag_mkt')
  
  ## Renal
  renal.23 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        !(molecule %in% c("FOLIC ACID", "CYANOCOBALAMIN+FOLIC ACID+NICOTINAMIDE")) & 
          stri_sub(atc4, 1, 3) == "B03" ~ 23, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  renal.24 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        molecule == "POLYSTYRENE SULFONATE" ~ 24, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  renal.25 <- raw.az %>% 
    mutate(
      flag_mkt = case_when(
        product == "Lokelma" ~ 25, 
        TRUE ~ 0
      )
    ) %>% 
    filter(flag_mkt != 0)
  
  raw.renal <- bind_rows(renal.23, renal.24, renal.25) %>% 
    mutate(TA = 'Renal') %>% 
    group_by(year, date, quarter, province, city, district, pchc, TA, atc4, nfc, 
             molecule, product, corp, packid, flag_mkt) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(market.mapping, by = 'flag_mkt')
  
  ## total
  raw.total <- bind_rows(raw.gi, raw.re, raw.cv, raw.dm, raw.renal) %>% 
    filter(!is.na(pchc), pchc != '#N/A', units > 0, sales > 0) %>% 
    group_by(pchc) %>% 
    mutate(province = first(na.omit(province)), 
           city = first(na.omit(city)), 
           district = first(na.omit(district))) %>% 
    ungroup() %>% 
    group_by(year, date, quarter, province, city, district, pchc, TA, atc4, 
             molecule, product, packid, flag_mkt) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  return(raw.total)
}