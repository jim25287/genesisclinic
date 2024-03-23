df03_FLC_self_report <- tmp_03 %>% select(-mobile)

#adjust
df03_FLC_self_report <- df03_FLC_self_report %>% filter(program %in% c("宋醫師專班 -FLC班","經典八週","2023 FLC-2個助教","診所八週(週一開班)-宋醫師班/初日班","宋醫師進階計畫","診所進階計畫","診所八週(週四啟動)-初日班","進階計畫","經典八週（202109新版）享瘦班"))

df03_FLC_self_report <- df03_FLC_self_report %>% select(-c(age, measurement_after_program_date, measurement_before_program_date))

#C1. col_names
names(df03_FLC_self_report) <- names(df03_FLC_self_report) %>% lin_ch_en_format(., format = "en", origin = "raw_en")
#C1-2. filter by program not "^診所"
df03_FLC_self_report <- df03_FLC_self_report[df03_FLC_self_report[["program"]] %>% grepl("^診所",.) %>% not(),]

df03_FLC_self_report <- df03_FLC_self_report[with(df03_FLC_self_report, order(date_flc_T0, id)),]

#C2. age: btd - date_t0 年齡(療程起始當天計算)
df03_FLC_self_report$age <- (lubridate::ymd(df03_FLC_self_report$date_flc_T0) - lubridate::ymd(df03_FLC_self_report$btd)) %>% as.numeric() %>% divide_by(365) %>% floor()



## Data Cleaning
# - Weight record: from 10_daily_flc.sql(tmp_03_day)
# - Diet record: from 07_Deit_meal.sql > (df06_Diet_day)

a1_weight <- 
  tmp_03_day %>% 
  mutate(id = client_id) %>% 
  select(
    c(
      "id",
      "date",
      "weight",
      "bmi",
      "waist_circumference",
      "body_fat_mass"
    )
  )

# [Duplicated id, date_flc_T0, 2420320宜婷: 可能是營養師加入班級見習，兩筆都不是正式data。]
a1_weight <- a1_weight %>% distinct(id, date, .keep_all = TRUE)


a1_diet <- 
  df06_Diet_day %>%
  mutate(id = client_id) %>%
  mutate(date = date_diet) %>%
  select(
    c(
      "id",
      "date",
      "note_counts",
      "pic_counts",
      "light_green_count",
      "light_yellow_count",
      "light_red_count",
      "carb_e_day",
      "protein_e_day",
      "fat_e_day",
      "calorie_day",
      "carbohydrate_target",
      "protein_target",
      "fat_target",
      "calorie_target",
      "grains_day",
      "meat_bean_day",
      "oil_day",
      "vegetables_day",
      "fruits_day",
      "milk_day",
      "grains_target",
      "meat_bean_target",
      "oil_target",
      "vegetables_target",
      "fruits_target",
      "milk_target",
      "grains_day_deficit",
      "meat_bean_day_deficit",
      "oil_day_deficit",
      "vegetables_day_deficit",
      "fruits_day_deficit",
      "milk_day_deficit",
      "carb_e_day_deficit",
      "protein_e_day_deficit",
      "fat_e_day_deficit",
      "calorie_deficit"
    )
  )

# [Duplicated id, date_flc_T0, 2420320宜婷: 可能是營養師加入班級見習，兩筆都不是正式data。]
a1_diet <- a1_diet %>% distinct(id, date, .keep_all = TRUE)

# Merge Weight & Diet Record.
a1 <- left_join(a1_diet, a1_weight, by = c("id", "date"))


# class info
a2 <- 
df03_FLC_self_report %>% select(id, date_flc_T0, date_flc_T1) %>% 
  mutate(date_flc_w8 = lubridate::ymd(df03_FLC_self_report$date_flc_T0) + 56) %>% 
  mutate(program_duration_by_day = as.numeric((lubridate::ymd(df03_FLC_self_report$date_flc_T1) - lubridate::ymd(df03_FLC_self_report$date_flc_T0)))) %>% 
  mutate(program_duration_by_weak = as.numeric((lubridate::ymd(df03_FLC_self_report$date_flc_T1) - lubridate::ymd(df03_FLC_self_report$date_flc_T0))/7))

# Abnormal case <8 wks program
a2 <- a2 %>% filter(program_duration_by_weak >= 8)

# [Duplicated id, date_flc_T0, 2420320宜婷: 可能是營養師加入班級見習，兩筆都不是正式data。]
#[*把重複的人刪掉]
a2 <- a2 %>% filter(!(id %in% (a2[which(duplicated(a2[c("id", "date_flc_T0")]) | duplicated(a2[c("id", "date_flc_T0")], fromLast = TRUE)), "id"])))
# a2 <- a2 %>% distinct(id, date_flc_T0, .keep_all = TRUE)

# -- debug line begins

# [Duplicated id, date_flc_T0, 2420320宜婷: 可能是營養師加入班級見習，兩筆都不是正式data。]
# a2[which(duplicated(a2[c("id", "date_flc_T0")]) | duplicated(a2[c("id", "date_flc_T0")], fromLast = TRUE)),] %>% view()

# -- debug line ends

# [參與多次課程] set multiple = "all"
# a3: aggregate Weight / Diet / Program info
a3 <- left_join(a1, a2, by = "id", multiple = "all")

tmp <- df03_FLC_self_report %>% select(
  c(
    "class_id",
    "class_name",
    "program",
    "date_flc_T0",
    "date_flc_T1",
    "nutritionist_online",
    "id",
    "date_latest_update",
    "btd",
    "gender",
    "height",
    "age"
  )
)

# -- debug line begins

# [Duplicated id, date_flc_T0, 2420321宜婷: ]
# tmp[which(duplicated(tmp[c("id", "date_flc_T0")]) | duplicated(tmp[c("id", "date_flc_T0")], fromLast = TRUE)),] %>% view()

# -- debug line ends

# filter out可能是測試資料 12人共24 raws
tmp <- tmp %>% filter(!(id %in% (tmp[which(duplicated(tmp[c("id", "date_flc_T0")]) | duplicated(tmp[c("id", "date_flc_T0")], fromLast = TRUE)), "id"])))


tmp <- left_join(tmp, a3, by = c("id", "date_flc_T0", "date_flc_T1"), multiple = "all")


rm(list = c("a1", "a2", "a1_weight", "a1_diet"))
# Set start date cut-off for analysis

a3 <- tmp

a3 <-
  a3 %>% filter(!(is.na(date)) &
                  !(is.na(date_flc_T0)) & (date_flc_T0 >= "2017-01-01"))

# [random sampling-check] Good to go!
# a3[sample(a3 %>% nrow(), 10),] %>% select(id, date, calorie_day) %>% view()


# Performance of Entire Program

a3 %>% filter( (date >= date_flc_T0) & (date <= date_flc_T1) ) %>% filter((date_flc_T1 <= Sys.Date()))

## Weight Record
# !!2Do

## Diet Record
# !!2Do






# Performance of 8-week Program
df03_FLC_self_report_w8df <- 
  a3 %>% filter( (date >= date_flc_T0) & (date <= date_flc_w8) ) %>% filter((date_flc_w8 <= Sys.Date()))

df03_FLC_self_report_w8df <- df03_FLC_self_report_w8df %>% mutate(day_no = as.numeric((lubridate::ymd(df03_FLC_self_report_w8df$date) - as.numeric((lubridate::ymd(df03_FLC_self_report_w8df$date_flc_T0)))))) %>% 
  mutate(week_no = (day_no/7) %>% ceiling())

## Weight Record

a1_profile <- 
  df03_FLC_self_report_w8df %>% select(
    c(
      "class_id",
      "class_name",
      "program",
      "date_flc_T0",
      "date_flc_T1",
      "date_flc_w8",
      "nutritionist_online",
      "id",
      "date_latest_update",
      "btd",
      "gender",
      "height",
      "age"
    )
  ) %>% distinct(id, date_flc_T0, .keep_all = TRUE)

  
a1_weight <- 
df03_FLC_self_report_w8df %>% select(
  c(
    "date_flc_T0",
    "id",
    "date",
    "weight",
    "bmi",
    "waist_circumference",
    "body_fat_mass",
    "program_duration_by_day",
    "program_duration_by_weak",
    "day_no",
    "week_no"
  )
) %>% filter(!is.na(weight)) %>%
  group_by(id, date_flc_T0) %>% 
  summarise(
    `weight(T0)` = first(weight, na_rm = TRUE),
    `weight(T1)` = last(weight, na_rm = TRUE),
    `∆weight` = `weight(T1)`-`weight(T0)`,
    `∆weight%` = ((`weight(T1)`-`weight(T0)`)/`weight(T0)`*100) %>% round(2),
    
    `BMI(T0)` = first(bmi, na_rm = TRUE),
    `BMI(T1)` = last(bmi, na_rm = TRUE),
    `∆BMI` = `BMI(T1)`-`BMI(T0)`,
    `∆BMI%` = ((`BMI(T1)`-`BMI(T0)`)/`BMI(T0)`*100) %>% round(2),

    `Fat(T0)` = first(body_fat_mass, na_rm = TRUE),
    `Fat(T1)` = last(body_fat_mass, na_rm = TRUE),
    `∆Fat` = `Fat(T1)`-`Fat(T0)`,
    `∆Fat%` = ((`Fat(T1)`-`Fat(T0)`)/`Fat(T0)`*100) %>% round(2),
    
        
    `wc(T0)` = first(waist_circumference, na_rm = TRUE),
    `wc(T1)` = last(waist_circumference, na_rm = TRUE),
    `∆wc` = `wc(T1)`-`wc(T0)`,
    `∆wc%` = ((`wc(T1)`-`wc(T0)`)/`wc(T0)`*100) %>% round(2),
    
    `day_no(T0)_weight` = first(day_no, na_rm = TRUE),
    `day_no(T1)_weight` = last(day_no, na_rm = TRUE),
    `week_no(T0)_weight` = first(week_no, na_rm = TRUE),
    `week_no(T1)_weight` = last(week_no, na_rm = TRUE),
    
  )



## Diet Record

a1_diet <- 
df03_FLC_self_report_w8df %>% select(
  c(
    "date_flc_T0",
    "id",
    "date",
    "note_counts",
    "pic_counts",
    "light_green_count",
    "light_yellow_count",
    "light_red_count",
    "carb_e_day",
    "protein_e_day",
    "fat_e_day",
    "calorie_day",
    "carbohydrate_target",
    "protein_target",
    "fat_target",
    "calorie_target",
    "grains_day",
    "meat_bean_day",
    "oil_day",
    "vegetables_day",
    "fruits_day",
    "milk_day",
    "grains_target",
    "meat_bean_target",
    "oil_target",
    "vegetables_target",
    "fruits_target",
    "milk_target",
    "grains_day_deficit",
    "meat_bean_day_deficit",
    "oil_day_deficit",
    "vegetables_day_deficit",
    "fruits_day_deficit",
    "milk_day_deficit",
    "carb_e_day_deficit",
    "protein_e_day_deficit",
    "fat_e_day_deficit",
    "calorie_deficit",
    "program_duration_by_day",
    "program_duration_by_weak",
    "day_no",
    "week_no"
  )
) %>% filter(!is.na(note_counts)) %>%
  group_by(id, date_flc_T0) %>% 
  summarise(
    upload_day_p = (n() *100 / (program_duration_by_day + 1)) %>% round(2), 
    note_counts = sum(note_counts, na.rm = TRUE),
    pic_count = sum(pic_counts, na.rm = TRUE),
    
    light_green_p = (sum(light_green_count, na.rm = TRUE) *100 / (sum(sum(light_green_count, na.rm = TRUE),
                                                                      sum(light_yellow_count, na.rm = TRUE),
                                                                      sum(light_red_count, na.rm = TRUE)))) %>% round(2),
    light_yellow_p = (sum(light_yellow_count, na.rm = TRUE) *100 / (sum(sum(light_green_count, na.rm = TRUE),
                                                                        sum(light_yellow_count, na.rm = TRUE),
                                                                        sum(light_red_count, na.rm = TRUE)))) %>% round(2),
    light_red_p = (sum(light_red_count, na.rm = TRUE) *100 / (sum(sum(light_green_count, na.rm = TRUE),
                                                                  sum(light_yellow_count, na.rm = TRUE),
                                                                  sum(light_red_count, na.rm = TRUE)))) %>% round(2),
    
    #Actual intake
    calorie_day_tmp = (sum(calorie_day, na.rm = TRUE) / (n())) %>% round(2), 
    carb_ep = (sum(carb_e_day, na.rm = TRUE)*100 / sum(calorie_day, na.rm = TRUE)) %>% round(2),
    protein_ep = (sum(protein_e_day, na.rm = TRUE)*100 / sum(calorie_day, na.rm = TRUE)) %>% round(2),
    fat_ep = (sum(fat_e_day, na.rm = TRUE)*100 / sum(calorie_day, na.rm = TRUE)) %>% round(2),
    
    grains_day = (sum(grains_day, na.rm = TRUE) / (n())) %>% round(2),  
    meat_bean_day = (sum(meat_bean_day, na.rm = TRUE) / (n())) %>% round(2),  
    oil_day = (sum(oil_day, na.rm = TRUE) / (n())) %>% round(2),
    vegetables_day = (sum(vegetables_day, na.rm = TRUE) / (n())) %>% round(2),  
    fruits_day = (sum(fruits_day, na.rm = TRUE) / (n())) %>% round(2),  
    milk_day = (sum(milk_day, na.rm = TRUE) / (n())) %>% round(2),  
    
    #Target intake
    calorie_target_tmp = (mean(calorie_target, na.rm = TRUE)) %>% round(2), 
    carb_ep_target = (mean(carbohydrate_target, na.rm = TRUE)*4*100 / mean(calorie_target, na.rm = TRUE)) %>% round(2),
    protein_ep_target = (mean(protein_target, na.rm = TRUE)*4*100 / mean(calorie_target, na.rm = TRUE)) %>% round(2),
    fat_ep_target = (mean(fat_target, na.rm = TRUE)*9*100 / mean(calorie_target, na.rm = TRUE)) %>% round(2),
    
    grains_target = (sum(grains_target, na.rm = TRUE) / (n())) %>% round(2),  
    meat_bean_target = (sum(meat_bean_target, na.rm = TRUE) / (n())) %>% round(2),  
    oil_target = (sum(oil_target, na.rm = TRUE) / (n())) %>% round(2),
    vegetables_target = (sum(vegetables_target, na.rm = TRUE) / (n())) %>% round(2),  
    fruits_target = (sum(fruits_target, na.rm = TRUE) / (n())) %>% round(2),  
    milk_target = (sum(milk_target, na.rm = TRUE) / (n())) %>% round(2),  
    
    #Deficit (Target - Actual, make deficit as positive(+) value)
    calorie_day_deficit_tmp = (mean(calorie_deficit, na.rm = TRUE)*(-1)) %>% round(2),
    carb_e_day_deficit = (sum(carb_e_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    protein_e_day_deficit = (sum(protein_e_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    fat_e_day_deficit = (sum(fat_e_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    
    grains_day_deficit = (sum(grains_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),  
    meat_bean_day_deficit = (sum(meat_bean_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),  
    oil_day_deficit = (sum(oil_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    vegetables_day_deficit = (sum(vegetables_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),  
    fruits_day_deficit = (sum(fruits_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),  
    milk_day_deficit = (sum(milk_day_deficit, na.rm = TRUE)*(-1) / (n())) %>% round(2),
    
    `day_no(T0)_diet` = first(day_no, na_rm = TRUE),
    `day_no(T1)_diet` = last(day_no, na_rm = TRUE),
    `week_no(T0)_diet` = first(week_no, na_rm = TRUE),
    `week_no(T1)_diet` = last(week_no, na_rm = TRUE),
    
  ) %>% distinct(id, .keep_all = TRUE)

colnames(a1_diet) <- gsub("_tmp", colnames(a1_diet), replacement = "")

names(a1_diet) <- names(a1_diet) %>% lin_ch_en_format(format = "en", origin = "raw_en")


## [Client w/t weight record]
# setdiff(a1_diet$id, a1_weight$id)

a1_profile <- left_join(
  a1_profile,
  a1_weight,
  by = c(
    "id",
    "date_flc_T0")
)

a1_profile <- left_join(
  a1_profile,
  a1_diet,
  by = c(
    "id",
    "date_flc_T0")
)


# !![Well-Cleaned df]
df03_FLC_self_report_w8df <- a1_profile

# [NaN -> NA Conversion]: 沒有燈號、沒有設定目標(e.g., 48361)
df03_FLC_self_report_w8df <- df03_FLC_self_report_w8df %>%
  mutate_if(is.numeric, ~ ifelse(is.nan(.), NA, .)) %>% 
  mutate_if(is.numeric, ~ ifelse(is.infinite(.), NA, .))

df03_FLC_self_report_w8df <-
df03_FLC_self_report_w8df %>% filter((`∆weight%` > quantile(df03_FLC_self_report_w8df$`∆weight%`, 0.01, na.rm = TRUE)) & (`∆weight%` < quantile(df03_FLC_self_report_w8df$`∆weight%`, 0.99, na.rm = TRUE)))




# Combination Tag of weight/diet upload types: "week_1st/week_last" 

# by week

tab <- table(T0 = df03_FLC_self_report_w8df$`week_no(T0)_weight`, T1 = df03_FLC_self_report_w8df$`week_no(T1)_weight`)

tab1 <- tab %>% prop.table() %>% round(4)
tab1 <- as.data.frame(tab1, stringsAsFactors = TRUE)

tab <- as.data.frame(tab, stringsAsFactors = TRUE)
tab$Percent <- tab1$Freq

tab %>% 
  filter(Freq != 0) %>% 
  ggplot(aes(T0, T1, fill = Percent)) +
  geom_tile() +
  # geom_text(aes(label = scales::percent(Percent))) +
  geom_text(aes(label = paste(Freq, scales::percent(Percent), sep = "\n") )) +
  # scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "OrRd")) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = .185) +
  # scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = .185) +
  # scale_fill_gradient(low = "white", high = "darkred") +
  labs(x = "1st Record", y = "Last Record", title = "Confusion Matrix of Weight Record",
       fill = "分佈佔比") +
  theme_classic() +
  theme(plot.title = element_text(size = 25, hjust = 0.5, 
                                  margin = margin(20, 0, 20, 0)),
        legend.title = element_text(size = 14, margin = margin(0, 20, 10, 0)),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 18),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 18))


tab <- tab %>% mutate(wk_diff = as.numeric(T1) - as.numeric(T0))
tab %>% 
  group_by(wk_diff) %>% 
  summarise(
    Freq = sum(Freq),
    Percent = sum(Percent)*100 %>% round(3)
  ) %>% 
  as.data.frame() %>% 
  filter(Freq != 0) %>% 
  mutate(output = paste0(Freq, " (", Percent, "%)"))


# by day  
tab <- table(T0 = df03_FLC_self_report_w8df$`day_no(T0)_weight`, T1 = df03_FLC_self_report_w8df$`day_no(T1)_weight`)

tab1 <- tab %>% prop.table() %>% round(4)
tab1 <- as.data.frame(tab1, stringsAsFactors = TRUE)

tab <- as.data.frame(tab, stringsAsFactors = TRUE)
tab$Percent <- tab1$Freq

tab %>% 
  filter(Freq != 0) %>% 
  ggplot(aes(T0, T1, fill = Percent)) +
  geom_tile() +
  # geom_text(aes(label = scales::percent(Percent))) +
  # geom_text(aes(label = paste(Freq, scales::percent(Percent), sep = "\n") )) +
  scale_fill_gradientn(colours = c("#BFFF70", "#FFFF40",
                                   "#FFD000", "#FFA000", "#FF7000", "#FF4000", "#FF0000")) +
  # scale_fill_gradient(low = "white", high = "#3575b5") +
  labs(x = "1st Record", y = "Last Record", title = "Confusion matrix of Weight Record",
       fill = "分佈佔比") +
  theme_classic() +
  theme(plot.title = element_text(size = 25, hjust = 0.5, 
                                  margin = margin(20, 0, 20, 0)),
        legend.title = element_text(size = 14, margin = margin(0, 20, 10, 0)),
        axis.title.x = element_text(margin = margin(20, 20, 20, 20), size = 18),
        axis.title.y = element_text(margin = margin(0, 20, 0, 10), size = 18))

tab <- tab %>% mutate(day_diff = as.numeric(T1) - as.numeric(T0))
tab %>% 
  group_by(day_diff) %>% 
  summarise(
    Freq = sum(Freq),
    Percent = sum(Percent)*100 %>% round(3)
  ) %>% 
  as.data.frame() %>% 
  filter(Freq != 0) %>% 
  mutate(output = paste0(Freq, " (", Percent, "%)"))



df03_FLC_self_report_w8df <- df03_FLC_self_report_w8df %>% mutate(wk_diff = ((as.numeric(`day_no(T1)_weight`) - as.numeric(`day_no(T0)_weight`))/7) %>% ceiling())
# df03_FLC_self_report_w8df$wk_diff %>% table()

df03_FLC_self_report_w8df %>% 
  mutate(a = -`∆weight%`) %>% 
  group_by(wk_diff) %>% 
  summarise(
    a = mean(a, na.rm = T),
    N = n()
  )

df03_FLC_self_report_w8df %>% 
  mutate(a = -`∆weight%`) %>% 
  ggbarplot(x = "wk_diff", y = "a", fill = "wk_diff", alpha = 0.5,
            add = "mean_se", add.params = list(group = "wk_diff"),
            position = position_dodge(0.8), legend = "none", legend.title = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "週數差", y = "Mean ± SE", title = "減重成效(%)") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
  ) 
