###数据筛选出需要的样本
###训练集进行数据拆分。
###对训练集，测试集进行对比。
###对分析样本，验证样本，SEER 12 剩余样本 进行对比。
rm(list = ls())
load("~/JMZ/SEER/lung/1data/SEER_lung_spm4class.rdata")
load("~/JMZ/SEER/lung/1data/SEER_lung_spm4class_timevalid.rdata")
load("~/JMZ/SEER/lung/1data/SEER_lung_spm4class_12.rdata")

###mp2的筛选可知，distant的样本量保存最多。
###m1 和 stage 的性能基本一致。
sample_f <- function(spm) {
  mp2 <- spm %>% 
    # filter(SEER_M_origin!="M1") %>%
    filter(Combined_Stage_origin != "Distant") %>%
    # filter(SEER_Stage_origin != "Stage IV") %>%
    # filter((Combined_Stage_origin == "Localized" |
    #           Combined_Stage_origin == "Regional") &
    #          SEER_M_origin == "M0" &
    #          (SEER_Stage_origin == "Stage I" |
    #             SEER_Stage_origin == "Stage II" |
    #             SEER_Stage_origin == "Stage III")
    # ) %>% 
    select(-Combined_Stage_origin)
  # Grade 这个指标 还是有点用
  rownames(mp2) <- mp2$Patient_ID
  mp2$Combined_Stage_spm <- as.character(mp2$Combined_Stage_spm) %>% as.factor()
  return(mp2)
}

mp2_origin = sample_f(spm_origin)  %>%   
  filter(Survival_time != 0) %>%  #生存时间不为0 
  na.omit() 
mp3_origin = mp2_origin %>% select(-SEER_death,-Patient_ID, -Follow_up_years) 
mp2_valid = sample_f(spm_valid)   %>%   
  filter(Survival_time != 0) %>%  #生存时间不为0 
  na.omit() 
mp3_valid = mp2_valid %>% select(-SEER_death,-Patient_ID, -Follow_up_years) 

###SEER 12 剩余样本
mp3_12 = sample_f(spm_12) %>%   
  filter(Survival_time != 0) %>%  #生存时间不为0 
  na.omit() %>% 
  left_join(mp2_origin %>% mutate(mark = "origin") %>% select(Patient_ID,mark),
            by = "Patient_ID") %>% filter(is.na(mark) == T) %>% select(-mark) %>% 
  left_join(mp2_valid %>% mutate(mark = "valid") %>% select(Patient_ID,mark),
            by = "Patient_ID") %>% filter(is.na(mark) == T) %>% select(-mark) %>% 
  select(-SEER_death,-Patient_ID, -Follow_up_years) 

mp3_out = sample_f(spm_origin) %>% 
  left_join(mp2_origin %>% mutate(mark = "mark") %>% select(Patient_ID,mark),
            by = "Patient_ID") %>% filter(is.na(mark) == T) %>% select(-mark) %>% 
  select(colnames(mp3_origin))

table(mp3_origin$Year_of_diagnosis.x)
table(mp3_valid$Year_of_diagnosis.x)
table(mp3_12$Year_of_diagnosis.x)


###数据拆分
set.seed(180615)
library(tidymodels)
data_split <- initial_split(mp3_origin,
                            prop = 0.7,
                            strata = Year_of_diagnosis.x)#等比例拆分
# Create data frames for the two sets:
traindata <- training(data_split) %>% select(-Year_of_diagnosis.x)
testdata  <- testing(data_split) %>% select(-Year_of_diagnosis.x)

####基线特征对比
d_train_test = rbind(traindata %>% mutate(group = "train"),
                     testdata %>% mutate(group = "test"))
d_in_out = rbind(mp3_origin %>% mutate(group = "analytical") %>% select(-Year_of_diagnosis.x),
                 mp3_out %>% mutate(group = "exclude") %>% select(-Year_of_diagnosis.x))
d_in_valid = rbind(mp3_origin %>% mutate(group = "development") %>% select(-Year_of_diagnosis.x),
                   mp3_valid %>% mutate(group = "time_valid") %>% select(-Year_of_diagnosis.x))
d_in_12 = rbind(mp3_origin %>% mutate(group = "development") %>% select(-Year_of_diagnosis.x),
                mp3_12 %>% mutate(group = "external_valid") %>% select(-Year_of_diagnosis.x))

datacompare_f <- function(dataset) {
  library(gtsummary)
  base_result <- dataset %>% 
    tbl_summary(
      by = group,
      type = list(Status ~ "categorical",
                  Gender ~ "categorical",
                  Surgery_origin ~ "categorical",
                  Radiation_origin ~ "categorical",
                  Chemotherapy_origin ~ "categorical",
                  Reg_lymph_nd_sur_origin ~ "categorical",
                  
                  Surgery_spm ~ "categorical",
                  Radiation_spm ~ "categorical",
                  Chemotherapy_spm ~ "categorical",
                  Reg_lymph_nd_sur_spm ~ "categorical"),
      statistic = list(all_continuous() ~ "{mean} ± {sd}",
                       Months_since_index ~"{median} ({p25}, {p75})",
                       all_categorical() ~ "{n} ({p})"),
      percent = "column",
      digits = list(all_continuous()~c(2,2),
                    all_categorical()~c(0,2)),
      missing = "no") |>
    add_p(pvalue_fun = ~style_pvalue(.x, digits = 3),simulate.p.value = TRUE) |>
    add_significance_stars() |>
    # add_overall() %>% 
    as.data.frame()
  return(base_result)
}

result_train_test = datacompare_f(d_train_test)
result_in_out = datacompare_f(d_in_out)
result_in_valid = datacompare_f(d_in_valid)
result_in_12 = datacompare_f(d_in_12)

path1 = "~/R/SEER/lung/1data/"
if (!dir.exists(path1)) dir.create(path1)
save(mp3_origin,mp3_valid, mp3_out, mp3_12,
     traindata,testdata, file = paste0(path1, "SEER_lung_spm4task.rdata"))

path2 = "~/R/SEER/lung/3result/01datacompare/"
if (!dir.exists(path2)) dir.create(path2)
openxlsx::write.xlsx(list(result_train_test, result_in_out, result_in_valid, result_in_12),
                     file = paste0(path2, "base_result.xlsx"))
