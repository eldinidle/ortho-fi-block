# Ortho FI Block Study
# First Commit
library(tidyverse)
library(haven)

# Importing data...
analgesics <- read_csv("analgesics.csv")
complications <- read_csv("complications.csv")
pmh <- read_csv("pmh.csv")
blocks <- read_csv("blocks.csv")
cci <- read_csv("cci.csv")
cohort <- read_csv("cohort.csv")
master_list <- read_csv("master_list.csv")
no_opioids <- read_csv("no_opioids.csv")
pain <- read_csv("pain.csv")

# Select data for analysis
pmh <- pmh %>% select(adm_csn,pmh_aortic_stenosis = aortic_stenosis,pmh_dementia = dementia)
analgesics <- analgesics %>% select(adm_csn,mme,taken_time)
blocks <- blocks %>% select(adm_csn,doa,dos,dob,block_type)
# cci_# and compl_# are composite score components
# cci <-
cohort <- cohort %>% select(adm_csn,sex,RACE,BMI,dod,ADM_SOURCE, DISCH_DISP, VITAL_STATUS)
# pain <- pain %>% select(adm_csn,pain_rating,taken_time_pain)
master_list <- master_list %>% select(adm_csn, age, opioid_admin, num_act_opioid_scripts)
no_opioids <- no_opioids %>% 
  select(adm_csn) %>%
  mutate(no_opioids = 1)

# Fix pain data
pain_dt <- pain %>%
  mutate(dop_posix = as.POSIXct(time_pain,origin = "1970-01-01",tz=Sys.timezone(), format = "%d-%b-%y %H:%M"),
         dop_date = as.Date(dop_posix),
         dop_hour = as.POSIXlt(dop_posix)$hour,
         date = dop_date,
         hours = dop_hour) %>%
  select(adm_csn,pain_score,date,hours,dop_date, dop_hour) %>%
  group_by(adm_csn,date,hours) %>%
  summarise_all(funs(mean))

# Merge data
ortho <- master_list %>% 
  left_join(analgesics, by = "adm_csn") %>%
  left_join(blocks, by = "adm_csn")  %>%
  left_join(cohort, by = "adm_csn")  %>%
  left_join(no_opioids, by = "adm_csn") %>%
  left_join(cci, by = "adm_csn")

dt <- ortho %>% 
  group_by(adm_csn) %>% # Group by person
  select(adm_csn,doa,dos,dod,dob) %>% # Select specific variables
  summarise_all(funs(first)) # Apply the select first function and collapse data

dt <- dt %>%
  mutate(doa_posix = as.POSIXct(doa,origin = "1970-01-01",tz=Sys.timezone(), format = "%m/%d/%Y %H:%M"),
         dod_posix = as.POSIXct(dod,origin = "1970-01-01",tz=Sys.timezone(), format = "%m/%d/%Y %H:%M"),
         dos_posix = as.POSIXct(dos,origin = "1970-01-01",tz=Sys.timezone(), format = "%m/%d/%Y %H:%M"),
         dob_posix = as.POSIXct(dob,origin = "1970-01-01",tz=Sys.timezone(), format = "%m/%d/%Y %H:%M"),
         doa_date = as.Date(doa_posix),
         dod_date = as.Date(dod_posix),
         dos_date = as.Date(dos_posix),
         dob_date = as.Date(dob_posix),
         pod1_date = dos_date + 1,
         pod2_date = dos_date + 2,
         los = dod_date - doa_date,
         tts = dos_date - doa_date,
         ttb = dob_date - doa_date,
         sb_diff = dos_posix - dob_posix,
         doa_hour = as.POSIXlt(doa_posix)$hour,
         dos_hour = as.POSIXlt(dos_posix)$hour,
         dod_hour = as.POSIXlt(dod_posix)$hour,
         dob_hour = as.POSIXlt(dob_posix)$hour,
         block_8_hour = ((dob_posix-doa_posix)) <= 8) %>%
  filter(sb_diff >= (4*60) | is.na(dob)) 


ids <- dt %>% select(adm_csn)
max_days <- c(1:max(dt$los))
hours <- c(0:23)

ortho_skeleton <- 
  ids %>% 
  expand(adm_csn, max_days, hours)

ortho_reduce <- dt %>%
  select(adm_csn,doa_date,dob_date,dod_date,los,tts,dos_date,dos_hour,dob_hour,pod1_date,pod2_date,block_8_hour) %>%
  right_join(ortho_skeleton, by = c("adm_csn")) %>%
  mutate(date = doa_date + max_days - 1) %>% 
  group_by(adm_csn) %>%
  filter(max_days <= los + 1)

ortho_meds <- analgesics %>%
  mutate(mme_posix = as.POSIXct(taken_time,origin = "1970-01-01",tz=Sys.timezone(), format = "%m/%d/%Y %H:%M"),
         date = as.Date(mme_posix),
         hours = as.POSIXlt(mme_posix)$hour) %>%
  select(adm_csn,mme,date,hours) %>%
  group_by(adm_csn,date,hours) %>%
  summarise_all(funs(sum)) %>%
  right_join(ortho_reduce, by = c("adm_csn","date","hours")) %>%
  left_join(pain_dt, by = c("adm_csn","date","hours")) %>%
  mutate(mme = ifelse(is.na(mme),0,mme))

ortho_blocks <- blocks %>%
  select(adm_csn, block_type) %>%
  right_join(ortho_meds, by = "adm_csn") %>%
  mutate(block_bin = ifelse(block_type == 1,0,1))

ortho_blocks <- master_list %>%
  select(adm_csn,opioid_admin) %>%
  inner_join(ortho_blocks, by = "adm_csn")

write_dta(ortho_blocks,"C:/Users/dzubure/Desktop/ortho_fi.dta")

limit_blocks <- ortho_blocks %>%
  filter(tts <= 2)

tos_ortho <- limit_blocks %>%
  filter(date < dos_date | (date == dos_date & hours < dos_hour)) %>%
  mutate(tos_sum = 1) %>%
  group_by(adm_csn) %>%
  select(adm_csn,tos_sum,mme_tos_sum = mme) %>%
  summarise_all(funs(sum)) %>%
  right_join(limit_blocks,by = "adm_csn") 

pod1_ortho <- limit_blocks %>%
  filter(pod1_date == date) %>%
  mutate(pod1_sum = 1) %>%
  group_by(adm_csn) %>%
  select(adm_csn,pod1_sum,mme_pod1_sum = mme) %>%
  summarise_all(funs(sum)) %>%
  right_join(tos_ortho,by = "adm_csn") 

pod2_ortho <- limit_blocks %>%
  filter(pod2_date == date) %>%
  mutate(pod2_sum = 1) %>%
  group_by(adm_csn) %>%
  select(adm_csn,pod2_sum,mme_pod2_sum = mme) %>%
  summarise_all(funs(sum)) %>%
  right_join(pod1_ortho,by = "adm_csn") 

tos_ortho <- limit_blocks %>%
  filter(tts <= 2 & (date < dos_date | (date == dos_date & hours < dos_hour))) %>%
  group_by(adm_csn) %>%
  select(adm_csn,pain_tos_median = pain_score) %>%
  summarise_all(funs(median(.,na.rm = TRUE))) %>%
  right_join(pod2_ortho,by = "adm_csn") %>%
  filter(tts <= 2 & (date < dos_date | (date == dos_date & hours < dos_hour))) %>%
  group_by(adm_csn) %>%
  select(adm_csn,pain_tos_median, pain_tos_mean = pain_score) %>%
  summarise_all(funs(mean(.,na.rm = TRUE))) %>%
  right_join(pod2_ortho,by = "adm_csn") 

pod1_ortho <- limit_blocks %>%
  filter(pod1_date == date) %>%
  group_by(adm_csn) %>%
  select(adm_csn,pain_pod1_median = pain_score) %>%
  summarise_all(funs(median(.,na.rm = TRUE))) %>%
  right_join(tos_ortho,by = "adm_csn") %>%
  filter(pod1_date == date) %>%
  group_by(adm_csn) %>%
  select(adm_csn,pain_pod1_median, pain_pod1_mean = pain_score) %>%
  summarise_all(funs(mean(.,na.rm = TRUE))) %>%
  right_join(tos_ortho,by = "adm_csn") 

pod2_ortho <- limit_blocks %>%
  filter(pod2_date == date) %>%
  group_by(adm_csn) %>%
  select(adm_csn,pain_pod2_median = pain_score) %>%
  summarise_all(funs(median(.,na.rm = TRUE))) %>%
  right_join(pod1_ortho,by = "adm_csn") %>%
  filter(pod2_date == date) %>%
  group_by(adm_csn) %>%
  select(adm_csn,pain_pod2_median, pain_pod2_mean = pain_score) %>%
  summarise_all(funs(mean(.,na.rm = TRUE))) %>%
  right_join(pod1_ortho,by = "adm_csn") 


master_data <- pod2_ortho %>%
  left_join(cci, by = "adm_csn") %>%
  left_join(pmh, by = "adm_csn") %>%
  left_join(complications, by = "adm_csn") %>%
  left_join(master_list, by = "adm_csn") %>%
  mutate(num_act_opioid_scripts = ifelse(is.na(num_act_opioid_scripts),0,num_act_opioid_scripts))

write_dta(ortho_blocks,"C:/Users/dzubure/Desktop/ortho_fi_complete.dta")