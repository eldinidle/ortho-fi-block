# Ortho FI Block Study
# First Commit
library(tidyverse)

# Importing data...
analgesics <- read_csv("analgesics.csv")
block <- read_csv("blockdata.csv")
cci <- read_csv("cci_complete.csv")
cohort <- read_csv("cohort.csv")
pain <- read_csv("pain.csv")
dos <- read_csv("dos.csv")

# Select data for analysis
analgesics <- analgesics %>% select(adm_csn,mme,taken_time)
block <- block %>% select(adm_csn,block_type,time_block,block_additional,blocktime)
# cci_# and compl_# are composite score components
# cci <-
cohort <- cohort %>% select(adm_csn,age_verified,sex,race,bmi,hosp_admsn_time,hosp_disch_time,adm_source,disch_disp)
pain <- pain %>% select(adm_csn,pain_rating,taken_time_pain)


# Merge data
ortho <- cci %>% 
  left_join(analgesics, by = "adm_csn") %>%
  left_join(block, by = "adm_csn")  %>%
  left_join(cohort, by = "adm_csn")  %>%
  left_join(pain, by = "adm_csn") %>%
  left_join(dos, by = "adm_csn")

dt <- ortho %>% 
  group_by(adm_csn) %>%
  select(adm_csn,doa = hosp_admsn_time,dos,dod = hosp_disch_time) %>%
  summarise_all(funs(first))

dt <- dt %>%
  mutate(doa_posix = as.POSIXct(doa,origin = "1970-01-01",tz=Sys.timezone(), format = "%m/%d/%Y %H:%M"),
         dod_posix = as.POSIXct(dod,origin = "1970-01-01",tz=Sys.timezone(), format = "%m/%d/%Y %H:%M"),
         dos_posix = as.POSIXct(dos,origin = "1970-01-01",tz=Sys.timezone(), format = "%m/%d/%Y %H:%M"),
         doa_date = as.Date(doa_posix),
         dod_date = as.Date(dod_posix),
         dos_date = as.Date(dos_posix),
         pod1_date = dos_date + 1,
         pod2_date = dos_date + 2,
         los = dod_date - doa_date,
         tts = dos_date - doa_date,
         doa_hour = as.POSIXlt(doa_posix)$hour,
         dod_hour = as.POSIXlt(dod_posix)$hour)


ids <- dt %>% select(adm_csn)
max_days <- c(1:max(dt$los))
hours <- c(0:23)

ortho_skeleton <- 
  ids %>% 
  expand(adm_csn, max_days, hours)

test <- dt %>%
  select(adm_csn,doa_date) %>%
  right_join(ortho_skeleton, by = c("adm_csn"))

test2 <- dt %>%
  select(adm_csn,doa_date2 = dod_date,los) %>%
  right_join(test, by = c("adm_csn","doa_date2"))

test$doa_date2 <- test$doa_date + test$max_days - 1

# Block type 1:none 2: 3:
