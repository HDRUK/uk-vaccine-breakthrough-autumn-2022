source("scripts/r_clear_workspace.r")

# Only run this if there is a raw data refresh

# load data ===================================================================
cat("get cohort\n")

dt_admissions <- readRDS("../input_2023_03_21/admissions.rds") # has strict and broader

# assign a sequence value to records

d_admissions = dt_admissions %>%
  group_by(study_id) %>%
  arrange(study_id, admission_date, mcoa_covid, acoa_covid) %>%
  mutate(spell_num_e = 1:n()) %>%
  ungroup() %>% collect()

# match hosp admissions against criteria =======================================
cat("match hosp admissions against criteria\n")

# find hosp admissions that are:
#	* caused by covid-19
#	* with covid-19
#	* covid-19 was diagnosed during admission

d_hosp_c19_cause <-
  d_admissions %>%
  filter(
    admission_method == "emergency",
    mcoa_covid  == TRUE
  ) %>%
  select(
    study_id,
    spell_num_e,
    admission_date
  ) %>%
  distinct() %>%
  mutate(
    admis_covid19_cause_flg = 1
  ) %>% collect()

d_hosp_emergency_with_c19 <-
  d_admissions %>%
  filter(
    admission_method == "emergency",
    acoa_covid  == TRUE
  ) %>%
  select(
    study_id,
    spell_num_e,
    admission_date
  ) %>%
  distinct() %>%
  mutate(
    admis_emergency_with_covid19_flg = 1
  ) %>% collect()

d_hosp_non_emergency_with_c19 <-
  d_admissions %>%
  filter(
    admission_method != "emergency" | is.na(admission_method),
    acoa_covid  == TRUE
  ) %>%
  select(
    study_id,
    spell_num_e,
    admission_date
  ) %>%
  distinct() %>%
  mutate(
    admis_non_emergency_with_covid19_flg = 1
  ) %>% collect()

# combine ======================================================================
cat("combine\n")

d_hosp_c19 <-
  bind_rows(
    d_hosp_c19_cause,
    d_hosp_emergency_with_c19,
    d_hosp_non_emergency_with_c19
  ) %>%
  mutate(
    admis_covid19_cause_flg              = replace_na(admis_covid19_cause_flg,              0),
    admis_emergency_with_covid19_flg     = replace_na(admis_emergency_with_covid19_flg,     0),
    admis_non_emergency_with_covid19_flg = replace_na(admis_non_emergency_with_covid19_flg, 0)
  ) %>%
  group_by(
    study_id,
    admission_date,
    spell_num_e
  ) %>%
  summarise(
    admis_covid19_cause_flg              = max(admis_covid19_cause_flg),
    admis_emergency_with_covid19_flg     = max(admis_emergency_with_covid19_flg),
    admis_non_emergency_with_covid19_flg = max(admis_non_emergency_with_covid19_flg)
  ) %>%
  ungroup() %>%
  mutate(
    admis_covid19_cat =
      case_when(
        admis_covid19_cause_flg  == 1             ~ "emergency_admis_cause_c19",
        admis_emergency_with_covid19_flg == 1     ~ "emergency_admis_with_c19",
        admis_non_emergency_with_covid19_flg == 1 ~ "non_emergency_admis_with_c19"
      ) %>%
      factor(c(
        "emergency_admis_cause_c19",
        "emergency_admis_with_c19",
        "non_emergency_admis_with_c19"
      ))
  )

# Flag duplicates ================================================================================

# x = d_hosp_c19 %>%
#   group_by(study_id, admis_covid19_cat) %>%
#   mutate(flag = +(n()>1)) %>%
#   ungroup()
# 
# x %>% group_by(flag) %>% tally()

# combine ======================================================================
cat("combine\n")

d_hosp <-
  d_admissions %>%
  left_join(d_hosp_c19, by = c("study_id", "admission_date", "spell_num_e")) %>%
  arrange(admission_date, discharge_date, study_id) %>% collect()

# confirm all of d_hosp_c19 is in d_hosp_all
nrow_c19   <- d_hosp_c19 %>% nrow()
nrow_inner <- d_admissions %>% 
  inner_join(d_hosp_c19, by = c("study_id", "admission_date", "spell_num_e")) %>% collect() %>%
  nrow()

if (nrow_c19 != nrow_inner)
  stop("'d_hosp_all' is missing rows from d_hosp_c19 :(")

## Considered excluding treatment data but they SHOULD be recorded as day cases
## By filtering to emergency that should negate this issue

# Rename to match Welsh dataset

d_hosp = d_hosp %>%
  rename(admis_dt = admission_date, disch_dt = discharge_date)

d_hosp %>% group_by(admis_covid19_cat) %>% count()


# save =========================================================================
cat("save\n")

qsave(d_hosp, file = "input/d_hosp.qs")
