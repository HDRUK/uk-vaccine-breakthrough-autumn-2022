source("scripts/r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_cohort <- qread("input/d_cohort.qs")
d_death  <- qread("input/d_death.qs")
d_hosp   <- qread("input/d_hosp.qs")
d_vacc   <- qread("input/d_vacc.qs")

# prep vaccinations ============================================================
cat("prep vaccinations\n")

# checking the numbers according to method
d_vacc %>% filter(campaign=="Autumn2022Booster") %>% group_by(dose_number) %>% tally()
d_vacc %>% group_by(vacc_dose_num) %>% tally()
## Dose 4 and 5 under autumn booster matches dose 4 and 5 generally - don't need campaign variable
## Of those who received 4 - 7 doses, 0.7% had dose 6 and 7; OK to focus on 4 and 5 as standard 

d_vacc <-
    d_vacc %>%
    filter(
        vacc_dose_num <= 6,
        vacc_date <= useful_date$study_end 
    ) %>%
    rename(
        date = vacc_date,
        name = vacc_name
    ) %>%
    pivot_wider(
        id_cols      = c(study_id),
        names_from   = vacc_dose_num,
        names_glue   = "vacc{vacc_dose_num}_{.value}",
        values_from  = c(date, name)
    ) %>%
    select(
        study_id,
        starts_with("vacc1"),
        starts_with("vacc2"),
        starts_with("vacc3"),
        starts_with("vacc4"),
        starts_with("vacc5"),
        starts_with("vacc6")
    ) %>%
    mutate(
        vacc_autm22_date = case_when(
            (!is.na(vacc4_date) & vacc4_date >= useful_date$study_start) ~ vacc4_date,
            (!is.na(vacc5_date) & vacc5_date >= useful_date$study_start) ~ vacc5_date,
            (!is.na(vacc6_date) & vacc6_date >= useful_date$study_start) ~ vacc6_date,
            
        ),
        vacc_autm22_day14_date = vacc_autm22_date + ddays(14),
        vacc_autm22_name = case_when(
            vacc_autm22_date == vacc4_date ~ vacc4_name,
            vacc_autm22_date == vacc5_date ~ vacc5_name,
            vacc_autm22_date == vacc6_date ~ vacc6_name
        )
    )

# prep death ===================================================================
cat("prep death\n")

d_death <-
    d_death %>%
    # no deaths after study end
    mutate(
        death_date = if_else(death_date > useful_date$study_end, NA_Date_, death_date)
    ) %>%
    # make wide
    mutate(
        death_c19_date   = if_else(death_c19_underlying_flg == 1, death_date, NA_Date_),
        death_other_date = if_else(death_c19_underlying_flg == 0, death_date, NA_Date_)
    ) %>%
    select(
        study_id,
        death_c19_date,
        death_other_date
    )


# prep hosp ====================================================================
cat("prep hosp\n")

# main bits of info we need:
#   * flag anyone in hospital due to covid-19 at study start
#   * find date of most recent hospital admission before to study start
#   * find date of next admission after study start
#   * date of first non-covid-19 hospital admission during study window - to be
#     used as a covaraite in analysis

# how many times have people entered hospital with covid as the cause of
# admission?
# d_hosp %>% filter(admis_covid19_cause_flg == 1) %>% count(study_id) %>% count(n)

# remove hosp admissions after study end date
d_hosp <-
    d_hosp %>%
    filter(admis_dt <= useful_date$study_end) %>%
    inner_join(d_vacc, by = "study_id") %>%
    filter(!is.na(vacc_autm22_date))

# prep c19 hosp admissions
d_hosp_c19 <-
    d_hosp %>%
    filter(admis_covid19_cause_flg == 1) %>%
    mutate(
        disch_dt = replace_na(disch_dt, ymd("9999-01-01"))
    ) %>%
    mutate(
        hosp_c19_at_start_flg = as.numeric(admis_dt < vacc_autm22_day14_date & vacc_autm22_day14_date <= disch_dt),
        hosp_c19_date         = if_else(admis_dt >= vacc_autm22_day14_date, admis_dt, NA_Date_),
        prior_c19_hosp_date   = if_else(admis_dt < vacc_autm22_day14_date, admis_dt, NA_Date_)
    ) %>%
    lazy_dt() %>%
    group_by(study_id) %>%
    summarise(
        # we suppress warnings because when na.rm=TRUE removes all values, a
        # warning is raised that -Inf or Inf was returned. we handle Inf or -Inf
        # at the end of this pipe
        hosp_c19_at_start_flg = max(hosp_c19_at_start_flg),
        hosp_c19_date         = suppressWarnings(min(hosp_c19_date, na.rm = TRUE)),
        prior_c19_hosp_date   = suppressWarnings(max(prior_c19_hosp_date, na.rm = TRUE))
    ) %>%
    ungroup() %>%
    as_tibble() %>%
    mutate(
        prior_c19_hosp_flg  = as.numeric(!is.na(prior_c19_hosp_date)),
        hosp_c19_date       = if_else(hosp_c19_date       %in% c(-Inf, Inf), NA_Date_, hosp_c19_date),
        prior_c19_hosp_date = if_else(prior_c19_hosp_date %in% c(-Inf, Inf), NA_Date_, prior_c19_hosp_date)
    )

# prep other hospital admissions
d_hosp_other <-
    d_hosp %>%
    filter(
        (is.na(admis_covid19_cause_flg)              | admis_covid19_cause_flg == 0) &
        (is.na(admis_emergency_with_covid19_flg)     | admis_emergency_with_covid19_flg == 0) &
        (is.na(admis_non_emergency_with_covid19_flg) | admis_non_emergency_with_covid19_flg == 0)
    ) %>%
    mutate(
        disch_dt = replace_na(disch_dt, ymd("9999-01-01"))
    ) %>%
    mutate(
        vacc_autm22_day14_date = vacc_autm22_date + ddays(14),
        hosp_other_at_start_flg = as.numeric(admis_dt <= vacc_autm22_day14_date & vacc_autm22_day14_date <= disch_dt),
        hosp_other_date         = if_else(admis_dt > vacc_autm22_day14_date, admis_dt, NA_Date_)
    ) %>%
    lazy_dt() %>%
    group_by(study_id) %>%
    summarise(
        # we suppress warnings because when na.rm=TRUE removes all values, a
        # warning is raised that -Inf or Inf was returned. we handle Inf or -Inf
        # at the end of this pipe
        hosp_other_at_start_flg = max(hosp_other_at_start_flg),
        hosp_other_date         = suppressWarnings(min(hosp_other_date, na.rm = TRUE))
    ) %>%
    ungroup() %>%
    as_tibble() %>%
    mutate(
        hosp_other_date = if_else(hosp_other_date %in% c(-Inf, Inf), NA_Date_, hosp_other_date)
    ) %>%
    filter(hosp_other_at_start_flg == 1 | !is.na(hosp_other_date))


# join =========================================================================
cat("join\n")

d_cohort <-
    d_cohort %>%
    left_join(d_death,        by = "study_id") %>%
    left_join(d_hosp_c19,     by = "study_id") %>%
    left_join(d_hosp_other,   by = "study_id") %>%
    left_join(d_vacc,         by = "study_id") %>%
    assert(is_uniq, study_id)

# tidy values ==================================================================
cat("tidy values\n")

# tidy deprivation 
d_cohort<-mutate(d_cohort, nimdm =
                      if_else(mdm_decile=="1 " | mdm_decile=="2 ",1,
                              if_else(mdm_decile=="3 " | mdm_decile=="4 ",2,
                                      if_else(mdm_decile=="5 " | mdm_decile=="6 ",3,
                                              if_else(mdm_decile=="7 " | mdm_decile=="8 ",4,
                                                      if_else(mdm_decile=="9 " | mdm_decile=="10",5,0))))))

# replace NA with unknown
d_cohort = d_cohort %>%
    mutate(nimdm = ifelse(is.na(nimdm) | nimdm==0, 'unknown', nimdm))

# d_cohort %>% group_by(nimdm) %>% tally()
# d_cohort %>% group_by(mdm_decile) %>% tally()

lkp_nimd <- c(
    "1_most"  = "1",
    "2"       = "2",
    "3"       = "3",
    "4"       = "4",
    "5_least" = "5"
)

# tidy urban -rural 

d_cohort<-mutate(d_cohort,urban_rural=
                      if_else(settlement_band_2015=="A"|
                                  settlement_band_2015=="B"| 
                                  settlement_band_2015=="C"|
                                  settlement_band_2015=="D"|
                                  settlement_band_2015=="E", 1,
                              if_else(settlement_band_2015=="0",0,
                                      if_else(settlement_band_2015=="NA",9,2))))

## 1 is urban, 2 is rural, 0 wasn't linked, then NA

d_cohort = d_cohort %>%
    mutate(urban_rural = ifelse(is.na(urban_rural) | urban_rural == 0, 'unknown', urban_rural))

# d_cohort %>% group_by(urban_rural) %>% tally()
# d_cohort %>% group_by(settlement_band_2015) %>% tally()

lkp_urban_rural <- c(
    "urban" = "1",
    "rural" = "2",
    "unknown" = "unknown"
)

# d_cohort %>% group_by(sex) %>% tally()

## Convert Male and female to numbers to align with the Welsh code
d_cohort$sex[d_cohort$sex == "M"] = 1
d_cohort$sex[d_cohort$sex == "F"] = 2 

d_cohort <-
    d_cohort %>%
    # age
    mutate(
        age = interval(dob, useful_date$study_start) / dyears(),
        age = floor(age),
        age_3cat = cut(age,
            breaks = c(18, 50, 65, Inf),
            labels = c("18_49", "50_64", "65plus"),
            right = FALSE
        ),
        age_4y_cat = cut(age,
            breaks = c(18, 50, 65, 80, Inf),
            labels = c("18_49", "50_64", "65_79", "80+"),
            right = FALSE
        )
    ) %>%
    # sex
    mutate(
        sex = factor(sex, 1:2, c("male", "female"))
    ) %>%
    # hosp admissions
    mutate(
        hosp_c19_at_start_flg   = replace_na(hosp_c19_at_start_flg, 0),
        prior_c19_hosp_flg      = replace_na(prior_c19_hosp_flg, 0),
        hosp_other_at_start_flg = replace_na(hosp_other_at_start_flg, 0)
    ) %>%
    # residence
    mutate(
        # how many people are living at this household including our person
        hh_n_cat = case_when(
                is.na(household_size)               ~ "unknown",
                  0 <= household_size & household_size <=   2 ~ str_pad(household_size, width = 2, pad = "0"),
                  3 <= household_size & household_size <=   5 ~ "03_05",
                  6 <= household_size & household_size <=  10 ~ "06_10",
                 11 <= household_size               ~ "11plus"
            )
    ) %>%
    # area
    mutate(
        nimdm     = fct_recode(as.character(nimdm), !!!lkp_nimd),
        urban_rural  = fct_recode(urban_rural, !!!lkp_urban_rural),
        health_board = factor(lgd)
    )


# final select =================================================================
cat("final select\n")

d_cohort <-
    d_cohort %>%
    select(
        study_id,
    # demographics
        dob,
        age,
        age_3cat,
        age_4y_cat,
        sex,
    # residence
        uprn,
        hh_n_cat,
        nimdm,
        urban_rural,
        health_board,
    # BNF groups
        BNF_group,
    # vaccination
        vacc1_date,
        vacc1_name,
        vacc2_date,
        vacc2_name,
        vacc3_date,
        vacc3_name,
        vacc4_date,
        vacc4_name,
        vacc5_date,
        vacc5_name,
        vacc6_date,
        vacc6_name,
        vacc_autm22_date,
        vacc_autm22_day14_date,
        vacc_autm22_name,
    # death
        death_c19_date,
        death_other_date,
    # hosp admis
        prior_c19_hosp_flg,
        prior_c19_hosp_date,
        hosp_c19_at_start_flg,
        hosp_c19_date,
        hosp_other_at_start_flg,
        hosp_other_date
    )

# save =========================================================================
cat("save\n")

qsave(
    d_cohort,
    file = ("input/d_cohort_clean.qs")
)

beep()
