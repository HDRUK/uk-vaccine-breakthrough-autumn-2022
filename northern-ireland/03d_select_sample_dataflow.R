source("scripts/r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_cohort  <- qread(("input/d_cohort_clean.qs"))

# select sample ================================================================
cat("select sample\n")

d_cohort <-
    d_cohort %>%
    mutate(
        # number of vaccinations before study start
        vacc_prv_num = case_when(
            vacc6_date < useful_date$study_start ~ 6,
            vacc5_date < useful_date$study_start ~ 5,
            vacc4_date < useful_date$study_start ~ 4,
            vacc3_date < useful_date$study_start ~ 3,
            vacc2_date < useful_date$study_start ~ 2,
            vacc1_date < useful_date$study_start ~ 1,
                                            TRUE ~ 0
        ),
        # any outcome between vaccination and day 14?
        clearance_event = (vacc_autm22_date <= prior_c19_hosp_date & prior_c19_hosp_date < vacc_autm22_day14_date) |
                          (vacc_autm22_date <= death_c19_date & death_c19_date < vacc_autm22_day14_date),
        clearance_event = replace_na(clearance_event, FALSE)
    ) %>%
  mutate(date_diff =
           # creating date_diff between doses 
           case_when(
             !is.na(vacc6_date) & !is.na(vacc5_date) ~ interval(vacc5_date, vacc6_date) / weeks(),
             !is.na(vacc5_date) & !is.na(vacc4_date) ~ interval(vacc4_date, vacc5_date) / weeks(),
             is.na(vacc5_date) & !is.na(vacc4_date) ~ interval(vacc3_date, vacc4_date) / weeks()
           )
  ) %>%
  mutate(
    # create categories fpr analysis
    dose_diff = ifelse(is.na(date_diff), NA,
                       ifelse(date_diff >= 24,"24+","<24"))) %>%
    mutate(
        # demographics
        is_c20             = !is.na(study_id) & !is.na(dob) & !is.na(sex),
        has_lsoa           = not_na(health_board) & not_na(uprn) & not_na(hh_n_cat) & hh_n_cat!="unknown",
        is_alive           = (is.na(death_other_date) | death_other_date > vacc_autm22_day14_date) &
                             (is.na(death_c19_date)   | death_c19_date   > vacc_autm22_day14_date),
        #is_res             = wds_start_date <= ymd("2020-01-01") & (is.na(move_out_date) | move_out_date > useful_date$study_start),
        is_age_18          = age >= 18,
        # vacc records
        has_3to5_vacc      = !is.na(vacc3_date), #original
        # outcome
        has_autm_vacc      = !is.na(vacc_autm22_date) & vacc_autm22_name %in% c("PB", "MD"),
        has_follow_up      = vacc_autm22_day14_date < useful_date$study_end,
        no_clearance_event = !clearance_event
    )

t_sample_selection <- tribble(
    ~step, ~criteria, ~n,
        1, "is in covid-19 cohort 2020",
            d_cohort %>% filter(is_c20) %>% nrow(),
        2, "is alive at study start",
            d_cohort %>% filter(is_c20, is_alive) %>% nrow(),
        3, "has lsoa",
            d_cohort %>% filter(is_c20, is_alive, has_lsoa) %>% nrow(),
        4, "is aged >= 18 yo at study start",
            d_cohort %>% filter(is_c20, is_alive, has_lsoa, is_age_18) %>% nrow(),
        5, "has 3 to 5 vaccinations at study start",
            d_cohort %>% filter(is_c20, is_alive, has_lsoa, is_age_18, has_3to5_vacc) %>% nrow(),
        6, "has autumn covid vacc",
            d_cohort %>% filter(is_c20, is_alive, has_lsoa, is_age_18, has_3to5_vacc, has_autm_vacc) %>% nrow(),
        7, "is autumn covid vacc 14 days before study end date",
            d_cohort %>% filter(is_c20, is_alive, has_lsoa, is_age_18, has_3to5_vacc, has_autm_vacc, has_follow_up) %>% nrow(),
       8, "no covid19 hosp/death between vaccination and day 14",
            d_cohort %>% filter(is_c20, is_alive, has_lsoa, is_age_18, has_3to5_vacc, has_autm_vacc, has_follow_up, no_clearance_event) %>% nrow(),
    ) %>%
    mutate(
        n_diff = n - lag(n),
        p_diff = n / first(n) * 100,
        p_diff = round(p_diff, 1)
    )

print(as.data.frame(t_sample_selection))

d_sample <-
    d_cohort %>%
    filter(
        is_c20,
        is_alive,
        has_lsoa,
        is_age_18,
        has_3to5_vacc,
        has_autm_vacc,
        has_follow_up,
        no_clearance_event
    )


# save =========================================================================
cat("save\n")

qsave(
    t_sample_selection,
    file = "results/t_sample_selection.qs"
)

qsave(
    d_sample,
    file = ("input/d_sample.qs")
)

beep()
