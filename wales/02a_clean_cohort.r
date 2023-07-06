source("r_clear_workspace.r")

# load =========================================================================
cat("load\n")

d_cohort <- qread(save_dir("d_cohort.qs"))
d_death  <- qread(save_dir("d_death.qs"))
d_hh     <- qread(save_dir("d_hh.qs"))
d_hosp   <- qread(save_dir("d_hosp.qs"))
d_res    <- qread(save_dir("d_res.qs"))
d_vacc   <- qread(save_dir("d_vacc.qs"))

# prep vaccinations ============================================================
cat("prep vaccinations\n")

d_vacc <-
    d_vacc %>%
    filter(
        vacc_dose_num <= 5,
        vacc_date <= useful_date$study_end
    ) %>%
    rename(
        date = vacc_date,
        name = vacc_name
    ) %>%
    pivot_wider(
        id_cols      = c(alf_e, alf_all_valid_flg),
        names_from   = vacc_dose_num,
        names_glue   = "vacc{vacc_dose_num}_{.value}",
        values_from  = c(date, name)
    ) %>%
    select(
        alf_e,
        alf_vacc_all_valid_flg = alf_all_valid_flg,
        starts_with("vacc1"),
        starts_with("vacc2"),
        starts_with("vacc3"),
        starts_with("vacc4"),
        starts_with("vacc5")
    ) %>%
    mutate(
        vacc_autm22_date = case_when(
            (!is.na(vacc4_date) & vacc4_date >= useful_date$study_start) ~ vacc4_date,
            (!is.na(vacc5_date) & vacc5_date >= useful_date$study_start) ~ vacc5_date
        ),
        vacc_autm22_day14_date = vacc_autm22_date + ddays(14),
        vacc_autm22_name = case_when(
            vacc_autm22_date == vacc4_date ~ vacc4_name,
            vacc_autm22_date == vacc5_date ~ vacc5_name
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
        alf_e,
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
# d_hosp %>% filter(admis_covid19_cause_flg == 1) %>% count(alf_e) %>% count(n)

# remove hosp admissions after study end date
d_hosp <-
    d_hosp %>%
    filter(admis_dt <= useful_date$study_end) %>%
    inner_join(d_vacc, by = "alf_e") %>%
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
    group_by(alf_e) %>%
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
    group_by(alf_e) %>%
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


# prep household ===============================================================
cat("prep household\n")

d_hh <-
    d_hh %>%
    select(
        alf_e,
        ralf_e,
        hh_n         = household_n,
        hh_child_n   = child_n,
        hh_adult_n   = adult_n,
        hh_age_avg   = household_age_avg,
        lsoa2011_cd,
        wimd2019     = wimd2019_quintile,
        urban_rural  = urban_rural_class,
        health_board
    )


# prep res =====================================================================
cat("prep res\n")

d_res <-
    d_res %>%
    mutate(
        move_out_date = wds_end_date_1day,
        move_out_date = if_else(move_out_date > useful_date$study_end, NA_Date_, move_out_date)
    ) %>%
    select(
        alf_e,
        wds_start_date,
        move_out_date
    )

# join =========================================================================
cat("join\n")

d_cohort <-
    d_cohort %>%
    left_join(d_death,        by = "alf_e") %>%
    left_join(d_hosp_c19,     by = "alf_e") %>%
    left_join(d_hosp_other,   by = "alf_e") %>%
    left_join(d_hh,           by = "alf_e") %>%
    left_join(d_res,          by = "alf_e") %>%
    left_join(d_vacc,         by = "alf_e") %>%
    assert(is_uniq, alf_e)


# tidy values ==================================================================
cat("tidy values\n")

lkp_wimd <- c(
    "1_most"  = "1",
    "2"       = "2",
    "3"       = "3",
    "4"       = "4",
    "5_least" = "5"
)

lkp_urban_rural <- c(
    "urban" = "C1 Urban city and town",
    "urban" = "C2 Urban city and town in a sparse setting",
    "rural" = "D1 Rural town and fringe",
    "rural" = "D2 Rural town and fringe in a sparse setting",
    "rural" = "E1 Rural village and dispersed",
    "rural" = "E2 Rural village and dispersed in a sparse setting"
)

d_cohort <-
    d_cohort %>%
    # age
    mutate(
        age = interval(wob, useful_date$study_start) / dyears(),
        age = floor(age),
        age_3cat = cut(age,
            breaks = c(18, 50, 65, Inf),
            labels = c("18_49", "50_64", "65plus"),
            right = FALSE
        ),
        age_5y_cat = cut(age,
            breaks = c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
            labels = c("18_24", "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84", "85plus"),
            right = FALSE
        )
    ) %>%
    # sex
    mutate(
        sex = factor(gndr_cd, 1:2, c("male", "female"))
    ) %>%
    # ethnicity
    mutate(
        ethn_cat = fct_infreq(ethn_cat) %>% fct_explicit_na("unknown")
    ) %>%
    # health care utilisation
    mutate(
        hcu_hosp_spell_n    = if_else(qc_flg == 1, as.integer(replace_na(hcu_hosp_spell_n,      0)), hcu_hosp_spell_n),
        hcu_hosp_spell_cat  = if_else(hcu_hosp_spell_n >= 2, "2+", as.character(hcu_hosp_spell_n)),
        hcu_hosp_spell_cat  = factor(hcu_hosp_spell_cat),
        hcu_gp_att_n        = if_else(qc_flg == 1, as.integer(replace_na(hcu_gp_attendance_n,   0)), hcu_gp_attendance_n),
        hcu_gp_att_cat      = case_when(
                hcu_gp_att_n == 0 ~ "00",
                between(hcu_gp_att_n, 01, 19) ~ "01_19",
                between(hcu_gp_att_n, 20, 34) ~ "20_34",
                between(hcu_gp_att_n, 35, 59) ~ "35_59",
                hcu_gp_att_n >= 60            ~ "60plus"
            ) %>%
            factor(),
        hcu_gp_prescription_n   = if_else(qc_flg == 1, as.integer(replace_na(hcu_gp_prescription_n, 0)), hcu_gp_prescription_n),
        hcu_gp_prescription_cat = case_when(
                hcu_gp_prescription_n == 0 ~ "00",
                between(hcu_gp_prescription_n, 01, 06) ~ "01_06",
                between(hcu_gp_prescription_n, 07, 19) ~ "07_19",
                between(hcu_gp_prescription_n, 20, 49) ~ "20_49",
                hcu_gp_prescription_n >= 50 ~ "50plus"
            ) %>%
            factor()
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
                is.na(hh_n)               ~ "unknown",
                  0 <= hh_n & hh_n <=   2 ~ str_pad(hh_n, width = 2, pad = "0"),
                  3 <= hh_n & hh_n <=   5 ~ "03_05",
                  6 <= hh_n & hh_n <=  10 ~ "06_10",
                 11 <= hh_n               ~ "11plus"
            ),
        # is living with other adults?
        hh_adult_flg = ifelse(age >= 18, hh_adult_n - 1, hh_adult_n),
        hh_adult_flg = as.numeric(hh_adult_flg >= 1),
        hh_adult_cat = factor(hh_adult_flg, 0:1, c("no", "yes")),
        # is living with other children?
        hh_child_flg = ifelse(age < 18, hh_child_n - 1, hh_child_n),
        hh_child_flg = as.numeric(hh_child_flg >= 1),
        hh_child_cat = factor(hh_child_flg, 0:1, c("no", "yes"))
    ) %>%
    # area
    mutate(
        wimd2019     = fct_recode(as.character(wimd2019), !!!lkp_wimd),
        urban_rural  = fct_recode(urban_rural, !!!lkp_urban_rural),
        health_board = factor(health_board)
    )

# count qcovid items ===========================================================
cat("count qcovid items\n")

d_cohort <-
    d_cohort %>%
    mutate(
        qc_count =
            (qc_b2_82               == 1) +
            (qc_b2_leukolaba        == 1) +
            (qc_b2_prednisolone     == 1) +
            (qc_b_af                == 1) +
            (qc_b_ccf               == 1) +
            (qc_b_asthma            == 1) +
            (qc_b_bloodcancer       == 1) +
            (qc_b_cerebralpalsy     == 1) +
            (qc_b_chd               == 1) +
            (qc_b_cirrhosis         == 1) +
            (qc_b_congenheart       == 1) +
            (qc_b_copd              == 1) +
            (qc_b_dementia          == 1) +
            (qc_b_epilepsy          == 1) +
            (qc_b_fracture4         == 1) +
            (qc_b_neurorare         == 1) +
            (qc_b_parkinsons        == 1) +
            (qc_b_pulmhyper         == 1) +
            (qc_b_pulmrare          == 1) +
            (qc_b_pvd               == 1) +
            (qc_b_ra_sle            == 1) +
            (qc_b_respcancer        == 1) +
            (qc_b_semi              == 1) +
            (qc_b_sicklecelldisease == 1) +
            (qc_b_stroke            == 1) +
            (qc_diabetes_cat        != 0) +
            (qc_b_vte               == 1) +
            (qc_chemo_cat           != 0) +
            (qc_home_cat            != 0) +
            (qc_learn_cat           != 0) +
            (qc_p_marrow6           == 1) +
            (qc_p_radio6            == 1) +
            (qc_p_solidtransplant   == 1) +
            (qc_renal_cat           != 1)
    ) %>%
    mutate(
        qc_count_cat = case_when(
            qc_count %in% 0:4 ~ str_pad(qc_count, width = 2, pad = "0"),
            qc_count >= 5 ~ "05plus"
        ),
        qc_count_cat = factor(qc_count_cat)
    )


# final select =================================================================
cat("final select\n")

d_cohort <-
    d_cohort %>%
    select(
        alf_e,
    # gp and wds dates
        gp_start_date,
        gp_end_date,
        wds_start_date,
        move_out_date,
    # demographics
        wob,
        age,
        age_3cat,
        age_5y_cat,
        sex,
        ethn_cat,
    # health care util
        hcu_hosp_spell_cat,
        hcu_gp_att_cat,
        hcu_gp_prescription_cat,
    # residence
        ralf_e,
        hh_n_cat,
        hh_child_cat,
        hh_adult_cat,
        hh_age_avg,
        lsoa2011_cd,
        wimd2019,
        urban_rural,
        health_board,
    # qcovid
        qc_flg,
        qc_count,
        qc_count_cat,
        qc_82                = qc_b2_82,
        qc_leukolaba         = qc_b2_leukolaba,
        qc_prednisolone      = qc_b2_prednisolone,
        qc_af                = qc_b_af,
        qc_ccf               = qc_b_ccf,
        qc_asthma            = qc_b_asthma,
        qc_bloodcancer       = qc_b_bloodcancer,
        qc_cerebralpalsy     = qc_b_cerebralpalsy,
        qc_chd               = qc_b_chd,
        qc_cirrhosis         = qc_b_cirrhosis,
        qc_congenheart       = qc_b_congenheart,
        qc_copd              = qc_b_copd,
        qc_dementia          = qc_b_dementia,
        qc_epilepsy          = qc_b_epilepsy,
        qc_fracture4         = qc_b_fracture4,
        qc_neurorare         = qc_b_neurorare,
        qc_parkinsons        = qc_b_parkinsons,
        qc_pulmhyper         = qc_b_pulmhyper,
        qc_pulmrare          = qc_b_pulmrare,
        qc_pvd               = qc_b_pvd,
        qc_ra_sle            = qc_b_ra_sle,
        qc_respcancer        = qc_b_respcancer,
        qc_semi              = qc_b_semi,
        qc_sicklecelldisease = qc_b_sicklecelldisease,
        qc_stroke            = qc_b_stroke,
        qc_diabetes_cat      = qc_diabetes_cat,
        qc_vte               = qc_b_vte,
        qc_chemo_cat         = qc_chemo_cat,
        qc_home_cat          = qc_home_cat,
        qc_learn_cat         = qc_learn_cat,
        qc_marrow6           = qc_p_marrow6,
        qc_radio6            = qc_p_radio6,
        qc_solidtransplant   = qc_p_solidtransplant,
        qc_renal_cat         = qc_renal_cat,
    # bmi
        bmi = qc_bmi,
    # vaccination
        vacc_all_valid_flg = alf_vacc_all_valid_flg,
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
    file = save_dir("d_cohort_clean.qs")
)

beep()
