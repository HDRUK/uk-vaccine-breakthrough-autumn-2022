source("r_clear_workspace.r")

con <- db2_open()

# get household ================================================================
cat("get household\n")

q_household <- str_glue("
    WITH
        cohort AS
        (
            SELECT
                alf_e,
                DATE '{res_date}' AS res_date
            FROM
                sailwmc_v.c19_cohort20
        ),
        cohort_ralf AS
        (
            SELECT
                cohort.alf_e,
                cohort.res_date,
                wds_add.ralf_e,
                wds_add.ralf_sts_cd,
                wds_add.from_dt,
                wds_add.to_dt,
                wds_add.lsoa2011_cd,
                ROW_NUMBER() OVER (PARTITION BY cohort.alf_e, cohort.res_date ORDER BY wds_add.from_dt, wds_add.to_dt DESC) AS ralf_seq
            FROM
                cohort
            INNER JOIN
                sailwmc_v.c19_cohort_wdsd_ar_pers AS wds_prs
                ON wds_prs.alf_e = cohort.alf_e
            INNER JOIN
                sailwmc_v.c19_cohort_wdsd_ar_pers_add AS wds_add
                ON  wds_add.pers_id_e = wds_prs.pers_id_e
                AND wds_add.from_dt <= cohort.res_date
                AND wds_add.to_dt >= cohort.res_date
        ),
        cohort_ralf_uniq AS
        (
            SELECT
                cohort_ralf.*,
                lkp_wimd.overall_quintile                               AS wimd2019_quintile,
                lkp_health_board.ua_desc                                AS lad2011_nm,
                lkp_health_board.lhb19nm                                AS health_board,
                lkp_urban_rural.ruc11cd || ' ' || lkp_urban_rural.ruc11 AS urban_rural_class
            FROM cohort_ralf
            LEFT JOIN sailrefrv.wimd2019_index_and_domain_ranks_by_small_area AS lkp_wimd
                ON cohort_ralf.lsoa2011_cd = lkp_wimd.lsoa2011_cd
            LEFT JOIN sailw1151v.dacvap_lkp_lsoa_hb AS lkp_health_board
                ON cohort_ralf.lsoa2011_cd = lkp_health_board.lsoa2011_cd
            LEFT JOIN sailrefrv.rural_urban_class_2011_of_llsoareas_in_eng_and_wal AS lkp_urban_rural
                ON cohort_ralf.lsoa2011_cd = lkp_urban_rural.lsoa11cd
            WHERE ralf_seq = 1
        ),
        hh_member_all AS
        (
            SELECT
            -- person of interest
                cohort.alf_e,
                cohort.res_date,
                cohort.ralf_e,
                cohort.ralf_sts_cd,
            -- other household member
                wds_prs.alf_e   AS hh_alf_e,
                ROW_NUMBER() OVER (PARTITION BY cohort.alf_e, cohort.res_date, wds_prs.alf_e ORDER BY wds_add.from_dt, wds_add.to_dt DESC) AS ralf_seq,
                wds_prs.gndr_cd,
                floor((days(cohort.res_date) - days(wds_prs.wob)) / 365.25) AS age
            FROM
                cohort_ralf_uniq AS cohort
            INNER JOIN
                sailwmc_v.c19_cohort_wdsd_ar_pers_add AS wds_add
                ON cohort.ralf_e = wds_add.ralf_e
                AND cohort.res_date >= wds_add.from_dt
                AND cohort.res_date <= wds_add.to_dt
            INNER JOIN
                sailwmc_v.c19_cohort_wdsd_ar_pers AS wds_prs
                ON wds_prs.pers_id_e = wds_add.pers_id_e
            WHERE
                cohort.ralf_e IS NOT NULL
        ),
        hh_member_ind AS
        (
            SELECT
                *,
                -- make indicators used to summarise household members
                CAST(age >= 0 AND age <= 17                  AS smallint) AS is_child,
                CAST(age = 0                                 AS smallint) AS is_child_00,
                CAST(age >= 01 AND age <= 04                 AS smallint) AS is_child_01_04,
                CAST(age >= 05 AND age <= 09                 AS smallint) AS is_child_05_09,
                CAST(age >= 10 AND age <= 15                 AS smallint) AS is_child_10_15,
                CAST(age >= 16 AND age <= 17                 AS smallint) AS is_child_16_17,
                CAST(age >= 18                               AS smallint) AS is_adult,
                CAST(gndr_cd = 1 AND age >= 18 AND age <= 59 AS smallint) AS is_adult_male_18_59,
                CAST(gndr_cd = 1 AND age >= 60               AS smallint) AS is_adult_male_60_pl,
                CAST(gndr_cd = 2 AND age >= 18 AND age <= 59 AS smallint) AS is_adult_female_18_59,
                CAST(gndr_cd = 2 AND age >= 60               AS smallint) AS is_adult_female_60_pl
            FROM
                hh_member_all
            WHERE
                -- remove any duplicated rows (there should on be a small number of duplicates)
                ralf_seq = 1
        ),
        hh_summary AS
        (
            SELECT
                alf_e,
                res_date,
                ralf_e,
            -- all household members
                COUNT(*)                                           AS household_n,
                MIN(age)                                           AS household_age_min,
                AVG(age)                                           AS household_age_avg,
                MAX(age)                                           AS household_age_max,
            -- children
                SUM(is_child)                                      AS child_n,
                SUM(is_child_00)                                   AS child_00_n,
                SUM(is_child_01_04)                                AS child_01_04_n,
                SUM(is_child_05_09)                                AS child_05_09_n,
                SUM(is_child_10_15)                                AS child_10_15_n,
                SUM(is_child_16_17)                                AS child_16_17_n,
                MIN(CASE WHEN is_child = 1 THEN age ELSE NULL END) AS child_age_min,
                AVG(CASE WHEN is_child = 1 THEN age ELSE NULL END) AS child_age_avg,
                MAX(CASE WHEN is_child = 1 THEN age ELSE NULL END) AS child_age_max,
            -- adults
                SUM(is_adult)                                      AS adult_n,
                SUM(is_adult_male_18_59)                           AS adult_male_18_59_n,
                SUM(is_adult_male_60_pl)                           AS adult_male_60_pl_n,
                SUM(is_adult_female_18_59)                         AS adult_female_18_59_n,
                SUM(is_adult_female_60_pl)                         AS adult_female_60_pl_n,
                MIN(CASE WHEN is_adult = 1 THEN age ELSE NULL END) AS adult_age_min,
                AVG(CASE WHEN is_adult = 1 THEN age ELSE NULL END) AS adult_age_avg,
                MAX(CASE WHEN is_adult = 1 THEN age ELSE NULL END) AS adult_age_max
            FROM
                hh_member_ind
            GROUP BY
                alf_e, res_date, ralf_e
        )
    SELECT
        cohort_ralf_uniq.alf_e,
        cohort_ralf_uniq.res_date,
        cohort_ralf_uniq.ralf_e,
        cohort_ralf_uniq.ralf_sts_cd,
    -- area
        cohort_ralf_uniq.lsoa2011_cd,
        cohort_ralf_uniq.wimd2019_quintile,
        cohort_ralf_uniq.lad2011_nm,
        cohort_ralf_uniq.health_board,
        cohort_ralf_uniq.urban_rural_class,
    -- all household members
        hh_summary.household_n,
        hh_summary.household_age_min,
        hh_summary.household_age_avg,
        hh_summary.household_age_max,
    -- children
        hh_summary.child_n,
        hh_summary.child_00_n,
        hh_summary.child_01_04_n,
        hh_summary.child_05_09_n,
        hh_summary.child_10_15_n,
        hh_summary.child_16_17_n,
        hh_summary.child_age_min,
        hh_summary.child_age_avg,
        hh_summary.child_age_max,
    -- other adults
        hh_summary.adult_n,
        hh_summary.adult_male_18_59_n,
        hh_summary.adult_male_60_pl_n,
        hh_summary.adult_female_18_59_n,
        hh_summary.adult_female_60_pl_n,
        hh_summary.adult_age_min,
        hh_summary.adult_age_avg,
        hh_summary.adult_age_max
    FROM cohort_ralf_uniq
    LEFT JOIN hh_summary
        ON  hh_summary.alf_e    = cohort_ralf_uniq.alf_e
        AND hh_summary.res_date = cohort_ralf_uniq.res_date
        AND hh_summary.ralf_e   = cohort_ralf_uniq.ralf_e
    ;",
    res_date = useful_date$vacc_autumn22_start
)

d_hh <- db2_run(con, q_household)


# visualise household ==========================================================
cat("visualise household\n")

p_wimd <-
    d_hh %>%
    select(ralf_e, wimd2019_quintile) %>%
    distinct() %>%
    count(wimd2019_quintile) %>%
    ggplot(aes(x = wimd2019_quintile, y = n)) +
    geom_col()

p_hh_n <-
    d_hh %>%
    select(ralf_e, household_n) %>%
    distinct() %>%
    count(household_n) %>%
    mutate(household_n = ifelse(household_n > 100, 100, household_n)) %>%
    ggplot(aes(x = household_n, y = n)) +
    geom_col()

p_hh_age <-
    d_hh %>%
    select(ralf_e, household_age_avg) %>%
    distinct() %>%
    ggplot(aes(x = household_age_avg)) +
    geom_histogram()

p_hh <- p_wimd/ p_hh_n / p_hh_age

print(p_hh)

# get residency ================================================================
cat("get residency\n")

q_res <- "
WITH
    address_history AS (
        SELECT
            pers.alf_e,
            pers_add.from_dt AS start_date,
            pers_add.to_dt AS end_date
        FROM
            sailwmc_v.c19_cohort20 AS cohort
        INNER JOIN
            sailwmc_v.c19_cohort_wdsd_ar_pers AS pers
            ON cohort.alf_e = pers.alf_e
        INNER JOIN
            sailwmc_v.c19_cohort_wdsd_ar_pers_add AS pers_add
            ON pers.pers_id_e = pers_add.pers_id_e
        WHERE
            '2020-01-01' BETWEEN pers_add.from_dt AND pers_add.to_dt
            OR pers_add.from_dt > '2020-01-01'
        ORDER BY
            pers.alf_e,
            pers_add.from_dt
    ),
    residence_start AS (
        SELECT
            alf_e,
            min(start_date) AS start_date
        FROM
            address_history
        GROUP BY
            alf_e
    ),
    address_history_days AS (
        SELECT *,
            days(lead(start_date) OVER (PARTITION BY alf_e ORDER BY start_date)) - days(end_date) AS lead_gap_days
        FROM
            address_history
        ORDER BY
            alf_e,
            start_date
    ),
    /* for an alf, the first end date of an address interval before there was any length of gap
     * with the next address i.e. the person had moved out of Wales for at least one day */
    residence_end_1day AS (
        SELECT
            alf_e,
            min(end_date) AS end_date
        FROM
            address_history_days
        WHERE
            lead_gap_days IS NULL
            OR lead_gap_days >= 1
        GROUP BY
            alf_e
    ),
    /* at least 7 days */
    residence_end_7day AS (
        SELECT
            alf_e,
            min(end_date) AS end_date
        FROM
            address_history_days
        WHERE
            lead_gap_days IS NULL
            OR lead_gap_days >= 7
        GROUP BY
            alf_e
    ),
    /* at least 28 days */
    residence_end_28day AS (
        SELECT
            alf_e,
            min(end_date) AS end_date
        FROM
            address_history_days
        WHERE
            lead_gap_days IS NULL
            OR lead_gap_days >= 28
        GROUP BY
            alf_e
    )
SELECT
    cohort.alf_e                 AS alf_e,
    residence_start.start_date   AS wds_start_date,
    residence_end_1day.end_date  AS wds_end_date_1day,
    residence_end_7day.end_date  AS wds_end_date_7day,
    residence_end_28day.end_date AS wds_end_date_28day
FROM
    sailwmc_v.c19_cohort20 AS cohort
INNER JOIN residence_start
    ON cohort.alf_e = residence_start.alf_e
LEFT JOIN residence_end_1day
    ON cohort.alf_e = residence_end_1day.alf_e
LEFT JOIN residence_end_7day
    ON cohort.alf_e = residence_end_7day.alf_e
LEFT JOIN residence_end_28day
    ON cohort.alf_e = residence_end_28day.alf_e
;"

d_res <- db2_run(con, q_res)

# visualise ====================================================================
cat("visualise\n")

p_res_start <-
    d_res %>%
    mutate(
        wds_start_date = pmax(wds_start_date, ymd("2020-01-01")),
        wds_start_date = floor_date(wds_start_date, "week")
    ) %>%
    count(wds_start_date) %>%
    ggplot(aes(
        x = wds_start_date,
        y = n
    )) +
    geom_col() +
    scale_y_log10(
        labels = comma
    )

p_res_end <-
    d_res %>%
    mutate(
        wds_end_date_1day = na_if(wds_end_date_1day, ymd("9999-01-01")),
        wds_end_date_1day = floor_date(wds_end_date_1day, "week")
    ) %>%
    filter(not_na(wds_end_date_1day)) %>%
    count(wds_end_date_1day) %>%
    ggplot(aes(
        x = wds_end_date_1day,
        y = n
    )) +
    geom_col() +
    scale_y_log10(
        labels = comma
    )

p_res <- p_res_start / p_res_end

print(p_res)

# save =========================================================================
cat("save\n")

qsave(p_hh,  file = "results/p_prep_household.qs")
qsave(p_res, file = "results/p_prep_res.qs")

qsave(d_res, file = save_dir("d_res.qs"))
qsave(d_hh,  file = save_dir("d_hh.qs"))


# goodbye! =====================================================================
cat("goodbye!\n")

db2_close(con)
beep()
