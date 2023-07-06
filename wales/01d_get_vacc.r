source("r_clear_workspace.r")

con <- db2_open()

# check covarage ===============================================================
cat("check covarage\n")

q_count <- "
SELECT
    CAST(DATE_TRUNC('WEEK', vacc_date) AS DATE) AS week_date,
    COUNT(*) AS total_n,
    SUM(alf_has_bad_vacc_record = 0) AS clean_n
FROM
    sailwmc_v.c19_cohort20_rrda_cvvd
WHERE
    vacc_date >= '2020-12-07'
GROUP BY
    DATE_TRUNC('WEEK', vacc_date)
;"

d_count <-
    db2_run(con, q_count) %>%
    arrange(desc(week_date)) %>%
    # suppression
    mutate(
        total_n = if_else(1 <= total_n & total_n <= 9, 10, round_half_up(total_n, -1)),
        clean_p = clean_n / total_n
    )

p_vacc_count <-
    d_count %>%
    ggplot(aes(
        x = week_date,
        y = total_n
    )) +
    geom_col() +
    scale_x_date(
        name = "",
        date_breaks = "2 months",
        date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
        name = "Weekly vaccinations",
        labels = comma,
        breaks = pretty_breaks()
    ) +
    ggtitle(
        "Number of vaccinations from RRDA_CVVD"
    )

p_vacc_clean <-
    d_count %>%
    ggplot(aes(
        x = week_date,
        y = clean_p
    )) +
    geom_col() +
    scale_x_date(
        name = "",
        date_breaks = "2 months",
        date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
        name = "Weekly vaccinations",
        labels = percent,
        breaks = pretty_breaks()
    ) +
    ggtitle(
        "Percentage of clean vaccination records"
    )

p_vacc_coverage <-
    p_vacc_count +
    p_vacc_clean +
    plot_layout(ncol = 1)

print(p_vacc_coverage)

# get covid vaccinations ======================================================
cat("get covid vaccinations\n")

q_vacc <- "
SELECT
    cohort.alf_e,
    cvvd.vacc_dose_seq,
    ROW_NUMBER() OVER (PARTITION BY cvvd.alf_e ORDER BY cvvd.vacc_date) AS vacc_dose_num,
    cvvd.vacc_date AS vacc_date,
    cvvd.vacc_name AS vacc_name,
    CASE
        WHEN vacc_loc_type = 'Covid 19 Trial' THEN 0
        ELSE 1
    END AS is_not_trial,
    cvvd.is_valid_vacc_name_and_date AS is_valid_name_and_date,
    cvvd.is_before_today,
    cvvd.is_gte_20_days,
    cvvd.is_valid_dose_seq
FROM
    sailw1151v.dacvap_cohort AS cohort
INNER JOIN
    sailwmc_v.c19_cohort20_rrda_cvvd AS cvvd
    ON cohort.alf_e = cvvd.alf_e
;"

d_vacc <- db2_run(con, q_vacc)


# make valid record flag =======================================================
cat("make valid record flag\n")

d_vacc <-
    d_vacc %>%
    mutate(
        is_not_trial           = replace_na(is_not_trial, 0),
        is_valid_name_and_date = replace_na(is_valid_name_and_date, 0),
        is_before_today        = replace_na(is_before_today, 0),
        is_gte_20_days         = replace_na(is_gte_20_days, 0),
        is_valid_dose_seq      = replace_na(is_valid_dose_seq, 0)
    ) %>%
    mutate(
        valid_record_flg = as.numeric(
            is_not_trial           == 1 &
            is_valid_name_and_date == 1 &
            is_before_today        == 1 &
            is_gte_20_days         == 1 &
            is_valid_dose_seq      == 1
        )
    ) %>%
    group_by(alf_e) %>%
    mutate(
        alf_all_valid_flg = min(valid_record_flg)
    ) %>%
    ungroup()


# split out ====================================================================
cat("split out\n")

# split out people's vaccination records according to:
# * good - names, dates, and spacing are all valid
# * bad  - vaccinated part of clinical trail, or issues with
#          when they got vaccinated that we cant resolve
# * ugly - recording issues that we're going to attempt to fix

d_vacc_good <-
    d_vacc %>%
    filter(
        alf_all_valid_flg == 1
    )

x_vacc_bad_alf <-
    d_vacc %>%
    filter(!(alf_e %in% d_vacc_good$alf_e)) %>%
    filter(
        is_not_trial == 0 |
        is_valid_name_and_date == 0 |
        is_before_today == 0
    ) %>%
    select(alf_e) %>%
    distinct() %>%
    pull(alf_e)

d_vacc_bad <-
    d_vacc %>%
    filter(alf_e %in% x_vacc_bad_alf)

d_vacc_ugly <-
    d_vacc %>%
    filter(
        !(alf_e %in% d_vacc_good$alf_e),
        !(alf_e %in% d_vacc_bad$alf_e)
    )


# fix ugly =====================================================================
cat("TODO: fix ugly\n")


# clean ========================================================================
cat("clean\n")

lkp_vacc_name <- c(
    "AZ" = "Astrazeneca",
    "PB" = "Comirnaty",
    "JS" = "Janssen",
    "MD" = "Moderna",
    "NV" = "Novavax",
    "PB" = "Pfizer Biontech",
    "PB" = "Pfizer child",
    "MD" = "Spikevax"
)

d_vacc <-
    d_vacc %>%
    select(
        alf_e,
        alf_all_valid_flg,
        vacc_dose_num,
        vacc_dose_seq,
        vacc_name,
        vacc_date
    ) %>%
    mutate(
        vacc_name  = factor(vacc_name, lkp_vacc_name, names(lkp_vacc_name)),
        vacc_date = if_else(vacc_date == ymd("2020-12-07"), ymd("2020-12-08"), vacc_date)
    )

# visualise ====================================================================
cat("visualise\n")

p_vacc_dose_num <-
    d_vacc %>%
    filter(
        alf_all_valid_flg == 1,
        vacc_dose_num <= 5
    ) %>%
    mutate(
        vacc_date = floor_date(vacc_date, "week"),
        vacc_dose_num = factor(vacc_dose_num)
    ) %>%
    count(vacc_dose_num, vacc_date) %>%
    mutate(
        n = if_else(1 <= n & n <= 9, as.integer(10), n),
        n = round_half_up(n, -1)
    ) %>%
    ggplot(aes(
        x = vacc_date,
        y = n,
        fill = vacc_dose_num
    )) +
    geom_col() +
    scale_x_date(
        name = "",
        date_breaks = "2 months",
        date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
        name = "",
        label = comma,
        breaks = pretty_breaks()
    ) +
    scale_fill_brewer(
        name = "Dose\nnumber",
        palette = "Set1"
    ) +
    theme(
        axis.title.x = element_blank(),
        legend.position = c(0.98, 0.95),
        legend.justification = c(1, 1)
    )

p_vacc_name <-
    d_vacc %>%
    filter(
        alf_all_valid_flg == 1,
        vacc_dose_num <= 5,
        vacc_name != "JS"
    ) %>%
    mutate(
        vacc_date = floor_date(vacc_date, "week")
    ) %>%
    count(vacc_name, vacc_date) %>%
    mutate(
        n = if_else(1 <= n & n <= 9, as.integer(10), n),
        n = round_half_up(n, -1)
    ) %>%
    ggplot(aes(
        x = vacc_date,
        y = n,
        fill = vacc_name
    )) +
    geom_col() +
    scale_x_date(
        name = "",
        date_breaks = "2 months",
        date_labels = "%b\n%Y"
    ) +
    scale_y_continuous(
        name = "",
        label = comma,
        breaks = pretty_breaks()
    ) +
    scale_fill_brewer(
        name = "Vaccine\nname",
        palette = "Set1"
    ) +
    theme(
        axis.title.x = element_blank(),
        legend.position = c(0.98, 0.95),
        legend.justification = c(1, 1)
    )

p_vacc_desc <-
    p_vacc_dose_num +
    p_vacc_name +
    plot_layout(ncol = 1)

print(p_vacc_desc)


# save =========================================================================
cat("save\n")

qsave(p_vacc_coverage, file = "results/p_prep_vacc_covarage.qs")

qsave(p_vacc_desc, file = "results/p_prep_vacc_desc.qs")

qsave(d_vacc, file = save_dir("d_vacc.qs"))


# goodbye! =====================================================================
cat("goodbye!\n")

db2_close(con)
beep()
