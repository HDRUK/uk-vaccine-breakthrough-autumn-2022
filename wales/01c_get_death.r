source("r_clear_workspace.r")

con <- db2_open()

# get deaths ===================================================================
cat("get deaths\n")

q_death <- "
SELECT
	death.alf_e,
	death.dod,
	death.covid_yn_underlying,
	death.covid_yn_underlying_qcovid,
	death.covid_yn_underlying_or_secondary,
	death.covid_yn_underlying_or_secondary_qcovid,
	death.covid_yn_secondary,
	death.cdds_positive_covid_19_flg
FROM
	sailw1151v.dacvap_cohort AS cohort
INNER JOIN
	sailwmc_v.c19_cohort20_mortality AS death
	ON death.alf_e = cohort.alf_e
WHERE
	death.dod IS NOT NULL
	AND death.alf_e IS NOT NULL
;"

d_death <- db2_run(con, q_death)

# clean ========================================================================
cat("clean\n")

d_death <-
	d_death %>%
	mutate(
		death_c19_underlying_flg = as.numeric(covid_yn_underlying == "y"),
		death_c19_underlying_flg = replace_na(death_c19_underlying_flg, 0),
		death_c19_secondary_flg  = as.numeric(covid_yn_secondary == "y")
	) %>%
	select(
		alf_e,
		death_date = dod,
		death_c19_underlying_flg,
		death_c19_secondary_flg
	)

# is alf unique?
if (n_distinct(d_death$alf_e) != nrow(d_death))
	stop("ALF not unique")

# visualise ====================================================================
cat("visualise\n")

p_death <-
	d_death %>%
	mutate(
		death_c19_cat = case_when(
			death_c19_underlying_flg == 1 ~ "c19_underlying",
			death_c19_secondary_flg  == 1 ~ "c19_secondary",
			TRUE ~ "other"
		),
		week = floor_date(death_date, "week")
	) %>%
	count(
		week,
		death_c19_cat
	) %>%
	ggplot(aes(
		x = week,
		y = n
	)) +
	facet_wrap(~ death_c19_cat, ncol = 1) +
	geom_col()

print(p_death)


# save =========================================================================
cat("save\n")

qsave(p_death, file = "results/p_prep_death.qs")

qsave(d_death, file = save_dir("d_death.qs"))


# goodbye! =====================================================================
cat("goodbye!\n")

db2_close(con)
beep()
