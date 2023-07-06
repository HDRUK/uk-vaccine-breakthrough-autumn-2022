# ==============================================================================
# Title:         Categorise hospital admissions by COVID-19 status
# Create date:   2023-01-24
# Author:        Stuart Bedston
# Data sources:  C20 cohort, PEDW
# SAIL git repo: dcp07-coalesce-under-vaccinated
# Description:   Creates a table of basic info for all hospital spells (admissions)
#                from 1st Jan 2020, and flags each against four separate criteria
#                identify if a hospital admission is related to COVID-19:
#                  (1) Emergency hospital spell in which the cause of admission is COVID-19
#                  (2) Emergency hospital spell in which they were admitted for another
#                      reason but also with COVID-19
#                  (3) Emergency hospital spell starting between 14 days after a
#                      positive PCR test or 2 days before
#                  (4) Any hospital spell in which they became infected with
#                      COVID-19 during admission i.e. emergency admission but
#                      after the first episode, or non-emergency any episode
#
#                Also note that admission method codes correspond to the
#                following prefixes:
#                  * 1 = Elective
#                  * 2 = Emergency
#                  * 3 = Maternity
#                  * 8 = Other
#                  * 9 = Other
# ==============================================================================

source("r_clear_workspace.r")

con <- db2_open()

# check pedw coverage ==========================================================
cat("check pedw coverage\n")

# count episodes per week in current extract

q_pedw_count <- "
SELECT
	CAST(DATE_TRUNC('WEEK', epi.epi_str_dt) AS DATE) AS epi_week,
	COUNT(*)                                         AS n
FROM
	sailwmc_v.c19_cohort_pedw_spell AS spell
INNER JOIN
	sailwmc_v.c19_cohort_pedw_episode AS epi
	ON spell.prov_unit_cd = epi.prov_unit_cd
	AND spell.spell_num_e = epi.spell_num_e
WHERE
	spell.alf_e IS NOT NULL
	AND spell.alf_sts_cd in (1, 4, 39)
	AND epi.epi_str_dt BETWEEN '2022-08-01' AND CURRENT_DATE
GROUP BY
	DATE_TRUNC('WEEK', epi.epi_str_dt)
;"

d_pedw_count <-
	db2_run(con, q_pedw_count) %>%
	arrange(desc(epi_week)) %>%
	mutate(n = round_half_up(n, -1))

p_pedw_count <-
	d_pedw_count %>%
	ggplot(aes(
		x = epi_week,
		y = n
	)) +
	geom_col() +
	scale_x_date(
		date_breaks = "1 month",
		date_labels = "%b\n%Y"
	) +
	scale_y_continuous(
		name = "Episodes per week",
		label = comma
	) +
	ggtitle(
		"(a) Number of episodes per week"
	) +
	theme(
		axis.title.x = element_blank(),
		panel.grid.minor = element_blank()
	)

# summarise proportion of episodes with diag code

q_pedw_diag_count <- "
SELECT
	MIN(epi.avail_from_dt)                           AS extract_date,
	CAST(DATE_TRUNC('WEEK', epi.epi_str_dt) AS DATE) AS epi_week,
	CAST(epi.diag_cd_1234 IS NOT NULL AS SMALLINT)   AS epi_diag_flg,
	COUNT(*)                                         AS n
FROM
	sailwmc_v.c19_cohort_pedw_spell AS spell
INNER JOIN
	sailwmc_v.c19_cohort_pedw_episode AS epi
	ON spell.prov_unit_cd = epi.prov_unit_cd
	AND spell.spell_num_e = epi.spell_num_e
WHERE
	spell.alf_e IS NOT NULL
	AND spell.alf_sts_cd in (1, 4, 39)
	AND epi.epi_str_dt BETWEEN '2022-08-01' AND CURRENT_DATE
GROUP BY
	DATE_TRUNC('WEEK', epi.epi_str_dt),
	epi.diag_cd_1234 IS NOT NULL
;"

d_pedw_diag_count <-
	db2_run(con, q_pedw_diag_count) %>%
	arrange(epi_week) %>%
	mutate(epi_diag_flg = factor(epi_diag_flg)) %>%
	group_by(epi_week) %>%
	mutate(p = n / sum(n)) %>%
	ungroup()

d_pedw_diag_summary <-
	bind_rows(
		d_pedw_diag_count %>% filter(epi_diag_flg == 1, p >= 0.75) %>% summarise(across(everything(), last)),
		d_pedw_diag_count %>% filter(epi_diag_flg == 1, p >= 0.50) %>% summarise(across(everything(), last))
	) %>%
	mutate(
		label = str_glue(
			"{date}\n{percent}",
			date = format(epi_week, "%d %b"),
			percent = percent(p, accuracy = 0.1)
		)
	)

p_pedw_diag_summary <-
	d_pedw_diag_count %>%
	filter(epi_diag_flg == 1) %>%
	ggplot() +
	geom_col(
		mapping = aes(x = epi_week, y = p)
	) +
	geom_vline(
		data = d_pedw_diag_summary,
		mapping = aes(xintercept = epi_week),
		linetype = 2
	) +
	geom_label(
		data = d_pedw_diag_summary,
		mapping = aes(x = epi_week, y = 0.9, label = label),
		size = 2.5
	) +
	scale_x_date(
		date_breaks = "1 month",
		date_labels = "%b\n%Y"
	) +
	scale_y_continuous(
		name = "Episodes per week",
		label = percent,
		limits = c(0, 1),
		breaks = c(0, 0.25, 0.5, 0.75, 1)
	) +
	ggtitle(
		"(b) Percentage of episodes per week with at least one diag code"
	) +
	theme(
		axis.title.x = element_blank(),
		panel.grid.minor = element_blank()
	)

# combine plots

p_pedw_coverage <-
	p_pedw_count +
	p_pedw_diag_summary +
	plot_layout(ncol = 1)

print(p_pedw_coverage)

# get covid hosp admissions ====================================================
cat("get covid hosp admissions\n")

# start by summarising all hospital admissions with a covid-19 diagnosis
# in any position across all episodes

q_hosp_admis_c19 <- "
-- find all hospital admissions with a diagnosis code for covid-19 anywhere
-- and summarise
WITH
	-- pull together all rows and features needed to summarise hospital admissions with
	-- a covid-19 diagnosis anywhere
	hosp_diag AS
	(
		SELECT
			spell.alf_e,
			spell.prov_unit_cd,
			spell.spell_num_e,
			spell.admis_dt,
			spell.admis_mthd_cd,
			-- is emergency admission?
			CAST(REGEXP_LIKE(spell.admis_mthd_cd, '^2') AS SMALLINT) AS admis_emergency_flg,
			epi.epi_num,
			-- old diagnosis rank within episode
			diag.diag_num,
			-- new diagnosis rank within episode, which ignores R and Z codes because
			-- of condition in the WHERE clause below
			ROW_NUMBER () OVER (
				PARTITION BY
					diag.prov_unit_cd,
					diag.spell_num_e,
					diag.epi_num
				ORDER BY
					diag.diag_num
			) AS diag_rank_non_rz,
			-- four-digit diagnosis code
			diag.diag_cd_1234,
			-- is diagnosis code for covid-19?
			CAST(diag.diag_cd_1234 IN ('U071', 'U072') AS SMALLINT) AS diag_covid19_flg
		FROM
			sailw1151v.dacvap_cohort AS cohort
		INNER JOIN
			sailwmc_v.c19_cohort_pedw_spell AS spell
			ON cohort.alf_e = spell.alf_e
		INNER JOIN
			sailwmc_v.c19_cohort_pedw_episode AS epi
			ON spell.prov_unit_cd = epi.prov_unit_cd
			AND spell.spell_num_e = epi.spell_num_e
		INNER JOIN
			sailwmc_v.c19_cohort_pedw_diag AS diag
			ON epi.prov_unit_cd = diag.prov_unit_cd
			AND epi.spell_num_e = diag.spell_num_e
			AND epi.epi_num     = diag.epi_num
		WHERE
			spell.alf_e IS NOT NULL
			AND spell.alf_sts_cd IN (1, 4, 39)
			AND spell.admis_mthd_cd IS NOT NULL
			-- reduce the size of our search by only looking for spells from this date onwards
			AND  spell.admis_dt BETWEEN '2020-01-01' AND CURRENT DATE
			-- exclude diag codes that start with R or Z
			-- R is for symptoms, signs and abnormal clinical and lab findings i.e. not a diagnosis
			-- Z is for factors influencing contact with health services not related to disease or injury i.e. problems with education, employment or housing
			AND NOT REGEXP_LIKE(diag.diag_cd_1234, '^[RZ]')
	),
	-- summarise all spells with covid-19 diag anywhere
	hosp_covid19 AS
	(
		SELECT
			alf_e,
			prov_unit_cd,
			spell_num_e,
			admis_dt,
			MAX(admis_emergency_flg) AS admis_emergency_flg,
			MAX(diag_covid19_flg) AS diag_covid19_flg,
			-- first episode with a covid-19 diag code
			MIN(CASE WHEN diag_covid19_flg = 1 THEN epi_num END) AS epi_num_covid19
		FROM hosp_diag
		WHERE diag_covid19_flg = 1
		GROUP BY
			alf_e,
			prov_unit_cd,
			spell_num_e,
			admis_dt
	),
	-- addon ranking for covid-19 diag of first covid-19 episode
	-- if multiple, pick first
	hosp_covid19_rank AS
	(
		SELECT
			hosp_covid19.alf_e,
			hosp_covid19.prov_unit_cd,
			hosp_covid19.spell_num_e,
			hosp_covid19.admis_dt,
			hosp_covid19.admis_emergency_flg,
			hosp_covid19.epi_num_covid19,
			hosp_covid19.diag_covid19_flg,
			MIN(hosp_diag.diag_rank_non_rz) AS diag_rank_non_rz
		FROM hosp_covid19
		INNER JOIN hosp_diag
			ON  hosp_covid19.alf_e            = hosp_diag.alf_e
			AND hosp_covid19.prov_unit_cd     = hosp_diag.prov_unit_cd
			AND hosp_covid19.spell_num_e      = hosp_diag.spell_num_e
			AND hosp_covid19.epi_num_covid19  = hosp_diag.epi_num
			AND hosp_covid19.diag_covid19_flg = hosp_diag.diag_covid19_flg
		GROUP BY
			-- all columns in hosp_covid19
			hosp_covid19.alf_e,
			hosp_covid19.prov_unit_cd,
			hosp_covid19.spell_num_e,
			hosp_covid19.admis_dt,
			hosp_covid19.admis_emergency_flg,
			hosp_covid19.epi_num_covid19,
			hosp_covid19.diag_covid19_flg
	)
-- final sort
SELECT *
FROM hosp_covid19_rank
ORDER BY
	alf_e,
	admis_dt,
	prov_unit_cd,
	spell_num_e
;"

d_hosp_admis_c19 <- db2_run(con, q_hosp_admis_c19)


# verify row uniqueness ========================================================
cat("verify row uniqueness\n")

nrow_orig <- d_hosp_admis_c19 %>% n_distinct()
nrow_uniq <- d_hosp_admis_c19 %>% select(alf_e, admis_dt, prov_unit_cd, spell_num_e) %>% n_distinct()

if (nrow_orig != nrow_uniq)
	stop("Rows in 'd_hosp_admis_c19 are not unique enough :(")


# match hosp admissions against criteria =======================================
cat("match hosp admissions against criteria\n")

# find hosp admissions that are:
#	* caused by covid-19
#	* with covid-19
#	* covid-19 was diagnosed during admission

d_hosp_c19_cause <-
	d_hosp_admis_c19 %>%
	filter(
		admis_emergency_flg == 1,
		epi_num_covid19  == 1,
		diag_rank_non_rz == 1
	) %>%
	select(
		alf_e,
		prov_unit_cd,
		spell_num_e,
		admis_dt
	) %>%
	distinct() %>%
	mutate(
		admis_covid19_cause_flg = 1
	)

d_hosp_emergency_with_c19 <-
	d_hosp_admis_c19 %>%
	filter(
		admis_emergency_flg == 1,
		epi_num_covid19  == 1,
		diag_rank_non_rz >= 2
	) %>%
	select(
		alf_e,
		prov_unit_cd,
		spell_num_e,
		admis_dt
	) %>%
	distinct() %>%
	mutate(
		admis_emergency_with_covid19_flg = 1
	)

d_hosp_non_emergency_with_c19 <-
	d_hosp_admis_c19 %>%
	filter(
		admis_emergency_flg == 0,
		epi_num_covid19  == 1,
		diag_rank_non_rz >= 2
	) %>%
	select(
		alf_e,
		prov_unit_cd,
		spell_num_e,
		admis_dt
	) %>%
	distinct() %>%
	mutate(
		admis_non_emergency_with_covid19_flg = 1
	)

d_hosp_c19_during <-
	d_hosp_admis_c19 %>%
	filter(
		epi_num_covid19 >= 2
	) %>%
	select(
		alf_e,
		prov_unit_cd,
		spell_num_e,
		admis_dt
	) %>%
	distinct() %>%
	mutate(
		admis_covid19_during_flg = 1
	)


# get hosp admissions within 14 days of a positive pcr =========================
cat("get hosp admissions within 14 days of a positive pcr\n")

q_hosp_c19_pcr <- "
WITH
	pcr_positive AS
	(
		SELECT
		    pcr.alf_e,
		    CASE
		        WHEN pcr.spcm_collected_dt BETWEEN '2020-03-01' AND CURRENT DATE THEN pcr.spcm_collected_dt
		        WHEN pcr.spcm_received_dt  BETWEEN '2020-03-01' AND CURRENT DATE THEN pcr.spcm_received_dt
		    END AS pcr_spcm_dt
		FROM
		    sailw1151v.dacvap_cohort AS cohort
		INNER JOIN
		    sailwmc_v.c19_cohort_patd_df_covid_lims_testresults AS pcr
		    ON cohort.alf_e = pcr.alf_e
		WHERE
		    pcr.alf_e IS NOT NULL
		    AND pcr.covid19testresult = 'Positive'
		    AND
		    (
		    	pcr.spcm_collected_dt IS NOT NULL
		    	OR pcr.spcm_received_dt IS NOT NULL
		    )
	),
	hosp_admis AS
	(
		SELECT
			spell.alf_e,
			spell.prov_unit_cd,
			spell.spell_num_e,
			spell.admis_dt
		FROM
			sailw1151v.dacvap_cohort AS cohort
		INNER JOIN
			sailwmc_v.c19_cohort_pedw_spell AS spell
			ON cohort.alf_e = spell.alf_e
		WHERE
			spell.alf_e IS NOT NULL
			AND spell.alf_sts_cd IN (1, 4, 39)
			AND spell.admis_mthd_cd IS NOT NULL
			-- reduce the size of our search by only looking for spells from this date onwards
			AND spell.admis_dt BETWEEN '2020-01-01' AND CURRENT DATE
			-- is emergency admission
			AND REGEXP_LIKE(spell.admis_mthd_cd, '^2')
	)
-- find spells within 14 days of a positive pcr
SELECT *,
	-- how many days from positive PCR test
	hosp_admis.admis_dt - pcr_positive.pcr_spcm_dt AS days_since_pcr
FROM hosp_admis
INNER JOIN pcr_positive
	ON hosp_admis.alf_e = pcr_positive.alf_e
	AND hosp_admis.admis_dt BETWEEN (pcr_positive.pcr_spcm_dt - 1 DAYS) AND (pcr_positive.pcr_spcm_dt + 13 DAYS)
;"

d_hosp_c19_pcr <-
	db2_run(con, q_hosp_c19_pcr) %>%
	select(
		alf_e,
		prov_unit_cd,
		spell_num_e,
		admis_dt
	) %>%
	distinct() %>%
	mutate(
		admis_covid19_pcr_flg = 1
	)


# combine ======================================================================
cat("combine\n")

d_hosp_c19 <-
	bind_rows(
		d_hosp_c19_cause,
		d_hosp_emergency_with_c19,
		d_hosp_non_emergency_with_c19,
		d_hosp_c19_during,
		d_hosp_c19_pcr
	) %>%
	mutate(
		admis_covid19_cause_flg              = replace_na(admis_covid19_cause_flg,              0),
		admis_emergency_with_covid19_flg     = replace_na(admis_emergency_with_covid19_flg,     0),
		admis_non_emergency_with_covid19_flg = replace_na(admis_non_emergency_with_covid19_flg, 0),
		admis_covid19_during_flg             = replace_na(admis_covid19_during_flg,             0),
		admis_covid19_pcr_flg                = replace_na(admis_covid19_pcr_flg,                0)
	) %>%
	group_by(
		alf_e,
		prov_unit_cd,
		spell_num_e,
		admis_dt
	) %>%
	summarise(
		admis_covid19_cause_flg              = max(admis_covid19_cause_flg),
		admis_emergency_with_covid19_flg     = max(admis_emergency_with_covid19_flg),
		admis_non_emergency_with_covid19_flg = max(admis_non_emergency_with_covid19_flg),
		admis_covid19_during_flg             = max(admis_covid19_during_flg),
		admis_covid19_pcr_flg                = max(admis_covid19_pcr_flg)
	) %>%
	ungroup() %>%
	mutate(
		admis_covid19_cat =
			case_when(
				admis_covid19_cause_flg  == 1             ~ "emergency_admis_cause_c19",
				admis_emergency_with_covid19_flg == 1     ~ "emergency_admis_with_c19",
				admis_non_emergency_with_covid19_flg == 1 ~ "non_emergency_admis_with_c19",
				admis_covid19_during_flg == 1             ~ "any_hosp_admis_c19_during",
				admis_covid19_pcr_flg    == 1             ~ "emergency_admis_within_14days_positive_pcr"
			) %>%
			factor(c(
				"emergency_admis_cause_c19",
				"emergency_admis_with_c19",
				"non_emergency_admis_with_c19",
				"any_hosp_admis_c19_during",
				"emergency_admis_within_14days_positive_pcr"
			))
	)


# get all hospital admissions ==================================================
cat("get all hospital admissions\n")

lkp_admis_mthd <- c(
	"Elective"  = 1,
	"Emergency" = 2,
	"Maternity" = 3,
	"Other"     = 8,
	"Other"     = 9
)

q_hosp_all <- "
SELECT
	spell.alf_e,
	spell.prov_unit_cd,
	spell.spell_num_e,
	spell.admis_dt,
	LEFT(spell.admis_mthd_cd, 1) AS admis_mthd_cd,
	spell.disch_dt
FROM
	sailwmc_v.c19_cohort_pedw_spell AS spell
WHERE
	spell.alf_e IS NOT NULL
	AND spell.alf_sts_cd IN (1, 4, 39)
	AND spell.admis_mthd_cd IS NOT NULL
	-- reduce the size of our search by on looking for spells from this date
	AND  spell.admis_dt BETWEEN '2020-01-01' AND CURRENT DATE
;"

d_hosp_all <-
	db2_run(con, q_hosp_all) %>%
	mutate(
		spell_days = interval(admis_dt, disch_dt) / ddays()
	) %>%
	mutate(
		admis_mthd_cat = factor(admis_mthd_cd, lkp_admis_mthd, names(lkp_admis_mthd)),
		.after = "admis_mthd_cd"
	)


# combine ======================================================================
cat("combine\n")

d_hosp <-
	d_hosp_all %>%
	left_join(d_hosp_c19, by = c("alf_e", "prov_unit_cd", "spell_num_e", "admis_dt")) %>%
	arrange(admis_dt, disch_dt, alf_e)

# confirm all of d_hosp_c19 is in d_hosp_all
nrow_c19   <- d_hosp_c19 %>% nrow()
nrow_inner <- d_hosp_all %>% inner_join(d_hosp_c19, by = c("alf_e", "prov_unit_cd", "spell_num_e", "admis_dt")) %>% nrow()

if (nrow_c19 != nrow_inner)
	stop("'d_hosp_all' is missing rows from d_hosp_c19 :(")


# visualise ====================================================================
cat("visualise\n")

# d_hosp <- qread(save_dir("d_hosp.qs"))

p_hosp_all <-
	d_hosp %>%
	mutate(admis_week = floor_date(admis_dt, "week")) %>%
	count(admis_week) %>%
	# suppression
	mutate(
		n = if_else(1 <= n & n <= 9, as.integer(10), n),
		n = round_half_up(n, -1)
	) %>%
	# plot
	ggplot(aes(x = admis_week, y = n)) +
	facet_wrap(~ "all_admis") +
	geom_col() +
	scale_x_date(
		date_breaks = "6 months",
		date_labels = "%b\n%Y"
	) +
	scale_y_continuous(
		breaks = pretty_breaks(),
		labels = comma
	) +
	theme(
		axis.title.x = element_blank(),
		panel.grid.minor.y = element_blank()
	)

p_hosp_c19 <-
	d_hosp %>%
	filter(!is.na(admis_covid19_cat)) %>%
	mutate(admis_week = floor_date(admis_dt, "week")) %>%
	count(
		admis_week,
		admis_covid19_cat
	) %>%
	# suppression
	mutate(
		n = if_else(1 <= n & n <= 9, as.integer(10), n),
		n = round_half_up(n, -1)
	) %>%
	# plot
	ggplot(aes(x = admis_week, y = n)) +
	facet_wrap(~admis_covid19_cat, ncol = 1) +
	geom_col(fill = "#377eb8") +
	scale_x_date(
		date_breaks = "6 months",
		date_labels = "%b\n%Y"
	) +
	scale_y_continuous(
		breaks = pretty_breaks(),
		labels = comma
	) +
	theme(
		axis.title.x = element_blank(),
		panel.grid.minor.y = element_blank()
	)

p_hosp <-
	p_hosp_all +
	p_hosp_c19 +
	plot_layout(
		ncol = 1,
		heights = c(1, 5)
	)

print(p_hosp)


# save =========================================================================
cat("save\n")

qsave(p_pedw_coverage, file = "results/p_prep_hosp_coverage.qs")

qsave(p_hosp, file = "results/p_prep_hosp.qs")

qsave(d_hosp, file = save_dir("d_hosp.qs"))


# goodbye! =====================================================================
cat("goodbye!\n")

db2_close(con)
beep()
