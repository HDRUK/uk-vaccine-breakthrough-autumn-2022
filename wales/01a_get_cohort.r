source("r_clear_workspace.r")

con <- db2_open()

# get cohort ===================================================================
cat("get cohort\n")

q_cohort <- "
SELECT
-- main id
    cohort.alf_e,
-- c19 cohort 20
    cohort.pers_id_e,
    cohort.wob,
    cohort.gndr_cd,
    cohort.gp_start_date,
    cohort.gp_end_date,
    cohort.c20_start_date,
    cohort.c20_end_date,
-- ethnicity
    cohort.ethn_cat,
-- health care worker status
    cohort.hcw_flg,
    cohort.hcw_start_date,
    cohort.hcw_end_date,
    cohort.hcw_aug2022_flg,
    cohort.hcw_sep2022_flg,
    cohort.hcw_aug2022_staff_group,
    cohort.hcw_aug2022_staff_role,
    cohort.hcw_aug2022_patient_facing,
-- shielded
    cohort.shielded_flg,
    cohort.shielded_reason,
    cohort.shielded_start_date,
-- qcovid as at 2020-12-07
    cohort.qc_flg,
    cohort.qc_b2_82,
    cohort.qc_b2_leukolaba,
    cohort.qc_b2_prednisolone,
    cohort.qc_b_af,
    cohort.qc_b_ccf,
    cohort.qc_b_asthma,
    cohort.qc_b_bloodcancer,
    cohort.qc_b_cerebralpalsy,
    cohort.qc_b_chd,
    cohort.qc_b_cirrhosis,
    cohort.qc_b_congenheart,
    cohort.qc_b_copd,
    cohort.qc_b_dementia,
    cohort.qc_b_epilepsy,
    cohort.qc_b_fracture4,
    cohort.qc_b_neurorare,
    cohort.qc_b_parkinsons,
    cohort.qc_b_pulmhyper,
    cohort.qc_b_pulmrare,
    cohort.qc_b_pvd,
    cohort.qc_b_ra_sle,
    cohort.qc_b_respcancer,
    cohort.qc_b_semi,
    cohort.qc_b_sicklecelldisease,
    cohort.qc_b_stroke,
    cohort.qc_diabetes_cat,
    cohort.qc_b_vte,
    cohort.qc_chemo_cat,
    cohort.qc_home_cat,
    cohort.qc_learn_cat,
    cohort.qc_p_marrow6,
    cohort.qc_p_radio6,
    cohort.qc_p_solidtransplant,
    cohort.qc_renal_cat,
    cohort.qc_bmi,
-- hypertension over 5 years prior to 2020-12-07
    cohort.hypertension_flg,
-- health care utilisation over 2 years prior to 2020-12-07
    cohort.hcu_hosp_spell_n,
    cohort.hcu_gp_attendance_n,
    cohort.hcu_gp_prescription_n
FROM
    sailw1151v.dacvap_cohort AS cohort
;"

d_cohort <- db2_run(con, q_cohort)


# Save =========================================================================
cat("Save\n")

qsave(d_cohort, file = save_dir("d_cohort.qs"))


# Goodbye! =====================================================================
cat("Goodbye!\n")

db2_close(con)
beep()
