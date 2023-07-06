source("clear_workspace.r")


# lookup for tidy names --------------------------------------------------------

lkp_xvar_xlbl <-
  read_xlsx(path = "lkp_comorbidity_xvar_xlbl.xlsx") %>% 
  mutate(across(everything(), as.character))

lkp_xvar <-
  lkp_xvar_xlbl %>% 
  pull(clean_xvar) %>% 
  unique()

lkp_xlbl_pretty <-
  lkp_xvar_xlbl %>% 
  select(
    xvar        = clean_xvar,
    xlbl        = clean_xlbl,
    xlbl_pretty = pretty_xlbl
  ) %>% 
  drop_na()


# load England ----------------------------------------------------------------

d_england_np <-
  read_csv(
    file = "england_results/cohort_desc_strict_qcovid.csv",
    name_repair = make_clean_names
  ) %>% 
  filter(!(
    name %in% c("QConditions_ChronicKidneyLatestDate", "QConditions_DiabetesLatestDate", "QConditions_LearningDisabilityCardiacLatestDate")
  )) %>% 
  mutate(
    country = factor("england"),
    value = as.character(value)
  ) %>% 
  # apply tidy xvar and xlbl values
  left_join(lkp_xvar_xlbl, join_by(name == england_long_xvar, value == england_xlbl)) %>%
  # final select
  select(
    country,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    n = total_covid_event,
    events = covid_event
  ) %>% 
  # calc col percent
  group_by(xvar) %>% 
  mutate(p = n / sum(n) * 100, .after = n) %>% 
  ungroup()

d_england_pr <-
  read_csv(
    file = "england_results/p_years_strict_qcovid.csv",
    name_repair = make_clean_names
  ) %>% 
  filter(!(
    name %in% c("QConditions_ChronicKidneyLatestDate", "QConditions_DiabetesLatestDate", "QConditions_LearningDisabilityCardiacLatestDate")
  )) %>% 
  mutate(
    country = factor("england"),
    value = as.character(value)
  ) %>% 
  # apply tidy xvar and xlbl values
  left_join(lkp_xvar_xlbl, join_by(name == england_long_xvar, value == england_xlbl)) %>% 
  # final select
  select(
    country,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    pyears = person_years,
    rate = rate_per_1000_person_yrs
  )

d_england <-
  d_england_np %>% 
  left_join(d_england_pr, join_by(country, xvar, xlbl))

# calc a total
d_england_total <-
  d_england %>% 
  filter(xvar == "asthma") %>% 
  summarise(
    n = sum(n),
    p = sum(p),
    events = sum(events),
    pyears = sum(pyears)
  ) %>% 
  mutate(
    rate = events / pyears * 1000
  ) %>% 
  mutate(
    country = "england",
    xvar = "total",
    xlbl = "total",
    .before = n
  )

d_england <- bind_rows(d_england_total, d_england)

d_england %>% assert(not_na, xvar, xlbl, n, events)

# load Scotland ----------------------------------------------------------------

d_scotland <-
  read_csv("scotland_results/strict_hosp_death_qcovid_desc.csv") %>% 
  filter(!(
    xvar %in% c("smoking_status", "Q_DIAG_HIV_AIDS", "blood_pressure", "qc_home_cat")
  )) %>% 
  mutate(
    country = factor("scotland")
  ) %>% 
  # apply tidy xvar and xlbl values
  left_join(lkp_xvar_xlbl, join_by(xvar == scotland_xvar, xlbl == scotland_xlbl)) %>%
  # final select
  select(
    country,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    n = total_n,
    p = total_p,
    pyears,
    events,
    rate
  )

d_scotland %>% assert(not_na, xvar, xlbl, n, events)


# load Wales -------------------------------------------------------------------

d_wales <-
  read_csv("wales_results/strict_hosp_death_qcovid_desc.csv") %>% 
  filter(
    xvar != "qc_marrow6"
  ) %>% 
  mutate(
    country = factor("wales")
  ) %>% 
  # apply tidy xvar and xlbl values
  left_join(lkp_xvar_xlbl, join_by(xvar == wales_xvar, xlbl == wales_xlbl)) %>% 
  # final select
  select(
    country,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    n = total_n,
    p = total_p,
    pyears,
    events,
    rate
  ) %>% 
  # replace suppressed counts with 1
  mutate(
    events = if_else(!is.na(n) & is.na(events), 1, events, events),
    rate = events / pyears * 1000
  )

d_wales %>% assert(not_na, xvar, xlbl, n, events)


# combine nations --------------------------------------------------------------

d_pool <-
  bind_rows(
    d_england,
    d_scotland,
    d_wales
  ) %>% 
  # add counts together and suppress
  group_by(xvar, xlbl) %>% 
  summarise(
    n      = sum(n,      na.rm = TRUE),
    pyears = sum(pyears, na.rm = TRUE),
    events = sum(events, na.rm = TRUE),
    events = if_else(1 <= events & events <= 9, NA_real_, events, events),
    n      = round(n, -1),
    events = round(events, -1)
  ) %>% 
  ungroup() %>% 
  # calculate percentage
  group_by(xvar) %>% 
  mutate(p = n / sum(n) * 100) %>% 
  ungroup() %>% 
  # calc rate
  mutate(rate = events / pyears * 1000) %>% 
  # order rows (first row is total)
  mutate(
    xvar = factor(xvar, lkp_xvar)
  ) %>% 
  arrange(
    xvar
  ) %>% 
  # final select
  select(
    xvar,
    xlbl,
    n,
    p,
    e = events,
    r = rate
  )


# checking ---------------------------------------------------------------------

tmp_england <-
  d_england %>%
  select(xvar, xlbl, n_england = n, e_england = events)

tmp_scotland <-
  d_scotland %>%
  select(xvar, xlbl, n_scotland = n, e_scotland = events)

tmp_wales <-
  d_wales %>%
  select(xvar, xlbl, n_wales = n, e_wales = events)

d_pool_check <-
  d_pool %>% 
  select(xvar, xlbl, n_pool = n, e_pool = e) %>% 
  left_join(tmp_england,  join_by(xvar, xlbl)) %>% 
  left_join(tmp_scotland, join_by(xvar, xlbl)) %>% 
  left_join(tmp_wales,    join_by(xvar, xlbl)) %>% 
  select(
    xvar,
    xlbl,
    starts_with("n_"),
    starts_with("e_")
  )

# england NA for conditions:
#   - diabetes
#   - ckd
#   - learning disability
d_check_england <-
  d_pool_check %>%
  filter(is.na(n_england) & !(xvar %in% c("diabetes", "learning_disability", "prednisolone", "ckd"))) %>%
  select(xvar, xlbl, n_england) %>% 
  as.data.frame()

d_check_england %>% verify(nrow(.) == 0)

# scotland NA for conditions:
#   - bone_marrow_stem_cell_transplant
#   - chemotherapy
#   - leuko_laba
#   - prednisolone
#   - radiotherapy
#   - solid_organ_transplant
d_check_scotland <-
  d_pool_check %>%
  filter(is.na(n_scotland) & !(xvar %in% c("chemotherapy", "leuko_laba", "prednisolone", "radiotherapy", "solid_organ_transplant", "bone_marrow_stem_cell_transplant"))) %>%
  select(xvar, xlbl, n_scotland) %>% 
  as.data.frame()

d_check_scotland %>% verify(nrow(.) == 0)
  

# wales NA for conditions:
#   - bone_marrow_stem_cell_transplant
d_check_wales <-
  d_pool_check %>%
  filter(is.na(n_wales) & !(xvar %in% "bone_marrow_stem_cell_transplant")) %>%
  select(xvar, xlbl, n_wales) %>% 
  as.data.frame()

d_check_wales %>% verify(nrow(.) == 0)


# supplementary material - england, scotland, wales and meta counts ------------

tmp_england <-
  d_england %>%
  select(
    xvar,
    xlbl,
    england_n = n,
    england_p = p,
    england_e = events,
    england_r = rate
  )

tmp_scotland <-
  d_scotland %>%
  select(
    xvar,
    xlbl,
    scotland_n = n,
    scotland_p = p,
    scotland_e = events,
    scotland_r = rate
  )

tmp_wales <-
  d_wales %>%
  select(
    xvar,
    xlbl,
    wales_n = n,
    wales_p = p,
    wales_e = events,
    wales_r = rate
  )

tmp_pool <-
  d_pool %>% 
  select(
    xvar,
    xlbl,
    pool_n = n,
    pool_p = p,
    pool_e = e,
    pool_r = r
  )

d_all <-
  tmp_pool %>%
  left_join(tmp_england,  join_by(xvar, xlbl)) %>% 
  left_join(tmp_scotland, join_by(xvar, xlbl)) %>% 
  left_join(tmp_wales,    join_by(xvar, xlbl))

qsave(
  d_all,
  file = "meta_analysis_results/strict_qcovid_d_desc_all.qs"
)

d_all_pretty <-
  d_all %>% 
  # format
  mutate(across(
    .cols = ends_with("_n"),
    .fns  = ~ if_else(1 <= .x & .x <= 9, NA_character_, format(round_half_up(.x, -1), nsmall = 0, big.mark = ",", trim = TRUE))
  )) %>% 
  mutate(across(
    .cols = ends_with("_p"),
    .fns  = ~ percent(round_half_up(.x, 1), accuracy = 0.1, scale = 1)
  )) %>% 
  mutate(across(
    .cols = ends_with("_e"),
    .fns  = ~ if_else(1 <= .x & .x <= 9, NA_character_, format(round_half_up(.x, -1), nsmall = 0, big.mark = ",", trim = TRUE))
  )) %>% 
  mutate(across(
    .cols = ends_with("_r"),
    .fns  = ~ format(round_half_up(.x,  1), nsmall = 1, big.mark = ",", trim = TRUE)
  )) %>% 
  mutate(across(
    .cols = matches(".*_[nper]$"),
    .fns  = ~ if_else(is.na(.x) | .x == "NA", "-", .x)
  )) %>% 
  # concatenate
  mutate(
    england_np  = str_glue("{england_n}\n({england_p})"),
    england_er  = str_glue("{england_e}\n({england_r})"),
    scotland_np = str_glue("{scotland_n}\n({scotland_p})"),
    scotland_er = str_glue("{scotland_e}\n({scotland_r})"),
    wales_np    = str_glue("{wales_n}\n({wales_p})"),
    wales_er    = str_glue("{wales_e}\n({wales_r})"),
    pool_np     = str_glue("{pool_n}\n({pool_p})"),
    pool_er     = str_glue("{pool_e}\n({pool_r})")
  ) %>% 
  mutate(across(
    .cols = matches(".*_(np|er)$"),
    .fns  = ~ if_else(str_detect(.x, "^-"), "-", .x)
  )) %>% 
  # final select
  select(
    xvar,
    xlbl,
    england_np,
    england_er,
    scotland_np,
    scotland_er,
    wales_np,
    wales_er,
    pool_np,
    pool_er
  ) %>% 
  filter(xlbl != 0) %>% 
  left_join(lkp_xlbl_pretty, join_by(xvar, xlbl)) %>% 
  # order by name of condition
  mutate(
    xlbl_pretty = factor(xlbl_pretty),
    xlbl_pretty = fct_relevel(xlbl_pretty, "Total")
  ) %>% 
  arrange(xlbl_pretty) %>% 
  select(
    xvar,
    xlbl_pretty,
    england_np,
    england_er,
    scotland_np,
    scotland_er,
    wales_np,
    wales_er,
    pool_np,
    pool_er
  )

write.xlsx(
  x = d_all_pretty,
  file = "meta_analysis_results/strict_qcovid_t_desc_all.xlsx"
)


stop("HERE")

# derive footnote symbols ------------------------------------------------------

lkp_xvar_footnote <-
  d_all %>%
  filter(!is.na(xvar)) %>% 
  group_by(xvar) %>% 
  summarise(
    england  = any(!is.na(england_n)),
    scotland = any(!is.na(scotland_n)),
    wales    = any(!is.na(wales_n))
  ) %>% 
  mutate(
    footnote = case_when(
       england & !scotland & !wales ~ "†",
      !england &  scotland &  wales ~ "‡",
      !england & !scotland & !wales ~ "§",
       england & !scotland &  wales ~ "¶"
    )
  ) %>% 
  select(
    xvar,
    footnote
  )

print(lkp_xvar_footnote, n = Inf)

# pretty -----------------------------------------------------------------------

d_pool_pretty <-
  d_pool %>% 
  filter(xlbl != 0) %>% 
  left_join(lkp_xlbl_pretty, join_by(xvar, xlbl)) %>% 
  left_join(lkp_xvar_footnote, join_by(xvar)) %>% 
  # order by name of condition
  mutate(
    xlbl_pretty = if_else(is.na(footnote), xlbl_pretty, str_c(xlbl_pretty, " ", footnote)),
    xlbl_pretty = factor(xlbl_pretty),
    xlbl_pretty = fct_relevel(xlbl_pretty, "Total")
  ) %>% 
  arrange(xlbl_pretty) %>% 
  # final formatting
  mutate(
    n = comma(n, accuracy = 10),
    p = percent(p, accuracy = 0.1, scale = 1),
    e = comma(e, accuracy = 10),
    r = comma(r, accuracy = 0.1)
  ) %>% 
  # final select
  select(
    xvar,
    xlbl_pretty,
    n,
    p,
    e,
    r
  )

# View(d_pool_pretty)

write.xlsx(
  x    = d_pool_pretty,
  file = "meta_analysis_results/strict_qcovid_t_desc_pool.xlsx"
)



# done! ------------------------------------------------------------------------
cat("done!\n")
