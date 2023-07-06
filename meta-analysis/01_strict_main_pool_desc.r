source("clear_workspace.r")


# lookup for tidy names --------------------------------------------------------

lkp_strata <- c(
  "Overall"   = "overall",
  "Comirnaty" = "pb",
  "Spikevax"  = "md"
)

lkp_xvar_xlbl <- read_xlsx(path = "lkp_main_xvar_xlbl.xlsx")

lkp_xvar <-
  lkp_xvar_xlbl %>% 
  filter(!is.na(clean_xvar)) %>% 
  pull(clean_xvar) %>% 
  unique()

lkp_xlbl <-
  lkp_xvar_xlbl %>% 
  select(clean_xlbl) %>% 
  drop_na() %>% 
  distinct() %>% 
  arrange(
    clean_xlbl != 0,
    clean_xlbl != 1,
    clean_xlbl != 2,
    clean_xlbl != 3,
    clean_xlbl != 4,
    clean_xlbl != 5,
    clean_xlbl != "5th (Least)",
    clean_xlbl != "4th",
    clean_xlbl != "3rd",
    clean_xlbl != "2nd",
    clean_xlbl != "1st (Most)"
  ) %>% 
  pull(clean_xlbl)


# load England -----------------------------------------------------------------

l_england_pyears <- list()

l_england_pyears[["overall"]] <-
  read_csv(
    file = "england_results/p_years_strict.csv",
    col_types = "_ccdd",
    name_repair = make_clean_names
  )
  
l_england_pyears[["md"]] <-
  read_csv(
    file = "england_results/p_years_strict_m.csv",
    col_types = "_ccdd",
    name_repair = make_clean_names
  )

l_england_pyears[["pb"]] <-
  read_csv(
    file = "england_results/p_years_strict_p.csv",
    col_types = "_ccdd",
    name_repair = make_clean_names
  )

d_england_pyears <- bind_rows(l_england_pyears, .id = "strata")


l_england_desc <- list()

l_england_desc[["overall"]] <-
  bind_rows(
    read_csv(
      file = "england_results/cohort_desc_strict.csv",
      col_types = "_ccddd",
      name_repair = make_clean_names
    ),
    read_csv(
      file = "england_results/time_since_vacc_strict.csv",
      col_types = "_ccddd",
      name_repair = make_clean_names
    )
  )

l_england_desc[["md"]] <-
  bind_rows(
    read_csv(
      file = "england_results/cohort_desc_strict_m.csv",
      col_select = -1,
      col_types = "_ccddd",
      name_repair = make_clean_names
    ),
    read_csv(
      file = "england_results/time_since_vacc_strict_m.csv",
      col_select = -1,
      col_types = "_ccddd",
      name_repair = make_clean_names
    )
  )

l_england_desc[["pb"]] <-
  bind_rows(
    read_csv(
      file = "england_results/cohort_desc_strict_p.csv",
      col_select = -1,
      col_types = "_ccddd",
      name_repair = make_clean_names
    ),
    read_csv(
      file = "england_results/time_since_vacc_strict_p.csv",
      col_select = -1,
      col_types = "_ccddd",
      name_repair = make_clean_names
    )
  )

d_england <-
  bind_rows(l_england_desc, .id = "strata") %>% 
  left_join(d_england_pyears, join_by(strata, name, value)) %>% 
  mutate(country = "england") %>% 
  # apply tidy xvar and xlbl values
  left_join(lkp_xvar_xlbl, join_by(name == england_xvar, value == england_xlbl)) %>% 
  # final select
  select(
    country,
    strata,
    xvar    = clean_xvar,
    xlbl    = clean_xlbl,
    n       = total_covid_event,
    pyears  = person_years,
    events  = covid_event,
    rate    = rate_per_1000_person_yrs
  ) %>% 
  # calculate column percentage
  group_by(strata, xvar) %>% 
  mutate(percent = round(n / sum(n) * 100, 1), .after = n) %>% 
  ungroup()

# addon total
d_england_total <-
  d_england %>% 
  filter(xvar == "sex") %>% 
  group_by(strata) %>% 
  summarise(
    country = "england",
    xvar = "total",
    xlbl = "Total",
    n = sum(n),
    percent = sum(percent),
    events = sum(events),
    pyears = sum(pyears)
  ) %>% 
  mutate(
    rate = events / pyears * 1000
  )

d_england <-
  bind_rows(d_england_total, d_england) %>%
  arrange(strata) %>% 
  # remove vacc name from vaccine specific strata
  filter(!(
    strata %in% c("md", "pb") & xvar == "vacc_name"
  ))


# load Scotland ----------------------------------------------------------------

expr <- "([0-9]+) \\((.*)\\)"

d_scotland <-
  read_csv(
    file = "scotland_results/strict_hosp_death_main_desc.csv"
  ) %>% 
  rename(
    strata = vacc_group
  ) %>% 
  # remove unused xvars
  filter(!(
    xvar %in% c("thera_bef", "thera_aft", "n_tests_prebooster")
  )) %>% 
  # standardise strata
  mutate(
    strata = str_to_lower(strata),
    strata = if_else(strata == "mo", "md", strata)
  ) %>% 
  # un-format the columns
  mutate(
    country = "scotland",
    n       = str_replace(n_percentage, expr, "\\1"),
    n       = as.numeric(n),
    percent = str_replace(n_percentage, expr, "\\2"),
    percent = as.numeric(percent),
    events  = str_replace(events_rate, expr, "\\1"),
    events  = as.numeric(events),
    rate    = str_replace(events_rate, expr, "\\2"),
    rate    = as.numeric(rate)
  ) %>% 
  # apply tidy xvar and xlbl
  left_join(lkp_xvar_xlbl, join_by(xvar == scotland_xvar, xlbl == scotland_xlbl)) %>% 
  group_by(strata, xvar) %>% 
  mutate(percent = round(n / sum(n) * 100), 1) %>% 
  ungroup() %>% 
  # final select
  select(
    country,
    strata,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    n,
    percent,
    pyears,
    events,
    rate
  ) %>% 
  # add total sample size as a column
  group_by(strata) %>% 
  mutate(total_n = first(n)) %>% 
  ungroup() %>% 
  # re-calculate percentages
  # categories for vacc_week are overlapping
  group_by(strata, xvar) %>% 
  mutate(
    percent = n / ifelse(xvar == "vacc_week", total_n, sum(n)),
    percent = percent * 100
  ) %>%
  ungroup() %>% 
  select(-total_n) %>% 
  # remove vacc name from vaccine specific strata
  filter(!(
    strata %in% c("md", "pb") & xvar == "vacc_name"
  ))


# load Wales -------------------------------------------------------------------

l_wales <- list()
l_wales[["overall"]] <- read_csv("wales_results/strict_hosp_death_overall_desc.csv")
l_wales[["md"]]      <- read_csv("wales_results/strict_hosp_death_md_desc.csv")
l_wales[["pb"]]      <- read_csv("wales_results/strict_hosp_death_pb_desc.csv")

d_wales <- 
  bind_rows(l_wales, .id = "strata") %>% 
  # set country
  mutate(country = "wales") %>% 
  # tidy column names
  rename(
    n = total_n,
    percent = total_p
  ) %>% 
  # apply tidy xvar and xlbl values
  left_join(lkp_xvar_xlbl, join_by(xvar == wales_xvar, xlbl == wales_xlbl)) %>% 
  # final select
  select(
    country,
    strata,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    n,
    percent,
    pyears,
    events,
    rate
  ) %>% 
  # replace suppressed counts with 1
  mutate(
    events = if_else(!is.na(n) & is.na(events), 1, events, events)
  ) %>% 
  # add total sample size as a column
  group_by(strata) %>% 
  mutate(total_n = first(n)) %>% 
  ungroup() %>% 
  # re-calculate percentages
  # categories for vacc_week are overlapping
  group_by(strata, xvar) %>% 
  mutate(
    percent = n / ifelse(xvar == "vacc_week", total_n, sum(n)),
    percent = percent * 100
  ) %>%
  ungroup() %>% 
  select(-total_n) %>% 
  # remove vacc name from vaccine specific strata
  filter(!(
    strata %in% c("md", "pb") & xvar == "vacc_name"
  ))

# load Northern Ireland --------------------------------------------------------

l_ni <- list()
l_ni[["overall"]] <- read_csv("northern_ireland_results/strict_hosp_death_overall_desc.csv")
l_ni[["pb"]]      <- read_csv("northern_ireland_results/strict_hosp_death_pb_desc.csv")

d_ni <-
  bind_rows(l_ni, .id = "strata") %>% 
  # set country
  mutate(country = "ni") %>% 
  # tidy column names
  rename(
    n = total_n,
    percent = total_p
  ) %>% 
  # apply tidy xvar and xlbl values
  left_join(lkp_xvar_xlbl, join_by(xvar == ni_xvar, xlbl == ni_xlbl)) %>% 
  # final select
  select(
    country,
    strata,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    n,
    percent,
    pyears,
    events,
    rate
  ) %>% 
  # replace suppressed counts with 1
  mutate(
    events = if_else(!is.na(n) & is.na(events), 1, events, events)
  ) %>% 
  # add total sample size as a column
  group_by(strata) %>% 
  mutate(total_n = first(n)) %>% 
  ungroup() %>% 
  # re-calculate percentages
  # categories for vacc_week are overlapping
  group_by(strata, xvar) %>% 
  mutate(
    percent = n / ifelse(xvar == "vacc_week", total_n, sum(n)),
    percent = percent * 100
  ) %>%
  ungroup() %>% 
  select(-total_n) %>% 
  # remove vacc name from vaccine specific strata
  filter(!(
    strata %in% c("md", "pb") & xvar == "vacc_name"
  ))


# combine nations --------------------------------------------------------------
cat("combine nations\n")

d_pool <-
  bind_rows(
    d_england,
    d_scotland,
    d_wales,
    d_ni
  ) %>% 
  filter(xvar != "health_board") %>% 
  # add counts together and suppress
  group_by(strata, xvar, xlbl) %>% 
  summarise(
    n      = sum(n, na.rm = TRUE),
    n      = round(n, -1),
    pyears = sum(pyears, na.rm = TRUE),
    events = sum(events, na.rm = TRUE),
    events = round(events, -1),
    events = if_else(1 <= events & events <= 9, NA_real_, events, events),
  ) %>% 
  ungroup() %>%
  # reorder rows
  mutate(
    strata = factor(strata, lkp_strata),
    xvar = factor(xvar, lkp_xvar),
    xlbl = factor(xlbl, lkp_xlbl)
  ) %>% 
  arrange(
    strata,
    xvar,
    xlbl
  ) %>% 
  # add total sample size as a column
  group_by(strata) %>% 
  mutate(total_n = first(n)) %>% 
  ungroup() %>% 
  # calculate percentages and rates
  # categories for vacc_week are overlapping
  group_by(strata, xvar) %>% 
  mutate(
    p = n / ifelse(xvar == "vacc_week", total_n, sum(n)),
    rate = events / pyears * 1000
  ) %>%
  ungroup() %>% 
  # make pretty
  mutate(
    np = str_glue(
      "{n}\n({p})",
      n = comma(n, accuracy = 10),
      p = percent(p, accuracy = 0.1)
    ),
    er = str_glue(
      "{e}\n({r})",
      e = comma(events, accuracy = 10),
      r = comma(rate, accuracy = 0.1)
    )
  ) %>%
  # final select
  select(
    strata,
    xvar,
    xlbl,
    n,
    p,
    e = events,
    r = rate,
    np,
    er
  )


# checking ---------------------------------------------------------------------
cat("checking\n")

tmp_england <-
  d_england %>%
  select(strata, xvar, xlbl, n_england = n, e_england = events) %>% 
  filter(xvar != "health_board")

tmp_scotland <-
  d_scotland %>%
  select(strata, xvar, xlbl, n_scotland = n, e_scotland = events) %>% 
  filter(xvar != "health_board")

tmp_wales <-
  d_wales %>%
  select(strata, xvar, xlbl, n_wales = n, e_wales = events) %>% 
  filter(xvar != "health_board")

tmp_ni <-
  d_ni %>%
  select(strata, xvar, xlbl, n_ni = n, e_ni = events) %>% 
  filter(xvar != "health_board")

d_pool_check <-
  d_pool %>% 
  select(strata, xvar, xlbl, n_pool = n, e_pool = e) %>% 
  left_join(tmp_england,  join_by(strata, xvar, xlbl)) %>% 
  left_join(tmp_scotland, join_by(strata, xvar, xlbl)) %>% 
  left_join(tmp_wales,    join_by(strata, xvar, xlbl)) %>% 
  left_join(tmp_ni,       join_by(strata, xvar, xlbl)) %>% 
  select(
    strata,
    xvar,
    xlbl,
    starts_with("n_"),
    starts_with("e_")
  )

# england only NA for BNF chapters and household size
d_check_england <-
  d_pool_check %>%
  filter(xvar != "bnf_n" & is.na(n_england)) %>%
  filter(xvar != "household_n" & is.na(n_england)) %>%
  select(strata, xvar, xlbl, n_england) %>% 
  as.data.frame()

if (nrow(d_check_england) > 0) {
  cat("England:\n")
  print(d_check_england)
  stop("England check failed")
}

# scotland only NA for BNF chapters
d_check_scotland <-
  d_pool_check %>%
  filter(xvar != "bnf_n" & is.na(n_scotland)) %>%
  select(strata, xvar, xlbl, n_scotland) %>% 
  as.data.frame()

if (nrow(d_check_scotland) > 0) {
  cat("scotland:\n")
  print(d_check_scotland)
  stop("Scotland check failed")
}

# wales only NA for BNF chapters
d_check_wales <-
  d_pool_check %>%
  filter(xvar != "bnf_n" & is.na(n_wales)) %>%
  select(strata, xvar, xlbl, n_wales) %>% 
  as.data.frame()

if (nrow(d_check_wales) > 0) {
  cat("wales:\n")
  print(d_check_wales)
  stop("Wales check failed")
}

# northern ireland only has values for overall and pfizer strata and
# is missing values for covariates: ethnicity, bmi and qcovid

d_check_ni <-
  d_pool_check %>%
  filter(strata %in% c("overall", "pb") & !(xvar %in% c("ethnicity", "bmi", "qcovid_n")) & is.na(n_ni)) %>%
  select(strata, xvar, xlbl, n_ni) %>% 
  as.data.frame()

if (nrow(d_check_ni) > 0) {
  cat("northern ireland:\n")
  print(d_check_ni)
  stop("NI check failed")
}

# derive footnote symbols ------------------------------------------------------

lkp_xvar_footnote <-
  bind_rows(
    d_england,
    d_scotland,
    d_wales,
    d_ni
  ) %>% 
  filter(!is.na(xvar)) %>% 
  group_by(xvar) %>% 
  summarise(
    england  = any(country == "england"),
    ni       = any(country == "ni"),
    scotland = any(country == "scotland"),
    wales    = any(country == "wales")
  ) %>% 
  mutate(
    footnote = case_when(
       england & !ni &  scotland &  wales ~ "*",
       england & !ni & !scotland & !wales ~ "†",
      !england & !ni &  scotland &  wales ~ "‡",
      !england &  ni & !scotland & !wales ~ "§",
       england & !ni & !scotland &  wales ~ "¶",
                                     TRUE ~ ""
    )
  ) %>% 
  select(
    xvar,
    footnote
  )

# human friendly nation results ------------------------------------------------
cat("human friendly nation results\n")

tmp_england <-
  d_england %>%
  select(
    strata,
    xvar,
    xlbl,
    england_n = n,
    england_p = percent,
    england_e = events,
    england_r = rate
  ) %>% 
  filter(xvar != "health_board")

tmp_ni <-
  d_ni %>%
  select(
    strata,
    xvar,
    xlbl,
    ni_n = n,
    ni_p = percent,
    ni_e = events,
    ni_r = rate
  ) %>% 
  filter(xvar != "health_board")

tmp_scotland <-
  d_scotland %>%
  select(
    strata,
    xvar,
    xlbl,
    scotland_n = n,
    scotland_p = percent,
    scotland_e = events,
    scotland_r = rate
  ) %>% 
  filter(xvar != "health_board")

tmp_wales <-
  d_wales %>%
  select(
    strata,
    xvar,
    xlbl,
    wales_n = n,
    wales_p = percent,
    wales_e = events,
    wales_r = rate
  ) %>% 
  filter(xvar != "health_board")

d_all <-
  d_pool %>% 
  select(
    strata,
    xvar,
    xlbl,
    pool_n = n,
    pool_p = p,
    pool_e = e,
    pool_r = r
  ) %>% 
  mutate(
    pool_p = pool_p * 100
  ) %>% 
  left_join(tmp_england,  join_by(strata, xvar, xlbl)) %>% 
  left_join(tmp_ni,       join_by(strata, xvar, xlbl)) %>% 
  left_join(tmp_scotland, join_by(strata, xvar, xlbl)) %>% 
  left_join(tmp_wales,    join_by(strata, xvar, xlbl)) %>% 
  # pretty
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
  mutate(
    england_np  = str_glue("{england_n}\n({england_p})"),
    england_er  = str_glue("{england_e}\n({england_r})"),
    ni_np       = str_glue("{ni_n}\n({ni_p})"),
    ni_er       = str_glue("{ni_e}\n({ni_r})"),
    scotland_np = str_glue("{scotland_n}\n({scotland_p})"),
    scotland_er = str_glue("{scotland_e}\n({scotland_r})"),
    wales_np    = str_glue("{wales_n}\n({wales_p})"),
    wales_er    = str_glue("{wales_e}\n({wales_r})"),
    pool_np     = str_glue("{pool_n}\n({pool_p})"),
    pool_er     = str_glue("{pool_e}\n({pool_r})")
  ) %>% 
  mutate(across(
    .cols = matches(".*_(np|er)$"),
    .fns  = ~ if_else(str_detect(.x, "^- "), "-", .x)
  )) %>% 
  # final select
  select(
    strata,
    xvar,
    xlbl,
    england_np,
    england_er,
    ni_np,
    ni_er,
    scotland_np,
    scotland_er,
    wales_np,
    wales_er,
    pool_np,
    pool_er
  )

write_pretty_xlsx(
  df   = d_all,
  file = "meta_analysis_results/strict_main_t_desc_all.xlsx"
)

# pretty pooled results --------------------------------------------------------
cat("pretty pooled results\n")

d_pool_pretty <-
  d_all %>% 
  select(
    strata,
    xvar,
    xlbl,
    np = pool_np,
    er = pool_er
  ) %>% 
  pivot_wider(
    names_from = strata,
    values_from = c(np, er),
    names_glue = "{strata}_{.value}"
  ) %>% 
  select(
    xvar,
    xlbl,
    starts_with("overall"),
    starts_with("pb"),
    starts_with("md")
  )

write_pretty_xlsx(
  df   = d_pool_pretty,
  file = "meta_analysis_results/strict_main_t_desc_pool.xlsx"
)


# done! ------------------------------------------------------------------------
cat("done!\n")
