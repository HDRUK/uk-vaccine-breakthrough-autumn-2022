source("clear_workspace.r")

# lookup for tidy names --------------------------------------------------------

lkp_xvar_xlbl <-
  read_xlsx(path = "lkp_main_xvar_xlbl.xlsx") %>% 
  select(scotland_xvar, scotland_xlbl, clean_xvar, clean_xlbl) %>% 
  bind_rows(
    tribble(
      ~scotland_xvar,             ~scotland_xlbl,    ~clean_xvar, ~clean_xlbl,
         "thera_bef",                         NA, "thera_before",  "0",
         "thera_bef",                        "1", "thera_before",  "1",
         "thera_bef",                        "2", "thera_before",  "2",
         "thera_bef",                        "3", "thera_before",  "3",
         "thera_bef",                        "4", "thera_before",  "4",
         "thera_aft",                         NA,  "thera_after",  "0",
         "thera_aft",                        "1",  "thera_after",  "1",
         "thera_aft",                        "2",  "thera_after",  "2",
         "thera_aft",                        "3",  "thera_after",  "3",
         "thera_aft",                        "4",  "thera_after",  "4",
         "Ther_type",    "Multiple therapeutics",   "thera_type",  "Multiple therapeutics",
         "Ther_type",            "Not Specified",   "thera_type",  "Not specified",
         "Ther_type", "Only monoclonal antibody",   "thera_type",  "Only monoclonal antibody",
         "Ther_type", "Only antiviral treatment",   "thera_type",  "Only antiviral treatment"
    )
  )

lkp_xvar <-
  lkp_xvar_xlbl %>%
  select(clean_xvar) %>%
  distinct() %>% 
  drop_na() %>% 
  pull(clean_xvar)
  

# load and process Scotland data -----------------------------------------------

expr <- "(.*) \\((.*)\\)"

d_scotland <-
  read_csv(file = "scotland_results/strict_thera_overall_desc.csv") %>% 
  filter(
    time_therapeutics %in% c("Prebooster", "postbooster")
  ) %>% 
  mutate(
    n = str_replace(n_percentage, expr, "\\1"),
    p = str_replace(n_percentage, expr, "\\2"),
    e = str_replace(events_rate, expr, "\\1"),
    r = str_replace(events_rate, expr, "\\2")
  ) %>% 
  mutate(across(
    .cols = c(n, p, e, r),
    .fns = as.numeric
  )) %>%
  left_join(lkp_xvar_xlbl, join_by(xvar == scotland_xvar, xlbl == scotland_xlbl)) %>% 
  select(
    strata = time_therapeutics,
    xvar = clean_xvar,
    xlbl = clean_xlbl,
    n,
    p,
    e,
    r
  ) %>% 
  filter(
    xvar != "health_board"
  ) %>% 
  mutate(
    strata = str_to_lower(strata)
  ) %>% 
  mutate(
    n = if_else(1 <= n & n <= 9, NA_real_, n),
    n = format(round_half_up(n, -1), big.mark = ",", trim = TRUE),
    p = format(round_half_up(p, 1), nsmall = 1, trim = TRUE),
    np = if_else(n == "NA", "<10", str_glue("{n} ({p}%)")),
    e = if_else(1 <= e & e <= 9, NA_real_, e),
    e = format(round_half_up(e, -1), big.mark = ",", trim = TRUE),
    r = format(round_half_up(r, 1), nsmall = 1, trim = TRUE),
    er = if_else(e == "NA", "<10", str_glue("{e} ({r})"))
  ) %>% 
  select(
    -n, -p, -e, -r
  ) %>% 
  pivot_wider(
    id_cols = c(xvar, xlbl),
    names_from = strata,
    values_from = c(np, er)
  ) %>% 
  select(
    xvar, xlbl, matches("prebooster"), matches("postbooster")
  ) %>% 
  mutate(
    xvar = factor(xvar, lkp_xvar)
  ) %>% 
  arrange(
    xvar
  )

# save -------------------------------------------------------------------------

write_pretty_xlsx(
  df = d_scotland,
  file = "meta_analysis_results/strict_scotland_theraputics_desc.xlsx"
)
