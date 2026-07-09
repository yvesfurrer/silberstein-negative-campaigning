###############################################################################
# Silberstein Affair and Negative Campaigning: AUTNES 2017 Press Releases
# and Facebook Pages
# Author: Yves Furrer | MA Seminar Paper, University of Lucerne
# Purpose: Full replication - data preparation, models, tables, figures
###############################################################################

## Block 0: Packages -----------------------------------------------------------
pkgs <- c("dplyr", "tidyr", "readr", "MASS", "sandwich", "lmtest",
          "modelsummary", "ggplot2")
new  <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new) > 0) install.packages(new)
library(dplyr)
library(tidyr)
library(readr)
library(sandwich)
library(lmtest)
library(modelsummary)
library(ggplot2)
# MASS is NOT attached (MASS::select would mask dplyr::select); use MASS::glm.nb

## Block 1: Paths & parameters --------------------------------------------------
options(timeout = 300)
DIR_RAW  <- "Data/raw"
DIR_DATA <- "Data"
DIR_OUT  <- "Output"
dir.create(DIR_DATA, showWarnings = FALSE)
dir.create(DIR_OUT,  showWarnings = FALSE)

# Revelation published 30 Sep 2017 (profil via OTS); post = 30 Sep onwards
# (26 pre / 15 post days). Alternative cut-offs as robustness (Block 5).
POST_START <- as.Date("2017-09-30")
ALT_O1     <- as.Date("2017-10-01")
ALT_O2     <- as.Date("2017-10-02")

# Parliamentary party families (first 2 digits of AUTNES actor codes + "00000")
PARTIES <- c("1100000" = "SPOE", "1200000" = "OEVP", "1300000" = "FPOE",
             "1400000" = "GREENS", "2400000" = "NEOS", "2700000" = "PILZ")

# Vote shares, National Council election 2013 (%), as reported in
# Bodlos & Plescia (2018, Table 1, p. 1360); PILZ founded 2017 -> NA
VOTE13 <- c(SPOE = 26.8, OEVP = 24.0, FPOE = 20.5,
            GREENS = 12.4, NEOS = 5.0, PILZ = NA)

RUN_DATA_PREP <- FALSE   # set FALSE after first successful run

## Block 2: Data preparation ----------------------------------------------------
if (RUN_DATA_PREP) {
  
  # dates like "04sep2017" need English month names, independent of Windows locale
  loc_old <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  
  fam <- function(code) {
    code <- as.character(code)
    out  <- ifelse(!is.na(code) & nchar(code) == 7,
                   paste0(substr(code, 1, 2), "00000"), NA)
    unname(PARTIES[out])
  }
  
  ## --- Press releases (10726), 2017 subsample --------------------------------
  pr_raw <- read_csv(file.path(DIR_RAW, "10726_da_en_v1_0.csv"),
                     col_types = cols(.default = col_character()))
  pr17 <- pr_raw %>%
    filter(year == "2017") %>%
    mutate(date = as.Date(date, format = "%d%b%Y"))
  
  # 3 object slots: org (v19/v24/v29), predicate (v20/v25/v30), name (v18/v23/v28)
  pr_long <- bind_rows(
    pr17 %>% transmute(date, sender = fam(v0), t_org = v19, pred = v20, name = v18),
    pr17 %>% transmute(date, sender = fam(v0), t_org = v24, pred = v25, name = v23),
    pr17 %>% transmute(date, sender = fam(v0), t_org = v29, pred = v30, name = v28)
  ) %>%
    mutate(target = fam(t_org)) %>%
    filter(pred == "-1", !is.na(target), !is.na(sender), target != sender) %>%
    mutate(channel = "PR",
           personalised = as.integer(name != "1" & !is.na(name)))
  
  ## --- Facebook (10728) -------------------------------------------------------
  fb_raw <- read_csv(file.path(DIR_RAW, "10728_da_en_v1_0.csv"),
                     col_types = cols(.default = col_character()))
  fb_posts <- fb_raw %>%
    mutate(date = as.Date(substr(v05, 1, 9), format = "%d%b%Y"),
           sender = fam(v15a))
  
  # 10 object slots: org v27_i, predicate v29_i, name v26_i
  fb_long <- lapply(1:10, function(i) {
    fb_posts %>% transmute(date, sender,
                           t_org = .data[[paste0("v27_", i)]],
                           pred  = .data[[paste0("v29_", i)]],
                           name  = .data[[paste0("v26_", i)]])
  }) %>%
    bind_rows() %>%
    mutate(target = fam(t_org)) %>%
    filter(pred == "-1", !is.na(target), !is.na(sender), target != sender) %>%
    mutate(channel = "FB",
           personalised = as.integer(name != "1" & !is.na(name)))
  
  attacks <- bind_rows(pr_long, fb_long) %>% select(-t_org, -name)
  
  ## --- CHES 2019 left-right scores (2017 flash excludes Austria) --------------
  ches_file <- file.path(DIR_RAW, "CHES2019V3.csv")
  if (!file.exists(ches_file)) {
    download.file("https://www.chesdata.eu/s/CHES2019V3.csv", ches_file, mode = "wb")
  }
  ches <- read_csv(ches_file, col_types = cols(.default = col_character()))
  ches_map <- c("SPO" = "SPOE", "OVP" = "OEVP", "FPO" = "FPOE",
                "GRUNE" = "GREENS", "GRUENE" = "GREENS", "NEOS" = "NEOS",
                "JETZT" = "PILZ", "PILZ" = "PILZ")
  lr <- ches %>%
    filter(country %in% c("13", "aus", "at", "AT", "Austria")) %>%
    mutate(party_std = unname(ches_map[toupper(party)]),
           lrgen     = as.numeric(lrgen)) %>%
    filter(!is.na(party_std)) %>%
    select(party_std, lrgen)
  print(lr)   # expected: SPOE 4.0, OEVP 6.9, FPOE 9.1, GREENS 2.5, NEOS 5.9
  LRGEN <- setNames(lr$lrgen, lr$party_std)
  
  ## --- Dyad-day panel (incl. zero days, all derived variables) ----------------
  all_days <- seq(min(attacks$date), max(attacks$date), by = "day")
  grid <- expand_grid(sender  = unname(PARTIES),
                      target  = unname(PARTIES),
                      date    = all_days,
                      channel = c("PR", "FB")) %>%
    filter(sender != target)
  
  panel <- grid %>%
    left_join(attacks %>%
                group_by(sender, target, date, channel) %>%
                summarise(n_attacks      = n(),
                          n_personalised = sum(personalised),
                          .groups = "drop"),
              by = c("sender", "target", "date", "channel")) %>%
    mutate(n_attacks      = replace_na(n_attacks, 0),
           n_personalised = replace_na(n_personalised, 0),
           n_partylevel   = n_attacks - n_personalised,
           post           = as.integer(date >= POST_START),
           post_o1        = as.integer(date >= ALT_O1),
           post_o2        = as.integer(date >= ALT_O2),
           coalition      = as.integer((sender == "SPOE" & target == "OEVP") |
                                         (sender == "OEVP" & target == "SPOE")),
           size_ratio     = VOTE13[sender] / VOTE13[target],
           lr_dist        = abs(LRGEN[sender] - LRGEN[target]),
           spoe_target    = as.integer(target == "SPOE"),
           spoe_sender    = as.integer(sender == "SPOE"),
           dyad           = paste(sender, target, sep = "_"),
           fb             = as.integer(channel == "FB"))
  
  saveRDS(attacks, file.path(DIR_DATA, "attacks_long.rds"))
  saveRDS(panel,   file.path(DIR_DATA, "panel.rds"))
  Sys.setlocale("LC_TIME", loc_old)
  
} else {
  attacks <- readRDS(file.path(DIR_DATA, "attacks_long.rds"))
  panel   <- readRDS(file.path(DIR_DATA, "panel.rds"))
}

## Sanity checks (expected: PR 388, FB 928; 26 pre / 15 post days; 2460 rows) ---
print(table(attacks$channel))
print(attacks %>% group_by(channel, post = date >= POST_START) %>% tally())
print(panel %>% distinct(date, post) %>% count(post))
print(dim(panel))

## Convenience subsets -----------------------------------------------------------
pr_p     <- panel %>% filter(channel == "PR")
fb_p     <- panel %>% filter(channel == "FB")
pr_spoe  <- filter(pr_p, sender == "SPOE")
fb_spoe  <- filter(fb_p, sender == "SPOE")
panel_dt <- panel %>% filter(!date %in% c(as.Date("2017-09-30"), ALT_O1))
fb_dt    <- filter(panel_dt, channel == "FB")
fb_dt_sp <- filter(fb_dt, sender == "SPOE")

# SEs clustered by date (41 clusters; treatment varies at the day level).
cl_d  <- function(m, dat) coeftest(m, vcov = vcovCL(m, cluster = dat[, "date", drop = FALSE]))
# Dyad clustering: appendix robustness for full-sample models only (30 clusters).
cl_dy <- function(m, dat) coeftest(m, vcov = vcovCL(m, cluster = dat[, "dyad", drop = FALSE]))

## Block 3: Descriptives ----------------------------------------------------------
# Figure 1: daily inter-party attacks by channel, revelation marked
daily <- attacks %>% count(channel, date)
fig1 <- ggplot(daily, aes(x = date, y = n)) +
  geom_col(fill = "grey30") +
  geom_vline(xintercept = as.numeric(POST_START) - 0.5,
             linetype = "dashed", colour = "red") +
  facet_wrap(~ channel, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(FB = "Facebook", PR = "Press releases"))) +
  labs(x = NULL, y = "Inter-party attacks per day") +
  theme_minimal()
ggsave(file.path(DIR_OUT, "fig1_daily_attacks.png"), fig1,
       width = 8, height = 5, dpi = 300)

# Overdispersion check (justifies Negative Binomial over Poisson)
print(panel %>% group_by(channel) %>%
        summarise(mean = mean(n_attacks), var = var(n_attacks), ratio = var / mean))

# Daily attack rates pre/post (Table 1 in the paper)
print(panel %>% group_by(channel, post) %>%
        summarise(rate = sum(n_attacks) / n_distinct(date), .groups = "drop"))
print(panel %>% filter(target == "SPOE") %>% group_by(channel, post) %>%
        summarise(rate = sum(n_attacks) / n_distinct(date), .groups = "drop"))
print(panel %>% filter(sender == "SPOE") %>% group_by(channel, post) %>%
        summarise(rate = sum(n_attacks) / n_distinct(date), .groups = "drop"))

## Block 4: Main models (Negative Binomial) ---------------------------------------
## E1: overall volume (a: no controls; b: dyad controls, drops PILZ dyads)
m1a_pr <- MASS::glm.nb(n_attacks ~ post, data = pr_p)
m1a_fb <- MASS::glm.nb(n_attacks ~ post, data = fb_p)
m1b_pr <- MASS::glm.nb(n_attacks ~ post + lr_dist + coalition + size_ratio, data = pr_p)
m1b_fb <- MASS::glm.nb(n_attacks ~ post + lr_dist + coalition + size_ratio, data = fb_p)

## E2: targeting the SPOE (interaction + absolute subset models)
m2_pr  <- MASS::glm.nb(n_attacks ~ post * spoe_target, data = pr_p)
m2_fb  <- MASS::glm.nb(n_attacks ~ post * spoe_target, data = fb_p)
m2s_pr <- MASS::glm.nb(n_attacks ~ post, data = filter(pr_p, target == "SPOE"))
m2s_fb <- MASS::glm.nb(n_attacks ~ post, data = filter(fb_p, target == "SPOE"))

## E3: SPOE as sender (a: level shift; b: redirection towards FPOE)
m3a_pr <- MASS::glm.nb(n_attacks ~ post, data = pr_spoe)
m3a_fb <- MASS::glm.nb(n_attacks ~ post, data = fb_spoe)
m3b_pr <- MASS::glm.nb(n_attacks ~ post * I(target == "FPOE"), data = pr_spoe)
m3b_fb <- MASS::glm.nb(n_attacks ~ post * I(target == "FPOE"), data = fb_spoe)

## E4: channel differences (pooled post x FB)
m4   <- MASS::glm.nb(n_attacks ~ post * fb, data = panel)
m4_t <- MASS::glm.nb(n_attacks ~ post * fb, data = filter(panel, target == "SPOE"))
m4_s <- MASS::glm.nb(n_attacks ~ post * fb, data = filter(panel, sender == "SPOE"))

## Console output (exp(coefficient) = incidence rate ratio)
cl_d(m1a_pr, pr_p); cl_d(m1a_fb, fb_p)
cl_d(m1b_pr, filter(pr_p, !is.na(size_ratio)))
cl_d(m1b_fb, filter(fb_p, !is.na(size_ratio)))
cl_d(m2_pr, pr_p);  cl_d(m2_fb, fb_p)
cl_d(m2s_pr, filter(pr_p, target == "SPOE"))
cl_d(m2s_fb, filter(fb_p, target == "SPOE"))
cl_d(m3a_pr, pr_spoe); cl_d(m3a_fb, fb_spoe)
cl_d(m3b_pr, pr_spoe); cl_d(m3b_fb, fb_spoe)
cl_d(m4, panel)
cl_d(m4_t, filter(panel, target == "SPOE"))
cl_d(m4_s, filter(panel, sender == "SPOE"))

## Dyad-clustered check (full-sample models; reported in Table A1)
cl_dy(m1a_fb, fb_p); cl_dy(m1a_pr, pr_p)
cl_dy(m2_pr, pr_p);  cl_dy(m2_fb, fb_p); cl_dy(m4, panel)

## Block 5: Robustness models -------------------------------------------------------
## 5.1 Alternative post windows
r_fb_o1  <- MASS::glm.nb(n_attacks ~ post_o1, data = fb_p)
r_fb_o2  <- MASS::glm.nb(n_attacks ~ post_o2, data = fb_p)
r_pr_o1  <- MASS::glm.nb(n_attacks ~ post_o1, data = pr_p)
r_pr_o2  <- MASS::glm.nb(n_attacks ~ post_o2, data = pr_p)
r_s_o1   <- MASS::glm.nb(n_attacks ~ post_o1, data = fb_spoe)
r_s_o2   <- MASS::glm.nb(n_attacks ~ post_o2, data = fb_spoe)
r_red_o1 <- MASS::glm.nb(n_attacks ~ post_o1 * I(target == "FPOE"), data = fb_spoe)
r_red_o2 <- MASS::glm.nb(n_attacks ~ post_o2 * I(target == "FPOE"), data = fb_spoe)
r_4_o1   <- MASS::glm.nb(n_attacks ~ post_o1 * fb, data = panel)
r_4_o2   <- MASS::glm.nb(n_attacks ~ post_o2 * fb, data = panel)
r_fb_dt  <- MASS::glm.nb(n_attacks ~ post_o2, data = fb_dt)
r_s_dt   <- MASS::glm.nb(n_attacks ~ post_o2, data = fb_dt_sp)
r_red_dt <- MASS::glm.nb(n_attacks ~ post_o2 * I(target == "FPOE"), data = fb_dt_sp)

cl_d(r_fb_o1, fb_p);   cl_d(r_fb_o2, fb_p)
cl_d(r_pr_o1, pr_p);   cl_d(r_pr_o2, pr_p)
cl_d(r_s_o1, fb_spoe); cl_d(r_s_o2, fb_spoe)
cl_d(r_red_o1, fb_spoe); cl_d(r_red_o2, fb_spoe)
cl_d(r_4_o1, panel);   cl_d(r_4_o2, panel)
cl_d(r_fb_dt, fb_dt);  cl_d(r_s_dt, fb_dt_sp); cl_d(r_red_dt, fb_dt_sp)

## 5.2 Quasi-Poisson (headline models)
q_pr  <- glm(n_attacks ~ post, family = quasipoisson, data = pr_p)
q_fb  <- glm(n_attacks ~ post, family = quasipoisson, data = fb_p)
q_s   <- glm(n_attacks ~ post, family = quasipoisson, data = fb_spoe)
q_red <- glm(n_attacks ~ post * I(target == "FPOE"), family = quasipoisson, data = fb_spoe)
q_4   <- glm(n_attacks ~ post * fb, family = quasipoisson, data = panel)

cl_d(q_pr, pr_p); cl_d(q_fb, fb_p); cl_d(q_s, fb_spoe)
cl_d(q_red, fb_spoe); cl_d(q_4, panel)

## 5.3 Personalised vs party-level attacks
p_pers_pr <- MASS::glm.nb(n_personalised ~ post, data = pr_p)
p_pers_fb <- MASS::glm.nb(n_personalised ~ post, data = fb_p)
p_part_pr <- MASS::glm.nb(n_partylevel  ~ post, data = pr_p)
p_part_fb <- MASS::glm.nb(n_partylevel  ~ post, data = fb_p)
p_pers_4  <- MASS::glm.nb(n_personalised ~ post * fb, data = panel)
p_part_4  <- MASS::glm.nb(n_partylevel  ~ post * fb, data = panel)
p_pers_s  <- MASS::glm.nb(n_personalised ~ post, data = fb_spoe)
p_part_s  <- MASS::glm.nb(n_partylevel  ~ post, data = fb_spoe)

cl_d(p_pers_pr, pr_p); cl_d(p_pers_fb, fb_p)
cl_d(p_part_pr, pr_p); cl_d(p_part_fb, fb_p)
cl_d(p_pers_4, panel); cl_d(p_part_4, panel)
cl_d(p_pers_s, fb_spoe); cl_d(p_part_s, fb_spoe)

## Block 6: Publication tables -> Output/ --------------------------------------------
## (close open Word files first, else "permission denied")
COEF <- c("post"    = "Post revelation",
          "post_o1" = "Post revelation (1 Oct)",
          "post_o2" = "Post revelation (2 Oct)",
          "spoe_target" = "SPÖ target",
          "post:spoe_target" = "Post × SPÖ target",
          "lr_dist" = "Ideological distance",
          "coalition" = "Coalition dyad",
          "size_ratio" = "Size ratio",
          "fb" = "Facebook",
          "post:fb" = "Post × Facebook",
          "post_o1:fb" = "Post (1 Oct) × Facebook",
          "post_o2:fb" = "Post (2 Oct) × Facebook",
          'I(target == "FPOE")TRUE' = "FPÖ target",
          'post:I(target == "FPOE")TRUE' = "Post × FPÖ target",
          'post_o1:I(target == "FPOE")TRUE' = "Post (1 Oct) × FPÖ target",
          'post_o2:I(target == "FPOE")TRUE' = "Post (2 Oct) × FPÖ target",
          "(Intercept)" = "Intercept")
GOF  <- data.frame(raw = "nobs", clean = "N", fmt = 0)
NOTE_D  <- "Negative binomial regressions, incidence rate ratios. Standard errors clustered by day (41 clusters). + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."
NOTE_DY <- "Negative binomial regressions, incidence rate ratios. Standard errors clustered by directed party dyad (30 clusters). + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."

modelsummary(list("PR" = m1a_pr, "FB" = m1a_fb,
                  "PR ctrl" = m1b_pr, "FB ctrl" = m1b_fb,
                  "PR interaction" = m2_pr, "FB interaction" = m2_fb,
                  "PR SPÖ-target" = m2s_pr, "FB SPÖ-target" = m2s_fb),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_D,
             title = "Table 2: Attack volumes and SPÖ targeting after the revelation",
             output = file.path(DIR_OUT, "tab2_E1_E2.docx"))

modelsummary(list("PR SPÖ sender" = m3a_pr, "FB SPÖ sender" = m3a_fb,
                  "PR redirection" = m3b_pr, "FB redirection" = m3b_fb,
                  "Pooled" = m4, "Target SPÖ" = m4_t, "Sender SPÖ" = m4_s),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_D,
             title = "Table 3: The SPÖ's response and channel differences",
             output = file.path(DIR_OUT, "tab3_E3_E4.docx"))

modelsummary(list("PR" = m1a_pr, "FB" = m1a_fb,
                  "PR ctrl" = m1b_pr, "FB ctrl" = m1b_fb,
                  "PR interaction" = m2_pr, "FB interaction" = m2_fb,
                  "Pooled" = m4),
             vcov = ~ dyad, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_DY,
             title = "Table A1: Full-sample models with dyad-clustered standard errors",
             output = file.path(DIR_OUT, "tabA1_dyad_clustered.docx"))

modelsummary(list("FB 1 Oct" = r_fb_o1, "FB 2 Oct" = r_fb_o2,
                  "SPÖ FB 1 Oct" = r_s_o1, "SPÖ FB 2 Oct" = r_s_o2,
                  "Redirect 1 Oct" = r_red_o1, "Redirect 2 Oct" = r_red_o2,
                  "FB no trans." = r_fb_dt, "Redirect no trans." = r_red_dt),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_D,
             title = "Table A2: Alternative post-revelation windows",
             output = file.path(DIR_OUT, "tabA2_alt_windows.docx"))

modelsummary(list("QP PR" = q_pr, "QP FB" = q_fb, "QP SPÖ FB" = q_s,
                  "QP Redirect" = q_red, "QP Pooled" = q_4),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF,
             notes = "Quasi-Poisson regressions, incidence rate ratios. Standard errors clustered by day. + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.",
             title = "Table A3: Quasi-Poisson specifications",
             output = file.path(DIR_OUT, "tabA3_quasipoisson.docx"))

modelsummary(list("Pers PR" = p_pers_pr, "Pers FB" = p_pers_fb,
                  "Party PR" = p_part_pr, "Party FB" = p_part_fb,
                  "Pers pooled" = p_pers_4, "Party pooled" = p_part_4,
                  "Pers SPÖ FB" = p_pers_s, "Party SPÖ FB" = p_part_s),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_D,
             title = "Table A4: Personalised versus party-level attacks",
             output = file.path(DIR_OUT, "tabA4_personalised.docx"))

## Block 7: Appendix material ----------------------------------------------------
datasummary(n_attacks + n_personalised + n_partylevel + post + lr_dist +
              coalition + size_ratio ~ Factor(channel) * (Mean + SD + Min + Max),
            data = panel,
            title = "Table A5: Summary statistics by channel",
            output = file.path(DIR_OUT, "tabA5_summary_stats.docx"))

# Figure 2: SPÖ Facebook attacks per day, by target, pre vs post
fig2_dat <- fb_spoe %>%
  group_by(target, post) %>%
  summarise(rate = sum(n_attacks) / n_distinct(date), .groups = "drop") %>%
  mutate(period = factor(post, levels = c(0, 1),
                         labels = c("Pre-revelation", "Post-revelation")))
fig2 <- ggplot(fig2_dat, aes(x = target, y = rate, fill = period)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("grey70", "grey25")) +
  labs(x = NULL, y = "SPÖ Facebook attacks per day", fill = NULL) +
  theme_minimal()
ggsave(file.path(DIR_OUT, "fig2_spoe_fb_targets.png"), fig2,
       width = 7, height = 4, dpi = 300)

# Replication info
writeLines(capture.output(sessionInfo()), file.path(DIR_OUT, "sessionInfo.txt"))