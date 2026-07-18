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

# National Council election dates covered by the cumulative press release
# file (10726) and the four parties observed in all five campaigns; both are
# used for the historical benchmark (Block 5.4, Table A5)
ELEC <- c("2002" = as.Date("2002-11-24"), "2006" = as.Date("2006-10-01"),
          "2008" = as.Date("2008-09-28"), "2013" = as.Date("2013-09-29"),
          "2017" = as.Date("2017-10-15"))
P4   <- c("SPOE", "OEVP", "FPOE", "GREENS")

RUN_DATA_PREP <- TRUE   # set FALSE after first successful run

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
  
  ## --- Facebook posting volume per sender-day (exposure denominator, 5.6) ----
  fb_volume <- fb_posts %>%
    filter(!is.na(sender), !is.na(date)) %>%
    count(sender, date, name = "posts")
  
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
  
  ## --- Dyad-day panel 2017 (incl. zero days, all derived variables) -----------
  # trend counts days since 4 Sep 2017; since is 0 before the revelation and
  # counts days afterwards (both used in the trend models, Block 5.3)
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
           trend          = as.numeric(date - min(date)),
           since          = pmax(0, as.numeric(date - POST_START)),
           coalition      = as.integer((sender == "SPOE" & target == "OEVP") |
                                         (sender == "OEVP" & target == "SPOE")),
           size_ratio     = VOTE13[sender] / VOTE13[target],
           lr_dist        = abs(LRGEN[sender] - LRGEN[target]),
           spoe_target    = as.integer(target == "SPOE"),
           spoe_sender    = as.integer(sender == "SPOE"),
           fpoe_target    = as.integer(target == "FPOE"),
           dyad           = paste(sender, target, sep = "_"),
           fb             = as.integer(channel == "FB"))
  
  ## --- Historical press release panel 2002-2017 (benchmark, Block 5.4) --------
  # Cumulative 10726 file: five campaigns, each covering the six weeks before
  # the election. Restricted to the four parties observed in all campaigns;
  # final15 mirrors the 2017 post window (last 15 days before the election);
  # election-day releases (dte = 0) dropped.
  hist_long <- bind_rows(
    pr_raw %>% transmute(year, date, sender = fam(v0), t_org = v19, pred = v20),
    pr_raw %>% transmute(year, date, sender = fam(v0), t_org = v24, pred = v25),
    pr_raw %>% transmute(year, date, sender = fam(v0), t_org = v29, pred = v30)) %>%
    mutate(date   = as.Date(date, format = "%d%b%Y"),
           target = fam(t_org)) %>%
    filter(pred == "-1", sender %in% P4, target %in% P4, sender != target) %>%
    mutate(dte = as.numeric(ELEC[year] - date)) %>%
    filter(dte >= 1, dte <= 41)
  
  hpanel <- expand_grid(year = names(ELEC), sender = P4, target = P4, dte = 1:41) %>%
    filter(sender != target) %>%
    mutate(date = ELEC[year] - dte) %>%
    left_join(hist_long %>% count(year, sender, target, dte, name = "n"),
              by = c("year", "sender", "target", "dte")) %>%
    mutate(n       = replace_na(n, 0L),
           final15 = as.integer(dte <= 15),
           y2017   = as.integer(year == "2017"))
  
  saveRDS(attacks,   file.path(DIR_DATA, "attacks_long.rds"))
  saveRDS(panel,     file.path(DIR_DATA, "panel.rds"))
  saveRDS(fb_volume, file.path(DIR_DATA, "fb_volume.rds"))
  saveRDS(hpanel,    file.path(DIR_DATA, "hpanel.rds"))
  Sys.setlocale("LC_TIME", loc_old)
  
} else {
  attacks   <- readRDS(file.path(DIR_DATA, "attacks_long.rds"))
  panel     <- readRDS(file.path(DIR_DATA, "panel.rds"))
  fb_volume <- readRDS(file.path(DIR_DATA, "fb_volume.rds"))
  hpanel    <- readRDS(file.path(DIR_DATA, "hpanel.rds"))
}

## Sanity checks (expected: PR 388, FB 928; 26 pre / 15 post days; panel 2460
## rows = 30 dyads x 41 days x 2 channels; hpanel 2460 rows = 12 dyads x
## 41 days x 5 campaigns) --------------------------------------------------------
print(table(attacks$channel))
print(attacks %>% group_by(channel, post = date >= POST_START) %>% tally())
print(panel %>% distinct(date, post) %>% count(post))
print(dim(panel))
print(dim(hpanel))

## Convenience subsets -----------------------------------------------------------
pr_p     <- panel %>% filter(channel == "PR")
fb_p     <- panel %>% filter(channel == "FB")
pr_spoe  <- filter(pr_p, sender == "SPOE")
fb_spoe  <- filter(fb_p, sender == "SPOE")
panel_dt <- panel %>% filter(!date %in% c(as.Date("2017-09-30"), ALT_O1))
fb_dt    <- filter(panel_dt, channel == "FB")
fb_dt_sp <- filter(fb_dt, sender == "SPOE")
pr_spoe_t <- filter(pr_p, target == "SPOE")   # attacks ON the SPOE
fb_spoe_t <- filter(fb_p, target == "SPOE")
pr_pre   <- filter(pr_p, post == 0)           # 26 pre-revelation days (5.3)
fb_pre   <- filter(fb_p, post == 0)
fb_other <- filter(fb_p, spoe_sender == 0)    # placebo sample (5.5)
hp_pre   <- filter(hpanel, year != "2017")    # campaigns 2002-2013 (5.4)

# Sender-day panel with posting volume: attacks per Facebook post (5.6).
# All 246 sender-days have at least one post, so no observations are lost.
sd_panel <- fb_p %>%
  group_by(sender, date) %>%
  summarise(n_att = sum(n_attacks), .groups = "drop") %>%
  left_join(fb_volume, by = c("sender", "date")) %>%
  mutate(posts = replace_na(posts, 0L),
         post  = as.integer(date >= POST_START)) %>%
  filter(posts > 0)
sd_spoe  <- filter(sd_panel, sender == "SPOE")
sd_other <- filter(sd_panel, sender != "SPOE")

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
## H1: overall volume (a: no controls; b: dyad controls, drops PILZ dyads)
m1a_pr <- MASS::glm.nb(n_attacks ~ post, data = pr_p)
m1a_fb <- MASS::glm.nb(n_attacks ~ post, data = fb_p)
m1b_pr <- MASS::glm.nb(n_attacks ~ post + lr_dist + coalition + size_ratio, data = pr_p)
m1b_fb <- MASS::glm.nb(n_attacks ~ post + lr_dist + coalition + size_ratio, data = fb_p)

## H2: targeting the SPOE (interaction + absolute subset models)
m2_pr  <- MASS::glm.nb(n_attacks ~ post * spoe_target, data = pr_p)
m2_fb  <- MASS::glm.nb(n_attacks ~ post * spoe_target, data = fb_p)
m2s_pr <- MASS::glm.nb(n_attacks ~ post, data = pr_spoe_t)
m2s_fb <- MASS::glm.nb(n_attacks ~ post, data = fb_spoe_t)

## H3: SPOE as sender (a: level shift; b: redirection towards FPOE)
m3a_pr <- MASS::glm.nb(n_attacks ~ post, data = pr_spoe)
m3a_fb <- MASS::glm.nb(n_attacks ~ post, data = fb_spoe)
m3b_pr <- MASS::glm.nb(n_attacks ~ post * I(target == "FPOE"), data = pr_spoe)
m3b_fb <- MASS::glm.nb(n_attacks ~ post * I(target == "FPOE"), data = fb_spoe)

## H4: channel differences (pooled post x FB)
m4   <- MASS::glm.nb(n_attacks ~ post * fb, data = panel)
m4_t <- MASS::glm.nb(n_attacks ~ post * fb, data = filter(panel, target == "SPOE"))
m4_s <- MASS::glm.nb(n_attacks ~ post * fb, data = filter(panel, sender == "SPOE"))

## Console output (exp(coefficient) = incidence rate ratio)
cl_d(m1a_pr, pr_p); cl_d(m1a_fb, fb_p)
cl_d(m1b_pr, filter(pr_p, !is.na(size_ratio)))
cl_d(m1b_fb, filter(fb_p, !is.na(size_ratio)))
cl_d(m2_pr, pr_p);  cl_d(m2_fb, fb_p)
cl_d(m2s_pr, pr_spoe_t); cl_d(m2s_fb, fb_spoe_t)
cl_d(m3a_pr, pr_spoe); cl_d(m3a_fb, fb_spoe)
cl_d(m3b_pr, pr_spoe); cl_d(m3b_fb, fb_spoe)
cl_d(m4, panel)
cl_d(m4_t, filter(panel, target == "SPOE"))
cl_d(m4_s, filter(panel, sender == "SPOE"))

## Dyad-clustered check (full-sample models; reported in Table A1)
cl_dy(m1a_fb, fb_p); cl_dy(m1a_pr, pr_p)
cl_dy(m2_pr, pr_p);  cl_dy(m2_fb, fb_p); cl_dy(m4, panel)

## Block 5: Robustness models -------------------------------------------------------
## 5.1 Alternative post windows (Table A2)
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

## 5.2 Quasi-Poisson (headline models, Table A3)
q_pr  <- glm(n_attacks ~ post, family = quasipoisson, data = pr_p)
q_fb  <- glm(n_attacks ~ post, family = quasipoisson, data = fb_p)
q_s   <- glm(n_attacks ~ post, family = quasipoisson, data = fb_spoe)
q_red <- glm(n_attacks ~ post * I(target == "FPOE"), family = quasipoisson, data = fb_spoe)
q_4   <- glm(n_attacks ~ post * fb, family = quasipoisson, data = panel)

cl_d(q_pr, pr_p); cl_d(q_fb, fb_p); cl_d(q_s, fb_spoe)
cl_d(q_red, fb_spoe); cl_d(q_4, panel)

## 5.3 Campaign-trend and segmented models (Table A4) -----------------------------
## End-of-campaign confound, within-2017 check: post is the level shift at
## 30 Sep net of a linear daily trend; in the segmented models, since captures
## the change in slope after the revelation.
r_tr_pr  <- MASS::glm.nb(n_attacks ~ post + trend,         data = pr_p)
r_tr_fb  <- MASS::glm.nb(n_attacks ~ post + trend,         data = fb_p)
r_seg_pr <- MASS::glm.nb(n_attacks ~ trend + post + since, data = pr_p)
r_seg_fb <- MASS::glm.nb(n_attacks ~ trend + post + since, data = fb_p)

## Pre-period only: was the series already trending before the revelation?
r_pre_pr <- MASS::glm.nb(n_attacks ~ trend, data = pr_pre)
r_pre_fb <- MASS::glm.nb(n_attacks ~ trend, data = fb_pre)

cl_d(r_tr_pr, pr_p);    cl_d(r_tr_fb, fb_p)
cl_d(r_seg_pr, pr_p);   cl_d(r_seg_fb, fb_p)
cl_d(r_pre_pr, pr_pre); cl_d(r_pre_fb, fb_pre)

## 5.4 Historical press release benchmark 2002-2017 (Table A5) --------------------
## End-of-campaign confound, between-campaign check: do the final 15 days
## carry more attacks in campaigns without a revelation?
# Descriptive: daily attack rates per campaign, earlier vs final 15 days
print(hpanel %>% group_by(year, final15) %>%
        summarise(rate = sum(n) / n_distinct(dte), .groups = "drop") %>%
        pivot_wider(names_from = final15, values_from = rate) %>%
        mutate(ratio = `1` / `0`))

b_hist <- MASS::glm.nb(n ~ final15 + factor(year),                 data = hp_pre)
b_2017 <- MASS::glm.nb(n ~ final15 + final15:y2017 + factor(year), data = hpanel)
cl_d(b_hist, hp_pre)
cl_d(b_2017, hpanel)

## 5.5 Redirection placebo on Facebook (Table A6) ---------------------------------
## If the FPOE became more attackable for everyone, the other parties should
## show the same post x FPOE-target shift as the SPOE.
pl_other  <- MASS::glm.nb(n_attacks ~ post * fpoe_target, data = fb_other)
pl_triple <- MASS::glm.nb(n_attacks ~ post * fpoe_target * spoe_sender,
                          data = fb_p)
cl_d(pl_other, fb_other)
cl_d(pl_triple, fb_p)

## FPOE share of Facebook attacks, pre vs post, SPOE vs other senders
print(fb_p %>% mutate(grp = ifelse(spoe_sender == 1, "SPOE", "other")) %>%
        group_by(grp, post) %>%
        summarise(fpoe_share = sum(n_attacks[fpoe_target == 1]) / sum(n_attacks),
                  .groups = "drop"))

## 5.6 Exposure models: attacks per Facebook post (Table A7) ----------------------
## Sender-day counts with log(posts) as offset, so the post coefficient is the
## shift in attacks per post rather than in raw attack volume.
# Descriptive: posting volume and attacks per post, pre vs post
print(sd_panel %>% group_by(post) %>%
        summarise(posts_day = sum(posts) / n_distinct(date),
                  att_per_post = sum(n_att) / sum(posts), .groups = "drop"))
print(sd_panel %>% filter(sender == "SPOE") %>% group_by(post) %>%
        summarise(posts_day = sum(posts) / n_distinct(date),
                  att_per_post = sum(n_att) / sum(posts), .groups = "drop"))

e_all   <- MASS::glm.nb(n_att ~ post + offset(log(posts)), data = sd_panel)
e_spoe  <- MASS::glm.nb(n_att ~ post + offset(log(posts)), data = sd_spoe)
e_other <- MASS::glm.nb(n_att ~ post + offset(log(posts)), data = sd_other)
cl_d(e_all, sd_panel); cl_d(e_spoe, sd_spoe); cl_d(e_other, sd_other)

## Block 6: Decomposition - personalised vs party-level attacks (Chapter 4.5) -------
## All dyads, by channel
p_pers_pr <- MASS::glm.nb(n_personalised ~ post, data = pr_p)
p_part_pr <- MASS::glm.nb(n_partylevel   ~ post, data = pr_p)
p_pers_fb <- MASS::glm.nb(n_personalised ~ post, data = fb_p)
p_part_fb <- MASS::glm.nb(n_partylevel   ~ post, data = fb_p)

## SPOE as sender (Facebook)
p_pers_s <- MASS::glm.nb(n_personalised ~ post, data = fb_spoe)
p_part_s <- MASS::glm.nb(n_partylevel   ~ post, data = fb_spoe)

## SPOE as target (Facebook; press releases estimated for the text, both flat)
p_pers_t   <- MASS::glm.nb(n_personalised ~ post, data = fb_spoe_t)
p_part_t   <- MASS::glm.nb(n_partylevel   ~ post, data = fb_spoe_t)
p_pers_tpr <- MASS::glm.nb(n_personalised ~ post, data = pr_spoe_t)
p_part_tpr <- MASS::glm.nb(n_partylevel   ~ post, data = pr_spoe_t)

cl_d(p_pers_pr, pr_p);       cl_d(p_part_pr, pr_p)
cl_d(p_pers_fb, fb_p);       cl_d(p_part_fb, fb_p)
cl_d(p_pers_s, fb_spoe);     cl_d(p_part_s, fb_spoe)
cl_d(p_pers_t, fb_spoe_t);   cl_d(p_part_t, fb_spoe_t)
cl_d(p_pers_tpr, pr_spoe_t); cl_d(p_part_tpr, pr_spoe_t)

## Block 7: Publication tables -> Output/ --------------------------------------------
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
          "fpoe_target"      = "FPÖ target",
          "post:fpoe_target" = "Post × FPÖ target",
          "spoe_sender"      = "SPÖ sender",
          "post:spoe_sender" = "Post × SPÖ sender",
          "fpoe_target:spoe_sender"      = "FPÖ target × SPÖ sender",
          "post:fpoe_target:spoe_sender" = "Post × FPÖ target × SPÖ sender",
          "trend" = "Campaign day (trend)",
          "since" = "Days since revelation (slope change)",
          "final15" = "Final 15 days",
          "factor(year)2017" = "Campaign 2017",
          "final15:y2017" = "Final 15 days × 2017",
          "(Intercept)" = "Intercept")
GOF  <- data.frame(raw = "nobs", clean = "N", fmt = 0)
NOTE_D  <- "Negative binomial regressions, incidence rate ratios. Standard errors clustered by day (41 clusters). + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."
NOTE_DY <- "Negative binomial regressions, incidence rate ratios. Standard errors clustered by directed party dyad (30 clusters). + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."
NOTE_4  <- "Negative binomial regressions, incidence rate ratios. Standard errors clustered by day (41 clusters). Dependent variables are daily counts of attacks with at least one named individual as object (personalised) and attacks directed exclusively at party organisations (party-level). The SPÖ sender and SPÖ target columns cover Facebook. + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."
NOTE_A4 <- "Negative binomial regressions, incidence rate ratios. Campaign day counts days since 4 September 2017. Days since revelation is zero before 30 September and counts days afterwards, so its coefficient is the change in the daily trend after the revelation. The pre-period columns use only the 26 days before the revelation. Standard errors clustered by day (41 clusters, 26 in the pre-period columns). + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."
NOTE_A5 <- "Negative binomial regressions, incidence rate ratios. Press release attacks among SPÖ, ÖVP, FPÖ and Greens, the four parties observed in all five campaigns, in dyad-day panels over the 41 days before each National Council election (2002, 2006, 2008, 2013, 2017). Campaign fixed effects included. Standard errors clustered by campaign day (164 clusters in the first column, 205 in the second). + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."
NOTE_A6 <- "Negative binomial regressions, incidence rate ratios. Facebook dyad-day panel. The first column excludes dyads with the SPÖ as sender. Standard errors clustered by day (41 clusters). + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."
NOTE_A7 <- "Negative binomial regressions of daily Facebook attack counts per sending party, with the log of the party's posts on that day as offset. Coefficients are incidence rate ratios for attacks per post. Standard errors clustered by day (41 clusters). + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001."

## Table 2 (H1, H2)
modelsummary(list("PR" = m1a_pr, "FB" = m1a_fb,
                  "PR ctrl" = m1b_pr, "FB ctrl" = m1b_fb,
                  "PR interaction" = m2_pr, "FB interaction" = m2_fb,
                  "PR SPÖ-target" = m2s_pr, "FB SPÖ-target" = m2s_fb),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_D,
             title = "Table 2: Attack volumes and SPÖ targeting after the revelation",
             output = file.path(DIR_OUT, "tab2_H1_H2.docx"))

## Table 3 (H3, H4)
modelsummary(list("PR SPÖ sender" = m3a_pr, "FB SPÖ sender" = m3a_fb,
                  "PR redirection" = m3b_pr, "FB redirection" = m3b_fb,
                  "Pooled" = m4, "Target SPÖ" = m4_t, "Sender SPÖ" = m4_s),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_D,
             title = "Table 3: The SPÖ's response and channel differences",
             output = file.path(DIR_OUT, "tab3_H3_H4.docx"))

## Table 4 (decomposition, Chapter 4.5)
modelsummary(list("Pers. PR" = p_pers_pr, "Party PR" = p_part_pr,
                  "Pers. FB" = p_pers_fb, "Party FB" = p_part_fb,
                  "Pers. SPÖ sender" = p_pers_s, "Party SPÖ sender" = p_part_s,
                  "Pers. SPÖ target" = p_pers_t, "Party SPÖ target" = p_part_t),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_4,
             title = "Table 4: Personalised versus party-level attacks",
             output = file.path(DIR_OUT, "tab4_decomposition.docx"))

## Table A1 (dyad-clustered SEs)
modelsummary(list("PR" = m1a_pr, "FB" = m1a_fb,
                  "PR ctrl" = m1b_pr, "FB ctrl" = m1b_fb,
                  "PR interaction" = m2_pr, "FB interaction" = m2_fb,
                  "Pooled" = m4),
             vcov = ~ dyad, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_DY,
             title = "Table A1: Full-sample models with dyad-clustered standard errors",
             output = file.path(DIR_OUT, "tabA1_dyad_clustered.docx"))

## Table A2 (alternative windows)
modelsummary(list("FB 1 Oct" = r_fb_o1, "FB 2 Oct" = r_fb_o2,
                  "SPÖ FB 1 Oct" = r_s_o1, "SPÖ FB 2 Oct" = r_s_o2,
                  "Redirect 1 Oct" = r_red_o1, "Redirect 2 Oct" = r_red_o2,
                  "FB no trans." = r_fb_dt, "Redirect no trans." = r_red_dt),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_D,
             title = "Table A2: Alternative post-revelation windows",
             output = file.path(DIR_OUT, "tabA2_alt_windows.docx"))

## Table A3 (quasi-Poisson)
modelsummary(list("QP PR" = q_pr, "QP FB" = q_fb, "QP SPÖ FB" = q_s,
                  "QP Redirect" = q_red, "QP Pooled" = q_4),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF,
             notes = "Quasi-Poisson regressions, incidence rate ratios. Standard errors clustered by day. + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001.",
             title = "Table A3: Quasi-Poisson specifications",
             output = file.path(DIR_OUT, "tabA3_quasipoisson.docx"))

## Table A4 (campaign trend and segmented models, 5.3)
modelsummary(list("PR trend" = r_tr_pr, "FB trend" = r_tr_fb,
                  "PR segmented" = r_seg_pr, "FB segmented" = r_seg_fb,
                  "PR pre-period" = r_pre_pr, "FB pre-period" = r_pre_fb),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_A4,
             title = "Table A4: Campaign-trend and segmented models",
             output = file.path(DIR_OUT, "tabA4_trend.docx"))

## Table A5 (historical benchmark, 5.4)
modelsummary(list("2002-2013" = b_hist, "All campaigns" = b_2017),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_A5,
             title = "Table A5: Historical press release benchmark",
             output = file.path(DIR_OUT, "tabA5_benchmark.docx"))

## Table A6 (redirection placebo, 5.5)
modelsummary(list("Other senders" = pl_other, "Triple interaction" = pl_triple),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_A6,
             title = "Table A6: Redirection placebo on Facebook",
             output = file.path(DIR_OUT, "tabA6_placebo.docx"))

## Table A7 (exposure models, 5.6)
modelsummary(list("All senders" = e_all, "SPÖ" = e_spoe, "Other parties" = e_other),
             vcov = ~ date, exponentiate = TRUE, stars = TRUE,
             coef_map = COEF, gof_map = GOF, notes = NOTE_A7,
             title = "Table A7: Attacks per Facebook post (exposure models)",
             output = file.path(DIR_OUT, "tabA7_exposure.docx"))

## Table A8 (summary statistics)
datasummary(n_attacks + n_personalised + n_partylevel + post + lr_dist +
              coalition + size_ratio ~ Factor(channel) * (Mean + SD + Min + Max),
            data = panel,
            title = "Table A8: Summary statistics by channel",
            output = file.path(DIR_OUT, "tabA8_summary_stats.docx"))

## Block 8: Figure 2 and replication info ---------------------------------------------
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
