###############################################
###############################################

## API-Key einlesen
library(jsonlite)

api <- read_json("chatgpt/jonas_openai_api.json")
Sys.setenv(OPENAI_API_KEY = api$key)

## chatgpt-Paket laden
library(chatgpt)

###############################################
###############################################

library(tidyverse)
library(margins)

tib_fose <- readRDS("chatgpt/tib_fose.RDS")

data <-
  tib_fose %>%
  transmute(
    akademiker_mutter = case_when(
      SD13 == "(Fach-)Hochschulabschluss/MBA" ~ 1, SD13 != "(Fach-)Hochschulabschluss/MBA" ~ 0,
      is.na(SD13) ~ NA_real_,
      SD13 == "Unbekannt" ~ NA_real_,
      TRUE ~ 0
    ),
    akademiker_vater = case_when(
      SD14 == "(Fach-)Hochschulabschluss/MBA" ~ 1, SD14 != "(Fach-)Hochschulabschluss/MBA" ~ 0,
      is.na(SD14) ~ NA_real_,
      SD14 == "Unbekannt" ~ NA_real_,
      TRUE ~ 0
    ),
    akademiker = case_when(akademiker_vater == 1 | akademiker_mutter == 1 ~ 1,
                           akademiker_vater == 0 & akademiker_mutter == 0 ~ 0,
                           TRUE ~ NA_real_),
    finanzierung = case_when(
      RB01_03 == 1 | RB01_07 == 1 ~ 1,
      is.na(RB01_03) & is.na(RB01_07) ~ NA_real_,
      TRUE ~ 0
    ),
    geschlecht = case_when(
      SD01 == "Männlich" ~ 0, SD01 == "Weiblich" ~ 1,
      TRUE ~ NA_real_
    ),
    verdienst = ET11_01,
    suchart = case_when(
      as.numeric(SS04) %in% c(6:11) ~ 1,
      is.na(SS04) ~ NA_real_,
      !as.numeric(SS04) %in% c(6:11) ~ 0
    ),
    sicherer_job = case_when(
      ET03 %in% c("Befristet", "Bin/war selbstständig") ~ 0,
      is.na(ET03) ~ NA_real_,
      ET03 == "Unbefristet" ~ 1
    )
  )

data <-
  data |>
  mutate(geschlecht = factor(geschlecht,
                             levels = c("0", "1"),
                             labels = c("männlich", "weiblich")))

table(data$akademiker, data$sicherer_job, useNA = "ifany")

fit_log <-
  glm(
    formula = sicherer_job ~ . + akademiker_vater*akademiker_mutter,
    family = binomial(link = "logit"),
    data = data
  )

summary(fit_log)


fit_lm <-
  lm(
    formula = sicherer_job ~ . + akademiker_vater*akademiker_mutter,
    # family = binomial(link = "logit"), @ Das muss hier raus!
    data = data
  )

summary(fit_lm)

fit_log2 <-
  data %>%
  select(-suchart) %>%
  glm(
    formula = sicherer_job ~ . + akademiker_vater*akademiker_mutter,
    family = binomial(link = "logit"),
    data = .
  )

