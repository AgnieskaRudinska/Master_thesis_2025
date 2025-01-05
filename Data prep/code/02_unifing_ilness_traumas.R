#-------------------------------------------------------------------------------
# This code unifies factors to be used in analysis
#-------------------------------------------------------------------------------
rm(list = ls())

library(dplyr)
library(readxl)
library(openxlsx)

# dir
dir1 <- "../intermediate/"

dt <- read_excel(paste0(dir1, "01_unified_variables.xlsx"))


# Kaulai femur and dentine fix
mityba <- c(
  "d13C‰ (kaulas)", "d13C‰ (dentinas)",
  "d15N‰ (kaulas)", "d15N‰ (dentinas)"
)

for (col in mityba) {
  dt[[col]] <- sapply(dt[[col]], function(x) {
    x <- gsub(",", ".", x, fixed = TRUE)
    gsub("\\s+", "", x)
  })
}

dt <- dt %>%
  mutate(`d13C‰ (kaulas)` = ifelse(`d13C‰ (kaulas)` %in%
                                     c("AB", "Nera", "NERA",
                                       "A", "kartoti", "AS", "-"),
                                   NA, `d13C‰ (kaulas)`),
         `d13C‰ (dentinas)` = ifelse(`d13C‰ (dentinas)`
                                     %in% c("AB", "Nera", "NERA",
                                            "A", "kartoti", "AS", "-"),
                                     NA, `d13C‰ (dentinas)`),
         `d15N‰ (kaulas)` = ifelse(`d15N‰ (kaulas)`
                                   %in% c("AB", "Nera", "NERA",
                                          "A", "kartoti", "AS", "-"),
                                   NA, `d15N‰ (kaulas)`),
         `d15N‰ (dentinas)` = ifelse(`d15N‰ (dentinas)`
                                     %in% c("AB", "Nera", "NERA",
                                            "A", "kartoti", "AS", "-"),
                                     NA, `d15N‰ (dentinas)`))

dt[, mityba] <- lapply(dt[, mityba], function(x) as.numeric(x))
dt <- dt %>%
  rename(`d13C‰ (bone)` = `d13C‰ (kaulas)`,
         `d13C‰ (dentine)` = `d13C‰ (dentinas)`,
         `d15N‰ (bone)` = `d15N‰ (kaulas)`,
         `d15N‰ (dentine)` = `d15N‰ (dentinas)`)

# Cribra orbitalia
dt <- dt %>%
  mutate(`Cribra 1` = case_when(
    `Cribra orbitalia` %in%
      c("cribra1", "cribra 1") ~ 1,
    `Cribra orbitalia` %in%
      c("0", "cribra2", "Cribra2", "cribra3") ~ 0
  )) %>%
  mutate(`Cribra 2` = case_when(
    `Cribra orbitalia` %in%
      c("cribra2", "Cribra2") ~ 1,
    `Cribra orbitalia` %in%
      c("0", "cribra1", "cribra 1", "cribra3") ~ 0
  )) %>%
  mutate(`Cribra 3` = case_when(
    `Cribra orbitalia` %in% "cribra3" ~ 1,
    `Cribra orbitalia` %in%
      c("0", "cribra2", "Cribra2", "cribra1", "cribra 1") ~ 0
  )) %>%
  mutate(`Cribra orbitalia` = case_when(
    `Cribra orbitalia` %in%
      c("cribra1", "cribra 1", "cribra2", "Cribra2", "cribra3") ~ 1,
    `Cribra orbitalia` == "0" ~ 0
  )) %>%
  mutate(`Cribra orbitalia stages` = case_when(`Cribra 1` == 1 ~ 1,
                                               `Cribra 2` == 1 ~ 2,
                                               `Cribra 3` == 1 ~ 3,
                                               TRUE ~ `Cribra orbitalia`))

# (Osteo)periostitas
dt <- dt %>%
  mutate(`(Osteo)periostitas` = ifelse(`(Osteo)periostitas` != "0" &
                                         !is.na(`(Osteo)periostitas`),
                                       1, `(Osteo)periostitas`)) %>%
  mutate(`periostitas 1` = case_when(`Periostitas1-2` %in% c("1") ~ 1,
                                     `Periostitas1-2` %in% c("0", "2") ~ 0)) %>%
  mutate(`periostitas 2` = case_when(`Periostitas1-2` == "2" ~ 1,
                                     `Periostitas1-2` %in% c("1", "0") ~ 0)) %>%
  rename(`(Osteo)periostitis` = `(Osteo)periostitas`,
         `Periostitis 1` = `periostitas 1`,
         `Periostitis 2` = `periostitas 2`,
         `(Osteo)periostitis stages` = `Periostitas1-2`)

# TB
dt <- dt %>%
  mutate(TB = ifelse(`TB` != "0" & !is.na(`TB`), 1, TB))


# teeth
teeth_cols_cat <- c("Calculus", "LEH")
teeth_cols_con <- c("Caries Freq", "AMTL Freq", "DMI", "ICE", "Abscess Freq",
                    "av M1 wear", "av M2 wear")
dt <- dt %>%
  mutate(across(all_of(teeth_cols_con), ~ as.numeric(gsub(",", ".", .)))) %>%
  mutate(across(all_of(teeth_cols_cat), ~ as.factor(gsub(",", ".", .))))

# Injuries
dt <- dt %>%
  rename(Kaukole_traumos = `Kaukole...48`) %>%
  mutate(Šonkauliai = ifelse(Šonkauliai != "0" &
                               !is.na(Šonkauliai), 1, Šonkauliai)) %>%
  mutate(Kaukole_traumos = ifelse(Kaukole_traumos != "0" &
                                    !is.na(Kaukole_traumos),
                                  1, Kaukole_traumos)) %>%
  mutate(Stuburas = ifelse(Stuburas != "0" & !is.na(Stuburas),
                           1, Stuburas)) %>%
  mutate(Raktikaulis = ifelse(Raktikaulis != "0" &
                                !is.na(Raktikaulis), 1, Raktikaulis)) %>%
  mutate(Žastikaulis = ifelse(Žastikaulis != "0" &
                                !is.na(Žastikaulis), 1, Žastikaulis)) %>%
  mutate(Dilbis = ifelse(Dilbis != "0" & !is.na(Dilbis), 1, Dilbis)) %>%
  mutate(Dubenkaulis = ifelse(Dubenkaulis != "0" &
                                !is.na(Dubenkaulis), 1, Dubenkaulis)) %>%
  mutate(Šlaunikaulis = ifelse(Šlaunikaulis != "0" &
                                 !is.na(Šlaunikaulis), 1, Šlaunikaulis)) %>%
  mutate(Blauzda = ifelse(Blauzda != "0" & !is.na(Blauzda), 1, Blauzda)) %>%
  mutate(Injuries = ifelse(`AM/PM` != "0" & !is.na(`AM/PM`), 1, `AM/PM`)) %>%
  mutate(`K humerus` = gsub("\\*", "", `K humerus`)) %>%
  mutate(`D+K humerus avg.` =
           (as.numeric(`D humerus`) + as.numeric(`K humerus`)) / 2) %>%
  mutate(`D+K femur avg.` =
           (as.numeric(`D femur`) + as.numeric(`K femur`)) / 2) %>%
  rename(Ribs = Šonkauliai,
         Skull = Kaukole_traumos,
         Spine = Stuburas,
         Clavicle = Raktikaulis,
         Humerus = Žastikaulis,
         Forearm = Dilbis,
         `Hip bone` = Dubenkaulis,
         Femur = Šlaunikaulis,
         `Tibia + Fibula` = Blauzda)

# LEH
dt <- dt %>%
  mutate(`LEH 1` = case_when(
    `LEH` %in% c("1") ~ "1",
    `LEH` %in% c("0", "2") ~ "0"
  )) %>%
  mutate(`LEH 2` = case_when(
    `LEH` == "2" ~ "1",
    `LEH` %in% c("1", "0") ~ "0"
  )) %>%
  mutate(`LEH stages` = LEH) %>%
  mutate(LEH = case_when(LEH %in% c("1", "2") ~ "1",
                         TRUE ~ LEH))

dt <- dt %>%
  rename(Confession = Religion,
         Sex = Gender)

# summary with number of missing data
dt_for_table <- dt %>% filter(Site != "Kriveikiškiai")

used_drivers <- c("Cribra orbitalia", "TB", "(Osteo)periostitis", "LEH",
                  "Period", "Sex", "SES", "Confession",
                  "d13C‰ (bone)", "d13C‰ (dentine)", "d15N‰ (bone)",
                  "d15N‰ (dentine)", "D+K humerus avg.", "D+K femur avg.",
                  "Caries Freq", "AMTL Freq",
                  "Average age")


summary_list <- list()
for (var_name in used_drivers) {
  if (var_name == "Period") next

  summary_dt <- unique(na.omit(dt_for_table[, c("ID", var_name, "Period")]))
  n_non_na_total <- sum(!is.na(summary_dt[[var_name]]))
  n_na_total <- sum(is.na(summary_dt[[var_name]]))

  counts_by_period <- aggregate(summary_dt[[var_name]],
                                by = list(Period = summary_dt$Period),
                                FUN = function(x)
                                  c(N_nonNA = sum(!is.na(x)),
                                    N_NA = sum(is.na(x))))

  counts_by_period <- do.call(data.frame, counts_by_period)
  summary_list[[var_name]] <- list(
    Total = data.frame(Variable = var_name, N_nonNA = n_non_na_total,
                       N_NA = n_na_total),
    By_Period = counts_by_period
  )
}
total_summary <- do.call(rbind, lapply(summary_list, `[[`, "Total"))
by_period_summary <- lapply(summary_list, `[[`, "By_Period")

# save data
write.xlsx(dt, file = paste0(dir1, "02_unified_factors.xlsx"))
