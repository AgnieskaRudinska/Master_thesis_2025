# ------------------------------------------------------------------------------
# This code used to unify data to the same format
# ------------------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(readxl)
library(openxlsx)

# dir
dir1 <- "../data/"
dir2 <- "../intermediate/"

# import data
dt <- read_excel(paste0(dir1, "Vilniaus projekto DB.xlsx"))

# manually deleting values that were deleted by reaserchers
dt[dt$Objektas == "Katedra" &
     dt$`Kapo numeris` == "9" &
     dt$Tyr. == "Reitsema", c("d13C‰ (kaulas)", "d15N‰ (kaulas)")] <- NA
dt[dt$Objektas == "Katedra" &
     dt$`Kapo numeris` == "12" &
     is.na(dt$Tyr.), c("d13C‰ (kaulas)", "d15N‰ (kaulas)")] <- NA
dt[dt$Objektas == "Katedra" &
     dt$`Kapo numeris` == "12" &
     dt$Kaulas == "dentine" &
     dt$Tyr. == "Schotten, 2015",
   c("d13C‰ (dentinas)", "d15N‰ (dentinas)")] <- NA
dt[dt$Objektas == "Katedra" &
     dt$`Kapo numeris` == "17" &
     dt$Tyr. == "Reitsema", c("d13C‰ (kaulas)", "d15N‰ (kaulas)")] <- NA
dt[dt$Objektas == "Katedra" &
     dt$`Kapo numeris` == "18" &
     dt$Tyr. == "Reitsema", c("d13C‰ (kaulas)", "d15N‰ (kaulas)")] <- NA
dt[dt$Objektas == "Katedra" &
     dt$`Kapo numeris` == "19" &
     dt$Tyr. == "Reitsema", c("d13C‰ (kaulas)", "d15N‰ (kaulas)")] <- NA
dt[dt$Objektas == "Katedra" &
     dt$`Kapo numeris` == "20" &
     dt$Kaulas == "dentine" &
     dt$Tyr. == "Schotten, 2015",
   c("d13C‰ (dentinas)", "d15N‰ (dentinas)")] <- NA
dt[dt$Objektas == "Katedra" &
     dt$`Kapo numeris` == "25" &
     dt$Tyr. == "Reitsema", c("d13C‰ (kaulas)", "d15N‰ (kaulas)")] <- NA


# delete Suncovo data, because they do not allow us to use it
dt <- dt %>%
  filter((Tyr. != "Suncovas" | is.na(Tyr.))) %>%
  mutate(Periodas = case_when(
    Periodas == "1" ~ "13-15 a.",
    Periodas == "2" ~ "16 a.",
    Periodas == "3" ~ "17-18 a."
  ))

# add id column
dt$combined <- paste(dt$`Kapo numeris`, dt$Objektas, sep = "_")
dt$ID <- as.numeric(factor(dt$combined),
  levels = unique(dt$combined)
)
dt$combined <- NULL

# fixes
dt$Amžius <- gsub("±", "+/-", dt$Amžius)
dt$Amžius <- gsub("\\+\\/?-", "+/-", dt$Amžius)
dt$Amžius <- gsub("\\s+|\\.", "", dt$Amžius)
dt$Amžius <- gsub("–", "-", dt$Amžius)
dt$Amžius <- gsub(",", ".", dt$Amžius)
dt$Amžius <- gsub("\\?", "", dt$Amžius)

# unifying age column
# possible patterns
# number-number
pattern1 <- "\\d+\\-\\d+"
# number-number men
pattern2 <- "^\\d+-\\d+men$"
# number m+/-number men
pattern3 <- "^\\d+m\\+/-\\d+men$"
# number+/-number men
pattern4 <- "^\\d+\\+/-\\d+men$"
# number men
pattern5 <- "^\\d+men$"
# number men +/- number men
pattern6 <- "\\d+men\\+/-\\d+men"
# number
pattern7 <- "^\\d+$"

# calculate age ----------------------------------------------------------------

for (row in seq(1, nrow(dt))) {
  if (grepl(pattern1, dt$Amžius[row]) && !grepl("men", dt$Amžius[row])) {
    dt[row, "Amžius"] <- gsub("m|~|?", "", dt$Amžius[row])
    interval <- dt$Amžius[row]
    numbers <- as.numeric(strsplit(interval, "-")[[1]])
    dt[row, "Average age"] <- sum(numbers) / length(numbers)
    dt[row, "Age.check"] <- ifelse(numbers[2] - numbers[1] > 5,
      ">5", "<5"
    )
    dt[row, "Age.check"] <- ifelse(numbers[2] - numbers[1] > 10,
      ">10", dt[row, "Age.check"]
    )
    dt[row, "Lower interval"] <- numbers[1]
    dt[row, "Higher interval"] <- numbers[2]
  } else if (grepl(pattern2, dt$Amžius[row]) ||
               grepl(pattern4, dt$Amžius[row]) ||
               grepl(pattern6, dt$Amžius[row])) {
    dt[row, "Average age"] <- round(as.numeric(gsub(
      "(\\d+).*", "\\1",
      dt$Amžius[row]
    )) / 12, 2)

    if (grepl(pattern2, dt$Amžius[row])) {
      numbers <- unlist(regmatches(
        dt$Amžius[row],
        gregexpr("\\d+", dt$Amžius[row])
      ))
      dt[row, "Lower interval"] <- round(as.numeric(numbers[1]) / 12, 2)
      dt[row, "Higher interval"] <- round(as.numeric(numbers[2]) / 12, 2)
    } else {
      numbers <- unlist(regmatches(
        dt$Amžius[row],
        gregexpr("\\d+", dt$Amžius[row])
      ))
      if (as.numeric(numbers[1]) > as.numeric(numbers[2])) {
        dt[row, "Lower interval"] <- round(as.numeric(numbers[1]) / 12, 2) -
          round(as.numeric(numbers[2]) / 12, 2)
        dt[row, "Higher interval"] <- round(as.numeric(numbers[1]) / 12, 2) +
          round(as.numeric(numbers[2]) / 12, 2)
      } else {
        dt[row, "Lower interval"] <- round(as.numeric(numbers[1]) / 12, 2)
        dt[row, "Higher interval"] <- round(as.numeric(numbers[1]) / 12, 2) +
          round(as.numeric(numbers[2]) / 12, 2)
      }
    }
  } else if (grepl(pattern3, dt$Amžius[row])) {
    dt[row, "Average age"] <-
      as.numeric(gsub("(\\d+).*", "\\1", dt$Amžius[row]))
    numbers <- unlist(regmatches(
      dt$Amžius[row],
      gregexpr("\\d+", dt$Amžius[row])
    ))
    if (as.numeric(numbers[1]) > (as.numeric(numbers[2]) / 12)) {
      dt[row, "Lower interval"] <- as.numeric(numbers[1]) -
        round(as.numeric(numbers[2]) / 12, 2)
      dt[row, "Higher interval"] <- as.numeric(numbers[1]) +
        round(as.numeric(numbers[2]) / 12, 2)
    } else {
      dt[row, "Lower interval"] <- as.numeric(numbers[1])
      dt[row, "Higher interval"] <- as.numeric(numbers[1]) +
        round(as.numeric(numbers[2]) / 12, 2)
    }
  } else if (grepl(pattern5, dt$Amžius[row])) {
    dt[row, "Average age"] <- round(as.numeric(gsub(
      "men", "",
      dt$Amžius[row]
    )) / 12, 2)
  } else if (grepl(pattern7, dt$Amžius[row])) {
    dt[row, "Average age"] <- as.numeric(dt$Amžius[row])
  }
}

# manually calculating exceptions
dt$`Average age`[!is.na(dt$Amžius) & dt$Amžius == "7m+/-24"] <- 7
dt$`Lower interval`[!is.na(dt$Amžius) & dt$Amžius == "7m+/-24"] <- 5
dt$`Higher interval`[!is.na(dt$Amžius) & dt$Amžius == "7m+/-24"] <- 9

dt$`Average age`[!is.na(dt$Amžius) & dt$Amžius == "9+/-2"] <- 9
dt$`Lower interval`[!is.na(dt$Amžius) & dt$Amžius == "9+/-2"] <- 7
dt$`Higher interval`[!is.na(dt$Amžius) & dt$Amžius == "9+/-2"] <- 11

dt$`Average age`[!is.na(dt$Amžius) & dt$Amžius == "12m+/-/30men"] <- 12
dt$`Lower interval`[!is.na(dt$Amžius) & dt$Amžius == "12m+/-/30men"] <- 9.5
dt$`Higher interval`[!is.na(dt$Amžius) & dt$Amžius == "12m+/-/30men"] <- 14.5

dt <- dt %>%
  mutate(
    "Age group" =
      case_when(
        `Average age` < 6 ~ "0-5",
        `Average age` >= 6 & `Average age` < 11 ~ "6-10",
        `Average age` >= 11 & `Average age` < 16 ~ "11-15",
        `Average age` >= 16 & `Average age` < 21 ~ "16-20",
        `Average age` >= 21 & `Average age` < 31 ~ "21-30",
        `Average age` >= 31 & `Average age` < 41 ~ "31-40",
        `Average age` >= 41 & `Average age` < 51 ~ "41-50",
        `Average age` >= 51 & `Average age` < 61 ~ "51-60",
        `Average age` >= 61 & `Average age` < 71 ~ "61-70",
        `Average age` >= 71 & `Average age` < 81 ~ "71-80"
      )
  )

# how many intervals are >5, >10?
nrow(dt %>% filter(Age.check == ">5"))
nrow(dt %>% filter(Age.check == ">10"))

# amzius check
unique(dt[is.na(dt$`Average age`), "Amžius"])

# lytis
unique(dt$Lytis)
table(dt$Lytis)

# fix lytis v? - v. m? - m, ? - n if child, NA if not
# ar vaikai tai <18m.?
dt <- dt %>%
  mutate(Lytis = ifelse(!is.na(dt$Lytis) & Lytis == "?" &
                          !is.na(`Average age`) &
                          `Average age` < 18, "n", Lytis)) %>%
  mutate(Lytis = ifelse(!is.na(dt$Lytis) & Lytis == "?", NA, Lytis)) %>%
  mutate(Lytis = sub("\\?", "", Lytis)) %>%
  mutate(Lytis = ifelse(dt$Lytis == "v (arch mot)", "v", Lytis))

# SES, Konfesija
dt$SES <- sub("\\(\\?\\)", "", dt$SES)
dt$Konfesija <- sub("\\(\\?\\)", "", dt$Konfesija)

dt[
  dt$Objektas %in% c("Ciurlionio g. 3 2005", "Ciurlionio g. 3 2007-8") &
    dt$Konfesija == "katalikai arba protestantai",
  "Konfesija"
] <- "katalikai"

dt[dt$Objektas == "Bernardinu bažnycia" &
     dt$SES == "dvasininkai; elitas; miestieciai", "SES"] <-
  "dvasininkai; elitas"
dt[dt$Objektas %in% c(
  "Latako g.", "Maironio g. 14 (Soboras)",
  "Pranciškonu bažn.'23", "Prie Pranciškonu'93",
  "Pranciškonu bažn.'97"
) &
  dt$SES == "miestieciai; dvasininkai", "SES"] <- "miestieciai"
dt[dt$Objektas == "Bazilijonai" &
     dt$SES == "dvasininkai", "SES"] <- "dvasininkai; elitas"

# translate names
dt <- dt %>%
  rename(
    `Period` = Periodas,
    Gender = Lytis,
    Religion = Konfesija,
    Age = Amžius
  ) %>%
  mutate(
    `Period` = case_when(
      `Period` == "13-15 a." ~ "13-15 th.",
      `Period` == "16 a." ~ "16 th.",
      `Period` == "17-18 a." ~ "17-18 th."
    ),
    Gender = case_when(
      Gender == "v" ~ "Male",
      Gender == "m" ~ "Female",
      Gender == "n" ~ "Unknown"
    ),
    Religion = case_when(
      Religion == "katalikai" ~ "Catholic",
      Religion == "staciatikiai" ~ "Orthodox",
      Religion == "protestantai" ~ "Protestants"
    ),
    SES = case_when(
      SES == "dvasininkai; elitas" ~ "Elite and clergy",
      SES == "miestieciai" ~ "Commoners"
    )
  )

# Rename sites
dt <- dt %>%
  rename(`Site` = Objektas) %>%
  mutate(Site = case_when(
    Site == "Katedra" ~ "Vilnius Cathedral",
    Site == "Aguonu g. 10 (2006)" ~ "Aguonų str. 10",
    Site == "Aguonu g. 10 (2007)" ~ "Aguonų str. 10",
    Site == "Polocko g." ~ "Polocko str. ",
    Site == "Latako g." ~ "Latako str.",
    Site == "Subaciaus g. 7" ~ "Subačiaus str. 7",
    Site == "Bokšto g. 6" ~ "Bokšto str. 6",
    Site == "Pranciškonu bažn.'97" ~
      "Franciscan Church of the Assumption of the Blessed Virgin Maria",
    Site == "Prie Pranciškonu'93" ~
      "Franciscan Church of the Assumption of the Blessed Virgin Maria",
    Site == "Pranciškonu bažn.'23" ~
      "Franciscan Church of the Assumption of the Blessed Virgin Maria",
    Site == "Kriveikiškiai" ~ "Kriveikiškiai",
    Site == "Verkiai" ~ "Verkiai",
    Site == "Maironio g. 14 (Soboras)" ~ "Cathedral of the Theotokos",
    Site == "Bokšto g. 15" ~ "Bokšto str. 15",
    Site == "Liejyklos g. 4" ~ "Liejyklos str. 4",
    Site == "Subaciaus g. 41" ~ "Subačiaus str. 41",
    Site == "Aguonu g. 17" ~ "Aguonų str. 17",
    Site == "Šv. Pilypo ir Jokubo bažnycia" ~
      "Church of the Apostles Philip and Jacob",
    Site == "Ciurlionio g. 3 2005" ~ "Čiurlionio str. 3",
    Site == "Ciurlionio g. 3 2007-8" ~ "Čiurlionio str. 3",
    Site == "Bernardinu bažnycia" ~ "Church of St. Francis and St. Bernard",
    Site == "Bazilijonai" ~ "Holy Trinity Uniate Church",
    Site == "Šv. Stepono medžiaga 2022" ~ "Church of St. Stephen",
    Site == "Petro ir Povilo bažn. 2023" ~ "Church of St. Peter and St. Paul"
  ))


# save data
write.xlsx(dt, file = paste0(dir2, "01_unified_variables.xlsx"))
