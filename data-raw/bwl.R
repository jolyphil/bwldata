library(dplyr)
library(readr)


# Import results from Bundeswahlleiter ------------------------------------

bwl_results_raw <- read_csv2("data-raw/bundeswahlleiter_kerg.csv", 
                             skip = 5,
                             col_names = FALSE)

bwl_results <- bwl_results_raw %>%
  select(wkrnum = X1,
         wkrname = X2,
         bundesland = X3,
         wahlberechtigte = X4,
         waehlende = X8) %>%
  filter(!is.na(bundesland) & bundesland != 99) %>%
  mutate(wb = (waehlende / wahlberechtigte) * 100,
         wkrnum = as.numeric(wkrnum),
         bundesland = as.numeric(bundesland),
         bundesland = case_when(bundesland == 1 ~ "SH",
                                bundesland == 2 ~ "HH",
                                bundesland == 3 ~ "NI",
                                bundesland == 4 ~ "HB",
                                bundesland == 5 ~ "NW",
                                bundesland == 6 ~ "HE",
                                bundesland == 7 ~ "RP",
                                bundesland == 8 ~ "BW",
                                bundesland == 9 ~ "BY",
                                bundesland == 10 ~ "SL",
                                bundesland == 11 ~ "BE",
                                bundesland == 12 ~ "BB",
                                bundesland == 13 ~ "MV",
                                bundesland == 14 ~ "SN",
                                bundesland == 15 ~ "ST",
                                bundesland == 16 ~ "TH"),
         bundesland = factor(bundesland)) %>%
  select(bundesland,
         wkrnum,
         wkrname,
         wb)


# Import structure data ---------------------------------------------------

bwl_str_raw <- read_csv2("data-raw/bundeswahlleiter_strukturdaten.csv", 
                             skip = 8)

bwl_str <- bwl_str_raw %>%
  select(wkrnum = `Wahlkreis-Nr.`,
         bip = `Bruttoinlandsprodukt 2018 (EUR je EW)`,
         einkommen = `Verfügbares Einkommen der privaten Haushalte 2018 (EUR je EW)`,
         arbeitslos = `Arbeitslosenquote Februar 2021 - insgesamt`) %>%
  mutate(wkrnum = as.numeric(wkrnum),
         bip = bip / 1000,
         einkommen = einkommen / 1000)
  

# Merge data --------------------------------------------------------------

bwl <- bwl_results %>%
  left_join(bwl_str)

# Save data ---------------------------------------------------------------

saveRDS(bwl, file = "data/bwl.rds")
