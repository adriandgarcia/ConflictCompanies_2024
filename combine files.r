setwd("S:/Ignites/Editorial/Today's Ignites 2024/Infographics/Conflict Companies")

library(tidyverse)
library(lubridate)

data_path <- "data"   # path to the data
files <- dir(data_path, pattern = "\\.csv$", full.names = TRUE)

READ <- function(FILE) {
  read_csv(FILE,
           trim_ws = TRUE,
           guess_max = nrow(read_csv(FILE)) -12,
           n_max = nrow(read_csv(FILE)) -12,
           col_types = cols(
           )) %>% 
    rename_all(make.names) %>% 
    mutate(., filename = FILE) 
}

data <- files %>%
  map(READ) %>%    # read in all the files individually, using
  # the function READ from above
  reduce(full_join) # reduce with full_join into one dataframe



write_csv(data, "data_combined.csv")


results <- data %>% 
  mutate(Stock = str_replace(filename, "data/MD.", ""),
         Stock = str_replace(Stock, "(_..csv)|(_ADR.csv)", "")
           ) %>% 
  select(Name, Stock, FundId, everything()) %>% 
  filter((Portfolio..Owner.Type == "Mutual Fund" & Domicile == "United States") |
           !is.na(FundId) & is.na(Portfolio..Owner.Type) & Domicile == "United States")

results <- results %>% 
  filter(Stock != "BAESystems_BAESY",
         Fund.Legal.Structure %in% c("Closed Ended Investment Company", "Open Ended Investment Company") | is.na(Fund.Legal.Structure))

write_csv(results, "results.csv")

fundid_present <- results %>% 
  group_by(Portfolio..Owner.Type, FundId) %>% 
  mutate(Blank = case_when(
    is.na(FundId) ~ "blank",
    !is.na(FundId) ~ "present"
  )) %>% 
  group_by(Domicile, Portfolio..Owner.Type, Blank) %>% 
  count()

fundid_blank <- results %>% 
  filter(is.na(FundId))

fundids <- results %>% 
  filter(!is.na(FundId)) %>% 
  select(FundId) %>% 
  distinct() %>% 
  write_csv(., "fundids2.csv")

share_data <- results %>% 
  select(1:82) %>% 
  filter(!is.na(FundId)) %>% 
  distinct()


data_path <- "matches"   # path to the data
files <- dir(data_path, pattern = "\\.csv$", full.names = TRUE)

matches <- files %>% 
  map(READ) %>%    # read in all the files individually, using
  # the function READ from above
  reduce(full_join) # reduce with full


matches1 <- matches %>% 
  select(1:78) %>% 
  distinct()

missing_identified <- read_csv("missing_matched.csv")

matches2 <- left_join(matches1, missing_identified, by = "Name") %>% 
  mutate(ID =
           case_when(
             !is.na(ID) ~ ID,
             is.na(ID) ~ FundId
           ))


join <- left_join(share_data, matches2, by = c("FundId"="ID"))

missing <- join %>%
  filter(is.na(Name.y))


conflicts <- join %>% 
  filter(Investment.Type.y != "Insurance Product Fund" | is.na(Investment.Type.y),
         !is.na(Name.y)) %>% 
  mutate(
    Branding.Name.Mod =
      case_when(
        grepl("State Street", Branding.Name) ~ "State Street",
        grepl("TIAA", Branding.Name) ~ "TIAA/Nuveen",
        grepl("Nuveen", Branding.Name) ~ "TIAA/Nuveen",
        grepl("Eaton Vance", Branding.Name) ~ "Morgan Stanley",
        grepl("Calvert", Branding.Name) ~ "Eaton Vance/Calvert",
        grepl("iShares", Branding.Name) ~ "iShares/BlackRock",
        grepl("BlackRock", Branding.Name) ~ "iShares/BlackRock",
        grepl("PowerShares", Branding.Name) ~ "PowerShares/Invesco",
        grepl("Invesco", Branding.Name) ~ "PowerShares/Invesco",
        grepl("DWS$", Branding.Name) ~ "DWS/Xtrackers",
        grepl("Xtrackers", Branding.Name) ~ "DWS/Xtrackers",
        grepl("^Capital Group", Branding.Name) ~ "American Funds",
        Branding.Name %in% c("Harding Loevner", "Third Avenue",
                             "Tweedy, Browner") ~ "AMG",
        Branding.Name == "Oppenheimer" ~ "PowerShares/Invesco",
        Branding.Name == "Wells Fargo" ~ "Allspring",
        Branding.Name %in% "Amundi" ~ "Pioneer Investments",
        TRUE ~ Branding.Name
      ))


write_csv(conflicts, "conflicts.csv")



all_shops_assets <- conflicts %>% 
  filter(Fund.of..Funds == "No") %>% 
  group_by(Branding.Name.Mod) %>% 
  summarise(Total_March24 = sum(Position.Market.Value..2024.03..USD, na.rm = T)) %>% 
  arrange(desc(Total_March24))

all_shops_count <- conflicts %>% 
  filter(Fund.of..Funds == "No") %>% 
  group_by(Branding.Name.Mod, FundId) %>% 
  summarise(Funds = n()) %>% 
  group_by(Branding.Name.Mod) %>% 
  summarise(Funds_March24 = n()) %>% 
  arrange(desc(Funds_March24))

active_shops_assets <- conflicts %>% 
  filter(Fund.of..Funds == "No") %>% 
  filter(Index..Fund == "No") %>% 
  group_by(Branding.Name.Mod) %>% 
  summarise(Total_March24 = sum(Position.Market.Value..2024.03..USD, na.rm = T)) %>% 
  arrange(desc(Total_March24))

active_shops_count <- conflicts %>%
  filter(Fund.of..Funds == "No") %>% 
  filter(Index..Fund == "No") %>% 
  group_by(Branding.Name.Mod, FundId) %>% 
  summarise(Funds = n()) %>% 
  group_by(Branding.Name.Mod) %>% 
  summarise(Funds_March24 = n()) %>% 
  arrange(desc(Funds_March24))

stock <- conflicts %>%
  mutate(Stock_Clean =
           case_when(grepl("Royce", Stock) ~ "Rolls Royce",
         TRUE ~ Stock)) %>% 
  group_by(Stock_Clean, Index..Fund) %>%
  count(name = "Total") %>% 
  pivot_wider(
    names_from = Index..Fund,
    values_from = Total
  )


stock_assets <- conflicts %>%
  mutate(Stock_Clean =
           case_when(grepl("Royce", Stock) ~ "Rolls Royce",
                     TRUE ~ Stock)) %>% 
  group_by(Stock_Clean, Index..Fund) %>%
  summarise(Total = sum(Position.Market.Value..2024.03..USD, na.rm = T)) %>% 
  pivot_wider(
    names_from = Index..Fund,
    values_from = Total
  )

shares <- conflicts %>% 
  group_by(Index..Fund) %>% 
  summarise(
    `Q1 2023` = sum(Shares..2023.03, na.rm = T),
    `Q2 2023` = sum(Shares..2023.06, na.rm = T),
    `Q3 2023` = sum(Shares..2023.09, na.rm = T),
    `Q4 2023` = sum(Shares..2023.12, na.rm = T),
    `Q1 2024` = sum(Shares..2024.03, na.rm = T)
  )

shares_activeESG <- conflicts %>% 
  filter(Index..Fund == "No") %>%
  group_by(Sustainable.Investment.Overall..2024.03) %>% 
  summarise(
    `Q1 2023` = sum(Shares..2023.03, na.rm = T),
    `Q2 2023` = sum(Shares..2023.06, na.rm = T),
    `Q3 2023` = sum(Shares..2023.09, na.rm = T),
    `Q4 2023` = sum(Shares..2023.12, na.rm = T),
    `Q1 2024` = sum(Shares..2024.03, na.rm = T)
  )

  
shares_activeESGfunds <- conflicts %>% 
    filter(Index..Fund == "No") %>%
    group_by(Name.x, Sustainable.Investment.Overall..2024.03) %>% 
  summarise(
    `Q1 2023` = sum(Shares..2023.03, na.rm = T),
    `Q2 2023` = sum(Shares..2023.06, na.rm = T),
    `Q3 2023` = sum(Shares..2023.09, na.rm = T),
    `Q4 2023` = sum(Shares..2023.12, na.rm = T),
    `Q1 2024` = sum(Shares..2024.03, na.rm = T)
  )  
  
  
  shares_activeESGShops <- conflicts %>% 
    filter(Index..Fund == "No") %>%
    group_by(Branding.Name.Mod, Sustainable.Investment.Overall..2024.03) %>% 
  summarise(
    `Q1 2023` = sum(Shares..2023.03, na.rm = T),
    `Q2 2023` = sum(Shares..2023.06, na.rm = T),
    `Q3 2023` = sum(Shares..2023.09, na.rm = T),
    `Q4 2023` = sum(Shares..2023.12, na.rm = T),
    `Q1 2024` = sum(Shares..2024.03, na.rm = T)
  )  
write_csv(shares, "shares.csv")
