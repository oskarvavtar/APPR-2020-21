
# 2. faza: Uvoz podatkov


# glasbene lestvice ############################################################
  
uvozi_lestvico <- function(ime_datoteke){
  ime <- paste0("podatki/charts/", 
                ime_datoteke, ".csv")
  tabela <- read_csv2(ime) %>% select(2:4, 6) %>%
    dplyr::rename(Naslov=1, Izvajalec=2, Zvrst=3, Leto=4)
  return(tabela)
}

lestvica70 <- uvozi_lestvico("1970s")

lestvica80 <- uvozi_lestvico("1980s")

lestvica90 <- uvozi_lestvico("1990s")

lestvica00 <- uvozi_lestvico("2000s")

lestvica10 <- uvozi_lestvico("2010s")


# festivali ####################################################################

# ------------------------------------------------------------------------------

# zvrsti izvajalcev

izvajalci_zvrsti <- read_csv2("podatki/appearance_plus_genres.csv") %>% select(1:2, 4:5) %>%
  dplyr::rename(Festival=1, Leto=2, Izvajalec=3, Zvrst=4) %>% .[c(2, 3, 4, 1)] %>%
  mutate(Izvajalec=str_to_title(Izvajalec)) %>%
  mutate(Festival=str_to_title(Festival))

# ------------------------------------------------------------------------------

# nastopi

izvajalci_ostalo <- read_csv2("podatki/festival_headliners.csv") %>% 
  select(1:4, 7, 10:11, 14:15) %>%
  dplyr::rename(Leto=2, Lokacija=3, Grofija=4, Izvajalec=5, Ustanovitev=6, Izvor=7, Spol=8, Starost=9) %>%
  .[c(2, 5:9, 1, 3:4)] %>% mutate(Starost=(Starost-(2017-Leto))) %>%
  mutate(Festival=str_to_title(Festival)) %>%
  mutate(Lokacija=str_to_title(Lokacija)) %>%
  mutate(Grofija=str_to_title(Grofija)) %>%
  mutate(Izvajalec=str_to_title(Izvajalec)) %>%
  mutate(Izvor=str_to_title(Izvor)) %>%
  mutate(Spol=str_to_title(Spol))

# ------------------------------------------------------------------------------







