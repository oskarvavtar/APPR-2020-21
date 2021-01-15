# 3. faza: Vizualizacija podatkov

# Pogostost zvrsti #############################################################

pogostost_zvrsti <- count(izvajalci_zvrsti, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev))

graf_zvrsti <- ggplot(data=pogostost_zvrsti, aes(x=Zvrst)) +
  geom_col(aes(y=Pojavitev)) + 
  theme(axis.text.x = element_text(angle = 90))

# Izvor ########################################################################

izvor <- count(izvajalci_ostalo, Izvor) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev))
izvor[nrow(izvor), 1] <- "?"

izvor_graf <- ggplot(izvor, aes(x="", y=Pojavitev, fill=Izvor)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + 
  theme_void()

# Spol #########################################################################

s_priprava70 <- izvajalci_ostalo %>% filter(Leto >= 1970 & Leto <= 1979)
spol70 <- count(s_priprava70, Spol) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol70[2,] <- NA
spol70[3,] <- NA
spol70[4,] <- NA

s_priprava80 <- izvajalci_ostalo %>% filter(Leto >= 1980 & Leto <= 1989)
spol80 <- count(s_priprava80, Spol) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol80[4, 1] <- "?"

s_priprava90 <- izvajalci_ostalo %>% filter(Leto >= 1990 & Leto <= 1999)
spol90 <- count(s_priprava90, Spol) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol90[4,] <- NA

s_priprava00 <- izvajalci_ostalo %>% filter(Leto >= 2000 & Leto <= 2009)
spol00 <- count(s_priprava00, Spol) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol00[4, 1] <- "?"

s_priprava10 <- izvajalci_ostalo %>% filter(Leto >= 2010 & Leto <= 2019)
spol10 <- count(s_priprava10, Spol) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol10[4, 1] <- "?"

spol_tabela1 <- data.frame(spol80$Spol,
                          spol70$Pojavitev,
                          spol80$Pojavitev,
                          spol90$Pojavitev,
                          spol00$Pojavitev,
                          spol10$Pojavitev) %>%
  rename(Spol=1, "1970"=2, "1980"=3, "1990"=4, "2000"=5, "2010"=6) %>%
  pivot_longer(c=(-Spol), names_to="Desetletje", values_to="Pojavitev") %>%
  mutate(Desetletje=parse_number(Desetletje)) 
spol_tabela1[6, 3] <- 0
spol_tabela1[11, 3] <- 0
spol_tabela1[16, 3] <- 0
spol_tabela1[18, 3] <- 0


spol1 <- ggplot(data=spol_tabela1, aes(x=Desetletje, y=Pojavitev)) +
  geom_bar(stat = "identity", aes(fill=Spol))

spol_tabela2 <- data.frame(spol80$Spol,
                          spol70$Pogostost,
                          spol80$Pogostost,
                          spol90$Pogostost,
                          spol00$Pogostost,
                          spol10$Pogostost) %>%
  rename(Spol=1, "1970"=2, "1980"=3, "1990"=4, "2000"=5, "2010"=6) %>%
  pivot_longer(c=(-Spol), names_to="Desetletje", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje)) 
spol_tabela2[6, 3] <- 0
spol_tabela2[11, 3] <- 0
spol_tabela2[16, 3] <- 0
spol_tabela2[18, 3] <- 0


spol2 <- ggplot(data=spol_tabela2, aes(x=Desetletje, y=Pogostost)) +
  geom_bar(stat = "identity", aes(fill=Spol))

# Lestvice - rock ##############################################################

zvrsti70 <- count(lestvica70, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

zvrsti80 <- count(lestvica80, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

zvrsti90 <- count(lestvica90, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

zvrsti00 <- count(lestvica00, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

zvrsti10 <- count(lestvica10, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

lestvice_rock <- data.frame(zvrsti70[1,3],
                            zvrsti80[2,3],
                            zvrsti90[2,3],
                            zvrsti00[2,3],
                            zvrsti10[2,3]) %>%
  rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  rename(Desetletje=1, Pogostost=2)

# Festivali - rock #############################################################

priprava70 <- izvajalci_zvrsti %>% filter(Leto >= 1970 & Leto <= 1979)
festivali70 <- count(priprava70, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

priprava80 <- izvajalci_zvrsti %>% filter(Leto >= 1980 & Leto <= 1989)
festivali80 <- count(priprava80, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

priprava90 <- izvajalci_zvrsti %>% filter(Leto >= 1990 & Leto <= 1999)
festivali90 <- count(priprava90, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

priprava00 <- izvajalci_zvrsti %>% filter(Leto >= 2000 & Leto <= 2009)
festivali00 <- count(priprava00, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

priprava10 <- izvajalci_zvrsti %>% filter(Leto >= 2010 & Leto <= 2019)
festivali10 <- count(priprava10, Zvrst) %>% rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

festivali_rock <- data.frame(zvrsti70[1,3],
                             zvrsti80[2,3],
                             zvrsti90[1,3],
                             zvrsti00[1,3],
                             zvrsti10[1,3]) %>%
  rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  rename(Desetletje=1, Pogostost=2)

# pogostost rocka ##############################################################

pogostost_rock_tabela <- merge(lestvice_rock, festivali_rock, by="Desetletje") %>%
  rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))


pogostost_rock <- pogostost_rock_tabela %>%
  ggplot(aes(x = Desetletje, y = Pogostost, col=Vrste)) + 
  geom_point() + 
  geom_line()

# lokacije festivalov - grofije ################################################

#grofije_tabela <- izvajalci_ostalo %>%
#  select(Festival, Grofija) %>%
#  unique() %>%
#  count(Grofija) %>%
#  rename("id"=1, "Število festivalov"=2)

#zdruzeni_tabeli_zemljevid <- join(mapdata, grofije_tabela, by="id")

#zemljevid_festivalov <- ggplot() +
#  geom_polygon(data = zdruzeni_tabeli_zemljevid, aes(x = long, y = lat, group = group, fill = "Število festivalov"), color = "#FFFFFF", size = 0.25) +
#  scale_fill_gradient2(low = "blue", mid = "red", high = "yellow", na.value = "white")




