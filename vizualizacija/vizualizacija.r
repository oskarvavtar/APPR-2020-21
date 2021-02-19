
# 3. faza: Vizualizacija podatkov

#vektor <- rep(1, length(izvajalci_ostalo$Starost))
#podatki <- izvajalci_ostalo %>% 
  #mutate(Desetletje=(Leto%/%10*10)) 
#podatki$Pojavitev <- vektor 
#podatki <- podatki %>%
  #filter(Desetletje == 2010$Desetletje) %>%
  #select(Starost, Pojavitev) %>%
  #group_by(Starost) %>%
  #summarise(Število=sum(Pojavitev))

#podatki <- podatki %>% filter(Desetletje == 2010)
#podatki <- podatki %>% select(Starost, Pojavitev)
#podatki <- podatki %>% group_by(Starost) 
#podatki <- podatki %>% summarise(Število=sum(Pojavitev))

#grafek <- ggplot(data=podatki, aes(x=Starost, y=Število)) +
  #geom_col()


# Pogostost zvrsti #############################################################

pogostost_zvrsti <- count(izvajalci_zvrsti, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev))

graf_zvrsti <- ggplot(data=pogostost_zvrsti, aes(x=Zvrst)) +
  geom_col(aes(y=Pojavitev)) + 
  theme(axis.text.x = element_text(angle = 90))

# Izvor ########################################################################

izvor <- count(izvajalci_ostalo, Izvor) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev))
izvor[nrow(izvor), 1] <- "?"

izvor_graf <- ggplot(izvor, aes(x="", y=Pojavitev, fill=Izvor)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + 
  theme_void()

# Spol #########################################################################

s_priprava70 <- izvajalci_ostalo %>% filter(Leto >= 1970 & Leto <= 1979)
spol70 <- count(s_priprava70, Spol) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol70[2,] <- NA
spol70[3,] <- NA
spol70[4,] <- NA

s_priprava80 <- izvajalci_ostalo %>% filter(Leto >= 1980 & Leto <= 1989)
spol80 <- count(s_priprava80, Spol) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol80[4, 1] <- "?"

s_priprava90 <- izvajalci_ostalo %>% filter(Leto >= 1990 & Leto <= 1999)
spol90 <- count(s_priprava90, Spol) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol90[4,] <- NA

s_priprava00 <- izvajalci_ostalo %>% filter(Leto >= 2000 & Leto <= 2009)
spol00 <- count(s_priprava00, Spol) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol00[4, 1] <- "?"

s_priprava10 <- izvajalci_ostalo %>% filter(Leto >= 2010 & Leto <= 2019)
spol10 <- count(s_priprava10, Spol) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))
spol10[4, 1] <- "?"

spol_tabela1 <- data.frame(spol80$Spol,
                          spol70$Pojavitev,
                          spol80$Pojavitev,
                          spol90$Pojavitev,
                          spol00$Pojavitev,
                          spol10$Pojavitev) %>%
  dplyr::rename(Spol=1, "1970"=2, "1980"=3, "1990"=4, "2000"=5, "2010"=6) %>%
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
  dplyr::rename(Spol=1, "1970"=2, "1980"=3, "1990"=4, "2000"=5, "2010"=6) %>%
  pivot_longer(c=(-Spol), names_to="Desetletje", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje)) 
spol_tabela2[6, 3] <- 0
spol_tabela2[11, 3] <- 0
spol_tabela2[16, 3] <- 0
spol_tabela2[18, 3] <- 0


spol2 <- ggplot(data=spol_tabela2, aes(x=Desetletje, y=Pogostost)) +
  geom_bar(stat = "identity", aes(fill=Spol))

# Lestvice ##############################################################

zvrsti70 <- count(lestvica70, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

zvrsti80 <- count(lestvica80, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

zvrsti90 <- count(lestvica90, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

zvrsti00 <- count(lestvica00, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

zvrsti10 <- count(lestvica10, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

# Festivali - žanri ############################################################

priprava70 <- izvajalci_zvrsti %>% filter(Leto >= 1970 & Leto <= 1979)
festivali70 <- count(priprava70, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

priprava80 <- izvajalci_zvrsti %>% filter(Leto >= 1980 & Leto <= 1989)
festivali80 <- count(priprava80, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

priprava90 <- izvajalci_zvrsti %>% filter(Leto >= 1990 & Leto <= 1999)
festivali90 <- count(priprava90, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

priprava00 <- izvajalci_zvrsti %>% filter(Leto >= 2000 & Leto <= 2009)
festivali00 <- count(priprava00, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

priprava10 <- izvajalci_zvrsti %>% filter(Leto >= 2010 & Leto <= 2019)
festivali10 <- count(priprava10, Zvrst) %>% dplyr::rename(Pojavitev=2) %>%
  mutate(Pogostost=Pojavitev/sum(Pojavitev)) %>%
  arrange(desc(Pogostost))

# pogostost rocka ##############################################################

lestvice_rock <- data.frame(zvrsti70[1,3],
                            zvrsti80[2,3],
                            zvrsti90[2,3],
                            zvrsti00[2,3],
                            zvrsti10[2,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_rock <- data.frame(festivali70[1,3],
                             festivali80[2,3],
                             festivali90[1,3],
                             festivali00[1,3],
                             festivali10[1,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_rock_tabela <- merge(lestvice_rock, festivali_rock, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost popa ###############################################################

lestvice_pop <- data.frame(zvrsti70[2,3],
                           zvrsti80[1,3],
                           zvrsti90[1,3],
                           zvrsti00[1,3],
                           zvrsti10[1,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_pop <- data.frame(festivali70[2,3],
                            0,
                            festivali90[6,3],
                            festivali00[3,3],
                            festivali10[6,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_pop_tabela <- merge(lestvice_pop, festivali_pop, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# postostost indieja ###########################################################

lestvice_indie <- data.frame(0,
                             0,
                             0,
                             zvrsti00[11,3],
                             zvrsti10[4,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_indie <- data.frame(0,
                              0,
                              festivali90[2,3],
                              festivali00[5,3],
                              festivali10[2,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_indie_tabela <- merge(lestvice_indie, festivali_indie, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost folka ##############################################################

lestvice_folk <- data.frame(zvrsti70[6,3],
                            0,
                            zvrsti90[3,3],
                            zvrsti00[3,3],
                            zvrsti10[3,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_folk <- data.frame(0,
                             festivali80[4,3],
                             festivali90[5,3],
                             festivali00[6,3],
                             festivali10[8,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_folk_tabela <- merge(lestvice_folk, festivali_folk, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost hiphop #############################################################

lestvice_hiphop <- data.frame(0,
                              0,
                              0,
                              zvrsti00[4,3],
                              zvrsti10[5,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_hiphop <- data.frame(0,
                               0,
                               festivali90[7,3],
                               festivali00[8,3],
                               festivali10[5,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_hiphop_tabela <- merge(lestvice_hiphop, festivali_hiphop, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost jazza ##############################################################

lestvice_jazz <- data.frame(0,
                            0,
                            0,
                            zvrsti00[5,3],
                            0) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_jazz <- data.frame(0,
                             festivali80[5,3],
                             0,
                             festivali00[10,3],
                             festivali10[14,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_jazz_tabela <- merge(lestvice_jazz, festivali_jazz, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost soula ##############################################################

lestvice_soul <- data.frame(zvrsti70[4,3],
                            0,
                            zvrsti90[4,3],
                            zvrsti00[7,3],
                            zvrsti10[6,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_soul <- data.frame(0,
                             festivali80[6,3],
                             0,
                             festivali00[15,3],
                             festivali10[10,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_soul_tabela <- merge(lestvice_soul, festivali_soul, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost punka ##############################################################

lestvice_punk <- data.frame(zvrsti70[8,3],
                            0,
                            0,
                            zvrsti00[9,3],
                            0) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_punk <- data.frame(0,
                             0,
                             0,
                             festivali00[9,3],
                             festivali10[9,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_punk_tabela <- merge(lestvice_punk, festivali_punk, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost EDMa ###############################################################

lestvice_edm <- data.frame(0,
                           0,
                           0,
                           zvrsti00[6,3],
                           zvrsti10[7,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_edm <- data.frame(0,
                            0,
                            festivali90[3,3],
                            festivali00[2,3],
                            festivali10[3,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_edm_tabela <- merge(lestvice_edm, festivali_edm, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost metala #############################################################

lestvice_metal <- data.frame(0,
                             0,
                             0,
                             zvrsti00[8,3],
                             0) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

festivali_metal <- data.frame(0,
                              0,
                              festivali90[8,3],
                              festivali00[4,3],
                              festivali10[7,3]) %>%
  dplyr::rename("1970s"=1, "1980s"=2, "1990s"=3, "2000s"=4, "2010s"=5) %>%
  pivot_longer(col=1:5) %>%
  dplyr::rename(Desetletje=1, Pogostost=2)

pogostost_metal_tabela <- merge(lestvice_metal, festivali_metal, by="Desetletje") %>%
  dplyr::rename(Lestvice=2, Festivali=3) %>%
  pivot_longer(c=(-Desetletje), names_to="Vrste", values_to="Pogostost") %>%
  mutate(Desetletje=parse_number(Desetletje))

# pogostost žanrov - skupna tabela #############################################

#pogostost_zanrov <- merge(pogostost_rock_tabela, pogostost_pop_tabela,
                          #pogostost_indie_tabela, #pogostost_folk_tabela,
                          #pogostost_hiphop_tabela, pogostost_jazz_tabela,
                          #pogostost_soul_tabela, pogostost_punk_tabela,
                          #pogostost_edm_tabela, pogostost_metal_tabela,
                          #by=c("Desetletje", "Vrste"))

zanri <- list(pogostost_rock_tabela, pogostost_pop_tabela, 
              pogostost_indie_tabela, pogostost_folk_tabela,
              pogostost_hiphop_tabela, pogostost_jazz_tabela,
              pogostost_soul_tabela, pogostost_punk_tabela,
              pogostost_edm_tabela, pogostost_metal_tabela)

zdruzi <- function(tabela1, tabela2){
  merge(tabela1, tabela2, by=c("Desetletje", "Vrste"))
}

zanri <- Reduce(zdruzi, zanri) %>%
  dplyr::rename(Rock=3, Pop=4, Indie=5, Folk=6, Hiphop=7, Jazz=8, Soul=9, Punk=10,
         EDM=11, Metal=12)

# lokacije festivalov - grofije ################################################

grofije_tabela <- izvajalci_ostalo %>%
  select(Festival, Grofija) %>%
  unique() %>%
  count(Grofija) %>%
  dplyr::rename("id"=1, "Število festivalov"=2)
grofije_tabela[2,1] <- "Essex"
grofije_tabela[5,1] <- "Isle of Wight"
grofije_tabela[7,1] <- "Greater London"

mapdata <- uvozi.zemljevid("https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_GBR_shp.zip",
                           "gadm36_GBR_2", encoding="UTF-8")

scotland <- mapdata[mapdata$NAME_1 == "Scotland", ]$NAME_2 
zamenjava <- setNames(rep("Scotland", length(scotland)), scotland) %>%
  c("Bracknell Forest" = "Berkshire",
    "Reading"="Berkshire",
    "Slough" = "Berkshire",
    "West Berkshire"="Berkshire",
    "Wokingham"="Berkshire",
    "Leicester" = "Leicestershire",
    "Bath and North East Somerset" = "Somerset",
    "North Somerset" = "Somerset") 
mapdata$grofija <- zamenjava[as.character(mapdata$NAME_2)] %>% coalesce(mapdata$NAME_2)

zemljevid <- tm_shape(merge(mapdata, grofije_tabela, by.x="grofija", by.y="id")) +
  tm_polygons("Število festivalov", border.col=NULL)

