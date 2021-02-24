
# 4. faza: Analiza podatkov

# Po desetletjih ###############################################################

povprecne_starosti_tabela <- izvajalci_ostalo %>% 
  mutate(Desetletje=(Leto%/%10*10)) %>%
  group_by(Desetletje) %>% 
  summarise_at(vars(-c(Leto, 
                       Izvajalec, 
                       Ustanovitev, 
                       Izvor, 
                       Spol, 
                       Festival, 
                       Lokacija, 
                       Grofija)), 
                    funs(mean(., na.rm=TRUE)))

model <- lm(Starost ~ Desetletje, data = povprecne_starosti_tabela)
novo_desetletje <- data.frame(Desetletje = 2020)
predict(model, novo_desetletje)
napoved <- novo_desetletje %>% mutate(Starost = predict(model, .))

povprecne_starosti <- ggplot(povprecne_starosti_tabela, 
                             aes(x = Desetletje, y = Starost)) +
  geom_point(color = 'darkgreen',
             size = 2) + 
  geom_smooth(method = lm, formula=y~x, fullrange=TRUE) +
  geom_point(data=napoved, aes(x = Desetletje, y = Starost),
             color = 'red', 
             size = 5)


# Po letih #####################################################################  

povprecne_starosti_tabela_leta <- izvajalci_ostalo %>% 
  group_by(Leto) %>% 
  summarise_at(vars(-c(Izvajalec, 
                       Ustanovitev, 
                       Izvor, 
                       Spol, 
                       Festival, 
                       Lokacija, 
                       Grofija)), 
               funs(mean(., na.rm=TRUE)))

model_leta <- lm(Starost ~ Leto, data = povprecne_starosti_tabela_leta)
nova_leta <- data.frame(Leto = c(2018:2029))
predict(model_leta, nova_leta)
napoved_leta <- nova_leta %>% mutate(Starost = predict(model_leta, .))

povprecne_starosti_leta <- ggplot(povprecne_starosti_tabela_leta, 
                             aes(x = Leto, y = Starost)) +
  geom_point(color = 'darkgreen',
             size = 2) + 
  geom_smooth(method = lm, formula=y~x, fullrange=TRUE) +
  geom_point(data=napoved_leta, aes(x = Leto, y = Starost),
             color = 'red', 
             size = 2)