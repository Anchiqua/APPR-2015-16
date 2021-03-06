# 3. faza: Izdelava zemljevida

#Naredimo tabelo za igralce, ki so rojeni v prvi polovici
prvi <- tabela
prvi[prvi$mesec.rojstva >= 7,] <- NA
prvi <- prvi[!is.na(prvi$mesec.rojstva),]

#nareimo novo tabelo z povprečji
prvipovprecja <- matrix(data = NA, nrow=1, ncol=6, byrow = TRUE)
prvipovprecja <- data.frame(prvipovprecja)
names(prvipovprecja)<- imena

prvipovprecja$odigrane.tekme<- sum(prvi$odigrane.tekme)/383
prvipovprecja$streli <- sum(prvi$streli)/383
prvipovprecja$goli <- sum(prvi$goli)/383
prvipovprecja$asistence <- sum(prvi$asistence)/383
prvipovprecja$tocke <- sum(prvi$tocke)/383
prvipovprecja$procent.strela <- sum(prvi$procent.strela)/383


#Nareimo še tabelo igralcev, ki so rojeni v ostalih mesecih

drugi <- tabela
drugi[drugi$mesec.rojstva <= 6,] <- NA
drugi <- drugi[!is.na(drugi$mesec.rojstva),]
drugi[drugi$odigrane.tekme < 39,] <- NA
drugi <- drugi[!is.na(drugi$odigrane.tekme),]

#naredimo še njihovo povprečje
drugipovprecja <- matrix(data = NA, nrow=1, ncol=6, byrow = TRUE)
drugipovprecja <- data.frame(drugipovprecja)
names(drugipovprecja)<- imena

drugipovprecja$odigrane.tekme<- sum(drugi$odigrane.tekme)/304
drugipovprecja$streli <- sum(drugi$streli)/304
drugipovprecja$goli <- sum(drugi$goli)/304
drugipovprecja$asistence <- sum(drugi$asistence)/304
drugipovprecja$tocke <- sum(drugi$tocke)/304
drugipovprecja$procent.strela <- sum(drugi$procent.strela)/304



slika11 <- ggplot(tabela, aes(x = igralci, y=tocke)) + geom_point(data=prvi, color="red") +
  geom_point(data=drugi, color="blue") +
  geom_hline(data=prvipovprecja, aes(yintercept=tocke), color="red") +
   geom_hline(data=drugipovprecja, aes(yintercept=tocke), color="blue")
#opazimo, da imajo igralci rojeni v drugi polovici leta boljše povprečje, čeprav ni velike razlike

#zdaj pa naredimo tabelo 100 igralcev, ki imajo največ točk
tocke <- tabela
tocke <- arrange(tocke, desc(tocke))
tocke[tocke$tocke < 50,] <- NA
tocke <- tocke[!is.na(tocke$tocke),]



a.skupina <- filter(tocke, mesec.rojstva > 6)
b.skupina <- filter(tocke, mesec.rojstva < 7)

avsota <- sum(a.skupina$tocke)
bvsota <- sum(b.skupina$tocke)

slika12<- ggplot(tocke %>% group_by(polletje = ifelse(mesec.rojstva < 7, "H1", "H2")) %>%
         summarise(tocke = sum(tocke)), aes(x = polletje, y = tocke)) +
  geom_bar(stat = "identity", fill=rainbow(2))  


#vidimo, da je več igralcev iz druge polovice, ki imajo več točk
#pri profesionalni ligi mesec rojstva ne vpliva toliko, kot pri mlajših fantih

#poiščimo nekaj najboljših igralcev sezone
podatki2 <- select(tabela, igralci, drzava , odigrane.tekme, tocke)
podatki2["stevilo.tock.na.tekmo"] <- (podatki2$tocke/podatki2$odigrane.tekme)
podatki2 <- arrange(podatki2, desc(stevilo.tock.na.tekmo))
podatki2 <- filter(podatki2, stevilo.tock.na.tekmo >=1)

#naredimo zemljevid sveta in na njemu vidimo iz kje so igralci
svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                                        "ne_110m_admin_0_countries", encoding = "UTF-8")
tabela2$drzava <- factor(tabela2$drzava, levels = levels(svet$adm0_a3))
m1 <- match(svet$name_long, tabela2$Drzava)
sv <- pretvori.zemljevid(svet)
zem1 <- ggplot() + geom_polygon(data=tabela %>% group_by(drzava) %>%
                                  summarise(stevilo = length(igralci)) %>%
                                  right_join(sv, c("drzava" = "adm0_a3")), 
                                aes(x=long, y=lat, group=group, fill=stevilo),
                                color="grey") + 
  scale_fill_continuous(low="#69b8f6", high="#142d45") +
  xlab("") + ylab("")
                               
#plot(zem1)
                                 
#opazimo, da je največ igralcev v Kanadi

tabela3$drzava <- factor(tabela3$drzava, levels = levels(svet$adm0_a3))
m2 <- match(svet$name_long, tabela3$Drzava)
zem2 <- ggplot() + geom_polygon(data=tabela3 %>% right_join(sv, c("drzava" = "adm0_a3")), 
                                aes(x=long, y=lat, group=group, fill=tocke),
                                color="grey") + 
  scale_fill_continuous(low="#69b8f6", high="#142d45") +
  xlab("") + ylab("")

#plot(zem2)
