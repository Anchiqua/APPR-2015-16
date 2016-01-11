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
prvipovprecja$točke <- sum(prvi$točke)/383
prvipovprecja$procent.strela <- sum(prvi$procent.strela)/383


#Nareimo še tabelo igralcev, ki so rojeni v ostalih mesecih

drugi <- tabela
drugi[drugi$mesec.rojstva <= 6,] <- NA
drugi <- drugi[!is.na(drugi$mesec.rojstva),]

#naredimo še njihovo povprečje
drugipovprecja <- matrix(data = NA, nrow=1, ncol=6, byrow = TRUE)
drugipovprecja <- data.frame(drugipovprecja)
names(drugipovprecja)<- imena

drugipovprecja$odigrane.tekme<- sum(drugi$odigrane.tekme)/304
drugipovprecja$streli <- sum(drugi$streli)/304
drugipovprecja$goli <- sum(drugi$goli)/304
drugipovprecja$asistence <- sum(drugi$asistence)/304
drugipovprecja$točke <- sum(drugi$točke)/304
drugipovprecja$procent.strela <- sum(drugi$procent.strela)/304



slika11 <- ggplot(tabela, aes(x = igralci, y=točke)) + geom_point(data=prvi, color="red") +
  geom_point(data=drugi, color="blue") +
  geom_hline(data=prvipovprecja, aes(yintercept=točke), color="red") +
   geom_hline(data=drugipovprecja, aes(yintercept=točke), color="blue")
#opazimo, da imajo igralci rojeni v drugi polovici leta boljše povprečje, čeprav ni velike razlike

#zdaj pa naredimo tabelo 100 igralcev, ki imajo največ točk
točke <- tabela
točke <- arrange(točke, desc(točke))
točke[točke$točke < 50,] <- NA
točke <- točke[!is.na(točke$točke),]



a.skupina <- filter(točke, mesec.rojstva > 6)
b.skupina <- filter(točke, mesec.rojstva < 7)

avsota <- sum(a.skupina$točke)
bvsota <- sum(b.skupina$točke)
ggplot(točke %>% group_by(polletje = ifelse(mesec.rojstva < 7, "H1", "H2")) %>%
         summarise(točke = sum(točke)), aes(x = polletje, y = točke)) +
  geom_bar(stat = "identity")
  

#vidimo, da je več igralcev iz druge polovice, ki imajo več točk
#pri profesionalni ligi mesec rojstva ne vpliva toliko, kot pri mlajših fantih

