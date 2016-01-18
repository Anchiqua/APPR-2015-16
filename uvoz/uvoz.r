# 2. faza: Uvoz podatkov

r <- GET("http://www.nhl.com/stats/rest/grouped/skaters/season/skatersummary?cayenneExp=seasonId=20142015%20and%20gameTypeId=2")
text <- content(r, "text", encoding = "UTF-8")
data <- fromJSON(content(r, "text"))
tabela <- data.frame(data)

g <- GET("http://www.nhl.com/stats/rest/individual/skaters/season/bios?cayenneExp=seasonId=20142015%20and%20gameTypeId=2")
tekst <- content(g, "text", encoding ="UTF-8")
datoteka <- fromJSON(content(g, "text"))
podatki<- data.frame(datoteka)

tabela$data.plusMinus <- NULL
tabela$data.otGoals <- NULL
tabela$data.playerFirstName <- NULL
tabela$data.playerLastName <- NULL
tabela$total <- NULL
tabela$data.faceoffWinPctg <- NULL
tabela$data.playerId <- NULL
tabela$data.playerTeamsPlayedFor <- NULL
tabela$data.seasonId <- NULL
tabela$data.timeOnIcePerGame <- NULL
tabela$data.ppGoals <- NULL
tabela$data.gameWinningGoals <- NULL
tabela$data.shPoints <- NULL
tabela$data.shGoals <- NULL
tabela$data.ppPoints <- NULL
tabela$data.shiftsPerGame <- NULL
tabela$data.penaltyMinutes <- NULL
tabela$data.shootingPctg <- NULL


podatki$data.assists <- NULL
podatki$data.gamesPlayed <- NULL
podatki$data.goals <- NULL
podatki$data.penaltyMinutes <- NULL
podatki$data.playerBirthCity <- NULL
podatki$data.playerBirthStateProvince <-NULL
podatki$data.playerCurrentSweaterNumber <- NULL
podatki$data.playerDraftOverallPickNo <- NULL
podatki$data.playerDraftRoundNo <- NULL
podatki$data.playerDraftYear <- NULL
podatki$data.playerFirstName <- NULL
podatki$data.playerId <- NULL
podatki$data.playerLastName <- NULL
podatki$data.playerNationality <- NULL
podatki$data.playerPositionCode <- NULL
podatki$data.playerShootsCatches <- NULL
podatki$data.playerTeamsPlayedFor <- NULL
podatki$data.playerWeight <- NULL
podatki$data.plusMinus <- NULL
podatki$data.points <- NULL
podatki$data.seasonId <- NULL
podatki$total <- NULL

podatki <- podatki[c(4,1,2,3)]
names(podatki)[1] <- "igralci"
names(podatki)[3] <- "datum.rojstva"
names(podatki)[4] <- "visina"
names(podatki)[2] <- "drzava"


tabela <- tabela[c(4,2,7,3,1,6,5)]

names(tabela)[1] <- "igralci"
names(tabela)[2] <- "odigrane.tekme"
names(tabela)[3] <- "streli"
names(tabela)[4] <- "goli"
names(tabela)[5] <- "asistence"
names(tabela)[6] <- "tocke"
names(tabela)[7] <- "igralni.polozaj"

#združimo podatke
tabela <- inner_join(tabela, podatki)

#iz podatkov izračunamo procent strela
tabela["procent.strela"] <- (tabela$goli / tabela$streli)*100
tabela <- tabela[!is.na(tabela$procent.strela),]
tabela[tabela$odigrane.tekme <= 15, ] <- NA
tabela <- tabela[!is.na(tabela$odigrane.tekme),]

#razporedimo tabelo po igralcih
tabela <- arrange(tabela, igralci)

#zaokrožimo procent strela
tabela[,11] <- round(tabela[,11],2)

#preuredimo datume rojstva
datumi <- tabela$datum.rojstva %>% strapplyc("([0-9]+)-([0-9]+)-([0-9]+)") %>% sapply(as.numeric) %>% t()
datumi <- data.frame(datumi)
datumi$X3 <- NULL

tabela$letnica.rojstva <- datumi$X1
tabela$mesec.rojstva <- datumi$X2

tabela$datum.rojstva <- NULL

#naredimo novo tabelo, da vidimo povprečja podkategorij
imena <- c("odigrane.tekme", "streli", "goli", "asistence", "tocke", "procent.strela")
povprecja <- matrix(data = NA, nrow=1, ncol=6, byrow = TRUE)
povprecja <- data.frame(povprecja)
names(povprecja)<- imena
povprecja$odigrane.tekme<- sum(tabela$odigrane.tekme)/687
povprecja$streli <- sum(tabela$streli)/687
povprecja$goli <- sum(tabela$goli)/687
povprecja$asistence <- sum(tabela$asistence)/687
povprecja$tocke <- sum(tabela$tocke)/687
povprecja$procent.strela <- sum(tabela$procent.strela)/687
povprecja$visina <- sum(tabela$visina)/687

povprecja[,1] <- round(povprecja[,1],0)
povprecja[,2] <- round(povprecja[,2],0)
povprecja[,3] <- round(povprecja[,3],0)
povprecja[,4] <- round(povprecja[,4],0)
povprecja[,5] <- round(povprecja[,5],0)
povprecja[,6] <- round(povprecja[,6],2)
povprecja[,7] <- round(povprecja[,7],2)

tabela <- tabela[c(1,8,11,12,9,3,4,5,6,10,7,2)]


write.csv2(tabela, file="podatki/tabela.csv", fileEncoding = "UTF-8")
write.csv2(povprecja, file="podatki/povprecja.csv", fileEncoding = "UTF-8")


#grafi
slika1 <- plot_ly(tabela, x = odigrane.tekme, y = tocke, text = paste("igralci: ", igralci),
                  mode = "markers", color = tocke,  size=tocke)

slika2 <- plot_ly(tabela, x = odigrane.tekme, y = streli, text = paste("igralci: ", igralci),
                  mode = "markers", color = streli,  size=streli)

slika3 <- plot_ly(tabela, x = odigrane.tekme, y = goli, text = paste("igralci: ", igralci),
                  mode = "markers", color = goli,  size=goli)

slika4 <- plot_ly(tabela, x = odigrane.tekme, y = asistence, text = paste("igralci: ", igralci),
                  mode = "markers", color = asistence,  size=asistence)

slika5 <- plot_ly(tabela, x = streli, y = procent.strela, text = paste(igralci),
                  mode = "markers", color = procent.strela,  size=procent.strela)

slika6 <- ggplot(tabela, aes(x = igralci, y=tocke)) + geom_point(aes(colour=tocke)) +
  scale_colour_gradient2(low = "yellow", mid = "orange1",high = "red4", midpoint = povprecja$točke) + 
  geom_hline(data=povprecja, aes(yintercept=tocke))

slika7 <- ggplot(tabela, aes(x = igralci, y=goli)) + geom_point(aes(colour=goli)) +
  scale_colour_gradient2(low = "lightskyblue1", mid = "steelblue2",high = "navyblue", midpoint = povprecja$goli) + 
  geom_hline(data=povprecja, aes(yintercept=goli))


slika8 <- ggplot(tabela, aes(x = igralci, y=asistence)) + geom_point(aes(colour=asistence)) +
  scale_colour_gradient2(low = "yellow", mid = "springgreen2",high = "saddlebrown", midpoint = povprecja$asistence) + 
  geom_hline(data=povprecja, aes(yintercept=asistence))


slika9 <- ggplot(tabela, aes(x = igralci, y=streli)) + geom_point(aes(colour=streli)) +
  scale_colour_gradient2(low = "mistyrose", mid = "maroon1",high = "violetred4", midpoint = povprecja$streli) + 
  geom_hline(data=povprecja, aes(yintercept=streli))


slika10 <- ggplot(tabela, aes(x = igralci, y=visina)) + geom_point(aes(colour=višina)) +
  scale_colour_gradient2(low = "khaki1", mid = "sienna1",high = "lightsalmon4", midpoint = povprecja$višina) + 
  geom_hline(data=povprecja, aes(yintercept=visina))


#naredimo novo tabelo, kjer bomo imeli države in število igralcev v vsaki državi
tabela2 <- tabela %>% group_by(drzava) %>%
  summarise(stevilo = length(igralci))
