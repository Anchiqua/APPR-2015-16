# 2. faza: Uvoz podatkov
library(dplyr)
library(ggplot2)
require(jsonlite)
require(httr)
require(zoo)

#uvozimo podatke
r <- GET("http://www.nhl.com/stats/rest/grouped/skaters/season/skatersummary?cayenneExp=seasonId=20142015%20and%20gameTypeId=2")
text <- content(r, "text")
data <- fromJSON(content(r, "text"))
tabela <- data.frame(data)

#odstranimo stolpce, ki jih ne potrebujemo
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

#preuredimo stolpce
tabela <- tabela[c(4,2,7,3,1,6,5)]

#preimenujemo stolpce
names(tabela)[1] <- "igralci"
names(tabela)[2] <- "odigrane.tekme"
names(tabela)[3] <- "streli"
names(tabela)[4] <- "goli"
names(tabela)[5] <- "asistence"
names(tabela)[6] <- "točke"
names(tabela)[7] <- "igralni.položaj"

#izračunamo procente strela in dodamo stolpec v tabelo ( odstranimo tiste, ki imajo vrednost NA)
tabela["procent.strela"] <- (tabela$goli / tabela$streli)*100
tabela <- tabela[!is.na(tabela$procent.strela),]
tabela[tabela$odigrane.tekme <= 15, ] <- NA
tabela <- tabela[!is.na(tabela$odigrane.tekme),]

#vrstice uredimo po imenih igralcev
tabela <- arrange(tabela, igralci)

#naredimo novo tabelo, da vidimo povprečja podkategorij
imena <- c("odigrane.tekme", "streli", "goli", "asistence", "točke", "procent.strela")
povprecja <- matrix(data = NA, nrow=1, ncol=6, byrow = TRUE)
povprecja <- data.frame(povprecja)
names(povprecja)<- imena
povprecja$odigrane.tekme<- sum(tabela$odigrane.tekme)/687
povprecja$streli <- sum(tabela$streli)/687
povprecja$goli <- sum(tabela$goli)/687
povprecja$asistence <- sum(tabela$asistence)/687
povprecja$točke <- sum(tabela$točke)/687
povprecja$procent.strela <- sum(tabela$procent.strela)/687
