# 2. faza: Uvoz podatkov
library(dplyr)
library(plotly)
library(ggplot2)
require(jsonlite)
require(httr)
require(zoo)
library(rvest)

r <- GET("http://www.nhl.com/stats/rest/grouped/skaters/season/skatersummary?cayenneExp=seasonId=20142015%20and%20gameTypeId=2")
text <- content(r, "text", encoding = "UTF-8")
data <- fromJSON(content(r, "text"))
tabela <- data.frame(data)


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

tabela <- tabela[c(4,2,7,3,1,6,5)]

names(tabela)[1] <- "igralci"
names(tabela)[2] <- "odigrane.tekme"
names(tabela)[3] <- "streli"
names(tabela)[4] <- "goli"
names(tabela)[5] <- "asistence"
names(tabela)[6] <- "točke"
names(tabela)[7] <- "igralni.položaj"

tabela["procent.strela"] <- (tabela$goli / tabela$streli)*100
tabela <- tabela[!is.na(tabela$procent.strela),]
tabela[tabela$odigrane.tekme <= 15, ] <- NA
tabela <- tabela[!is.na(tabela$odigrane.tekme),]

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

write.csv2(tabela, "C:/Users/Anja/Desktop/Šola/Analiza podatkov/projekt/APPR-2015-16-master/tabela.csv", fileEncoding = "UTF-8")
write.csv2(povprecja, "C:/Users/Anja/Desktop/Šola/Analiza podatkov/projekt/APPR-2015-16-master/povprecja.csv", fileEncoding = "UTF-8")

#grafi
d <- tabela[sample(nrow(tabela), 687), ]
plot_ly(d, x = odigrane.tekme, y = točke, text = paste("igralci: ", igralci),
        mode = "markers", color = točke,  size=točke)

plot_ly(d, x = odigrane.tekme, y = streli, text = paste("igralci: ", igralci),
        mode = "markers", color = streli,  size=streli)

plot_ly(d, x = odigrane.tekme, y = goli, text = paste("igralci: ", igralci),
        mode = "markers", color = goli,  size=goli)

plot_ly(d, x = odigrane.tekme, y = asistence, text = paste("igralci: ", igralci),
        mode = "markers", color = asistence,  size=asistence)


p <- ggplot(tabela, aes(x = igralci, y=točke)) + geom_point()
p+geom_hline(data=povprecja, aes(yintercept=točke))

a <- ggplot(tabela, aes(x = igralci, y=odigrane.tekme)) + geom_point()
a+geom_hline(data=povprecja, aes(yintercept=odigrane.tekme))

b <- ggplot(tabela, aes(x = igralci, y=goli)) + geom_point()
b+geom_hline(data=povprecja, aes(yintercept=goli))

c <- ggplot(tabela, aes(x = igralci, y=asistence)) + geom_point()
c+geom_hline(data=povprecja, aes(yintercept=asistence))

d <- ggplot(tabela, aes(x = igralci, y=streli)) + geom_point()
d+geom_hline(data=povprecja, aes(yintercept=streli))

e <- ggplot(tabela, aes(x = igralci, y=procent.strela)) + geom_point()
e+geom_hline(data=povprecja, aes(yintercept=procent.strela))

