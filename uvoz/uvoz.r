# 2. faza: Uvoz podatkov
#uvozimo podatke
require(jsonlite)
require(httr)
r <- GET("http://www.nhl.com/stats/rest/grouped/skaters/season/skatersummary?cayenneExp=seasonId=20142015%20and%20gameTypeId=2")
text <- content(r, "text")
data <- fromJSON(content(r, "text"))
tabela <- data.frame(data)

#omejimo na manj kategorij
tabela$data.plusMinus <- NULL
tabela$data.otGoals <- NULL
tabela$data.playerFirstName <- NULL
tabela$data.playerLastName <- NULL
tabela$total <- NULL
tabela$data.faceoffWinPctg <- NULL
tabela$data.playerId <- NULL
tabela$data.playerTeamsPlayedFor <- NULL
tabela$data.seasonId <- NULL
tabela$data.playerPositionCode <- NULL
tabela$data.ppGoals <- NULL
tabela$data.gameWinningGoals <- NULL
tabela$data.shPoints <- NULL
tabela$data.shGoals <- NULL
tabela$data.ppPoints <- NULL
tabela$data.shiftsPerGame <- NULL
tabela$data.penaltyMinutes <- NULL
tabela$data.shootingPctg <- NULL

#preuredimo stolpce
tabela <- tabela[c(4,2,7,3,1,5,6)]

#preimenujemo stolpce
names(tabela)[1] <- "igralci"
names(tabela)[2] <- "odigrane tekme"
names(tabela)[3] <- "čas na ledu"
names(tabela)[4] <- "goli"
names(tabela)[5] <- "asistence"
names(tabela)[6] <- "točke"
names(tabela)[7] <- "streli"
