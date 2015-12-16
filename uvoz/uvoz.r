require(jsonlite)
require(httr)
uvoz <- GET("http://www.nhl.com/stats/rest/grouped/skaters/season/skatersummary?cayenneExp=seasonId=20142015%20and%20gameTypeId=2")
text <- content(uvoz, "text")
data <- fromJSON(content(uvoz, "text"))
podatki <-data$data
