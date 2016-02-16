# 4. faza: Analiza podatkov

tabela4 <- inner_join(tabela3, tabela2)

tabela4 <- tabela4[c( "stevilo" ,"tocke")]
tabela4.norm <- scale(tabela4)



k <- kmeans(tabela4.norm, 5)
#head(k$cluster, n = 15, nstart=1000)
table(k$cluster)
k <- kmeans(tabela4.norm, 5, nstart = 10000)

tabela4.skupine <- data.frame(Drzava = names(k$cluster), 
                               skupina = factor(k$cluster))

skupina <- tabela4.skupine


m3 <- match(svet$name_long, tabela4$drzava)
svet$skupina <- factor(k$cluster[drzava[m1]])
ggplot(sv, aes(x=long, y=lat, group=group, fill=skupina)) +
  geom_polygon(color="grey")
