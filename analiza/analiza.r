# 4. faza: Analiza podatkov

#naredimo skupine za države glede na število igralcev in točk
tabela5 <- inner_join(tabela3, tabela2)
rownames(tabela5) <- tabela5$drzava
tabela5.norm <- tabela5 %>% select(-drzava) %>% scale()




k1 <- kmeans(tabela5.norm, 3)
#head(k$cluster, n = 15, nstart=1000)
table(k$cluster)
k1 <- kmeans(tabela5.norm, 3, nstart = 10000)

tabela5.skupine <- data.frame(Drzava = factor(names(k1$cluster),
                                              levels = svet$adm0_a3),
                              skupina = factor(k1$cluster))

zem3 <- ggplot(tabela5.skupine %>% right_join(sv, by = c("Drzava" = "adm0_a3")),
       aes(x=long, y=lat, group=group, fill=skupina)) +
  geom_polygon(color="grey")



#naredimo skupine igralcev glede na število točk in procent strela
tabela6 <- tabela[c("igralci", "tocke", "procent.strela")]
rownames(tabela6) <- tabela6$igralci
tabela6.norm <- tabela6 %>% select(-igralci) %>% scale()
k2 <- kmeans(tabela6.norm, 5, nstart=10000) 
tabela6.skupine <- data.frame(tabela6, skupina = factor(k2$cluster))
lin <- lm(data = tabela6, tocke ~ procent.strela)

slika13 <- ggplot(tabela6.skupine,
                             aes(x=tocke, y=procent.strela, color=skupina)) + 
                              geom_point() + geom_text(aes(label=ifelse(procent.strela>20 & tocke>50,as.character(igralci),'')))


#naredimo še predikcijo za tiste, ki so rojeni v prvem polletju in drugem polletju
a.skupina[a.skupina$tocke < 65,] <- NA
a.skupina <- a.skupina[!is.na(a.skupina$tocke),]

b.skupina[b.skupina$tocke < 65,] <- NA
b.skupina <- b.skupina[!is.na(b.skupina$tocke),]
slika14 <- ggplot(a.skupina, aes(x = tocke, y=procent.strela)) + geom_point(colour="red") + geom_smooth(method="lm")
  
slika15 <- ggplot(b.skupina, aes(x = tocke, y=procent.strela)) + geom_point(colour="blue") + geom_smooth(method="lm")



