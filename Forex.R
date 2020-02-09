source_dir <- "/Users/michal/git/umcs/r/forex/data"
setwd(source_dir)

# ############# Step 1
danenam <- c("AUDUSD_2004.csv",
             "EURUSD_2004.csv",
             "GBPUSD_2004.csv",
             "USDCAD_2004.csv",
             "USDCHF_2004.csv",
             "USDJPY_2004.csv")
tab <- read.csv(danenam[[1]])
names(tab)[2:6] = paste0(names(tab)[2:6], substr(danenam[[1]], 1, 6))
for (i in 2:6) {
  tab1 <- read.csv(danenam[[i]])
  names(tab1)[2:6] = paste0(names(tab1)[2:6], substr(danenam[[i]], 1, 6))
  tab <- merge(tab, tab1, by = "DATE")
}
write.table(tab, file = "dane01.tab")


# ############# Step 2   Zaleznosci pomiedzy róznicami w rynkach
tab <- read.table("dane01.tab")

podz <- c(-20, -3, -1, -0.5, 0, 0.5, 1, 3, 10)


tab1 <- tab[, c(2, 7, 12, 17, 22, 27)]
nr <- nrow(tab1)
tab2 <- tab1[1:(nr - 1),]
tab1 <- tab1[2:nr,]
tab3 <- (tab1 - tab2) * 100 / tab2       # Wzgledna procentowa róznica
tab4 <- tab3
for (i in seq_len(ncol(tab4))) {
  tab4[, i] <- cut(tab4[, i], podz)
}
wyn <- matrix(nrow = 6, ncol = 6)
for (i in 1:5) {
  for (j in (i + 1):6) {
    wyn[i, j] <- chisq.test(table(tab4[, i], tab4[, j]))$p.value
  }
}

# Korelacje  (>0.25 dodatnio skorel. <-0.25 ujemnie, pomiedzy nieskorelowane)
cor(tab3)

# Jezeli p<0.05 to zachowanie dwóch gield jest zalezne
table(tab4[, 1], tab4[, 2])
twyn <- chisq.test(table(tab4[, 1], tab4[, 2]))
twyn$observed
twyn$expected

# https://www.edukacjagieldowa.pl/gieldowe-abc/analiza-techniczna/narzedzia-analizy-technicznej/formacje-liniowe/
