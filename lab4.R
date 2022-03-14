# Zadanie 4
wybraneAuta <- head(cars, 10)

wybraneAuta
plot(wybraneAuta)

plot(x = wybraneAuta$speed, y = wybraneAuta$dist,
     xlab = "szybkosc", ylab = "dystans",
     main = "Wykres szybkosc vs dystans",
     col = "blue", 
     pch = 16 
)
abline(h = mean(wybraneAuta$dist), col="red") # v - wertykalnie, h - horyzontalnie

mean(wybraneAuta$dist)

abline(v = mean(wybraneAuta$speed), col="green", lty = 2, lwd = 3)



# Zadanie 5

wybraneAuta
wybraneAuta$speed
najszybsze <- wybraneAuta$speed[wybraneAuta$speed>9]
najszybsze
idNajszybsze<-which(wybraneAuta$speed>9)
idNajszybsze

plot(wybraneAuta$speed,
     xlab = "numer auta",
     ylab = "szybkosc",
     main = "Wykres szybkosci",
     col = "blue", 
     pch = 20 
)
points(idNajszybsze, najszybsze, col = "red", pch = 17)
abline(h = mean(wybraneAuta$speed), col="red") # v - wertykalnie, h - horyzontalnie
text(2.5, 8.3, "srednia szybkosc", col = "red")

najwolniejsze <- wybraneAuta$speed[wybraneAuta$speed<7]
idNajwolniejsze <- which(wybraneAuta$speed<7)
points(idNajwolniejsze, najwolniejsze, col = 'purple', pch = 15)



# Zadanie 7

wiekPrzypadkowychLudzi2 <- data.frame(
  grupa = c("kobieta", "mezczyzna", "dziecko"),
  wiek = c(27,30,5,25,35,10,29,29,7,40,42,15)
)


par(mfrow=c(1, 2)) 
boxplot(wiekPrzypadkowychLudzi2$wiek ~ wiekPrzypadkowychLudzi2$grupa,
        xlab = "grupa",
        ylab = "wiek",
        data = wiekPrzypadkowychLudzi2, col = c("#99cc00", "#660099", "#0047b3")
)

stripchart(wiekPrzypadkowychLudzi2$wiek ~ wiekPrzypadkowychLudzi2$grupa, 
           xlab = "grupa",
           ylab = "wiek",
           data = wiekPrzypadkowychLudzi2, col = c("#99cc00", "#660099", "#0047b3"), pch = 17,
           vertical = TRUE
)
dev.off()



# Zadanie 8

install.packages("plotrix")
library("plotrix")

pewneStatystyki <- data.frame(
  grupa = c("lubi czekolade", "nie lubi czekolady", "brak opinii"),
  licznosc = c(86,30,9)
)

pie3D(pewneStatystyki$licznosc, labels = pewneStatystyki$grupa, col = c("#99cc00", "#660099", "#0047b3"))



# Zadanie 9

pie(pewneStatystyki$licznosc, 
    radius = 0.6,
    main = "Jak bardzo lubimy czekolade!",
    labels = paste(round(pewneStatystyki$licznosc / sum(pewneStatystyki$licznosc) * 100, 1), "%"), 
    col = c("#99cc00", "#660099", "#0047b3")
)
legend("topright", 
       cex = 0.6,
       c("osoby lubiace czkolade", "osoby nie lubiace czkolady", "osoby bez zdania"), 
       fill = c("#99cc00", "#660099", "#0047b3"),
       title = "legenda"
)
