plot(numbers <- 1:10)

install.packages("MASS")
library(MASS)
help("data")
help.search("data")
help("data")
data("cats")
plot(cats)
plot(cats$Sex)

ramka <- data.frame(id = c(1:3), wiek = c(21:23), czyKobieta = c(T, T, F))
ramka
ramka$wiek
ramka[ ,2] #wyswietlanie drugiej kolumny
ramka[2, ] #wyswietlanie drugiego wiersza
ramka [2] #wyswietlanie drugiego argumentu (i wiersza, i kolumny)

c(typeof(TRUE), typeof(1L), typeof(5.5), typeof(3.0i), typeof(2), typeof("siedem"))


zreplikowany <- rep(TRUE, 3)
zreplikowany  


#Zadanie 1

a <- 9
b <- 5
a %% b
a %/% b

#Zadanie 2

#Funkcja help.search() wyszukuje slowa kluczowe wsród stron z pomoca w R. 
#Funkcja q() sluzy do zakonczenia obecnej sesji w R.
#Funkcja demo() sluzy do demonstrowania niektórych skryptóW napisanych w R.
#Funkcja args() wyswietla nazwy argumentów i odpowiadajacym im wartosciom w funkcji.
#Funkcja apropos() zwraca nazwy obiektów, które w jakikolwiek sposób pokrywaja sie z frazami wprowadzonymi do funkcji.
#Funkcja example() kompiluje kod z sekcji Examples z pomocy R wraz z mozliwymi wyjatkami.


#Zadanie 3

install.packages("txtplot")
library("txtplot")
txtplot(cars[,1], cars[,2], xlab = "speed", ylab = "distance")

#Txtplot wyswietla uproszczony wykres funkcji utworzony ze znaków ascii bezposrednio w oknie terminala.
#Wynikiem dzialania polecenia zawartego w tresci zadania jest uproszczony wykres, na którym zaznaczone sa punkty odpowiadajace przebytej przez samochód odleglosci w zaleznosci od predkosci.


#Zadanie 4

sqrt(7)
log(23)
sin(43)
sin(90)
sin(60)
sqrt(9)
log(2.71)
log10(100)
log10(32)


#Zadanie 5

a = b = 3
# a i b maja wartosc 3.
a <- b <- 3
# a i b maja wartosc 3.
a = b <- 3
# a i b maja wartosc 3.
a <- b = 3
#operator '<-' ma wyzszy priorytet od operatora '=', dlatego najpierw do a przypisywana jest wartosc b, a dopiero potem do b przypisywana jest wartosc 3.
#blad jest dlatego, bo wartosc zmiennej b nie jest zadeklarowana w momencie przypisywania jej do zmiennej a.


#Zadanie 6

x <- 7
c(typeof(x))
#Jest to zmienna typu double.

mode(x)

z <- 1

class(x)

class(z) <- 8

x
z

#Zadanie 7

cat("Hello World!\n")
print("Hello World!\n")

#Cat wyswietla tekst na ekranie, print zwraca obiekt podany do funkcji.

#Zadanie 8

y = 0
assign("y", 8)
y

#Zadanie 9

A <- matrix(1:4, 2, 2)
B <- matrix(1:4, 2, 2)
C <- A %*% B
C

#Zadanie 10

wektor <- c(1, 10, NaN)
wektor
'NaN' %in% wektor
match('NaN', wektor)

#Mozna uzyc zarówno funkcji %in%, jak i funkcji match.