help(sapply)
help(lapply)


#Zadanie 1

dzielnik <- function(wektor, b){
  for(i in 1:length(wektor)){
    if(wektor[i]%%b == 0){
      cat(wektor[i], " ")
    }
  }
}
liczby <- c(5, 12, 8, 15, 3, 14)
dzielnik(liczby, 3)


#Zadanie 2

pierwiastki <- function(a, b, c){
  delta <- b^2 - 4*a*c
  if(delta > 0){
    x1 <- (-b - delta^(1/2))/ 2*a
    x2 <- (-b + delta^(1/2))/ 2*a
    cat(x1, ' ', x2, ' ')
  }
  if(delta == 0){
    x <- (-b)/2*a
    cat(x, ' ')
  }
  if(delta < 0){
    cat('Delta mniejsza od 0, brak rozwiazan')
  }
}

pierwiastki(1, 4, 2)


#Zadanie 3

sort_numbers <- function(wektor){
  sorted <- c(sort(wektor, FALSE))
  for(i in 1:3){
    cat(sorted[i], ' ')
  }
}

liczby <- c(3, 5, 14, 19, 2, 6, 7, 8, 21)
sort_numbers(liczby)


#Zadanie 4

sort_numbers <- function(wektor){
  if(length(wektor) < 3){
    cat('Wektor jest za krotki')
  }
  else{
    sorted <- c(sort(wektor, FALSE))
    cat('Elementy najmniejsze: ')
    for(i in 1:3){
      cat(sorted[i], ' ')
    }
    cat('Elementy najwieksze: ')
    for(i in length(sorted):(length(sorted)-2)){
      cat(sorted[i], ' ')
    }
  }
}

liczby <- c(3, 5, 14, 19, 2, 6, 7, 8, 21)
sort_numbers(liczby)


#Zadanie 5

mnozenie <- function(x, y){UseMethod("mnozenie")}
mnozenie.default <- function(x, y){
  print(x*y)
}
mnozenie.matrix <- function(x, y){
  print(x %*% y)
}

mnozenie(matrix(1:2, 2, 2), matrix(3:4, 2, 2))
mnozenie(c(1, 2, 4), c(2, 3, 4))

