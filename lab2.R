#Zadanie 3

p <- c(sample(1:100, 20, T))
numbers <- c(0)

for(i in 1:length(p)){
  if(p[i] <= 10){
    numbers[i] <- 1
  }
  if(p[i] > 10 && p[i] <= 40){
    numbers[i] <- 2
  }
  if(p[i] > 40){
    numbers[i] <- 3
  }
}

numbers


#Zadanie 6

liczba <- 1515
switch(class(liczba), 'factor' = 'typ czynnikowy', 'logical' = 'typ logiczny', 'numeric' = 'typ liczbowy', 'inny typ')

#Zadanie 8

j <- 1
numbers <- c(0)
for(i in 1:33){
  if(33%%i == 0){
    numbers[j] <- i
    j <- j+1
  }
}

numbers


#zadanie 9

fibnumbers <- c(0)
for(i in 2:15){
  if(i==2){
    fibnumbers[i] <- 1
  }
  if(i>2){
    fibnumbers[i] <- (fibnumbers[i-1] + fibnumbers[i-2])
  }
}
fibnumbers
which(fibnumbers == 233)


#Zadanie 10

ramka <- data.frame(id = c(1:10), n1 = c(sample(1:10, 10, F)), n2 = c(sample(10:20, 10, T)))
av1 <- 0
av2 <- 0
for(i in 1:10){
  av1 <- av1 + ramka[i, 2]
  av2 <- av2 + ramka[i, 3]
}

av1 <- av1 / 10
av2 <- av2 / 10

print(av1)
print(av2)