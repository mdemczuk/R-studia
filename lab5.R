setwd("C:\\Users\\marty\\Documents\\studia\\semestr 4\\rpis2")

#Zadanie2

studentData <- read.csv("dane.csv", sep=",") 
studentData


arithmetic_mean <- function(data_frame){
  srednia <- rep(0, 20)
  for(i in 1:20){
    sum <- 0
    for(j in 4:9){
      sum <- sum+data_frame[i,j]
    }
    av = sum / 6
    srednia[i] <- av
  }
  cat(srednia, ' ')
  data_frame <<- cbind(data_frame, srednia)
  
}

arithmetic_mean(studentData)

average_info <- data.frame(id = rep(0, 20), av = rep(0, 20))
for(i in 1:20){
  average_info[i, 1] <- studentData[i, 3]
  average_info[i, 2] <- studentData[i, 10]
}


plot(x = studentData$nr_indeksu, y = studentData$srednia,
     xlab = "nr indeksu", ylab = "srednia",
     main = "Srednia ocen",
     col = "blue", 
     pch = 16 
)

max_av <- with(average_info, average_info[av == max(av), ])
points(max_av, col = 'green', pch = 17)

write.csv(studentData, file = "StudentData_readcsv.csv")



#Zadanie 3

covidData <- read.csv("SelectedPolandData.csv", sep=",") 
covidData

check_numeric <- function(df){
  for(i in 1:7){
    if(is.numeric(df[,i]) == TRUE){
      min_val <- min(df[,i])
      max_val <- max(df[,i])
      mean_val <- mean(df[,i])
      cat('Kolumna ', i, '\nMinimalna wartosc = ',
          min_val, '\nMaksymalna wartosc = ',
          max_val, '\nSrednia wartosc = ', mean_val, '\n')
    }
  }
}

check_numeric(covidData)

#

even_id <- seq(2, nrow(covidData), 2)
covidData[even_id,]

#

cases <- with(covidData, covidData[(total_cases < 100 & total_deaths == 0), ])
cases
cat(nrow(cases)) # pokazuje ile rekordów zostalo wybranych


#

plot(x = as.Date(cases$date), y = cases$total_cases,
     xlab = "data", ylab = "liczba wszystkich przypadkow",
     main = "Zaleznosc miedzy dniem a liczba wszystkich przypadkow",
     col = "blue", 
     pch = 16 
)
