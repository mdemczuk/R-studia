#Zadanie 1

# W terminalu obecnym w programie RStudio wpisana zostala komenda: 
#"C:\Program Files\R\R-4.0.4\bin\R.exe" CMD BATCH --vanilla "--args 25-April-owid-covid-data.csv" lab5_zad1.R

#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Nalezy podac co najmniej jeden argument wejsciowy")
}

allCovidData <- read.csv(file=args[1], header=TRUE)

JapanData <- with(allCovidData, allCovidData[iso_code == "JPN", ])
JapanData
sink(file = "output.txt")
print(JapanData)
jpeg("covidJaponia.jpg", width = 1000, height = 600) 
plot(as.Date(JapanData$date), JapanData$new_cases, col = "red", 
     main = "Liczba nowych zarazen SARS-COV-2 w zaleznosci od dnia",
     ylab = "Liczba nowych przypadkow",
     xlab = "data",
     xlim = as.Date(c("2020-03-01", "2020-04-30"))
)
dev.off()

sink(file = NULL)