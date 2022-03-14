#!/usr/bin/env Rscript

if(!require(Hmisc)){
  install.packages("Hmisc")
}
if(!require(dplyr)){
  install.packages("dplyr")
}
if(!require(ggpubr)){
  install.packages("ggpubr")
}
if(!require(car)){
  install.packages("car")
}
if(!require(dunn.test)){
  install.packages("dunn.test")
}
if(!require(FSA)){
  install.packages("FSA")
}
if(!require(knitr)){
  install.packages("knitr")
}
if(!require(GmAMisc)){
  install.packages("GmAMisc")
}
if(!require(multcompView)){
  install.packages("multcompView")
}

library(Hmisc)
library(dplyr)
library(ggpubr)
library(car)
library(dunn.test)
library(FSA)
library(knitr)
library(GmAMisc)
library(multcompView)

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("Nalezy podac co najmniej jeden argument wejsciowy")
}

all_data <- read.csv2(file=args[1], header=TRUE, sep=";")
sink(file = "output.txt")

## imputowanie danych
is_na <- c(which(is.na(all_data)))
column_names <- colnames(all_data)

if(!is.null(is_na)){
  na_cols <- colnames(all_data)[ apply(all_data, 2, anyNA) ]
  for(col_name in na_cols){
    column_number <- which(column_names == col_name)
    column <- all_data[,column_number]
    na_rows <- c(which(is.na(column)))
    for(rownum in na_rows){
      row_number <- rownum
      meanVal <- mean(all_data[which(all_data[,1]==all_data[row_number,1]),column_number], na.rm=TRUE)
      all_data[row_number, column_number] <- impute(all_data[row_number, column_number], meanVal)
      cat("Zaimputowano dane w", row_number, "wierszu i", column_number, "kolumnie:", meanVal, "\n")
    }
  }
}
cat("\n\n===================\n")

all_data$grupa <- ordered(all_data$grupa, levels = c("KONTROLA", "CHOR1", "CHOR2"))
df <- data.frame(grupa = all_data[,1], column = c(rep(0, (length(all_data[,1])))))
groups <- c(levels(all_data$grupa))
rmcol <- c()
for(i in 2:length(column_names)){
  if(!all.is.numeric(all_data[,i])){
    rmcol <- append(rmcol, column_names[i])
  }
}
numericData = all_data[,!(names(all_data) %in% rmcol)]
col_names<-colnames(numericData)


## charakterystka grup

group_characteristics <- function(){
  cat("\n\tCharakterystyki dla grup")
  for(i in 1:(length(col_names))){
    df$column <- numericData[,i]
    if(all.is.numeric(df$column)){
      column_summary <- group_by(df, grupa) %>%
        summarise(
          count = n(),
          max = format(round(max(column, na.rm = TRUE), 2), nsmall = 2),
          min = format(round(min(column, na.rm = TRUE), 2), nsmall = 2),
          IQR = format(round(IQR(column, na.rm = TRUE), 2), nsmall = 2),
          mean = format(round(mean(column, na.rm = TRUE), 2), nsmall = 2),
          sd = format(round(sd(column, na.rm = TRUE), 2), nsmall = 2),
          median = format(round(median(column, na.rm = TRUE), 2), nsmall = 2)
        )
      
      print(kable(column_summary, format = "pandoc", caption = col_names[i]))
    }
  }
}

group_characteristics()


## analiza porownawcza grup

# zgodnosc danych z rozkladem normalnym - test shapiro
pValues <- data.frame(grupa = groups)
for(i in 2:(length(col_names))){
  col <- c(rep(0, (length(groups))))
  pValues$col <- col
  names(pValues)[i] <- col_names[i]
}

normality_test <- function(){
  cat("\n\n===================\n")
  cat("\n\tSprawdzanie zgodnosci z rozkladem normalnym - test Shapiro")
  for(i in 1:(length(col_names))){
    df$column <- numericData[,i]
    if(all.is.numeric(df$column)){
      column_ShapiroTest <- group_by(df, grupa) %>%
        summarise(
          statistic = format(round(shapiro.test(column)$statistic, 2), nsmall = 2),
          p.value = format(round(shapiro.test(column)$p.value, 2), nsmall = 2)
        )
      ggd = ggdensity(all_data, x = col_names[i],
                color = "grupa", fill = "grupa",
                palette = c("#99cc00", "#660099", "#0047b3"),
                ylab = "gestosc",
                xlab =  col_names[i]
      ) + facet_wrap(~ grupa, scales = "free")
      file_name = paste(col_names[i], "-ggdensity.png", sep="")
      png(file_name)
      print(ggd)
      dev.off()
      print(kable(column_ShapiroTest, format = "pandoc", caption = col_names[i]))
      for(j in 1:(length(groups))){
        pValues[j,i] <<-(column_ShapiroTest$p.value[(column_ShapiroTest$grupa == groups[j])])
      }
    }
  }
}

normalityCheck <- function(){
  for(i in 1:(length(col_names))){
    if(all.is.numeric(numericData[,i])){
      for(j in 1:(length(groups))){
        if(pValues[j,i] < 0.05){
          normalityValues[i] <<- 0
          pValuesTrue[j,i] <<- 0
        }else{
          normalityValues[i] <<- 1
          pValuesTrue[j,i] <<- 1
        }
      }
    }
  }
}

varEq <- c(rep(NA, length(col_names)))


varianceEquality <- function(){
  for(i in 1:(length(col_names))){
    if(all.is.numeric(numericData[,i])){
      leveneTestResult <- leveneTest(numericData[[i]] ~ factor(numericData[[1]]), data = numericData)
      result <- (leveneTestResult$"Pr(>F)"[1])
      varEq[i] <<- result
      if(result < 0.05){
        varianceEq[i] <<- 0
      }else{
        varianceEq[i] <<- 1
      }
    }
  }
}

multiple_groups_analysis <- function(){
  cat("\n\n===================\n")
  cat("\n\n\tAnaliza porownawcza wielu grup")
  for(i in 1:(length(col_names))){
    if(all.is.numeric(numericData[,i])){
      cat("\n\n-----------------\n")
      cat(">>>", col_names[i], "<<<\n\n")
      #jezeli rozklad jest zgodny z normalnym oraz wariancja jest jednorodna
      if((normalityValues[i] == 1) && (varianceEq[i] == 1)){
        
        ##############testANOVA
        cat("[Test ANOVA]\n")
        pvalueAOVtest <- summary(aov(numericData[[i]] ~ factor(numericData[[1]]), data = numericData))[[1]][["Pr(>F)"]][[1]]
        if(pvalueAOVtest < 0.05){
          cat("Dla", col_names[i], "pValue < 0.05 ISTNIEJA znaczace roznice miedzy grupami \n")
          ##############test Tukeya
          cat("\n[Test Tukeya]\n")
          TUKEY <- TukeyHSD(aov(numericData[[i]] ~ factor(numericData[[1]]), data = numericData))
          postTukey_text <- TukeyHSD(aov(numericData[[i]] ~ factor(numericData[[1]])))[1] %>%
            stargazer::stargazer(., type = "text", summary = FALSE)
          file_name = paste(col_names[i], "-tukeyPlot.png", sep="")
          png(file_name)
          print(plot(TUKEY, las = 1, col = "brown"))
          dev.off()
        }
        else{
          cat("Dla", col_names[i], "pValue > 0.05, NIE istnieja znaczace roznice miedzy grupami \n")
        }
      }
      #jezeli rozklad jest zgodny z normalnym ale wariancja nie jest jednorodna LUB jezeli rozklad jest niezgodny z normalnym
      if(((normalityValues[i] == 1) && (varianceEq[i] == 0)) || (normalityValues[i] == 0)){
        
        ##############testKruskala
        cat("[Test Kruskala-Wallisa]\n")
        pvalueKWtest <- kruskal.test(numericData[[i]] ~ factor(numericData[[1]]), data = numericData)$p.value
        #cat(pvalueKWtest, " \n")
        if(pvalueKWtest < 0.05){
          cat("Dla", col_names[i], "pValue < 0.05, ISTNIEJA znaczace roznice miedzy grupami \n")
          
          #test dunna
          cat("\n[Test Dunna]\n")
          result <- dunn.test(as.numeric(numericData[[i]]), numericData[[1]]) %>% as.data.frame
          file_name = paste(col_names[i], "-kwPlot.png", sep="")
          png(file_name)
          print(kwPlot(x = numericData[[i]], y = numericData$grupa, strip = TRUE, notch = FALSE, posthoc = TRUE))
          dev.off()
        }else{
          cat("Dla", col_names[i], "pValue > 0.05, NIE istnieja znaczace roznice miedzy grupami \n")
        }
      }
    }
  }
}


two_groups_analysis <- function(){
  cat("\n\n===================\n")
  cat("\n\n\tAnaliza porownawcza wielu grup")
  for(i in 1:(length(col_names))){
    if(all.is.numeric(numericData[,i])){
      cat("\n\n-----------------\n")
      cat(">>>", col_names[i], "<<<\n\n")
      #jezeli rozklad jest zgodny z normalnym oraz wariancja jest jednorodna
      if((normalityValues[i] == 1) && (varianceEq[i] == 1)){
        
        #####test t-Studenta
        cat("[Test t-Studenta]\n")
        pvalueTtest <- t.test(numericData[[i]] ~ factor(numericData[[1]]), data = numericData, var.equal = TRUE)$p.value
        if(pvalueTtest < 0.05){
          cat("Dla", col_names[i], "pValue < 0.05, ISTNIEJA znaczace roznice miedzy grupami \n")
        }else{
          cat("Dla", col_names[i], "pValue > 0.05, NIE istnieja znaczace roznice miedzy grupami \n")
        }
      }
      if((normalityValues[i] == 1) && (varianceEq[i] == 0)){
        
        #####test Welcha
        cat("[Test Welcha]\n")
        pvalueTtest <- t.test(numericData[[i]] ~ factor(numericData[[1]]), data = numericData, var.equal = FALSE)$p.value
        if(pvalueTtest < 0.05){
          cat("Dla", col_names[i], "pValue < 0.05, ISTNIEJA znaczace roznice miedzy grupami \n")
        }else{
          cat("Dla", col_names[i], "pValue > 0.05, NIE istnieja znaczace roznice miedzy grupami \n")
        }
      }
      if((normalityValues[i] == 0) && (varianceEq[i] == 0)){
        cat("[Test Wilcoxona]\n")
        pvalueWilcoxTest <- wilcox.test(numericData[[i]] ~ factor(numericData[[1]]), data = numericData)$p.value
        cat(pvalueWilcoxTest, " \n")
        if(pvalueWilcoxTest < 0.05){
          cat("Dla", col_names[i], "pValue < 0.05, ISTNIEJA znaczace roznice miedzy grupami \n")
        }
        else{
          cat("Dla", col_names[i], "pValue > 0.05, NIE istnieja znaczace roznice miedzy grupami \n")
        }
      }
    }
  }
}




correlation_analysis <- function(){
  cat("\n\n===================\n")
  cat("\n\n\tAnaliza korelacji")
  for(group in groups){
    gr_num <- which(groups == group)
    selectedData <- numericData %>% filter(grupa == group)
    cat("\n\n-----------------\n")
    cat("\n\tAnaliza dla grupy", group, "\n")
    for(i in 1:(length(col_names))){
      for(j in 1:(length(col_names))){
        if((i != j) && (all.is.numeric(selectedData[,i])) && (all.is.numeric(selectedData[,j]))){
          #cat(col_names[i], "z", col_names[j], "\n")
          pvalue <- FALSE
          variances <- FALSE
          if(pValuesTrue[gr_num,i] == 1 && pValuesTrue[gr_num,j] == 1){
            pvalue <- TRUE
          }
          if(varianceEq[i] == 1 && varianceEq[j] == 1){
            variances <- TRUE
          }
          if(pvalue == TRUE && variances == TRUE){
            ##pearson
            cat("\n\n===================\n")
            cat("[Pearson test]\n")
            test <- cor.test(selectedData[[i]], selectedData[[j]], method = "pearson")
            if(test$p.value < 0.05){
              if(test$estimate > -1 && test$estimate < -0.7){
                cat("Bardzo silna korelacja ujemna")
              }
              if(test$estimate >= -0.7 && test$estimate < -0.5){
                cat("Silna korelacja ujemna")
              }
              if(test$estimate >= -0.5 && test$estimate < -0.3){
                cat("Korelacja ujemna o srednim natezeniu")
              }
              if(test$estimate >= -0.3 && test$estimate < -0.2){
                cat("Slaba korelacja ujemna")
              }
              if(test$estimate >= -0.2 && test$estimate < 0.2){
                cat("Brak korelacji")
              }
              if(test$estimate >= 0.2 && test$estimate < 0.3){
                cat("Slaba korelacja dodatnia")
              }
              if(test$estimate >= 0.3 && test$estimate < 0.5){
                cat("Korelacja dodatnia o srednim natezeniu")
              }
              if(test$estimate >= 0.5 && test$estimate < 0.7){
                cat("Silna korelacja dodatnia")
              }
              if(test$estimate >= 0.7 && test$estimate < 1){
                cat("Bardzo silna korelacja dodatnia")
              }
              cat(" atrybutu", col_names[i], "oraz", col_names[j], "(wspolczynnik korelacji", test$estimate, ")\n")
              
              if(test$estimate < -0.3 || test$estimate >= 0.5){             
                file_name = paste(colnames(selectedData)[i], "-and-", colnames(selectedData)[j], "-correlationPearson.png", sep="")
                png(file_name)
                print(ggscatter(selectedData, 
                                x = paste(colnames(selectedData)[i]), 
                                y = paste(colnames(selectedData)[j]), 
                                add = "reg.line", 
                                conf.int = TRUE, 
                                cor.coef = TRUE, 
                                cor.method = "pearson",
                                color = "grupa", 
                                fill = "grupa",
                                palette = c("#99cc00"),
                                ylab = paste(colnames(selectedData)[j]), 
                                xlab = paste(colnames(selectedData)[i])
                ))
                dev.off()
                cat("Utworzono wykres o nazwie", file_name, "\n")
              }
            }else{
              cat("Atrybuty", col_names[i], "oraz", col_names[j], "sa prawdopodobnie nieskorelowane\n")
            }
          }else{
            ##spearman
            cat("\n\n===================\n")
            cat("[Spearman test]\n")
            test <- cor.test(selectedData[[i]], selectedData[[j]], method = "spearman")
            if(test$p.value < 0.05){
              if(test$estimate > -1 && test$estimate < -0.7){
                cat("Bardzo silna korelacja ujemna")
              }
              if(test$estimate >= -0.7 && test$estimate < -0.5){
                cat("Silna korelacja ujemna")
              }
              if(test$estimate >= -0.5 && test$estimate < -0.3){
                cat("Korelacja ujemna o srednim natezeniu")
              }
              if(test$estimate >= -0.3 && test$estimate < -0.2){
                cat("Slaba korelacja ujemna")
              }
              if(test$estimate >= -0.2 && test$estimate < 0.2){
                cat("Brak korelacji")
              }
              if(test$estimate >= 0.2 && test$estimate < 0.3){
                cat("Slaba korelacja dodatnia")
              }
              if(test$estimate >= 0.3 && test$estimate < 0.5){
                cat("Korelacja dodatnia o srednim natezeniu")
              }
              if(test$estimate >= 0.5 && test$estimate < 0.7){
                cat("Silna korelacja dodatnia")
              }
              if(test$estimate >= 0.7 && test$estimate < 1){
                cat("Bardzo silna korelacja dodatnia")
              }
              cat(" atrybutu", col_names[i], "oraz", col_names[j], "(wspolczynnik korelacji", test$estimate, ")\n")
              
              if(test$estimate < -0.3 || test$estimate >= 0.5){             
                file_name = paste(colnames(selectedData)[i], "-and-", colnames(selectedData)[j], "-correlationSpearman.png", sep="")
                png(file_name)
                print(ggscatter(selectedData, 
                                x = paste(colnames(selectedData)[i]), 
                                y = paste(colnames(selectedData)[j]), 
                                add = "reg.line", 
                                conf.int = TRUE, 
                                cor.coef = TRUE, 
                                cor.method = "spearman",
                                color = "grupa", 
                                fill = "grupa",
                                palette = c("#99cc00"),
                                ylab = paste(colnames(selectedData)[j]), 
                                xlab = paste(colnames(selectedData)[i])
                ))
                dev.off()
                cat("Utworzono wykres o nazwie", file_name, "\n")
              }
            }else{
              cat("Atrybuty", col_names[i], "oraz", col_names[j], "sa prawdopodobnie nieskorelowane\n")
            }
          }
        }
      }
    }
  }
}

normality_test()
pValues <- na.omit(pValues)
cat("\n\n===================\n")
cat("\n\tpValues dla testu Shapiro\n\n")
print(pValues)
normalityValues <- c(rep(-1, length(col_names)))
pValuesTrue <- data.frame(grupa = groups)
for(i in 2:(length(col_names))){
  col <- c(rep(0, (length(groups))))
  pValuesTrue$col <- col
  names(pValuesTrue)[i] <- col_names[i]
}


normalityCheck()

varianceEq <- c(rep(-1, length(col_names)))

varianceEquality()

cat("\n\n===================\n")
cat("\n\tpValues dla testu jednorodnosci wariancji\n\n")
for(i in 1:(length(col_names))){
  cat(col_names[i],":", varEq[i], "\n")
}



if(length(groups) == 2){
  two_groups_analysis()
}

if(length(groups) > 2){
  multiple_groups_analysis()
}

correlation_analysis()

sink(file = NULL)