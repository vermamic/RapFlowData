newyork <- readHumdrum('1 New York/*.txt')
atlanta <- readHumdrum('2 Atlanta/*.txt')
la <- readHumdrum('3 Los Angeles/*.txt')
nola <- readHumdrum('4 New Orleans/*.txt')
chicago <- readHumdrum('5 Chicago/*.txt')
east <- readHumdrum('6 East Coast/*.rap')
west <- readHumdrum('7 West Coast/*.rap')

eastDF <- as.data.frame(east)

indices_east <- c()
for (i in 1:nrow(eastDF)){
  if(eastDF[i,7] == "."){
    indices_east <- append(indices_east, i)
  }
}

eastDF_new <- eastDF[-indices_east,]

indices_nonstress <- c()

for (i in 1:nrow(eastDF_new)){
  if(eastDF_new[i,2] == "0"){
    indices_nonstress <- append(indices_nonstress, i)
  }
}

eastDF_stress <- eastDF_new[-indices_nonstress,]

percentages <- c()

for (i in 1:length(table(duration(eastDF_stress[,1])))){
  percentages <- append(percentages, table(duration(eastDF_stress[,1]))[i]/nrow(eastDF_stress))
}

percentagesDF <- as.data.frame(percentages)
labels <- c(0.03125, 0.041666, 0.0625, 0.09375, 0.125, 0.15625, 0.1875, 0.25, 0.375)
percentagesDF$percent = round(100*percentagesDF$percentages/sum(percentagesDF$percentages), digits = 1)
pie(percentagesDF$percent, labels = humdrumR::as.fraction(labels))

barplot(prop.table(percentages) * 100, ylim = c(0,50), names = humdrumR::as.fraction(labels), xlab = "Note Durations", ylab = "Relative Frequency", main = "Relative frequency (%) of note durations with stressed syllables, East Coast",col = rainbow(3))

westDF <- as.data.frame(west)

indices_west <- c()
for (i in 1:nrow(westDF)){
  if(westDF[i,7] == "."){
    indices_west <- append(indices_west, i)
  }
}

westDF_new <- eastDF[-indices_west,]

indices_nonstress <- c()

for (i in 1:nrow(westDF_new)){
  if(westDF_new[i,2] == "0"){
    indices_nonstress <- append(indices_nonstress, i)
  }
}

westDF_stress <- westDF_new[-indices_nonstress,]

percentages2 <- c()

for (i in 1:length(table(duration(westDF_stress[,1])))){
  percentages2 <- append(percentages2, table(duration(westDF_stress[,1]))[i]/nrow(westDF_stress))
}

percentagesDF2 <- as.data.frame(percentages2)
labels2 <- c(0.03125, 0.041666, 0.0625, 0.09375, 0.125, 0.15625, 0.1875, 0.25, 0.375, 0.5, 0.75, 0.8125, 0.875)
percentagesDF2$percent = round(100*percentagesDF2$percentages/sum(percentagesDF2$percentages), digits = 1)
pie(percentagesDF2$percent, labels2 = humdrumR::as.fraction(labels))

percentagesCombined <- cbind(percentages[1:9], percentages2[1:9])

percentagesCombined <- as.data.frame(cbind(percentages,percentages2))

percentagesCombined[9:13,1] <- 0

barplot(t(as.matrix(percentagesCombined)), beside = TRUE, ylim = c(0,0.5), names = humdrumR::as.fraction(labels2), xlab = "Note Durations", ylab = "Relative Frequency", main = "Relative frequency of note durations with stressed syllables, East v. West Coast")

legend("topright", legend = c("east", "west"), fill = c("black", "gray"))
