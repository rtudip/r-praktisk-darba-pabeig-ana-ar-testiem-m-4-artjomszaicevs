install.packages("dplyr")
library(dplyr) 
dati <- read.table("variants17.txt", header = TRUE, sep = "\t", dec = ",", strip.white = TRUE, row.names = 1)
#head(dati)

cols_to_factor <- names(dati)[9:ncol(dati)]
dati[cols_to_factor] <- lapply(dati[cols_to_factor], factor)

sink("result.txt")

cat("Faktoru kopsavilkums:\n")
for (col in cols_to_factor) {
  cat("\nKolonna:", col, "\n")
  print(summary(dati[[col]]))
}

sl_by_b <- split(dati$Slope, dati$b)
cat("\nSlope pēc b sadalījums:\n")
print(sl_by_b)

dati$Average <- rowMeans(dati[, c("Slope", "Intercept", "adj.r.squared")])

std_dev_by_f <- dati %>% group_by(f) %>% summarise(StdDev = sd(Slope, na.rm = TRUE))
cat("\nStandartnovirze:\n")
print(std_dev_by_f)

pdati <- dati %>% filter(adj.r.squared > 0.7)

pdati$Slope <- 1 - (1/ pdati$Slope)

print(pdati)

sink()

library(ggplot2)

scat <- ggplot(dati, aes(x = MAD, y = Average)) + geom_point() + ggtitle("MAD vs Average")
ggsave("scatter_plot17.svg", plot = scat)

p_box <- ggplot(dati, aes(x = f, y = Intercept, fill = f)) + geom_boxplot() + ggtitle("Intercept pec f faktoriem")
ggsave("boxplot17.svg", plot = p_box)

most_frequent_level <- names(which.max(table(unlist(dati[, cols_to_factor]))))
filtered_rows <- pdati[grep(most_frequent_level, rownames(pdati)), ]

print(filtered_rows)