df <- read.csv("../data/processed/4_macrotraces.csv", row.names = 1)
df[is.na(df)] <- 0
# df = as.matrix(df)

library(CAseriation)

check.ca.plot(df,1,2) 

sort.table(df,1)

plot.clusters.rows(df,1,2)

plot.clusters.cols(df,1,2)

evaluate(df,1,2, which='R') 