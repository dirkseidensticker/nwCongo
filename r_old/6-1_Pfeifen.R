###########
# Pfeifen #
###########

library(RSQLite)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")


# PCA
# ===

df <- read.csv("../data/base/CongoPfeifen_CongoDB.csv", dec = ',')

df_a <- df[,c(4:18)]
df_a[is.na(df_a)] <- 0

pc <- princomp(df_a, cor=TRUE, scores=TRUE)

#pdf("../output/figs/7_Gassmann2003_PCA.pdf", width = 12, height = 6)
par(mfrow=c(1,2))
plot(pc,type="lines", main = "")
biplot(pc)
#dev.off()