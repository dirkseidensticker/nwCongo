###########################################
# 7 Untersuchungen durch G. Gassmann 2003 #
###########################################

# Haupt- und Spurenelemente
df_spe <- read.csv("../data/base/Gassmann2003_Elemente.csv", dec = ',', row.names = 1)
head(df_spe)

df_spe_t <- data.frame(t(df_spe))

# Daten zu den Proben
df_env <- read.csv("../data/base/Gassmann2003_Proben.csv", row.names = 1)
head(df_env)

# Hauptkomponentenanlayse
# =======================

pc <- princomp(df_spe_t, cor=TRUE, scores=TRUE)

pdf("../output/figs/7_Gassmann2003_PCA.pdf", width = 12, height = 6)
par(mfrow=c(1,2))
plot(pc,type="lines", main = "")
biplot(pc)
dev.off()