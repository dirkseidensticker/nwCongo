####################################
# Bedeutung der Rouletteverzierung #
####################################

library(ca)
library(gclus)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(RSQLite)
library(scales)
library(sqldf)
library(vegan)

# Fabrverlauf festlegen
rf <- colorRampPalette(brewer.pal(9,'OrRd'))
r <- rf(32)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# Kartierung der Verbreitung von Roulette im Arbeitsgebiet

df = dbGetQuery(con, "SELECT
                    t_Obj.objID AS objID,
                    [t_Ort].[ort_name] || ' (Fpl. ' || [t_ort].[Kat-Nr] || ')' AS Ort,
                    t_Ort.x_long AS x_long,
                    t_Ort.y_lat AS y_lat,
                    t_K_Verz.verzName AS Typ,
                    t_obj.Typ AS Gruppe
                  FROM ((t_Ort LEFT JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                    LEFT JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID) t_Obj INNER JOIN 't_ObjPosVerz' ON t_Obj.objID = 't_ObjPosVerz'.objID
                    INNER JOIN t_K_Pos ON 't_ObjPosVerz'.posID = t_K_Pos.posID
                    INNER JOIN t_K_Verz ON 't_ObjPosVerz'.verzID = t_K_Verz.verzID
                  WHERE (((t_K_Verz.verzName) Like '%V08%'))")
head(df)

qmap("Dongou", zoom = 7, color = 'bw', maptype = 'roadmap', legend = "topleft") + 
  geom_hex(aes(x = x_long, y = y_lat), data = df, alpha = .7) + 
  scale_fill_gradientn(colours=rev(heat.colors(12))) + 
  geom_point(aes(x = x_long, y = y_lat), data = df)
ggsave("../output/figs/5_Roulette-Map.pdf", width = 11, height = 11)

# Facet-Plot
ggplot(df, aes(x = x_long, y = y_lat)) + 
  geom_point() + 
  facet_wrap(~ Typ, ncol = 6) + 
  coord_equal() + 
  geom_text(aes(x = x_long+0.4, y = y_lat+0.07, label = Ort), data = df, size = 1) + 
  scale_x_continuous(breaks = pretty_breaks(n = 5)) + 
  scale_y_continuous(breaks = pretty_breaks(n = 5)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("../output/figs/5_Roulette_facet_A.pdf", width = 11, height = 8.5)

# CA der Roulette-Keramik
# =======================

df <- sqldf('select * from df where Gruppe != "" and Gruppe Not Like "%(?)%"')

df_pivot <- tapply(df$objID, list(df$Typ, df$Gruppe), length)
df_pivot[is.na(df_pivot)] <- 0

# nach Borcard/Gillet u.a. 2011: 133 ff.
# --------------------------------------
spe <- df_pivot

spe.ca <- cca(spe)
spe.ca

# Eigenvalues herausziehen
ev <- eigenvals(spe.ca)
ev <- unclass(ev)
ev <- data.frame(ev)

pdf("../output/figs/5_Roulette_CA.pdf", width=6, height=6)
plot(spe.ca, scaling=1, xlab = paste("CA1 (",round(ev[1,], digits = 2),"%)"), ylab = paste("CA2 (",round(ev[2,], digits = 2),"%)"))
dev.off()

# Cluster-Analyse
# ===============
# nach Borcard/Gillet u.a. 2011: 65 ff.
# -------------------------------------

spe <- df_pivot

spe.norm <- decostand(spe, "normalize")
spe.ch <- vegdist(spe.norm, "euc")
spe.ch.ward <- hclust(spe.ch, method = "ward.D2")
spe.ch.ward

# plot(spe.ch.ward)

summary(spe.ch.ward)

# plot(spe.ch.ward$height, nrow(df_pivot):2, type = "S")


# Optimal number of clusters according to Mantel statistic (Pearson)
# ******************************************************************

# Function to compute a binary distance matrix from groups
grpdist <- function(X)
{
  require(cluster)
  gr <- as.data.frame(as.factor(X))
  distgr <- daisy(gr, "gower")
  distgr
}

# Run based on the Ward clustering
kt <- data.frame(k=1:nrow(spe), r=0)
for (i in 2:(nrow(spe)-1)) {
  gr <- cutree(spe.ch.ward, i)
  distgr <- grpdist(gr)
  mt <- cor(spe.ch, distgr, method="pearson")
  kt[i,2] <- mt
}
# kt
k.best <- which.max(kt$r)

k.best

plot(kt$k, kt$r, type="h", main="Mantel-optimal number of clusters - Ward", 
     xlab="k (number of groups)", ylab="Pearson's correlation")
axis(1, k.best, paste("optimum", k.best, sep="\n"), col="red", font=2,
     col.axis="red")
points(k.best, max(kt$r), pch=16, col="red", cex=1.5)
cat("", "Mantel-optimal number of clusters k =", k.best, "\n", 
    "with a matrix linear correlation of", max(kt$r), "\n")
# optimale Anzahl Cluster = 5

# Silhouette plot of the final partition
# **************************************

# Choose the number of clusters
k <- k.best
# Silhouette plot
cutg <- cutree(spe.ch.ward, k=k)
sil <- silhouette(cutg, spe.ch)
rownames(sil) <- row.names(spe)

plot(sil, main="Silhouette plot - Chord - Ward", 
     cex.names=0.8, col=2:(k+1), nmax=100)

# Final dendrogram with the selected groups
# *****************************************

spe.chwo <- reorder.hclust(spe.ch.ward, spe.ch)

# Plot reordered dendrogram with group labels

# plot(spe.chwo, hang=-1, xlab="4 groups", sub="", 
#     ylab="Height", main="Chord - Ward (reordered)", 
#     labels=cutree(spe.chwo, k=k))
# rect.hclust(spe.chwo, k=k)

# Plot the final dendrogram with group colors (RGBCMY...)
# Fast method using the additional hcoplot() function:
# Usage:
# hcoplot(tree = hclust.object, diss = dissimilarity.matrix, k = nb.clusters, 
#	title = paste("Reordered dendrogram from",deparse(tree$call),sep="\n"))

# source("../../Developer/R/docs/Borcard, Gillet et al 2011 - Numerical ecology with R/Numerical Ecology/hcoplot.R")


# Function hcoplot()
# Reorder and plot dendrogram with colors for groups and legend
#
# Usage:
# hcoplot(tree = hclust.object, diss = dissimilarity.matrix, k = nb.clusters, 
#	title = paste("Reordered dendrogram from",deparse(tree$call),sep="\n"))
#
# License: GPL-2 
# Author: Francois Gillet, 23 August 2012

"hcoplot" <- function(tree, diss, k) #, title=paste("Reordered dendrogram from", deparse(tree$call), sep="\n"))
{
  require(gclus)
  gr <- cutree(tree, k=k)
  tor <- reorder.hclust(tree, diss)
  plot(tor, hang=-1, xlab=paste(length(gr),"Verzierungselemente"), sub=paste(k,"Cluster"), main = "") #, main=title)
  so <- gr[tor$order]
  gro <- numeric(k)
  for (i in 1:k)
  {
    gro[i] <- so[1]
    if (i<k) so <- so[so!=gro[i]]
  }
  rect.hclust(tor, k=k, border=gro+1, cluster=gr)
  legend("top", ncol = 3, paste("Cluster",1:k), pch = 2:(k+1), xpd = TRUE, inset = c(0,-.15), col=2:(k+1))
}

pdf("../output/figs/5_Roulette_Cluster.pdf", width=6, height=6)
hcoplot(spe.ch.ward, spe.ch, k=k)
dev.off()

# Cluster-Zuweisung herausholen:
spebc.ward.g <- cutree(spe.ch.ward, k)

# CA mit Clustern:
# siehe http://www.davidzeleny.net/anadat-r/doku.php/en:indirect_ordination_viz
plot(spe.ca)

pdf("../output/figs/5_Roulette_CA-2.pdf", width=6, height=6)
plot(spe.ca, 
     display = 'si', 
     type = 'n', 
     xlab = paste("CA1 (",round(ev[1,], digits = 2),"%)"), 
     ylab = paste("CA2 (",round(ev[2,], digits = 2),"%)"))
# for (i in unique (spebc.ward.g)) ordihull (spe.ca, groups = spebc.ward.g, show.group = i, col = i+1, draw = 'polygon', alpha = 25)
points(spe.ca, col = ((spebc.ward.g)+1), pch = (spebc.ward.g)+1)
text(spe.ca, labels = rownames(as.data.frame(spe.ca$rowsum)), pos = 4, cex = 0.6)
# text(spe.ca, display = "spe", head.arrow = 0.05, cex = 0.6, col = "red")
legend("bottomright", paste("Group", 1:k), pch = 2:(k+1), col = 2:(k+1))
dev.off()


?text()

plot(spe.ca)


# Heat map
# ********

# Heat map of the distance matrix ordered with the dendrogram
dend <- as.dendrogram(spe.chwo)
heatmap(as.matrix(spe.ch), Rowv=dend, symm=TRUE, margin=c(3,3))





# Nicht-Roulette
# --------------

df = dbGetQuery(con, "SELECT
                    t_Obj.objID,
                    t_Ort.ort_name,
                    t_Ort.x_long AS x_long,
                    t_Ort.y_lat AS y_lat,
                    t_K_Verz.verzName AS Typ
                  FROM ((t_Ort LEFT JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                    LEFT JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID) t_Obj INNER JOIN 't_ObjPosVerz' ON t_Obj.objID = 't_ObjPosVerz'.objID
                    INNER JOIN t_K_Pos ON 't_ObjPosVerz'.posID = t_K_Pos.posID
                    INNER JOIN t_K_Verz ON 't_ObjPosVerz'.verzID = t_K_Verz.verzID
                  WHERE (((t_K_Verz.verzName) Not Like '%V08%')
                    AND ((t_Ort.ort_lit) == 'DS'))")
head(df)

qmap("Dongou", zoom = 7, color = 'bw', maptype = 'roadmap', legend = "topleft") + 
  geom_hex(aes(x = x_long, y = y_lat), data = df, alpha = .7) + 
  scale_fill_gradientn(colours=rev(heat.colors(12))) + 
  geom_point(aes(x = x_long, y = y_lat), data = df)
# ggsave("../output/figs/5_Roulette-Map.pdf", width = 11, height = 8.5)

# Facet-Plot
ggplot(df, aes(x = x_long, y = y_lat)) + 
  geom_point(size = 1) + 
  facet_wrap(~ Typ, ncol = 14) + 
  coord_equal() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_x_continuous(breaks = pretty_breaks(n = 5)) + 
  scale_y_continuous(breaks = pretty_breaks(n = 5))
ggsave("../output/figs/5_Roulette_facet_B_nichtRoulette.pdf", width = 11, height = 8.5)




