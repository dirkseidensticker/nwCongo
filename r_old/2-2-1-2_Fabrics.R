##########################
# 2.2.2 Scherben-fabrics #
##########################

library(ca)
library(gclus)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(reshape)
library(rgl)
library(RSQLite)
library(scales)
library(sqldf)
library(vegan)

# Fabrverlauf festlegen
rf <- colorRampPalette(brewer.pal(9,'OrRd'))
r <- rf(32)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# Korrespondenzanalyse
# ====================

df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                t_Obj.Typ, 
                t_Obj.Fabric
                FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
                WHERE (((t_Ort.ort_lit)='DS') 
                AND ((t_Obj.Fabric) Not Like '%?%')
                AND ((t_Obj.Fabric) Not Like '%/%')
                AND ((t_Obj.Typ) Not Like '%?%')
                AND ((t_Obj.Typ) Not Like '%/%'))")
head(df)

df_pivot <- tapply(df$objID, list(df$Typ, df$Fabric), length)
df_pivot[is.na(df_pivot)] <- 0

pdf("../output/figs/2-2-1-2_Keramik_Fabrics-Stilgr_CA_C.pdf", width=12, height=6, useDingbats=FALSE)
par(mfrow = c(1, 2))
plot(ca(df_pivot), labels = c(2, 0))
title(sub = "1. und 2. Hauptachse")
plot(ca(df_pivot), dim = c(2, 3),labels = c(2, 0))
title(sub = "2. und 3. Hauptachse")
title("CA aus Fabrics und Stilgruppen", line = -2, outer = TRUE)
dev.off()




plot(ca(df_pivot))

plot(ca(df_pivot))




# fabric 1
# ======== 

df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                t_Ort.ort_kurz, 
                't_Ort'.'ort_kurz' || '/' || 't_Ort'.'x_long' || '/' || 't_Ort'.'y_lat' AS SITE, 
                t_Komplex.bef_nr, 
                t_Obj.Individuum, 
                t_Obj.MagerungArt, 
                t_Obj.Fabric, 
                t_Ort.x_long, 
                t_Ort.y_lat 
                FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
                WHERE (((t_Ort.ort_lit)='DS') 
                AND ((t_Obj.Fabric) Like '1%')
                OR ((t_Obj.Fabric) Like '2%')
                AND ((t_Obj.Fabric) Not Like '%/%'))")
head(df)

# > Problem: ((t_Obj.Fabric) Like '1%' liefert auch alle 10-18er Fabrics aus
# erstmal alles mit 1 aus der DB

# letzte Stelle raus (siehe CA) - das sollte nur noch 1 und 10 etc übriglassen
df$Fabric_1 <- substr(df$Fabric, 1, nchar(df$Fabric)-1)

# nur die 1er
df <- sqldf('select * from df where Fabric_1 = 1 or Fabric_1 = 2')



# **Kartierung**
# --------------

qmap("Dongou", zoom = 7, color = 'bw', legend = "topleft") + 
  stat_density2d(data = df, aes(x = x_long, y = y_lat, fill = ..level.., alpha = ..level..), bins = 20, alpha = 0.3, geom = "polygon") + 
  geom_point(aes(x = x_long, y = y_lat), data = df, size=1.5, shape = 19) + 
  geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + 
  ggtitle("fabric 1\n")

# Mit roten Isolinien:

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + stat_density2d(data = df, aes(x = x_long, y = y_lat), color = 'red') + geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + ggtitle("fabric 1\n")
#ggsave(file = "../output/figs/2-2-1-2_Keramik_fabric1_Verbreitung_Iso.pdf", width = 11, height = 11)

# Variante mit hexbin
qmap("Dongou", zoom = 7, maptype = 'hybrid') + 
  geom_hex(aes(x = x_long, y = y_lat), data = df, alpha = .7) + 
  scale_fill_gradientn(colours=rev(heat.colors(12))) + 
  geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + 
  geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + 
  ggtitle("fabric 1\n")
#ggsave("../output/figs/2-2-1-2_Keramik_fabric1_Verbreitung_Hex.pdf", width = 11, height = 11)



# Prozent je Fpl darstellen
# -------------------------

df_a <- data.frame(tapply(df$objID, list(df$SITE), length))
names(df_a)[1] <- "value"
df_a$variable <- rownames(df_a) 
head(df_a)

df_merge <- merge(x = df_a, y = df_all_a, by = 'variable')

# Prozent je Fpl ausrechnen
df_merge$pc <- df_merge$value / df_merge$value_all * 100

# variable-Feld wieder auseinander schneiden - um Koordinaten zu bekommen
df_merge <- transform(df_merge, test = do.call(rbind, strsplit(df_merge$variable, '/', fixed = TRUE)), stringsAsFactors = F)

# Spalten in numeric umwandeln
df_merge$test.2 <- as.numeric(as.character(df_merge$test.2))
df_merge$test.3 <- as.numeric(as.character(df_merge$test.3))

# Kartierung
qmap("Dongou", zoom = 7, maptype = 'roadmap', color = 'bw') + 
  geom_point(aes(x = test.2, y = test.3, size = pc, fill = pc), data = df_merge, shape = 22, alpha = 0.8) + 
  scale_size_continuous(range = c(1,10), name  ="Anteil je\nFpl. [%]") + 
  scale_fill_gradientn(colours = rev(heat.colors(12)), name  ="Anteil je\nFpl. [%]") + 
#  ggtitle("Äquator-Co-Tradition - fabric 1\n") + 
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("../output/figs/2-2-1-2_Keramik_fabric1-2_Verbreitung_PC.pdf", width = 8, height = 8)

# ---------------------

# Beobachtung: die Grabungen in PIK & MUN stören > ohne Grabungen, nur Obfl

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "data/CongoDB.sqlite")
df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                t_Ort.ort_kurz, 
                t_Komplex.bef_nr, 
                t_Obj.Individuum, 
                t_Obj.MagerungArt, 
                t_Obj.Fabric, 
                t_Ort.x_long, 
                t_Ort.y_lat 
                FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
                WHERE (((t_Ort.ort_lit)='DS') 
                AND ((t_Obj.MagerungDichte) Like '<1')
                AND ((t_Komplex.bef_art) Like 'Oberfl'))")
head(df)

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + stat_density2d(data = df, aes(x = x_long, y = y_lat), color = 'red') + geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + ggtitle("fabric - nur Oberfl 1\n")
#ggsave(file = "../output/figs/2-2-1-2_Keramik_fabric1_ObflFpl-Verbreitung_Iso.pdf", width = 11, height = 11)

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + geom_hex(aes(x = x_long, y = y_lat), data = df, alpha = .7) + scale_fill_gradientn(colours=rev(heat.colors(12))) + geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + ggtitle("fabric - nur Obfl 1\n")
#ggsave("../output/figs/2-2-1-2_Keramik_fabric1_ObflFpl-Verbreitung_Hex.pdf", width = 11, height = 11)

# fabric 3
# ======== 

df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                't_Ort'.'ort_kurz' || '/' || 't_Ort'.'x_long' || '/' || 't_Ort'.'y_lat' AS SITE, 
                t_Ort.ort_kurz, 
                t_Komplex.bef_nr, 
                t_Obj.Individuum, 
                t_Obj.MagerungArt, 
                t_Obj.Fabric, 
                t_Ort.x_long, 
                t_Ort.y_lat 
                FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
                WHERE (((t_Ort.ort_lit)='DS') AND ((t_Obj.Fabric) Like '3%'))")
head(df)


# Kartierung
# ----------

# Legende: Kreise = unsicher, Punkte = sicher

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + geom_hex(aes(x = x_long, y = y_lat), data = df, alpha = .7) + scale_fill_gradientn(colours=rev(heat.colors(12))) + geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + ggtitle("fabric 3\n")
#ggsave("../output/figs/2-2-1-2_Keramik_fabric3_Verbreitung_Hex.pdf", width = 11, height = 11)

#### Mit roten Isolinien:
qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + stat_density2d(data = df, aes(x = x_long, y = y_lat), color = 'red') + geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + ggtitle("fabric 3\n")
#ggsave("../output/figs/2-2-1-2_Keramik_fabric3_Verbreitung_Iso.pdf", width = 11, height = 11)

# Prozent je Fpl darstellen
# -------------------------

df_a <- data.frame(tapply(df$objID, list(df$SITE), length))
names(df_a)[1] <- "value"
df_a$variable <- rownames(df_a) 
head(df_a)

df_merge <- merge(x = df_a, y = df_all_a, by = 'variable')

# Prozent je Fpl ausrechnen
df_merge$pc <- df_merge$value / df_merge$value_all * 100

# variable-Feld wieder auseinander schneide - um Koordinaten zu bekommen
df_merge <- transform(df_merge, test=do.call(rbind, strsplit(df_merge$variable, '/', fixed = TRUE)), stringsAsFactors = F)

# Spalten in numeric umwandeln
df_merge$test.2 <- as.numeric(as.character(df_merge$test.2))
df_merge$test.3 <- as.numeric(as.character(df_merge$test.3))

# Kartierung
qmap("Dongou", zoom = 7, maptype = 'roadmap', color = 'bw') + 
  geom_point(aes(x = test.2, y = test.3, size = pc, fill = pc), data = df_merge, shape = 22, alpha = 0.8) + 
  scale_size_continuous(range = c(1,10), name  ="Anteil je\nFpl. [%]") + 
  scale_fill_gradientn(colours = rev(heat.colors(12)), name  ="Anteil je\nFpl. [%]") + 
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggtitle("Ngoko-Tradition - fabric 3\n")
ggsave("../output/figs/2-2-1-2_Keramik_fabric3_Verbreitung_PC.pdf", width = 8, height = 8)



# Schamott-Magerung (fabric 9)
# ============================

df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                t_Ort.ort_kurz, 
                't_Ort'.'ort_kurz' || '/' || 't_Ort'.'x_long' || '/' || 't_Ort'.'y_lat' AS SITE, 
                t_Komplex.bef_nr, 
                t_Obj.Individuum, 
                t_Obj.MagerungArt, 
                t_Obj.Fabric, 
                t_Ort.x_long, 
                t_Ort.y_lat 
                FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
                WHERE (((t_Ort.ort_lit)='DS') 
                AND ((t_Obj.Fabric) Like '9%'))")
head(df)

# WICHTIG: ggmap funktioniert nur mit R 3.0, nicht in R 3.1!!!

# Kartierung
# ----------

# Legende: Kreise = unsicher, Punkte = sicher

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + 
  stat_density2d(data = df, aes(x = x_long, y = y_lat, fill = ..level.., alpha = ..level..), bins = 20, alpha = 0.3, geom = "polygon") + 
  geom_point(aes(x = x_long, y = y_lat), data = df, size=1.5, shape = 19) + 
  geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + 
  ggtitle("Schamott-Magerung - fabric 9\n")
# ggsave("../output/figs/2-2-2_Keramik_fabric9_Schamott_Verbreitung.pdf")

#### Mit roten Isolinien:

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + 
  stat_density2d(data = df, aes(x = x_long, y = y_lat), color = 'red') + 
  geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + 
  geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + 
  ggtitle("Schamott-Magerung - fabric 9\n")
#ggsave(file = "../output/figs/2-2-1-2_Keramik_fabric9_Schamott_Verbreitung_Iso.pdf", width = 11, height = 11)

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + geom_hex(aes(x = x_long, y = y_lat), data = df, alpha = .7) + scale_fill_gradientn(colours=rev(heat.colors(12))) + geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + ggtitle("Schamott-Magerung - fabric 9\n")
#ggsave(file = "../output/figs/2-2-1-2_Keramik_fabric9_Schamott_Verbreitung_Hex.pdf", width = 11, height = 11)

# Prozent je Fpl darstellen
# -------------------------

df_a <- data.frame(tapply(df$objID, list(df$SITE), length))
names(df_a)[1] <- "value"
df_a$variable <- rownames(df_a) 
head(df_a)

df_merge <- merge(x = df_a, y = df_all_a, by = 'variable')

# Prozent je Fpl ausrechnen
df_merge$pc <- df_merge$value / df_merge$value_all * 100

# variable-Feld wieder auseinander schneide - um Koordinaten zu bekommen
df_merge <- transform(df_merge, test=do.call(rbind, strsplit(df_merge$variable, '/', fixed = TRUE)), stringsAsFactors = F)

# Spalten in numeric umwandeln
df_merge$test.2 <- as.numeric(as.character(df_merge$test.2))
df_merge$test.3 <- as.numeric(as.character(df_merge$test.3))

# Kartierung
qmap("Dongou", zoom = 7, maptype = 'roadmap', color = 'bw') + 
  geom_point(aes(x = test.2, y = test.3, size = pc, fill = pc), data = df_merge, shape = 22, alpha = 0.8) + 
  scale_size_continuous(range = c(1,10), name  ="Anteil je\nFpl. [%]") + 
  scale_fill_gradientn(colours = rev(heat.colors(12)), name  ="Anteil je\nFpl. [%]") + 
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggtitle("Schamott-Magerung - fabric 9\n")
ggsave("../output/figs/2-2-1-2_Keramik_fabric9_Verbreitung_PC.pdf", width = 8, height = 8)



# Ubangi-Tradition (fabrics 4, 5, 10, 14)
# =======================================

df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                't_Ort'.'ort_kurz' || '/' || 't_Ort'.'x_long' || '/' || 't_Ort'.'y_lat' AS SITE, 
                t_Ort.ort_kurz, 
                t_Komplex.bef_nr, 
                t_Obj.Individuum, 
                t_Obj.MagerungArt, 
                t_Obj.Fabric, 
                t_Ort.x_long, 
                t_Ort.y_lat 
                FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
                WHERE (((t_Ort.ort_lit)='DS') 
                AND ((t_Obj.Fabric) Like '4%')
                OR ((t_Obj.Fabric) Like '5%')
                OR ((t_Obj.Fabric) Like '10%')
                OR ((t_Obj.Fabric) Like '12%')
                OR ((t_Obj.Fabric) Like '14%'))")
head(df)

# WICHTIG: ggmap funktioniert nur mit R 3.0, nicht in R 3.1!!!

# **Kartierung**
# Legende: Kreise = unsicher, Punkte = sicher

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + geom_hex(aes(x = x_long, y = y_lat), data = df, alpha = .7) + scale_fill_gradientn(colours=rev(heat.colors(12))) + geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + ggtitle("Ubangi-Tradition - fabrics 4, 5, 10, 12 und 14\n")
#ggsave(file = "../output/figs/2-2-1-2_Keramik_fabric4-5-10_Ubangi-Trad_Verbreitung_Hex.pdf", width = 11, height = 11)

#### Mit roten Isolinien:

qmap("Dongou", zoom = 7, maptype = 'hybrid', legend = "topleft") + stat_density2d(data = df, aes(x = x_long, y = y_lat), color = 'red') + geom_point(aes(x = x_long, y = y_lat), data = df, size=2, fill = 'white', shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df, size = 2) + ggtitle("Ubangi-Tradition - fabrics 4, 5, 10\n")
#ggsave(file = "../output/figs/2-2-1-2_Keramik_fabric4-5-10_Ubangi-Trad_Verbreitung_Iso.pdf", width = 11, height = 11)


# Prozent je Fpl darstellen
# -------------------------

df_a <- data.frame(tapply(df$objID, list(df$SITE), length))
names(df_a)[1] <- "value"
df_a$variable <- rownames(df_a) 
head(df_a)

df_merge <- merge(x = df_a, y = df_all_a, by = 'variable')

# Prozent je Fpl ausrechnen
df_merge$pc <- df_merge$value / df_merge$value_all * 100

# variable-Feld wieder auseinander schneide - um Koordinaten zu bekommen
df_merge <- transform(df_merge, test=do.call(rbind, strsplit(df_merge$variable, '/', fixed = TRUE)), stringsAsFactors = F)

# Spalten in numeric umwandeln
df_merge$test.2 <- as.numeric(as.character(df_merge$test.2))
df_merge$test.3 <- as.numeric(as.character(df_merge$test.3))

# Kartierung
qmap("Dongou", zoom = 7, maptype = 'roadmap', color = 'bw') + 
  geom_point(aes(x = test.2, y = test.3, size = pc, fill = pc), data = df_merge, shape = 22, alpha = 0.8) + 
  scale_size_continuous(range = c(1, 10), name = "Anteil je\nFpl. [%]") + 
  scale_fill_gradientn(colours = rev(heat.colors(12)), name = "Anteil je\nFpl. [%]") + 
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggtitle("Ubangi-Tradition - fabrics 4, 5, 10\n")
ggsave("../output/figs/2-2-1-2_Keramik_fabric4-5-10-12-14_Ubangi-Trad_Verbreitung_PC.pdf", width = 8, height = 8)




# fabrics 2
# =========

df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                't_Ort'.'ort_kurz' || '/' || 't_Ort'.'x_long' || '/' || 't_Ort'.'y_lat' AS SITE, 
                t_Ort.ort_kurz, 
                t_Komplex.bef_nr, 
                t_Obj.Individuum, 
                t_Obj.MagerungArt, 
                t_Obj.Fabric, 
                t_Ort.x_long, 
                t_Ort.y_lat 
                FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
                WHERE (((t_Ort.ort_lit)='DS') 
                AND ((t_Obj.Fabric) Like '2%'))")
# head(df)

df_a <- data.frame(tapply(df$objID, list(df$SITE), length))
names(df_a)[1] <- "value"
df_a$variable <- rownames(df_a) 
head(df_a)

df_merge <- merge(x = df_a, y = df_all_a, by = 'variable')

# Prozent je Fpl ausrechnen
df_merge$pc <- df_merge$value / df_merge$value_all * 100

# variable-Feld wieder auseinander schneide - um Koordinaten zu bekommen
df_merge <- transform(df_merge, test=do.call(rbind, strsplit(df_merge$variable, '/', fixed = TRUE)), stringsAsFactors = F)

# Spalten in numeric umwandeln
df_merge$test.2 <- as.numeric(as.character(df_merge$test.2))
df_merge$test.3 <- as.numeric(as.character(df_merge$test.3))

# Kartierung
qmap("Dongou", zoom = 7, maptype = 'roadmap', color = 'bw') + 
  geom_point(aes(x = test.2, y = test.3, size = pc, fill = pc), data = df_merge, shape = 22, alpha = 0.8) + 
  scale_size_continuous(range = c(1, 10), name = "Anteil je\nFpl. [%]") + 
  scale_fill_gradientn(colours = rev(heat.colors(12)), name = "Anteil je\nFpl. [%]") + 
  theme(legend.justification=c(1,0), 
        legend.position=c(1,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# ggtitle("Ubangi-Tradition - fabrics 4, 5, 10\n")
ggsave("../output/figs/2-2-1-2_Keramik_fabric2_Verbreitung_PC.pdf", width = 8, height = 8)


