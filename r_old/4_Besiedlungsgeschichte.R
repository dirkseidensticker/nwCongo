############################################################
# 4. Besiedlungsgeschichte des nordwestlichen Kongobeckens #
############################################################

library(ggplot2)
library(ggmap)
library(ca)
library(RSQLite)
library(reshape)
library(sqldf)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# Korrespondenzanalyse der Keramikformen
# --------------------------------------

df = dbGetQuery(con, "SELECT
           t_Obj.objID,
           t_Obj.ort_kurz, 
           t_Obj.Typ 
       FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
           INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID
       WHERE (((t_Obj.Typ) Not Like '%?%')
           AND (t_Obj.Typ) != ''
           AND (t_Obj.ort_kurz) != ''
           AND (t_Ort.ort_lit)='DS')")

df_pivot <- tapply(df$objID, list(df$ort_kurz, df$Typ), length)
df_pivot[is.na(df_pivot)] <- 0

# summary(ca(df_pivot))

plot(ca(df_pivot))
title(sub = "1. und 2. Hauptachse")

par(mfrow = c(1, 2))
plot(ca(df_pivot), dim = c(2, 3))
title(sub = "2. und 3. Hauptachse")
plot(ca(df_pivot), dim = c(3, 4))
title(sub = "3. und 4. Hauptachse")
dev.print(device=pdf, "../output/figs/4-Besiedl_CA_KeramikGr-Orte.pdf", width = 12, height = 6)
# **Abb. #** Besiedlungsgeschichte: Korrespondenzanalyse der keramischen Stilgruppen und Fundorte ...

par(mfrow = c(1, 1)) # zurücksetzen auf einzelne Grafiken

# LKW 186 fällt voll raus:

df = dbGetQuery(con, "SELECT
           t_Obj.objID,
           t_Obj.ort_kurz, 
           t_Obj.Typ 
       FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
           INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID
       WHERE (((t_Obj.Typ) Not Like '%?%')
           AND (t_Obj.Typ) != ''
           AND (t_Obj.ort_kurz) != ''
           AND (t_Ort.ort_lit)='DS'
           AND (t_Ort.ort_kurz) NOT LIKE '%LKW%')")

df_pivot <- tapply(df$objID, list(df$ort_kurz, df$Typ), length)
df_pivot[is.na(df_pivot)] <- 0

# summary(ca(df_pivot))

plot(ca(df_pivot))
title(sub = "1. und 2. Hauptachse")

par(mfrow = c(1, 2))
plot(ca(df_pivot), dim = c(2, 3))
title(sub = "2. und 3. Hauptachse")
plot(ca(df_pivot), dim = c(3, 4))
title(sub = "3. und 4. Hauptachse")
dev.print(device=pdf, "../output/figs/4-Besiedl_CA_KeramikGr-Orte_ohneLKW.pdf", width = 12, height = 6)
# **Abb. #** Besiedlungsgeschichte: Korrespondenzanalyse der keramischen Stilgruppen und Fundorte ...

par(mfrow = c(1, 1)) # zurücksetzen auf einzelne Grafiken



# 3d-Plots
# ========

library(vegan3d)

ord <- cca(df_pivot)

ordiplot3d(ord)

ordiplot3d(ord, type = "h")

# angle: der Kippwinkel
pl <- ordiplot3d(ord, angle=45, type="n")
points(pl, "points", pch=16, col="red", cex = 0.7)
