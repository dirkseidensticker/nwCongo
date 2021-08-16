###################
# 4.3 Integration #
###################

library(rgl)
library(ca)
library(RSQLite)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")


# Seriation aus Stilgruppen und Typen
# ===================================

df = dbGetQuery(con, "SELECT 
                  t_Obj.objID,
                  t_Obj.Typ, 
                  t_Obj.Form_Gef
                FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
                WHERE (((t_Ort.ort_lit) = 'DS')
                  AND ((t_Obj.Typ) != '')
                  AND ((t_Obj.Typ) Not Like '%(?)%')
                  AND ((t_Obj.Form_Gef) != '')
                  AND ((t_Obj.Form_Gef) Not Like '%(?)%'))")
head(df)

# Kreuztabelle erzeugen
df_pivot <- as.data.frame(tapply(df$objID, list(df$Typ, df$Form_Gef), length))
# NA durch 0 ersetzen
df_pivot[is.na(df_pivot)] <- 0

# Seriation
# ---------
library(CAseriation)

check.ca.plot(df_pivot,1,2) 

sort.table(df_pivot,1)

plot.clusters.rows(df_pivot,1,2)

plot.clusters.cols(df_pivot,1,2)

evaluate(df_pivot,1,2, which='R') 




# CA aus Typen und Fundorten


# nur sicher
df = dbGetQuery(con, "SELECT t_Ort.ort_kurz, t_Obj.objID, t_Obj.Typ, t_Ort.ort_lit FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID WHERE (((t_Ort.ort_lit) = 'DS') AND ((t_Obj.Art) Not Like '%Sonder%') AND ((t_Obj.Typ) Not Like '%?%') AND ((t_Obj.Typ) != ''))")
head(df)

# Kreuztabelle erzeugen
df_m <- tapply(df$objID, list(df$Typ, df$ort_kurz), length)
# NA durch 0 ersetzen
df_m[is.na(df_m)] <- 0
head(df_m[,1:5])

pdf("../output/figs/8-3_Stilgr_FdSt.pdf", width=12, height=6, useDingbats=FALSE)
par(mfrow = c(1, 2))
plot(ca(df_m), dim = c(2, 3), labels = c(2, 0))
title(sub = "2. und 3. Hauptachse")
plot(ca(df_m), dim = c(3, 4),labels = c(2, 0))
title(sub = "3. und 4. Hauptachse")
title("CA aus Typen und Fundorten", line = -2, outer = TRUE)
dev.off()


# CA aus Verzierungselmenten und Stilgruppen

# Bedingungen:
# * keine typen mit '?' oder '/'

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# nur sicher
df = dbGetQuery(con, "SELECT t_Obj.objID, t_K_Verz.verzName, t_Obj.Typ, t_Ort.ort_lit FROM t_Obj INNER JOIN 't_ObjPosVerz' ON t_Obj.objID = 't_ObjPosVerz'.objID INNER JOIN t_K_Pos ON 't_ObjPosVerz'.posID = t_K_Pos.posID INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID INNER JOIN t_K_Verz ON 't_ObjPosVerz'.verzID = t_K_Verz.verzID WHERE (((t_Ort.ort_lit) = 'DS') AND ((t_Obj.Art) Not Like '%Sonder%') AND ((t_K_Pos.posID)<>1) AND ((t_K_Verz.verzName) Like 'V%') AND ((t_Obj.Typ) Not Like '%?%') AND ((t_Obj.Typ) Not Like '%/%') AND ((t_Obj.Typ) != ''))")
head(df)

# Kreuztabelle erzeugen
df_m <- tapply(df$objID, list(df$Typ, df$verzName), length)
# NA durch 0 ersetzen
df_m[is.na(df_m)] <- 0
head(df_m[,1:5])

pdf("../output/figs/4-3_Verz-Stilgr.pdf", width=12, height=6, useDingbats=FALSE)
par(mfrow = c(1, 2))
plot(ca(df_m), labels = c(2, 0))
title(sub = "1. und 2. Hauptachse")
plot(ca(df_m), dim = c(2, 3),labels = c(2, 0))
title(sub = "2. und 3. Hauptachse")
title("CA aus Verzierungselmenten und Stilgruppen", line = -2, outer = TRUE)
dev.off()

# CA aus Gefäßposition + Verzierungselement und Stilgruppe

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "data/CongoDB.sqlite")

# nur sicher
df = dbGetQuery(con, "SELECT t_Obj.objID, [t_K_Pos].[posReihe] || ' ' || [t_K_Verz].[verzName] AS verz, t_Obj.Typ, t_Ort.ort_lit FROM t_Obj INNER JOIN 't_ObjPosVerz' ON t_Obj.objID = 't_ObjPosVerz'.objID INNER JOIN t_K_Pos ON 't_ObjPosVerz'.posID = t_K_Pos.posID INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID INNER JOIN t_K_Verz ON 't_ObjPosVerz'.verzID = t_K_Verz.verzID WHERE (((t_Ort.ort_lit) = 'DS') AND ((t_Obj.Art) Not Like '%Sonder%') AND ((t_K_Pos.posID)<>1) AND ((t_K_Verz.verzName) Like 'V%') AND ((t_Obj.Typ) Not Like '%?%') AND ((t_Obj.Typ) Not Like '%/%') AND ((t_Obj.Typ) != ''))")
head(df)

# Kreuztabelle erzeugen
df_m <- tapply(df$objID, list(df$Typ, df$verz), length)
# NA durch 0 ersetzen
df_m[is.na(df_m)] <- 0
head(df_m[,1:5])

pdf("output/figs/4-3_Verz_Pos-Stilgr.pdf", width=12, height=6, useDingbats=FALSE)
par(mfrow = c(1, 2))
plot(ca(df_m), labels = c(2, 0))
title(sub = "1. und 2. Hauptachse")
plot(ca(df_m), dim = c(2, 3),labels = c(2, 0))
title(sub = "2. und 3. Hauptachse")
title("Kombination aus Gefäßposition + Verzierungselement im Vergleich mit Stilgruppe", line = -2, outer = TRUE)
dev.off()