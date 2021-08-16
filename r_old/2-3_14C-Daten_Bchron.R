#####################################
# 14C-Daten Kalibrierung mit Bchron #
#####################################

library(RSQLite)
library(Bchron)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# bounding-box Arbeitsgebiet
df_sitesDS = dbGetQuery(con, "SELECT t_Ort.ort_name, t_Ort.x_long, t_Ort.y_lat FROM t_Ort WHERE (((t_Ort.y_lat) <> '') AND ((t_Ort.ort_lit) Like '%DS%'))")
xmin <- min(df_sitesDS$x_long)
xmax <- max(df_sitesDS$x_long)
ymin <- min(df_sitesDS$y_lat)
ymax <- max(df_sitesDS$y_lat)

# eigene Daten

df = dbGetQuery(con, "SELECT
           t_14C.LABNR,
           t_14C.C14AGE,
           t_14C.C14STD,
           t_Ort.ort_name, 
           t_Ort.ort_lit
       FROM ((t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
           INNER JOIN t_14c ON t_Komplex.komplexID = t_14c.komplexID)
       WHERE (((t_Ort.ort_lit) == 'DS'))")
write.csv(df, file = "../data/processed/2-3_14C-Daten_A_eigeneDaten.csv")
head(df)
# Spalte mit Kalibrationskurve hinzufügen
df$calCurves <- "intcal13"
nrow(df)

ages = BchronCalibrate(ages=df$C14AGE, ageSds=df$C14STD, calCurves= df$calCurves)
summary(ages)
# generiert einen Plot je Datum!
# plot(ages)

# Summenkurve
ages2 = BchronDensity(ages=df$C14AGE, ageSds=df$C14STD, calCurves= df$calCurves)
pdf("../output/figs/2-3_14C-Daten_A_eigeneDaten.pdf")
plot(ages2, xlab = "Age (cal years BP)")
abline(v = 2000, lty = 2)
dev.off()

# Daten aus der Literatur aus dem Arbeitsgebiet (Koordinaten begrenzen)

df = dbGetQuery(con, "SELECT
           t_14C.LABNR,
           t_14C.C14AGE,
           t_14C.C14STD,
           t_Ort.ort_name, 
           t_Ort.ort_lit
       FROM ((t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
           INNER JOIN t_14c ON t_Komplex.komplexID = t_14c.komplexID)
       WHERE (((t_Ort.ort_lit) Not Like '%DS%')
           AND ((t_Ort.ort_lit) Not Like '%Wotzka%')
           AND ((t_Ort.x_long) > 15.5)
           AND ((t_Ort.x_long) < 20)
           AND ((t_Ort.y_lat) > -1.2)
           AND ((t_Ort.y_lat) < 5.2)
           AND ((t_14C.C14AGE) < 3000))")
write.csv(df, file = "../data/processed/2-3_14C-Daten_B_LitDaten.csv")
head(df)
# Spalte mit Kalibrationskurve hinzufügen
df$calCurves <- "intcal13"
nrow(df)


ages2 = BchronDensity(ages=df$C14AGE, ageSds=df$C14STD, calCurves= df$calCurves)
pdf("../output/figs/2-3_14C-Daten_B_LitDaten.pdf")
plot(ages2, xlab = "Age (cal years BP)")
abline(v = 2000, lty = 2)
dev.off()

# Arbeitsgebiet Wotzka - Inkl. neue Daten 
# ---------------------------------------
# > Ohne die Daten aus Hannover!

df = dbGetQuery(con, "SELECT
                  t_14C.LABNR,
                  t_14C.C14AGE,
                  t_14C.C14STD,
                  t_Ort.ort_name, 
                  t_Ort.ort_lit
                  FROM ((t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                  INNER JOIN t_14c ON t_Komplex.komplexID = t_14c.komplexID)
                  WHERE (((t_Ort.ort_lit) Like '%Wotzka%')
                      AND ((t_14C.LABNR) Not Like '%Hv%'))")
write.csv(df, file = "../data/processed/2-3_14C-Daten_C_Wotzka_ICB.csv")
head(df)
# Spalte mit Kalibrationskurve hinzufügen
df$calCurves <- "intcal13"
nrow(df)


ages2 = BchronDensity(ages=df$C14AGE, ageSds=df$C14STD, calCurves= df$calCurves)
pdf("../output/figs/2-3_14C-Daten_C_Wotzka_ICB.pdf")
plot(ages2, xlab = "Age (cal years BP)")
abline(v = 2000, lty = 2)
dev.off()