#######################
# 4.1 Eisenverhüttung #
#######################

library(grid)
library(gridExtra)
library(ggplot2)
library(ggmap)
library(RSQLite)
library(reshape)
library(RColorBrewer)

source("myfunctions.R")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# bounding-box Arbeitsgebiet
df_sitesDS = dbGetQuery(con, "SELECT t_Ort.ort_name, t_Ort.x_long, t_Ort.y_lat FROM t_Ort WHERE (((t_Ort.y_lat) <> '') AND ((t_Ort.ort_lit) Like '%DS%'))")
xmin <- min(df_sitesDS$x_long)
xmax <- max(df_sitesDS$x_long)
ymin <- min(df_sitesDS$y_lat)
ymax <- max(df_sitesDS$y_lat)

# Kartierung der Schlackfunde
# ---------------------------

# Kreuztabelle von Pandas produziert
df_pivot <- read.csv("../data/processed/Schlacke - Kartierung - Gewicht.csv")

qmap("Dongou", zoom = 7, color = 'bw') + 
  geom_point(aes(x = x_long, y = y_lat, size = Gewicht..kg., fill = Gewicht..kg.), data = df_pivot, shape = 22, alpha = 0.8) + 
  scale_size_continuous(range = c(2,12), name  ="Anteil je\nFpl. [%]") + 
  scale_fill_gradientn(colours = rev(heat.colors(12)), name  ="Anteil je\nFpl. [%]") + 
  geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = Ort), data = df_pivot, size = 4) + 
  theme(legend.justification = c(0,0), 
        legend.position = c(0,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("../output/figs/7_Eisen_KarteSchlacken.pdf", width = 11, height = 11)

# **ToDo**: Label der einzelnen Pkt mit FdSt-Kürzel hinzufügen!


# Kartierung mit Density:

myLocation <- c(10, -5, 25, 7.5)
myMap <- get_map(location = myLocation, maptype = "hybrid")

ggmap(myMap) + stat_density2d(data = df, aes(x = x_long, y = y_lat, fill = ..level.., alpha = ..level..), bins = 25, alpha = 0.3, geom = "polygon")

# Kartierung mit Hexbin:
myLocation <- c(10, -5, 25, 7.5)
myMap <- get_map(location = myLocation, maptype = "hybrid")

ggmap(myMap) + geom_hex(aes(x = x_long, y = y_lat), data = df_pivot)


# Ofentypen in Zentralafrika 
# --------------------------
df <- read.csv("../data/base/Eisen_OfenTyp.csv", dec = ',')

max <- max(df$kaBP, na.rm = TRUE)
min <- min(df$kaBP, na.rm = TRUE)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

qmap("Bokungu", zoom = 5, maptype = 'satellite') +
  geom_point(aes(x = x_long, y = y_lat, fill = kaBP, shape = factor(TYP)), data = df, size = 5) +
  scale_fill_gradientn(colours = myPalette(100), limits=c(min, max), name = "Datierung\n(kaBP)") + 
  scale_shape_manual(values=c(22, 24, 25, 23, 21), 
                     name="Ofentyp",
                     breaks=c("Schachtofen mit Schlackegrube", "?", "offen", "Typ-BAM", "vegetabilisch"),
                     labels=c("Schachtofen", "?", "offener Ofen", "offener Ofen mit seitlichen\nSchlackeabfluss (Typ 'Bamanya')", "vegetabilischer Ofen")) +
  theme(legend.box = "horizontal", 
        legend.justification = c(0,0), 
        legend.position = c(0,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +  
  geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = TRUE), alpha = 0.025, color = "black", fill = 'black', size = 0.1) + 
  geom_text(aes(x = x_long+0.2, y = y_lat+0.2, label = name), data = df, size = 4, color = 'white', hjust=0, vjust=0)
ggsave("../output/figs/7_Eisen_OfenTyp_map.pdf", width = 11, height = 11)


## Dimensionen der Öfen
df <- read.csv('../data/base/Eisen_OfenTyp.csv', dec = ',')

# Zeilen, in denen kein Dm und T angegeben sind herausfiltern
df <- df[!(is.na(df$Dm_m) | is.na(df$T_m)), ]

df$Dm_m <- replace(df$Dm_m, is.na(df$Dm_m), 0)
df$T_m <- replace(df$T_m, is.na(df$T_m), 0)

ggplot(df, aes(x = Dm_m, y = T_m, label = feature, fill = TYP)) + 
  geom_point(pch = 21, size = 6, alpha = I(0.75)) + 
  scale_x_continuous("Durchmesser [m]", limits = c(0, 2)) + 
  coord_fixed() +
  scale_y_reverse("Tiefe [m]") + 
  geom_text(hjust = 0, vjust = -1, size = 4) + 
  theme_bw() +
  theme(legend.position="bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,1,1,5), "mm"),
        legend.key = element_blank()) +
  #      axis.ticks.length = unit(-0.15, "cm")) +
  #      axis.text.x = element_text(margin(0.35, unit = "cm"))) + 
  guides(fill=guide_legend(ncol=3))
ggsave("../output/figs/7_Eisen_OfenDimensions.pdf", width = 11, height = 8)


## Fragmentierung (Darstellung wie bei der Keramik)

df = dbGetQuery(con, "SELECT
                t_Obj.objID,
                t_Obj.Gr_Clist,
                t_Obj.Anzahl,
                t_Obj.Gewicht
              FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
              INNER JOIN t_Obj ON (t_Ort.ortID = t_Obj.ortID)
              AND (t_Komplex.komplexID = t_Obj.komplexID)
              WHERE (((t_Obj.Art) Like '%Schlacke%')
                AND ((t_Komplex.bef_art) Not Like '%Ober%')
                AND ((t_Obj.Schlacke_Typ) != '')
                AND ((t_Obj.ort_kurz) != 'BBS')
                AND ((t_Obj.ort_kurz) != 'MDJ'))")
head(df)

mappingSet <- FragmentationGraphics(df)

pdf("../output/figs/7_Schlacken_Fragmentierung.pdf", width = 10, height = 2.5)
grid.arrange(mappingSet[[2]], mappingSet[[3]], mappingSet[[1]], ncol = 3, widths = c(4/9,1/9,4/9))
dev.off()

# MUN 87/1
# --------
df = dbGetQuery(con, "SELECT
                t_Obj.objID,
                t_Obj.Gr_Clist,
                t_Obj.Anzahl,
                t_Obj.Gewicht
              FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
              INNER JOIN t_Obj ON (t_Ort.ortID = t_Obj.ortID)
              AND (t_Komplex.komplexID = t_Obj.komplexID)
              WHERE (((t_Obj.Art) Like '%Schlacke%')
                AND ((t_Obj.ort_kurz) = 'MUN')
                AND ((t_Komplex.bef_nr) = '87/1'))")
head(df)

mappingSet <- FragmentationGraphics(df)

pdf("../output/figs/7_Schlacken_Fragmentierung_MUN87-1.pdf", width = 10, height = 2.5)
grid.arrange(mappingSet[[2]], mappingSet[[3]], mappingSet[[1]], ncol = 3, widths = c(4/9,1/9,4/9), top = 'MUN 87/1')
dev.off()


# MUN 87/2-1-1
# ------------
df = dbGetQuery(con, "SELECT
                t_Obj.objID,
                t_Obj.Gr_Clist,
                t_Obj.Anzahl,
                t_Obj.Gewicht
                FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                INNER JOIN t_Obj ON (t_Ort.ortID = t_Obj.ortID)
                AND (t_Komplex.komplexID = t_Obj.komplexID)
                WHERE (((t_Obj.Art) Like '%Schlacke%')
                AND ((t_Obj.ort_kurz) = 'MUN')
                AND ((t_Komplex.bef_nr) = '87/2-1-1'))")
head(df)

mappingSet <- FragmentationGraphics(df)

pdf("../output/figs/7_Schlacken_Fragmentierung_MUN87-211.pdf", width = 10, height = 2.5)
grid.arrange(mappingSet[[2]], mappingSet[[3]], mappingSet[[1]], ncol = 3, widths = c(4/9,1/9,4/9), top = 'MUN 87/2-1-1')
dev.off()


# MUN 87/3
# --------
df = dbGetQuery(con, "SELECT
                t_Obj.objID,
                t_Obj.Gr_Clist,
                t_Obj.Anzahl,
                t_Obj.Gewicht
                FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                INNER JOIN t_Obj ON (t_Ort.ortID = t_Obj.ortID)
                AND (t_Komplex.komplexID = t_Obj.komplexID)
                WHERE (((t_Obj.Art) Like '%Schlacke%')
                AND ((t_Obj.ort_kurz) = 'MUN')
                AND ((t_Komplex.bef_nr) = '87/3'))")
head(df)

mappingSet <- FragmentationGraphics(df)

pdf("../output/figs/7_Schlacken_Fragmentierung_MUN87-3.pdf", width = 10, height = 2.5)
grid.arrange(mappingSet[[2]], mappingSet[[3]], mappingSet[[1]], ncol = 3, widths = c(4/9,1/9,4/9), top = 'MUN 87/3')
dev.off()


# PIK 87/3
# --------
df = dbGetQuery(con, "SELECT
                t_Obj.objID,
                t_Obj.Gr_Clist,
                t_Obj.Anzahl,
                t_Obj.Gewicht
                FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                INNER JOIN t_Obj ON (t_Ort.ortID = t_Obj.ortID)
                AND (t_Komplex.komplexID = t_Obj.komplexID)
                WHERE (((t_Obj.Art) Like '%Schlacke%')
                AND ((t_Obj.ort_kurz) = 'PIK')
                AND ((t_Komplex.bef_nr) = '87/3'))")
head(df)

mappingSet <- FragmentationGraphics(df)

pdf("../output/figs/7_Schlacken_Fragmentierung_PIK87-3.pdf", width = 10, height = 2.5)
grid.arrange(mappingSet[[2]], mappingSet[[3]], mappingSet[[1]], ncol = 3, widths = c(4/9,1/9,4/9), top = 'PIK 87/3')
dev.off()
