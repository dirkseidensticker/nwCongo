---
title: "3.2.1.4 Bondongo-Gruppe"
output: html_document
---

```{r}
library(ggplot2)
library(ggmap)
library(RSQLite)
library(reshape)
library(sqldf)
```

## Verbreitung

```{r}
drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# nur sicher Imbonga
df1 = dbGetQuery(con, "SELECT t_Obj.objID, t_Obj.ort_kurz, t_Obj.Komplex, t_Obj.Individuum, [t_Ort].[Kat-Nr], t_Obj.Typ, t_Ort.y_lat, t_Ort.x_long 
                 FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID) INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID 
                 WHERE (((t_Ort.ort_lit)='DS') AND ((t_Obj.Typ) = 'Bondongo'))")
head(df1)

# nur nicht sicher Imbonga
df2 = dbGetQuery(con, "SELECT t_Obj.objID, t_Obj.ort_kurz, t_Obj.Komplex, t_Obj.Individuum, [t_Ort].[Kat-Nr], t_Obj.Typ, t_Ort.y_lat, t_Ort.x_long 
                 FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID) INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID 
                 WHERE (((t_Ort.ort_lit)='DS') AND ((t_Obj.Typ) Like '%Bondongo%?%'))")
head(df2)

# Daten aus der Sequenz von Wotzka.1995

df_hpw <- read.csv("../data/base/Wotzka1995_Sequenz.csv", dec=",")
# Auswahl Datensätz = Botendo
df_hpw_a <- sqldf('select * from df_hpw where Bondongo Like "%X%"')
```

WICHTIG: ggmap funktioniert nur mit R 3.0, nicht in R 3.1!!!

**Kartierung**

Legende: Kreise = unsicher, Punkte = sicher

```{r, fig.align='center'}
qmap("Dongou", zoom = 7, color = 'bw', maptype = 'roadmap') + geom_point(aes(x = x_long, y = y_lat), data = df2, size=1.5, shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df2, size = 2.5) + geom_point(aes(x = x_long, y = y_lat), data = df1, size=2) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df1, size = 2.5) + geom_point(aes(x = X, y = Y), data = df_hpw_a, size = 2, alpha = .3)
ggsave("../output/figs/3-2-1-4_Bondongo_Verbreitung.pdf", width = 11, height = 11)
```

```{r, fig.align='center'}
qmap("Dongou", zoom = 7, color = 'bw', maptype = 'satellite') + geom_point(aes(x = x_long, y = y_lat), data = df2, size=1.5, shape = 21, color = 'white') + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df2, size = 2.5, color = 'white') + geom_point(aes(x = x_long, y = y_lat), data = df1, size=2, color = 'white') + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df1, size = 2.5, color = 'white') + geom_point(aes(x = X, y = Y), data = df_hpw_a, size = 2, color = 'gray50')
ggsave("../output/figs/3-2-1-4_Bondongo_Verbreitung_B.pdf", width = 11, height = 11)


# stumme Karte
library(maptools)
library(raster)
library(rworldmap)

locs1 <- subset(df1, select = c("Kat-Nr", "y_lat", "x_long"))
coordinates(locs1) <- c("x_long", "y_lat")  # set spatial coordinates
locs2 <- subset(df2, select = c("Kat-Nr", "y_lat", "x_long"))
coordinates(locs2) <- c("x_long", "y_lat")  # set spatial coordinates
locs3 <- subset(df_hpw_a, select = c("Nr", "Y", "X"))
coordinates(locs3) <- c("X", "Y")  # set spatial coordinates

# basemap
r <- brick("../data/GIS/basemap.tif")
# Vektordaten
gb.rivers <- readShapeLines("../data/GIS/OSM_rivers.shp")
gb.lakes <- readShapePoly("../data/GIS/OSM_natural_water.shp")
gb.countries <- readShapeLines("../data/GIS/10m_admin_0_countries.shp")
gb.regenwald <- readShapeLines("../data/GIS/RegenwaldGrenze.shp")

# projection(r)

# Karte
pdf("../output/figs/3-2-1-4_Bondongo_Verbreitung_C.pdf", width = 8, height = 8)
plotRGB(r)
plot(gb.regenwald, add = TRUE, col = 'gray50', lwd = 4, lty = 3)
plot(gb.rivers, add = TRUE, col = 'gray40', lwd = 1.5)
# QGIS:  '#c0dfff'
plot(gb.lakes, add = TRUE, col = 'gray40', border = 'gray40')
plot(gb.countries, add = TRUE, col = 'gray50', lwd = 1)
plot(locs3, add = TRUE, pch = 21, bg = 'gray50', col = 'white', cex = 2)
text(x = locs3$X, y = locs3$Y, labels=locs3$Nr, pos=4, cex=0.8)
plot(locs2, add = TRUE, pch = 21, bg = 'gray90', col = 'gray50', cex = 2)
text(x = locs2$x_long, y = locs2$y_lat, labels=locs2$`Kat-Nr`, pos=4, cex=0.8)
plot(locs1, add = TRUE, pch = 21, col = 'white', cex = 2)
text(x = locs1$x_long, y = locs1$y_lat, labels=locs1$`Kat-Nr`, pos=4, cex=0.8)
dev.off()
