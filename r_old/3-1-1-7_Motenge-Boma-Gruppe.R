---
title: "3.1.1.7 Motenge-Boma-Gruppe"
output: html_document
---

```{r}
library(ggplot2)
library(ggmap)
library(RSQLite)
library(reshape)
library(RColorBrewer)

# Fabrverlauf festlegen
rf <- colorRampPalette(brewer.pal(9,'OrRd'))
r <- rf(32)
```

## Verbreitung

```{r}
drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# nur sicher
df1 = dbGetQuery(con, "SELECT t_Obj.objID, t_Obj.ort_kurz, t_Obj.Komplex, t_Obj.Individuum, t_Obj.Typ, [t_Ort].[Kat-Nr], t_Obj.Typ, t_Ort.y_lat, t_Ort.x_long 
                 FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID) INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID 
                 WHERE (((t_Ort.ort_lit)='DS') AND ((t_Obj.Typ) = 'MTB'))")
head(df1)

# nur nicht sicher
df2 = dbGetQuery(con, "SELECT t_Obj.objID, t_Obj.ort_kurz, t_Obj.Komplex, t_Obj.Individuum, t_Obj.Typ, [t_Ort].[Kat-Nr], t_Obj.Typ, t_Ort.y_lat, t_Ort.x_long 
                 FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID) INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID 
                 WHERE (((t_Ort.ort_lit)='DS') AND ((t_Obj.Typ) Like '%MTB%?%'))")
head(df2)
```

WICHTIG: ggmap funktioniert nur mit R 3.0, nicht in R 3.1!!!

**Kartierung**

Legende: Kreise = unsicher, Punkte = sicher

```{r, fig.align='center'}
qmap("Dongou", zoom = 7, color = 'bw', maptype = 'roadmap', legend = "topleft") + geom_hex(aes(x = x_long, y = y_lat), data = df1, alpha = .7) + scale_fill_gradientn(colours=rev(heat.colors(12))) + geom_point(aes(x = x_long, y = y_lat), data = df2, size=1.5, shape = 21) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df2, size = 2) + geom_point(aes(x = x_long, y = y_lat), data = df1, size=1.5, shape = 19) + geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df1, size = 2)
ggsave(file = "../output/figs/3-1-1-7_Motenge-Boma_Verbreitung.pdf", width = 11, height = 11)
```

qmap("Dongou", zoom = 7, maptype = 'roadmap', color = 'bw') + 
  geom_hex(aes(x = x_long, y = y_lat), data = df1, alpha = .8, color = "black") + 
  scale_fill_gradientn(colours=rev(heat.colors(12))) + 
  geom_point(aes(x = x_long, y = y_lat), data = df2, size=1.5, shape = 21) + 
  geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df2, size = 2) + 
  geom_point(aes(x = x_long, y = y_lat), data = df1, size=1.5, shape = 19) + 
  geom_text(aes(x = x_long+0.15, y = y_lat+0.07, label = ort_kurz), data = df1, size = 2) + 
  theme(legend.justification = c(0,1), 
        legend.position = c(0,1), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave(file = "../output/figs/3-1-1-7_Motenge-Boma_Verbreitung_B.pdf", width = 11, height = 11)

# stumme Karte
library(maptools)
library(raster)
library(rworldmap)

locs1 <- subset(df1, select = c("Kat-Nr", "y_lat", "x_long"))
coordinates(locs1) <- c("x_long", "y_lat")  # set spatial coordinates
locs2 <- subset(df2, select = c("Kat-Nr", "y_lat", "x_long"))
coordinates(locs2) <- c("x_long", "y_lat")  # set spatial coordinates

# basemap
r <- brick("../data/GIS/basemap.tif")
# Vektordaten
gb.rivers <- readShapeLines("../data/GIS/OSM_rivers.shp")
gb.lakes <- readShapePoly("../data/GIS/OSM_natural_water.shp")
gb.countries <- readShapeLines("../data/GIS/10m_admin_0_countries.shp")
gb.regenwald <- readShapeLines("../data/GIS/RegenwaldGrenze.shp")

# projection(r)

# Karte
pdf("../output/figs/3-1-1-7_Motenge-Boma_Verbreitung_C.pdf", width = 8, height = 8)
plotRGB(r)
plot(gb.regenwald, add = TRUE, col = 'gray50', lwd = 4, lty = 3)
plot(gb.rivers, add = TRUE, col = 'gray40', lwd = 1.5)
# QGIS:  '#c0dfff'
plot(gb.lakes, add = TRUE, col = 'gray40', border = 'gray40')
plot(gb.countries, add = TRUE, col = 'gray50', lwd = 1)
plot(locs2, add = TRUE, pch = 21, bg = 'gray90', col = 'gray50', cex = 2)
text(x = locs2$x_long, y = locs2$y_lat, labels=locs2$`Kat-Nr`, pos=4, cex=0.8)
plot(locs1, add = TRUE, pch = 21, col = 'white', cex = 2)
text(x = locs1$x_long, y = locs1$y_lat, labels=locs1$`Kat-Nr`, pos=4, cex=0.8)
dev.off()
