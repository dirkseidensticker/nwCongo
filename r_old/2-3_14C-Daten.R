####################
# 14C-Daten aus DB #
####################

library(RSQLite)
library(ggplot2)
library(sqldf)
library(ggmap)


drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# nur Daten jünger als 3500 bp und für die es Koordinaten gibt

df = dbGetQuery(con, "SELECT
                t_14C.LABNR,
                t_14C.C14AGE,
                t_14C.C14STD,
                t_14C.C13,
                t_14C.MATERIAL,
                t_14C.SPECIES,
                [t_Ort].[ort_name] AS SITE, 
                [t_Komplex].[bef_art] AS CULTURE, 
                [t_Komplex].[bef_nr] AS PHASE,
                [t_ort].[ort_land] AS COUNTRY,
                [t_ort].[y_lat] AS LATITUDE,
                [t_ort].[x_long] AS LONGITUDE
                FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                  INNER JOIN t_14c ON t_Komplex.komplexID = t_14c.komplexID
                WHERE t_14C.C14AGE < 3000
                  AND t_ort.x_long != ''")
head(df)

# Koordinaten müssen gerundet wwerden

df['X'] <- ceiling(df['LONGITUDE'])
df['Y'] <- ceiling(df['LATITUDE'])

# Plot aller Daten in der DB
ggplot(df, aes(x=C14AGE)) + geom_histogram() + scale_x_reverse() + facet_grid(Y ~ X)


# Arbeitsgebeit auswählen
# Wotzka 2006: Daten zwischen 5°N -- 5°S 
df_a <- sqldf('select * from df where Y < 5
              AND Y > -5')

ggplot(df_a, aes(x=C14AGE)) + geom_histogram() + scale_x_reverse() + facet_grid(Y ~ X, as.table = FALSE) + geom_vline(xintercept = 1950, colour="red") + theme_bw()

# Arbeitsgebeit auswählen
# Oslsily et al. 2013: Daten zwischen 7°S -- 6°N & 9°E -- 20° E
df_b <- sqldf('select * from df where Y < 7
              AND Y > -8
              AND X > 8
              AND X < 21')

ggplot(df_b, aes(x=C14AGE)) + 
  geom_histogram() + 
  scale_x_reverse() + 
  facet_grid(Y ~ X, as.table = FALSE) + 
  geom_vline(xintercept = 1950, colour="red") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(filename = "../output/figs/2-3_14C_Kooord_1grad_facet.pdf", width = 16.5, height = 11.7, units = 'in')


# Raster der Histogramm-Zellen auf der Karte:
qmap(c(8, -8, 21, 8), zoom = 6, color = 'bw') + 
  geom_hline(yintercept=seq(-8, 7, by=1)) + 
  geom_vline(xintercept=seq(8, 21, by=1)) + 
  geom_hline(yintercept=0, color = 'red') + 
  theme(legend.position="none")
ggsave(filename = "../output/figs/2-3_14C_Kooord_1grad_facet_map.pdf", width = 11, height = 11)


qmap(c(8, -8, 21, 8), zoom = 6, color = 'bw') + 
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = df, shape = 21, size = 5, fill = "white") + 
  geom_rect(mapping = aes(xmin = 11, xmax = 15, ymin = -1.5, ymax = 4, fill = False), alpha = 0, color = 'khaki', fill = 'black', size = 1.5) + 
  theme(legend.justification = c(0,1), 
        legend.position = c(0,1), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("../output/figs/2-3_14C_Vgl_Oslisly2013_map.pdf", width = 11, height = 11)


qmap(c(8, -8, 21, 8), zoom = 6, color = 'bw') + 
  geom_hex(aes(x = LONGITUDE, y = LATITUDE), data = df, alpha = .7) + 
  scale_fill_gradientn(colours=rev(heat.colors(12)))
ggsave("../output/figs/2-3_14C_DB_map.pdf", width = 11, height = 11)

# -------------------------------------------
# da es mit 1°-Zellen doch sehr fein aufgelöst ist, mal mit 2°-Zellen probieren:
# http://www.dbux.ch/math/roundnumber.html
df['X2'] <- ceiling(df['LONGITUDE'] / 2.) * 2
df['Y2'] <- ceiling(df['LATITUDE'] / 2.) * 2

# Arbeitsgebeit auswählen
# Oslsily et al. 2013: Daten zwischen 7°S -- 6°N & 9°E -- 20° E
df_c <- sqldf('select * from df where Y < 7
              AND Y > -8
              AND X > 8
              AND X < 21')

library(plyr)
cdat <- ddply(df_c, c("X2", "Y2"), summarise, C14AGE.mean=max(C14AGE))
cdat
library(dplyr)

# Anzahl Datierungen pro 2Grad-Zelle:
a <- tapply(df_c$C14AGE, list(df_c$Y2, df_c$X2), length)
a[nrow(a):1,]

# ältestes unkalibriertes 14C-Alter je 2Grad-Zelle
b <- tapply(df_c$C14AGE, list(df_c$Y2, df_c$X2), max)
b[nrow(b):1,]

# Plot
ggplot(df_c, aes(x=C14AGE)) + 
  geom_histogram() + 
  scale_x_reverse() + 
  facet_grid(Y2 ~ X2, as.table = FALSE) + 
  geom_vline(xintercept = 1950, linetype = "dashed") + 
  geom_vline(data = cdat, aes(xintercept = C14AGE.mean), colour = "red") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(filename = "../output/figs/2-3_14C_Kooord_2grad_facet.pdf", width = 16.5, height = 11.7, units = 'in')

# Karte der 2Grad-Grids
qmap(c(8, -8, 21, 8), zoom = 6, color = 'bw') + 
  geom_hline(yintercept=seq(-8, 7, by=2)) + 
  geom_vline(xintercept=seq(8, 21, by=2)) + 
  geom_hline(yintercept=0, color = 'red') + 
  theme(legend.position="none")
ggsave(filename = "../output/figs/2-3_14C_Kooord_2grad_facet_map.pdf", width = 11, height = 11)
