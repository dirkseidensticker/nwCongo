################
# 1 Einleitung #
################

library(ggplot2)
library(ggmap)
library(RSQLite)
library(reshape)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# bounding-box Arbeitsgebiet
# --------------------------
df_sitesDS = dbGetQuery(con, "SELECT t_Ort.ort_name, t_Ort.x_long, t_Ort.y_lat FROM t_Ort WHERE (((t_Ort.y_lat) <> '') AND ((t_Ort.ort_lit) Like '%DS%'))")
xmin <- min(df_sitesDS$x_long)
xmax <- max(df_sitesDS$x_long)
ymin <- min(df_sitesDS$y_lat)
ymax <- max(df_sitesDS$y_lat)

# Karte vom Arbeitsgebeit / Überischt
# -----------------------------------
qmap("lac tele", zoom = 3, color = 'bw') + 
  geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = TRUE), alpha = 0.1, color = 'black', fill = 'black') + 
  theme(legend.position = 'none')
#ggsave('../output/figs/1_Einleitung_Arbeitsgebiet1.pdf', width = 11, height = 11)

# *sonst.:*
qmap("lac tele", zoom = 3, maptype = "satellite") + 
  geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = TRUE), alpha = 0, color="red") + 
  theme(legend.position = "none", 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("../output/figs/1_Einleitung_Arbeitsgebiet2.pdf", width = 11, height = 11)

qmap("lac tele", zoom = 3) + geom_rect(mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=TRUE), alpha = 0, color="red") + theme(legend.position="none")

# Kartierung aller Fundstellen
# ----------------------------
# >> Datenaufbereitung in Python
# 
# Daten einlesen:
df <- read.csv("../data/processed/1 Fundstellen - Kartierung.csv")

# Kartierung
qmap(c(10, -5, 25, 7.5), zoom = 6, color = 'bw', legend = "topleft") + 
  geom_point(aes(x = x_long, y = y_lat, shape = ort_lit), data = df, size=3, fill = "white") + 
  scale_shape_manual(values=c(25,21, 24))
#ggsave("../output/figs/1_Einleitung_Fundstellen1.pdf", width = 11, height = 11)

qmap(c(10, -5, 25, 7.5), zoom = 6) + 
  geom_rect(mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, color="red", size = 0.2) + 
  geom_point(aes(x = x_long, y = y_lat, fill = ort_lit), data = df, size=3, shape = 21) + 
  theme(legend.justification=c(1,0), 
      legend.position=c(1,0), 
      axis.ticks = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) + 
  scale_fill_discrete(guide_legend(title = "Quelle"))
ggsave("../output/figs/1_Einleitung_Fundstellen2.pdf", width = 11, height = 11)

# Kartierung der eigene FdSt im Arbeitsgebiet
# -------------------------------------------
df_sitesDS = dbGetQuery(con, "SELECT
           t_Ort.ort_name,
           t_Ort.'Kat-Nr',
           t_Ort.x_long,
           t_Ort.y_lat
       FROM t_Ort
       WHERE (((t_Ort.y_lat) <> '')
           AND ((t_Ort.ort_lit) Like '%DS%'))")
colnames(df_sitesDS)[2] <- "Kat.Nr"
head(df_sitesDS)

df_sitesNotDS = dbGetQuery(con, "SELECT
           t_Ort.ort_name,
           t_Ort.'Kat-Nr',
           t_Ort.x_long,
           t_Ort.y_lat
       FROM t_Ort
       WHERE (((t_Ort.y_lat) <> '')
           AND ((t_Ort.ort_lit) Not Like '%DS%'))")
colnames(df_sitesNotDS)[2] <- "Kat.Nr"

# df <- read.csv("~/Dropbox/Dissertation/Auswertung/data/processed/1 Fundstellen DS - Kartierung.csv")

qmap("Dongou", zoom = 7, color = 'bw') + 
  geom_point(aes(x = x_long, y = y_lat), data = df, size=3, shape = 21, fill = "white") + 
  scale_shape_manual(values=c(21,23, 24)) + 
  theme(legend.position="none") + 
  geom_text(aes(x = x_long+0.05, y = y_lat+0.05, label = ort_name), data = df, size = 2, hjust=0, vjust=0)
#ggsave("../output/figs/1_Einleitung_FundstellenName.pdf", width = 11, height = 11)

qmap("Dongou", zoom = 7, color = 'bw') + 
  geom_point(aes(x = x_long, y = y_lat), data = df_sitesNotDS, size = 2, shape = 21, fill = "white") + 
  geom_point(aes(x = x_long, y = y_lat), data = df_sitesDS, size = 2, shape = 19) + 
  theme(legend.position="none") + 
  geom_text(aes(x = x_long+0.03, y = y_lat, label = Kat.Nr), data = df_sitesDS, size = 3, hjust=0, vjust=0)
#ggsave("../output/figs/1_Einleitung_FundstellenKatNr.pdf", width = 11, height = 11)
