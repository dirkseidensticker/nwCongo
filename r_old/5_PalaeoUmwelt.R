#################################
# 1.5 Klima und Klimageschichte #
#################################

library(ggplot2)
library(ggmap)
library(RSQLite)
library(reshape)
library("RColorBrewer")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# bounding-box Arbeitsgebiet
df_sitesDS = dbGetQuery(con, "SELECT  t_Ort.ort_name, 
                                      t_Ort.x_long, 
                                      t_Ort.y_lat 
                              FROM t_Ort 
                              WHERE (((t_Ort.y_lat) <> '') 
                                      AND ((t_Ort.ort_lit) Like '%DS%'))")
xmin <- min(df_sitesDS$x_long)
xmax <- max(df_sitesDS$x_long)
ymin <- min(df_sitesDS$y_lat)
ymax <- max(df_sitesDS$y_lat)

# Verbreitung

df = dbGetQuery(con, "SELECT t_Ort.ort_name, t_Ort.y_lat, t_Ort.x_long, t_Ort.ort_notiz FROM t_Ort WHERE (((t_Ort.ort_beschr) Like '%Pollendaten%'))")
head(df)

# Datensätze aus Literatur (Sangen 2009)
Sangen2009Abb16 <- read.csv("../../../01_documents/archaeology/Afrika/Geographie/Sangen2009/43_Abb16.csv", dec=",")
Sangen2009Abb17A <- read.csv("../../../01_documents/archaeology/Afrika/Geographie/Sangen2009/46_Abb17A.csv", dec=",")
Sangen2009Abb17B <- read.csv("../../../01_documents/archaeology/Afrika/Geographie/Sangen2009/46_Abb17B.csv", dec=",")

# WICHTIG: ggmap funktioniert nur mit R 3.0, nicht in R 3.1!!!
# Kartierung
# Legende: Kreise = unsicher, Punkte = sicher

qmap("Mbandaka", zoom = 5, color = 'bw') + 
  geom_point(aes(x = x_long, y = y_lat), data = Sangen2009Abb16, size=3, shape = 21, fill = 'green') + 
  geom_point(aes(x = x_long, y = y_lat), data = Sangen2009Abb17A, size=3, shape = 21, fill = 'blue') + 
  geom_point(aes(x = x_long, y = y_lat), data = Sangen2009Abb17B, size=3, shape = 21, fill = 'yellow') + 
  geom_point(aes(x = x_long, y = y_lat), data = df, size=3, shape = 21, fill = 'red') + 
  geom_text(aes(x = x_long+2, y = y_lat+0.5, label = ort_name), data = df, size = 5) + ggtitle("Pollendaten\n")
# ggsave("../output/1-5_Pollendaten_Kartierung.pdf", width = 11, height = 8.5)

# Daten aus der DB auf Tabellennamen von Mark abändern
colnames(df) <- c("NAME", "y_lat", "x_long", "kaBP")
     
# die drei Datensätze von Mark zusammen bringen
Sangen2009 <- rbind(Sangen2009Abb16, Sangen2009Abb17A, Sangen2009Abb17B)
library(gtools)
data <- smartbind(Sangen2009, df)

max <- max(data$kaBP, na.rm = TRUE)
min <- min(data$kaBP, na.rm = TRUE)

# die NA's aus der DB austauschen
library(car)
data$TYP <- recode(data$TYP, 'c(NA) = "(?)"')
     
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(min, max), name = "Datierung (kaBP)")
     
qmap("Mbandaka", zoom = 5, color = 'bw') + 
  geom_rect(mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=TRUE), alpha = 0.025, color="black", fill = 'black', size = 0.1) + 
  geom_point(aes(x = x_long, y = y_lat, fill = kaBP, shape = factor(TYP)), data = data, size = 5) + 
  sc + 
  scale_shape_manual(values = c(23,24,25), name = "Typ") + 
  theme(legend.justification = c(1,0), 
        legend.position = c(1,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("../output/figs/5_PalaeoUmwelt_B.pdf", width = 11, height = 11)


# Variante mit Arbeitsgebiet des Kongo-Projekts und größeren Symbolen
# -------------------------------------------------------------------
qmap("Mbandaka", zoom = 5, color = 'bw') + 
  geom_rect(mapping=aes(xmin = 17.5, xmax = 24, ymin = -1.75, ymax = 2.5, fill=TRUE), alpha = 0.025, color="black", fill = 'black', size = 0.1) + 
  geom_point(aes(x = x_long, y = y_lat, fill = kaBP, shape = factor(TYP)), data = data, size = 5) + 
  sc + 
  geom_point(x = 18.185858, y = -0.034686, shape = 21, fill = "indianred1", size = 5) + 
  geom_text(x = 18.185858 + 0.3, y = -0.034686 + 0.3, label = "Iyonda", size = 6, color = 'indianred1', hjust=0, vjust=0) + 
  geom_point(x = 21.253168, y = -0.305801, shape = 21, fill = "indianred1", size = 5) + 
  geom_text(x = 21.253168 + 0.3, y = -0.305801 + 0.3, label = "Bolondo", size = 6, color = 'indianred1', hjust=0, vjust=0) + 
  scale_shape_manual(values = c(23,24,25), name = "Typ") + 
  theme(legend.justification = c(0,0), 
        legend.position = c(0,0), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave("../output/figs/5_PalaeoUmwelt_C.pdf", width = 11, height = 11)
ggsave("../output/figs/5_PalaeoUmwelt_C.jpg", width = 11, height = 11)
