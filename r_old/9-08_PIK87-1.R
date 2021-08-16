#######################
# Kat.-Nr. 8 PIK 87/1 #
#######################

library(ggplot2)
library(sqldf)

# Seriation Stilgruppen
# ---------------------




# Variante als battleship-plot

library(plotrix)
df <- read.csv("../data/processed/9-08_PIK87-1_SeriationPivot_transf.csv", row.names = 1)
df <- data.matrix(df, rownames.force = NA)

battleship.plot(df, main = "Keramikgruppen in PIK 87/1", xlab = "Gruppen", ylab = "Tiefe [cm u. NP]", xaxlab = NA, yaxlab = NA)

battleship.plot(df, col = "slategrey", ylab = "Tiefe [cm u. NP]", xaxlab = NA, yaxlab = NA, border = 0)



# Fragmentierung der Stücke
# =========================

library(grid)
library(gridExtra)
library(plyr)
library(ggplot2)
library(RSQLite)
library(reshape2)
library(sqldf)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

df = dbGetQuery(con, "SELECT 
                  t_Obj.objID, 
                  t_Obj.Gr_Clist,
                  t_Obj.Anzahl,
                  t_Obj.Gewicht
                FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                  INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID 
                WHERE (((t_Obj.ort_kurz) = 'PIK')
                  AND ((t_Obj.Komplex) = '87/1')
                  AND ((t_Obj.Gr_Clist) != '')
                  AND ((t_Obj.Art) = 'K'))")
head(df)

# Dummy-Werte erzeugen

objID = c(0, 0, 0, 0, 0)
Gr_Clist = c(30, 70, 120, 200, 500) 
Anzahl = c(0, 0, 0, 0, 0) 
Gewicht = c(0, 0, 0, 0, 0)
df_dummy = data.frame(objID, Gr_Clist, Anzahl, Gewicht) 
# Dummy-Liste an df anhängen
df <- rbind(df, df_dummy)
  
# Gewicht Summe ausrechnen
df_a <- data.frame(tapply(df$Gewicht, list(df$Gr_Clist), sum))
names(df_a)[1] <- "value"
# Index als Spalte
df_a$variable <- rownames(df_a) 

# Anzahl Summe ausrechnen
df_b <- data.frame(tapply(df$Anzahl, list(df$Gr_Clist), sum))
names(df_b)[1] <- "value"
# Index als Spalte
df_b$variable <- rownames(df_b) 

# Sortierung beibehalten
df_a$variable[df_a$variable == "500"] <- ">200"
df_a$variable <- as.character(df_a$variable)
df_a$variable <- factor(df_a$variable, levels = unique(df_a$variable), ordered = TRUE)

df_b$variable[df_b$variable == "500"] <- ">200"
df_b$variable <- as.character(df_b$variable)
df_b$variable <- factor(df_b$variable, levels = unique(df_b$variable), ordered = TRUE)

# für Limit maximum ermitteln und 5% draufschlagen
max_a <- max(df_a$value, na.rm = TRUE)
max_a <- max_a + (0.05*max_a)
max_b <- max(df_b$value, na.rm = TRUE)
max_b <- max_b + (0.05*max_b)


# Einzelteile des späteren Plots erzeugen
g.mid <- ggplot(df_a,aes(x = 1,y = variable)) + 
  geom_text(aes(label = variable), size = 4) +
  geom_segment(aes(x = 0.94,xend = 0.96,yend = variable))+
  geom_segment(aes(x = 1.04,xend = 1.065,yend = variable))+
  ylab(NULL)+
  scale_x_continuous(name = "", expand=c(0,0),limits=c(0.94,1.065)) + 
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = NA),
        axis.ticks.x = element_line(color = NA),
        plot.margin = unit(c(1,-1,1,1), "mm"))

g1 <- ggplot(df_a, aes(x = variable, y = value)) +
  geom_bar(stat="identity", width = 0.5, fill = "grey", color = "black") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,5,1,1), "mm"), 
        axis.ticks.margin = unit(0.35, "cm")) + 
  scale_x_discrete(name = "") + 
  scale_y_continuous(name = "Gewicht [g]", limits = c(0, max_a), expand = c(0,0)) + 
  coord_flip()

g2 <- ggplot(df_b, aes(x = variable, y = value)) +
  geom_bar(stat="identity", width = 0.5, fill = "grey", color = "black") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,1,1,5), "mm"), 
        axis.ticks.margin = unit(0.35, "cm")) +  
  scale_x_discrete(name = "") + 
  scale_y_reverse(name = "Anzahl", limits = c(max_b, 0), expand = c(0,0)) + 
  coord_flip()

gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

pdf("../output/figs/9-08_PIK87-1_Fragmentierung_2.pdf", width = 10, height = 2.5)
grid.arrange(gg2, gg.mid, gg1, ncol = 3, widths = c(4/9,1/9,4/9))
dev.off()

# n in dieser Abb:
sum(df_b$value)
