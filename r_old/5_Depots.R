#########################
# 5 Funktion von Gruben #
#########################

library(ggplot2)
library(RSQLite)
library(reshape2)
library(sqldf)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                [t_Ort].[ort_kurz] || ' ' || [t_Komplex].[bef_nr] AS Bef,
                t_Obj.Gr_Clist,
                t_Obj.Gewicht
                FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID) 
                  INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID 
                WHERE (((t_Ort.ort_lit)='DS') 
                AND ((t_Komplex.bef_art) Not Like '%Oberfl%')
                AND ((t_Obj.Gr_Clist) != ''))")
head(df)

# Anzahlen ausrechnen
df_a <- data.frame(tapply(df$objID, list(df$Bef, df$Gr_Clist), length))

# Zielensummen errechnen und als Spalte anhängen
df_a$sums <- rowSums(df_a, na.rm = TRUE, dims = 1)
df_a$names <- rownames(df_a) 

# Auswahl: nur Komplexe mit mind 50 Stücken
# =======
df_b <- sqldf('select * from df_a where sums >= 50')

# Summenspalte wieder raus
df_b <- df_b[ ,!(colnames(df_b) == "sums")]

# Tabelle vom wide ins long-Format bringen
df_c <- melt(df_b, id.vars=c("names"))

# Plot erstellen
ggplot(df_c, aes(x = variable, y = value)) +
  geom_bar(stat="identity") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  facet_wrap(~ names, scales = "free_y")
ggsave("../output/figs/5_Depot_Bef-GrClist_gr50.pdf", width = 11, height = 8)

# Auswahl: nur Komplexe mit mind 9 Stücken (damit BLK 87/1 dabei ist)
# =======
df_b <- sqldf('select * from df_a where sums >= 9')

# Summenspalte wieder raus
df_b <- df_b[ ,!(colnames(df_b) == "sums")]

# Tabelle vom wide ins long-Format bringen
df_c <- melt(df_b, id.vars=c("names"))

# Plot erstellen
ggplot(df_c, aes(x = variable, y = value)) +
  geom_bar(stat="identity") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  facet_wrap(~ names, scales = "free_y")



# mit Summe des Gewichts -- nur Gruben oder Gräber (PIK, MUN & BLK):
# ------------------------------------------------------------------
df = dbGetQuery(con, "SELECT 
                t_Obj.objID, 
                [t_Ort].[ort_kurz] || ' ' || [t_Komplex].[bef_nr] AS Bef,
                t_Komplex.bef_art,
                t_Obj.Gr_Clist,
                t_Obj.Anzahl,
                t_Obj.Gewicht
                FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID) 
                INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID 
                WHERE (((t_Ort.ort_lit)='DS')
                AND ((t_Obj.Gr_Clist) != '')
                AND ((t_ort.ort_kurz) Like '%PIK%')
                OR ((t_ort.ort_kurz) Like '%BLK%')
                OR ((t_ort.ort_kurz) Like '%MUN%')
                OR ((t_ort.ort_kurz) Like '%MLB%'))")
head(df)

df <- sqldf('select * from df where Gr_Clist != 0')
df <- sqldf('select * from df where bef_art != "Oberfl" AND bef_art != "rezent"')

# unerwünschte Komplexe herausfiltern
df <- sqldf('select * from df where Bef != "MLB 85/1-4-3"')
df <- sqldf('select * from df where Bef != "MLB 85/103"')
df <- sqldf('select * from df where Bef != "PIK 87/3"')

# Bezeichnungen austauschen
df$Bef <- gsub("MLB 85/1-3-1", "Siedlungsgrube - MLB 85/1-3-1", df$Bef)
df$Bef <- gsub("MLB 85/1-3-2", "Siedlungsgrube - MLB 85/1-3-2", df$Bef)
df$Bef <- gsub("BLK 87/1", "Grab - BLK 87/1", df$Bef)
df$Bef <- gsub("MUN 87/1", "Verhüttungsbefund - MUN 87/1", df$Bef)
df$Bef <- gsub("MUN 87/2-1-1", "Keramikdeponierung - MUN 87/2-1-1", df$Bef)
df$Bef <- gsub("MUN 87/2-1-3", "Keramikdeponierung - MUN 87/2-1-3", df$Bef)
df$Bef <- gsub("MUN 87/3", "Verhüttungsbefund - MUN 87/3", df$Bef)
df$Bef <- gsub("PIK 87/1", "Siedlungsgrube - PIK 87/1", df$Bef)
df$Bef <- gsub("PIK 87/2", "Siedlungsgrube - PIK 87/2", df$Bef)
# df$Bef <- gsub("PIK 87/3", "Verhüttungsbefund - PIK 87/3", df$Bef)

# Anzahlen ausrechnen
df_a <- data.frame(tapply(df$Gewicht, list(df$Bef, df$Gr_Clist), sum))
df_a <- rename(df_a, c("X30" = "30", "X70" = "70", "X120" = "120", "X200" = "200", "X500" = ">200"))

# Zeilensummen errechnen und als Spalte anhängen
df_a$sums <- rowSums(df_a, na.rm = TRUE, dims = 1)
df_a$names <- rownames(df_a) 

# Auswahl: nur Komplexe mit mind 50 Stücken
# =======
df_b <- df_a

# df_b <- sqldf('select * from df_a where sums >= 50')

# Summenspalte wieder raus
df_b <- df_b[ ,!(colnames(df_b) == "sums")]

# Tabelle vom wide ins long-Format bringen
df_c <- melt(df_b, id.vars=c("names"))

# Plot erstellen
ggplot(df_c, aes(x = variable, y = value)) +
  geom_bar(stat="identity", width = 0.75) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  facet_wrap(~ names, scales = "free_y", ncol = 3) + 
  scale_x_discrete(name = "Größenklassen") + 
  scale_y_continuous(name = "Gewicht [g]")
ggsave("../output/figs/5_Depot_Bef-Gewicht.pdf", width = 10, height = 8)


# Kombination Gewicht und Anzahl
# ==============================
# siehe: http://stackoverflow.com/questions/18265941/two-horizontal-bar-charts-with-shared-axis-in-ggplot2-similar-to-population-pyr

library(grid)
library(gridExtra)
library(plyr)

# Summen der Anzahlen
df_d <- data.frame(tapply(df$Anzahl, list(df$Bef, df$Gr_Clist), sum))
df_d <- rename(df_d, c("X30" = "30", "X70" = "70", "X120" = "120", "X200" = "200", "X500" = ">200"))


# Zeilensummen errechnen und als Spalte anhängen
df_d$sums <- rowSums(df_d, na.rm = TRUE, dims = 1)
df_d$names <- rownames(df_d) 

# Summenspalte wieder raus
df_d <- df_d[ ,!(colnames(df_d) == "sums")]

# Tabelle vom wide ins long-Format bringen
df_e <- melt(df_d, id.vars=c("names"))

# Einzelteile des späteren Plots erzeugen
g.mid <- ggplot(df_c,aes(x = 1,y = variable)) + 
  geom_text(aes(label = variable), size = 4) +
  geom_segment(aes(x = 0.94,xend = 0.96,yend = variable))+
  geom_segment(aes(x = 1.04,xend = 1.065,yend = variable))+
  ylab(NULL)+
  scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065)) + 
  theme(axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(color = NA),
        axis.ticks.x = element_line(color = NA),
        plot.margin = unit(c(1,-1,1,1), "mm"))


g1 <- ggplot(df_c, aes(x = variable, y = value)) +
  geom_bar(stat="identity", width = 0.5) + 
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,5,1,1), "mm")) + 
  scale_x_discrete(name = "") + 
  scale_y_continuous(name = "Gewicht [g]") + 
  coord_flip()

g2 <- ggplot(df_e, aes(x = variable, y = value)) +
  geom_bar(stat="identity", width = 0.5) +
  theme_bw() + 
  theme(axis.title.x = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,1,1,5), "mm")) +  
  scale_x_discrete(name = "") + 
  scale_y_reverse(name = "Anzahl") + 
  coord_flip()


gg1 <- ggplot_gtable(ggplot_build(g1))
gg2 <- ggplot_gtable(ggplot_build(g2))
gg.mid <- ggplot_gtable(ggplot_build(g.mid))

pdf("../output/figs/5_Depot_Bef_Kombo.pdf", width = 10, height = 4)
grid.arrange(gg2, gg.mid, gg1, ncol = 3, widths = c(4/9,1/9,4/9))
dev.off()

# Gabun (Clist 2004/2005)
# =======================

df <- read.csv("../../docs/Arch/Afrika/Zentralafrika/Clist20042005/GAB_Fragmentierung.csv")

# nur Fpl. Okala
df <- sqldf("select * from df where site = 'Okala'")

df$complex <- paste(df$site, df$feature, sep = " - Grube ")

df <- rename(df, c("size" = "Gr_Clist", "n" = "Anzahl"))

df <- df[, c("complex", "Gr_Clist", "Anzahl")]

# Dummy-Werte erzeugen

complex = c('', '', '', '', '')
Gr_Clist = c(30, 70, 120, 200, 500) 
Anzahl = c(0, 0, 0, 0, 0) 
df_dummy = data.frame(complex, Gr_Clist, Anzahl) 
# Dummy-Liste an df anhängen
df <- rbind(df, df_dummy)

# Anzahl Summe ausrechnen
df_b <- data.frame(tapply(df$Anzahl, list(df$complex, df$Gr_Clist), sum,na.rm = TRUE))
df_b <- rename(df_b, c("X30" = "30", "X70" = "70", "X120" = "120", "X200" = "200", "X500" = ">200"))

df_b[is.na(df_b)] <- 0

df_c <- prop.table(as.matrix(df_b), 1)

df_c <- df_c * 100

df_d <- melt(df_c, id.vars=c("names"))

df_d <- na.omit(df_d)

#names(df_b)[1] <- "value"
# Index als Spalte
#df_b$variable <- rownames(df_b) 

# Sortierung beibehalten
#df$Gr_Clist[df$Gr_Clist == "500"] <- ">200"
#df$Gr_Clist <- as.character(df$Gr_Clist)
#df$Gr_Clist <- factor(df$Gr_Clist, levels = unique(df$Gr_Clist), ordered = TRUE)

# Plot
ggplot(df_d, aes(x = Var2, y = value)) + 
  geom_bar(stat="identity", width = 0.5, fill = "grey", color = "black") + 
  coord_flip() + 
  facet_wrap(~ Var1, ncol = 4) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(), 
#        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1,1,1,5), "mm"), 
        axis.ticks.length = unit(-0.15, "cm"), 
        axis.ticks.margin = unit(0.35, "cm")) + 
  scale_y_continuous(name = "Prozent")
ggsave("../output/figs/5_Depot_Gabun_Clist20042005.pdf", width = 8, height = 11)
