library(ggplot2)
library(RSQLite)


drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

### Proportionen der Gefäße
df = dbGetQuery(con, "SELECT
                  [t_Ort].[ort_kurz] || ' ' || [t_Komplex].[bef_nr] || ':' || [t_Obj].[Individuum] AS Ind,
                  t_Obj.minD,
                  t_Obj.muendungsH,
                  t_Obj.maxD,
                  t_Obj.Form_Gef
                FROM (t_Ort LEFT JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
                  LEFT JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID
                WHERE (((t_Ort.ort_lit)='DS')
                  AND ((t_Obj.Form_Gef) != '')
                  AND ((t_Obj.Form_Gef) Not Like '%?%')
                  AND ((t_Obj.Form_Gef) Not Like '%/%'))")
head(df)

# Spalten in numeric umwandeln
df[, c(2,4)] <- sapply(df[, c(2,4)], as.numeric)

ggplot(df, aes(x = maxD, y = muendungsH, colour = Form_Gef)) + 
  geom_point(colour = "grey50", size = I(6)) + 
  geom_point(size = I(5), alpha = I(0.75)) + 
  coord_equal() +
  scale_y_continuous("Mündungshöhe [cm]") + 
  scale_x_continuous("Maximaldurchmesser [cm]")
#ggsave("../output/figs/3-1-2-12_Jeke_Keramik_Proportionen.pdf", width = 11, height = 8.5)


# Eng-/Weitmündigkeit errechnen

df$minD_maxD <- (df$minD / df$maxD)
df$minD_muendungsH <- (df$minD / df$muendungsH)

ggplot(df, aes(x = minD_maxD, y = minD_muendungsH, fill = Form_Gef)) + 
  geom_point(pch = 21, size = I(5), alpha = I(0.75)) + 
#  coord_equal() +
  scale_y_continuous("Minimaler Durchmesser / Maximaler Durchmesser") + 
  scale_x_continuous("Minimaler Durchmesser / Höhe der Mündung")

# [ ] Gefäße gegen neue Systematik ersetzen und dann nur die Grundförmen plotten! (DS, 06.02.2016)

