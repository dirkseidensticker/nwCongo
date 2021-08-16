#'---
#'title: "4.3 Integration - Kanonische CA"
#'author: "Dirk Seidensticker"
#'date: "`r format(Sys.time(), '%d %B %Y')`"
#'output: html_notebook
#'---

library(reshape2)
library(RSQLite)
library(vegan)

source("myfunctions.R")

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")

# für Test erst einmal nur Roulette
spe.l = dbGetQuery(con, "SELECT 
  t_Obj.objID As OBJ,
  [t_Ort].[ort_kurz] || ' ' || [t_Komplex].[bef_nr] || ':' || [t_Obj].[Individuum] AS IND,
  [t_K_Pos].[posReihe] || '_' || [t_K_Verz].[verzName] AS VERZ
FROM ((t_Ort LEFT JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
  LEFT JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID) t_Obj INNER JOIN 't_ObjPosVerz' ON t_Obj.objID = 't_ObjPosVerz'.objID
  INNER JOIN t_K_Pos ON 't_ObjPosVerz'.posID = t_K_Pos.posID
  INNER JOIN t_K_Verz ON 't_ObjPosVerz'.verzID = t_K_Verz.verzID
WHERE (((t_K_Pos.posID)<>1)
  AND ((t_K_Verz.verzName) != '')
  AND ((t_K_Verz.verzName) Like '21.%')
  AND ((t_Obj.Typ) != ''))")

spe.l <- spe.l[complete.cases(spe.l), ]

spe <- dcast(spe.l, IND ~ VERZ,
             value.var = "OBJ",
             fun.aggregate = length)

rownames(spe) <- spe[,1]
spe <- spe[,-1]

spe.ca <- cca(spe)

evplot(spe.ca$CA$eig)

plot(spe.ca)
