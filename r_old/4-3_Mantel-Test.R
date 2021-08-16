###################
# 4.3 Integration #
###################

library(data.table)
library(gplots)
library(RColorBrewer)
library(RSQLite)
library(vegan)

drv <- dbDriver("SQLite")
con <- dbConnect(drv, "../data/CongoDB.sqlite")


# Heatmap der p-Werte
# scaleCol <- colorRampPalette(brewer.pal(11, "Spectral"), space="Lab")
scaleCol <- colorRampPalette(c("forestgreen", "yellow", "red"))(n = 4)

# Funktion für die eigentliche Arbeit
# ===================================
mantel_test <- function(TYP){
  n <- length(TYP)
  for(i in 1:n)
  {
    df_blank <- read.csv('../data/processed/Keramik - PosVerz-Matrix - leer.csv', dec=',')    # Blankotabelle laden
    df = dbGetQuery(con, paste("SELECT 
                               [t_K_Pos].[posReihe] || ' ' || [posName] AS pos,
                               t_K_Verz.verzName
                               FROM t_Obj INNER JOIN 't_ObjPosVerz' ON t_Obj.objID = 't_ObjPosVerz'.objID
                               INNER JOIN t_K_Pos ON 't_ObjPosVerz'.posID = t_K_Pos.posID
                               INNER JOIN t_K_Verz ON 't_ObjPosVerz'.verzID = t_K_Verz.verzID
                               WHERE (((t_K_Pos.posID)<>1)
                               AND ((t_K_Verz.verzName) != '')
                               AND ((t_Obj.Typ) Like '%",TYP[[i]],"%'))", sep = ""))
    df$n <- 1     # Zählspalte anfügen
    df <- rbind(df, df_blank)    # an Blank-Liste anhängen
    df <- data.frame(tapply(df$n, list(df$pos, df$verzName), sum))    # Matrix erzeugen
    write.csv(df, paste('../data/processed/MantelTest_VerzPos_',TYP[[i]],'.csv', sep = ""))   # in CSV schreiben, um sie im übernächsten Schritt wieder einzulesen
    assign(TYP[[i]],df)    # Ausgabe Dataframe erzeugen
  }
  
  # Mantel-Test
  # ===========
  komb <- table(unlist(TYP), unlist(TYP))   # leere Kombinationsmatrix aufbauen
  komblist <- data.frame(komb)    # Liste mit allen Kombinationen (die kann man jetzt in einer Schleife durchgehen!)
  n <- nrow(komblist)
  for(i in 1:n)
  {
    x <- read.csv(paste('../data/processed/MantelTest_VerzPos_',as.name(paste(komblist$Var1[[i]])),'.csv', sep = ""), row.names = 1)   # Daten aus den CSVs wieder einlesen
    y <- read.csv(paste('../data/processed/MantelTest_VerzPos_',as.name(paste(komblist$Var2[[i]])),'.csv', sep = ""), row.names = 1)
    xdis <- dist(x)   # Berechnung der Distanzmatrizen
    ydis <- dist(y)
    r1 <- mantel(xdis, ydis)    # Mantel-Test (vegan)
    komblist$Freq[[i]] <- r1$signif
  }
  
  mantel <- tapply(komblist$Freq, list(komblist$Var1, komblist$Var2), sum)    # aus der gefüllten Liste wieder eine Kreuz-Tabelle machen
  
  mantel <- data.frame(mantel)
  # Sortierung der Zeilen nach der Eingabelist - Chronologisch!
  mantel <- mantel[match(TYP, rownames(mantel)),]
  # mantel <- mantel[match(rev(TYP), rownames(mantel)),]    # Sortierung der Zeilen umdrehen (=stratigraphische Abfolge)

  # Sortierung der Spalten nach der Eingabelist - Chronologisch!
  setcolorder(mantel, unlist(TYP))
  mantel <- as.matrix(mantel)
  return(mantel)
}

# Allen Stilgruppen
# -----------------

TYP_all <- list("BTM", "NGB", "MKL", "BKW", "DON", "BBL", "MTB", "KPT", "DAM", "BAN", "PKM", "NGO", "MAT", "BBS", "EBA", "OUE", "MDB", "KON", "PDM", "MBJ", "MKA", "EPE")   # Gefäßpositions-Verzierungselemente für alle Stilgruppen:
TYP <- TYP_all

mantel <- mantel_test(TYP)

write.csv(mantel, "../data/processed/4-3_Mantel-Test.csv")

#pdf("../output/figs/4-3_UBA-Region_Mantel-Test_heatmap2.pdf", width = 8, height = 8)
heatmap.2(mantel, Rowv=NA, Colv=NA, col=scaleCol, trace="none")
#dev.off()

library('corrplot') #package corrplot
corrplot(mantel, 
         method = "circle") #plot matrix


# PCA see https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
# ----------------------------------------------------------------------

# log transform 
log.mantel <- log(mantel)
mantel.species <- rownames(log.mantel)

# apply PCA 
mantel.pca <- prcomp(log.mantel,
                     center = TRUE,
                     scale. = TRUE) 

plot(mantel.pca, type = "l")

summary(mantel.pca)

biplot(mantel.pca, scale = 0)






# Ubangi-/Lua-Region
# ------------------
TYP_UBA <- list("BatMLB", "NGB", "MKL", "BKW", "DON", "BBL", "MTB", "KPT", "DAM", "BAN")
TYP <- TYP_UBA

mantel <- mantel_test(TYP)

write.csv(mantel, "../data/processed/4-3_UBA-Region_Mantel-Test.csv")

pdf("../output/figs/4-3_UBA-Region_Mantel-Test_heatmap2.pdf", width = 8, height = 8)
heatmap.2(mantel, Rowv=NA, Colv=NA, col=scaleCol, trace="none")
dev.off()

# Sangha/Likwala-Region
# ---------------------
TYP_SHG <- list("PIKMUN", "NGO", "MAT", "BBS", "EBA", "OUE", "MDB", "KON", "PDM", "MBJ", "MKA", "JEK")
TYP <- TYP_SHG

mantel <- mantel_test(TYP)

write.csv(mantel, "../data/processed/4-3_SGH-Region_Mantel-Test.csv")

pdf("../output/figs/4-3_SGH-Region_Mantel-Test_heatmap2.pdf", width = 8, height = 8)
heatmap.2(mantel, Rowv=NA, Colv=NA, col=scaleCol, trace="none")
dev.off()

