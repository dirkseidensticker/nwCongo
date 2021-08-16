# Funktionen
# ==========

# Removing white space around R figures
# -------------------------------------
# http://robjhyndman.com/hyndsight/crop-r-figures/

savepdf <- function(file, width=16, height=10)
{
  fname <- paste(file)
  pdf(fname, width=width, height=height,
      pointsize=10)
  par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.5,1.1))
}


# Verbreitungskarte von Fabrics
# -----------------------------

FabricsMappingQuery <- function(x){
df = dbGetQuery(con, paste("SELECT 
                t_Obj.objID, 
                t_Ort.ort_kurz, 
                't_Ort'.'ort_kurz' || '/' || 't_Ort'.'x_long' || '/' || 't_Ort'.'y_lat' AS SITE, 
                t_Komplex.bef_nr, 
                t_Obj.Individuum, 
                t_Obj.MagerungArt, 
                t_Obj.Fabric, 
                t_Ort.x_long, 
                t_Ort.y_lat 
            FROM t_Obj INNER JOIN t_Komplex ON t_Obj.komplexID = t_Komplex.komplexID INNER JOIN t_Ort ON t_Komplex.ortID = t_Ort.ortID 
            WHERE (((t_Obj.Fabric) = '",x,"')
                AND ((t_Obj.Fabric) Not Like '%/%'))", sep = ""))

    # variable-Feld wieder auseinander schneiden - um Koordinaten zu bekommen
    df_lable <- as.data.frame(unique(df$SITE))
    names(df_lable)[1] <- "SITE"
    df_lable$SITE <- as.character(df_lable$SITE)
    df_lable <- transform(df_lable, Var = do.call(rbind, strsplit(df_lable$SITE, '/', fixed = TRUE)), stringsAsFactors = F)
    df_lable$Var.2 <- as.numeric(as.character(df_lable$Var.2))
    df_lable$Var.3 <- as.numeric(as.character(df_lable$Var.3))
    
    # Prozentanteil je Fpl ausrechnen
    df_a <- data.frame(tapply(df$objID, list(df$SITE), length))
    names(df_a)[1] <- "value"
    df_a$variable <- rownames(df_a) 
    # head(df_a)

    # Prozent je Fpl ausrechnen
    df_merge <- merge(x = df_a, y = df_all_a, by = 'variable')
    df_merge$pc <- df_merge$value / df_merge$value_all * 100

    # variable-Feld wieder auseinander schneiden - um Koordinaten zu bekommen
    df_merge <- transform(df_merge, test = do.call(rbind, strsplit(df_merge$variable, '/', fixed = TRUE)), stringsAsFactors = F)

    # Spalten in numeric umwandeln
    df_merge$test.2 <- as.numeric(as.character(df_merge$test.2))
    df_merge$test.3 <- as.numeric(as.character(df_merge$test.3))
    
    df_list <- list(df, df_lable, df_merge)
    
    return(df_list)
}


# Fragmentierung mit Anzahl und Gewicht
# -------------------------------------

FragmentationGraphics <- function(df){
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
          axis.ticks.length = unit(0.15, "cm"), 
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
          axis.ticks.length = unit(0.15, "cm"), 
          axis.ticks.margin = unit(0.35, "cm")) +  
    scale_x_discrete(name = "") + 
    scale_y_reverse(name = "Anzahl", limits = c(max_b, 0), expand = c(0,0)) + 
    coord_flip()
  
  gg1 <- ggplot_gtable(ggplot_build(g1))
  gg2 <- ggplot_gtable(ggplot_build(g2))
  gg.mid <- ggplot_gtable(ggplot_build(g.mid))
  
  return_list <- list(gg1, gg2, gg.mid)
  return(return_list)
}

# https://github.com/JoeyBernhardt/NumericalEcology/blob/master/evplot.R
# Plot eigenvalues and percentages of variation of an ordination object
# Kaiser rule and broken stick model
# Usage:
# evplot(ev)
# where ev is a vector of eigenvalues

# License: GPL-2 
# Author: Francois Gillet, 25 August 2012

evplot <- function(ev)
{
  # Broken stick model (MacArthur 1957)
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p <- 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op <- par(mfrow=c(2,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="% variation", col=c("bisque",2), las=2)
  legend("topright", c("% eigenvalue", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}

# blank theme for ggplto2 (http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
