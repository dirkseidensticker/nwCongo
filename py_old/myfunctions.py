import math
import matplotlib.colors as colors
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sqlite3

con = sqlite3.connect("../data/CongoDB.sqlite")


# eigene Graustufen-cmap
# http://stackoverflow.com/questions/18926031/how-to-extract-a-subset-of-a-colormap-as-a-new-colormap-in-matplotlib
def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=100):
    new_cmap = colors.LinearSegmentedColormap.from_list(
        'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
        cmap(np.linspace(minval, maxval, n)))
    return new_cmap


# Grenzen des Arbeitsgebietes:

def studyarea_box ():
    sql = """SELECT
           t_Ort.ort_kurz,
           t_Ort."Kat-Nr",
           t_Ort.y_lat,
           t_Ort.x_long
       FROM t_Ort
       WHERE ((t_Ort.ort_lit) Like '%DS%')""".replace('\n',' ')

    df = pd.read_sql(sql, con)
    df['x_long'] = pd.to_numeric(df['x_long'])
    df['y_lat'] = pd.to_numeric(df['y_lat'])
    # display(df.head())

    a = pd.Series({'X/Länge - Minimum':np.min(df['x_long']),
               'X/Länge - Maximum':np.max(df['x_long']),
               'Y/Breite - Minimum':np.min(df['y_lat']),
               'Y/Breite - Maximum':np.max(df['y_lat'])})
    return (df, a)


# Matrizen aus Gefäßpositionen und Verzierungselemnten erstellen
# nur GE bei denen der erstegenannten Typ der gesuchten Stilgruppe entspricht (nicht 2./3.-Wahl-stücke)
def decoration_positon_matrices_print ( style ):
    sql = """SELECT
           t_Obj.objID,
           t_K_Pos.posID,
           t_K_Verz.verzID,
           [t_K_Pos].[posReihe] || ' ' || [posName] AS pos,
           t_K_Verz.verzName,
           t_K_Verz.verzBeschr,
           t_Obj.Typ,
           t_Obj.Form_Gef
       FROM t_Obj INNER JOIN "t_ObjPosVerz" ON t_Obj.objID = "t_ObjPosVerz".objID
           INNER JOIN t_K_Pos ON "t_ObjPosVerz".posID = t_K_Pos.posID
           INNER JOIN t_K_Verz ON "t_ObjPosVerz".verzID = t_K_Verz.verzID
       WHERE (((t_K_Pos.posID)<>1)
           AND ((t_K_Verz.verzName) != '')
           AND ((t_Obj.Typ) Like '"""+style+"""%'))""".replace('\n',' ')
    df = pd.read_sql(sql, con)
    
    # Auswahl der Spalten 'pos' und 'verzName'
    df1 = df[['pos','verzName']]
    # Neue Spalte hinzufügen, die einfach den Wert 1 enthält
    df1['n'] = 1
    # Blankliste aus der CSV_Datei, die in a Keramik PosVerz-Matrix leer.ipynb erzeugt wird
    dfblank = pd.read_csv('../data/processed/Keramik - PosVerz-Matrix - leer.csv', decimal=',')

    # Blankoliste an Liste der Verzierungen anhängen
    bigdata = df1.append(dfblank,ignore_index = True)

    # Kreuztabelle aus der Summe von Feld n berechnen – Blankoliste = 0 vs. DB = 1
    df_pivotBig = bigdata.pivot_table(values = 'n', index = 'pos', columns = 'verzName', aggfunc = np.sum)
    df_pivotBig = df_pivotBig.sort_index(ascending=False)

    # Prozente bezogen auf die Summe der Zeilen-Summen (also insgesamt)
    df_pivotBig_perc = df_pivotBig.apply(lambda c: c / df_pivotBig.sum().sum() * 100, axis=1)

    # Sortierung für graphische Darstellung umdrehen
    df_pivotBig_perc = df_pivotBig_perc.sort_index(ascending = True)

    # Sortierung ändern
    # Spalten-Namen herausziehen und individuell sortieren
    a = pd.DataFrame(df_pivotBig_perc.columns)
    b = a['verzName'].str.split('.', expand=True).astype(str)
    ab = pd.concat([a, b], axis = 1)
    ab[1] = ab[1].convert_objects(convert_numeric=True)
    ab = ab.sort_values([0,1], ascending=[True,True])
    # Liste aus neu sortiertem Index
    ls = ab['verzName'].tolist()

    df_pivotBig_perc = df_pivotBig_perc.reindex(columns=ls)
    
    # Beschriftung für die Zellen separat erstellen:
    df_pivotBig_perc_a = df_pivotBig_perc
    df_pivotBig_perc_a[df_pivotBig_perc_a == 0] = 5000
    df_pivotBig_perc_a[df_pivotBig_perc_a < 0.5] = 0.51
    df_pivotBig_perc_a = df_pivotBig_perc_a.round(0)
    df_pivotBig_perc_a = df_pivotBig_perc_a.astype(int)
    df_pivotBig_perc_a = df_pivotBig_perc_a.replace(5000, '-')
    
    # neu: stellt die 0er-Felder wiede auf NaN zurück, damit diese von der cmap nicht ausgefüllt werden (28.01.2017)
    df_pivotBig_perc[df_pivotBig_perc == 5000] = 0
    df_pivotBig_perc = df_pivotBig_perc.replace(0, np.nan)
    
    cmap = plt.get_cmap('gray_r')
    new_cmap = truncate_colormap(cmap, 0, 0.4)

    return (df_pivotBig_perc, df_pivotBig_perc_a, new_cmap)


# Dataframe aus Fpl.-Daten um die Verbreitungskarten in QGIS zu erstellen
# CSV-Export + Anhang von Sonderfällen in jeweiligem Notebook!
def style_mapping_gis_export ( style ):
    # Fpl. mit sicher zuweisbaren Stücken
    sql = """SELECT
           t_Obj.objID,
           't_Ort'.'Kat-Nr',
           't_Ort'.'ort_name' || ' (Fpl. ' || 't_ort'.'Kat-Nr' || ')' AS Ort,
           t_Obj.Typ,
           t_Ort.y_lat AS Y, 
           t_Ort.x_long AS X
       FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
           INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID
       WHERE (((t_Ort.ort_lit) = 'DS')
           AND ((t_Obj.Typ) = '"""+style+"""'))""".replace('\n',' ')

    df_sicher = pd.read_sql(sql, con)

    df_sicher_pivot = df_sicher.pivot_table(values = 'objID', index = ['Ort', 'Kat-Nr', 'X', 'Y'], columns = 'Typ', aggfunc = len)
    df_sicher_pivot = df_sicher_pivot.reset_index()
    df_sicher_pivot['TYP'] = style
    df_sicher_pivot = df_sicher_pivot[['Ort', 'Kat-Nr', 'X', 'Y', 'TYP']]
    
    # Fpl. mit fraglichen Stücken
    sql = """SELECT
           t_Obj.objID,
           't_Ort'.'Kat-Nr',
           't_Ort'.'ort_name' || ' (Fpl. ' || 't_ort'.'Kat-Nr' || ')' AS Ort,
           t_Obj.Typ,
           t_Ort.y_lat AS Y, 
           t_Ort.x_long AS X
       FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
           INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID
       WHERE (((t_Ort.ort_lit) = 'DS')
           AND ((t_Obj.Typ) Like '%"""+style+"""%'))""".replace('\n',' ')

    df_fraglich = pd.read_sql(sql, con)

    df_fraglich_pivot = df_fraglich.pivot_table(values = 'objID', index = ['Ort', 'Kat-Nr', 'X', 'Y'], columns = 'Typ', aggfunc = len)
    df_fraglich_pivot = df_fraglich_pivot.reset_index()
    df_fraglich_pivot['TYP'] = style+' (?)'
    df_fraglich_pivot = df_fraglich_pivot[['Ort', 'Kat-Nr', 'X', 'Y', 'TYP']]

    # alles raus, was schon in df_sicher_pivot drin ist:
    df_fraglich_pivot = df_fraglich_pivot[~df_fraglich_pivot['Kat-Nr'].isin(df_sicher_pivot['Kat-Nr'])]
    # die beiden Dataframes aneinander hängen
    df = pd.concat([df_sicher_pivot, df_fraglich_pivot])
    return (df)

# Umrechnung von Dezimalgrad in Grad/Minute/Sekunde
# siehe http://anothergisblog.blogspot.de/2011/11/convert-decimal-degree-to-degrees.html
# bei Werte 0<1° bzw. -1<0° funktiniert der Skript nicht korrekt
def decimalDegrees2DMS(value,type):
    """
        Converts a Decimal Degree Value into
        Degrees Minute Seconds Notation.
        
        Pass value as double
        type = {Latitude or Longitude} as string
        
        returns a string as D:M:S:Direction
        created by: anothergisblog.blogspot.com 
    """
    degrees = int(value)
    submin = abs( (value - int(value) ) * 60)
    minutes = int(submin)
    subseconds = abs((submin-int(submin)) * 60)
    direction = ""
    if type == "Longitude":
        if degrees < 0:
            direction = "W"
        elif degrees > 0:
            direction = "E"
        else:
            direction = ""
    elif type == "Latitude":
        if degrees < 0:
            direction = "S"
        elif degrees > 0:
            direction = "N"
        else:
            direction = "" 
    notation = str(degrees) + "º " + str(minutes) + "' " +\
               str(subseconds)[0:5] + "'' " + direction
    return notation