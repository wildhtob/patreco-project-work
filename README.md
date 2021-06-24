---
editor_options: 
  markdown: 
    wrap: 72
---

# Recherche

## Suhlen (Aus Keuling und Stier, 2009):

-   Keine vorhanden im Februar und März NÁNDEZ-LLARIO (2005)
-   Suhlen in der warmen Jahreszeit MEYNHARDT (1990)
-   erstreckten sich ausschließlich über die Nachtphasen, wobei kein
    eindeutiger allgemeiner nächtlicher Aktivi- tätshöhepunkt zu
    erkennen war
-   Auch MEYNHARDT (1990) und RAHN (2004) gaben die Hauptaktivitäts-
    phasen an Suhlen hauptsächlich in den Morgenstunden an.
-   Die kürzere Aufenthaltsdauer an der Suhle im Gegensatz zu den
    Kirrungen ist nicht auf Unsicherheit oder Hektik begründet, sondern
    vielmehr auf ein kürzeres Grundbedürfnis der Körperpflege.
-   Suhlen ist ein Komfortverhalten und dient fürs Wohlgefühl, daher
    findet es weit weniger statt als Nahrungsuche und Sicherung
-   Suhlen befinden sich zumeist in ruhigen Revierteilen (RAHN 2004) und
    können somit als Ruhezone für das Schwarzwild angesehen werden.
-   Rund um die Suhle findet Körperpflege statt. Dafür werden Bäume
    benutzt (Buchenholz)
-   Feuchtland wird nachts stark genutzt. Es dient also neben der
    Deckungsqualität im Schilf als Ta- geseinstand auch als
    Nahrungsquelle durch verschiedene Rhizome (TUCAK 1996) und wird
    zusätzlich zum Suhlen genutzt. An den Kirrungen hielt sich das
    Schwarzwild selten länger als 45 Minuten auf (IHDE 2004,
    SAEBEL 2007)
-   Kirren: Angewöhnen von Futterstellen, Anfütterung.

## Reproduktive Nester:

-   Aus Schlageter 2015: Group composition is a function of life cycle
    and seasonal changes. Reproductive females separate from the group
    shortly before giving birth. About one or two weeks after having
    built a farrowing nest and having given birth to an average of 5
    piglets, the sow usually rejoins her initial group (Martys 1982,
    Teillaud 1986). Most births take place around March – April but
    reproduction can occur all year round.

## Schlafnester:

-   Aus Suter et al. 2018: Neben Frassschäden entstehen dabei auch
    Schäden an jenen Orten, an denen die Tiere ihre Schlafnester bauen.
    Beliebte Einstände bieten Raps, Weizen und Mais. In Jahr 2016
    verblieben auch einige besenderte Individuen tagsüber in den
    Feldern.

## Schlussbericht Wildschweinprojekt (Suter 2010):

-   Zwischen 8:00 und 17:00 ruhen Bachen in ihrem Tagesnest.
-   Ab 70cm Vegetationshöhe (ab Mai) auch Tagesnest in einem Feld
    möglich (Raps- Mais- Getreide)
-   Aufenthaltsort vs. Feldhöhe nach Monat -\> Abb. 11
-   bis zu achtmal wird derselbe Einstand genutzt, im Schnitt 1.4 mal
    (aufeinanderfolgend).
-   Distanz zwischen zwei Feldern über mehrere Tage: max. 900m,
    durchschnittlich 417m.
-   Des Weiteren geht aus den Daten hervor, dass die untersuchten
    Wildschweine eine sogenannte alternierende Nutzung von
    Tageseinständen aufweisen. Dabei wird für den Tageseinstand, über
    mehrere Tage, jeweils täglich zwischen zwei verschiedenen Feldern,
    die sich bis zu 900m voneinander entfernt befinden können, hin und
    her gewechselt.
-   Präferenz für Uferwälder

## Interessante Quellen für Movement patterns:

-   Dissertation von Keuling 2009: Managing wild boar-Considerations for
    wild boar management based on game biology data
-   Methodisches Paper zu Homerange und KDE: LoCoH: Nonparameteric
    Kernel Methods for Constructing Home Ranges and Utilization
    Distributions
-   [Geocomputation with R](https://geocompr.robinlovelace.net/)

# Vorgehen

-   Preprocessing: Convenience Variabeln erstellen (segment_id,
    Kriterien)
-   Schritt 0 Sample 1 Tier, begrenter Zeitraum
-   Schritt 1: Segementierung
-   Schritt 2: Steplength und Speed berechnen
-   Schritt 3: Verteilung anschauen
-   Schritt 4: Schwellenwert definieren und resting / moving festlegen
-   Schritt 5: Nur resting Data weiterverarbeiten
-   Schritt 6: Kriterien für Nester und Suhlen definieren und anwenden
-   Schritt 7: Heatmap erstellen mit zu definierender Rastergrösse
    (Convex hull wurde verworfen) \>\> Vorschlag: 100m da Arealstatistik
    dieses Mass aufweist
-   Schritt 8: Rasterfelder zuweisen, Context herstellen
-   Schritt 9: Prozess mit Gesamtdaten testen
-   Schritt 11: Profit

# Definitionen

**Nester:**

-   day = Tag

-   resting

-   bodennutzung == wald \| feld

-   wenn bodennutzung == feld:  
    vegetationshöhe \>= 70cm & \# Höhe in den Luftbildern statisch,
    daher für Felder besser nicht berücksichtigen  
    month (mai - oktober) &  
    feldaufnahmen == raps \| mais \| getreide

-   wenn vegetationstyp == feld , dann anbau == raps \| mais \| getreide

**Suhlen:**

-   day = Tag

-   bodennutzun == wald \| bodennutzung == unproduktive vegetation

-   monate != November bis März

-   aufenhaltsdauer?

# Auswertung der zugewiesenen Punkte

## Vor Trennung der 45 min Segmente

site_type

-   both : 2853, 14.24%
-   NA : 704, 3.51%
-   nest : 1058, 5.28%
-   none :14132, 70.56%
-   wallow: 1281, 6.39%
-   total: 20028

## Mit segment_dur \<= 3

site_type

-   both : 399, 0.33%
-   NA : 4238, 3.52%  
-   nest :22897, 19.06%
-   none :92432, 76.96%
-   wallow: 128, 0.10%
-   total: 120094

## Mit Berücksichtigung der Wintermonate

### Nur mit Wälder als möglicher Einstand:

site_type

-   both : 399, 0.33%
-   NA :11198, 9.32%
-   nest :31470, 26.20%
-   none :76899, 64.03%
-   wallow: 128, 0.10%

### Mit Wälder und Feuchtgebiete als möglicher Einstand: site_type

-   both : 399, 0.33%  
-   NA :11198, 9.32%  
-   nest :52709, 43.88%
-   none :55660, 46.34%  
-   wallow: 128, 0.10%

## Mit Berücksichtigung vom Monat März, Morgendaemmerung und segment_dur \>= 3 für Nester

site_type

-   NA :11071, 9.21%
-   nest :52238, 45.99%
-   none :56034, 46.66%  
-   wallow: 751, 0.62%

# Rasterkarten

-   jeweils ein Layer erstellt für Nester und Suhlen (gefiltertet nach
    Nest == TRUE resp. Wallow == TRUE

-   diese überlagern sich wie zu erwarten war (wegen der "both"-Werte)

-   Layer lassen sich bei Bedarf ausblenden, Karte interaktiv

-   Auflösung: 100m, analog zu Arealstatistik (die wir allerdings nicht
    verwendet haben), feinere Auflösung wäre also möglich (z. B. 50
    Meter)

-   Einfügen des Layers Feldaufnahmen versucht aber nicht sinnvoll, da
    zu unübersichtlich

# Fragen an Nils

-   Datenbezug Arealstatistik/Bodennutzung: Wie bekommt man die
    entsprechenden Daten so, dass sie in R importiert werden können?
    Portale lassen Download eines csv-Files der Gesamtschweiz zu.
    Scheint aber mega umständlich. Was ist sein Vorschlag für den Bezug?
    Oder hätte er die Daten schon?

-   Antwort von Nils: mycsv \<- read_csv("arealstat.csv") mycsv %\>%
    select(x, y, meinespalte) %\>% raster::rasterFromXYZ()

    [Bodennutzung /
    Arealstatistik](https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/boden-nutzung-bedeckung-eignung/arealstatistik-schweiz/bodennutzung.html)

    [Vereinfachte
    Bodennutzung](https://map.geo.admin.ch/?topic=ech&lang=de&bgLayer=ch.swisstopo.pixelkarte-grau&layers=ch.bfs.gebaeude_wohnungs_register,ch.bfs.arealstatistik-hintergrund&layers_visibility=false,true&catalogNodes=457,532,477,599&layers_opacity=1,0.65&zoom=2&E=2704737.50&N=1230875.00)

-   Evtl. geplantes Vorgehen kurz durchsprechen für Feedback oder
    Ergänzungen seinerseits

# Tipps von Nils

-   Passt auf, die Daten haben eine Vignette (nicht alle sind im
    viertelstundentakt)
-   Nach dem alle Daten enriched wurden müssen diese auf einzelne Suhlen
    und Nester aufgetrennt werden. dafür rle_id verwenden (cma-week3).
-   Wichtige Funktion: rle_id \<- function(vec){ x \<- rle(vec)$lengths
    as.factor(rep(seq_along(x), times=x)) } Task 4: Segment-based
    analysis caro \<- caro %\>% mutate(segment_id = rle_id(static))

# Offene Fragen

-   Müssen wir vor dem Erstellen der Raster nach einzigartigen seq_ids
    filtern? Oder wie war das von Nils gemeint? To be discussed.
    Umsetzung wäre easy.

-   Weitere Visualisierungen bezüglich Landnutzung oder besuchten
    Gebieten von Einzeltieren? So im Proposal enthalten, bezüglich der
    Beantwortung der Forschungsfragen jedoch wenig hilfreich.
    Recurse-Package ebenfalls nicht verwendet: Vorschlag: Diesen Ansatz
    diskutieren (Nicht angewendet, da Literatur keine Informationen zur
    Beurteilung von häufig besuchten Orten hergibt).

-   Vorgehen Arbeit abfassen besprechen. Vorschlag Aufbau besprechen
    (Mischung Template Nils und eigener Vorlage)

-   Quellen Zotero
