# auteur: Leonard Vanbrabant
# organisatie: GGD West-Brabant
# datum bestand aangemaakt: 12-02-2021
# datum laatst gewijzigd: 15-02-2021


# laad pakketten
library(lubridate) # maakt werken met datums makkelijker 
library(sf)        # functies voor geo-objecten
library(ggplot2)   # plot functies



# laad shape file om het totaal aantal inwoners per GGD regio of per VR te bepalen.
# Als je het aantal inwoners weet, heb je dit niet nodig. 
path_shape <- "C:/Users/l.vanbrabant/stack/ShapeFiles"

# selecteer juist VR: hier MWB
VRMWB <- "VR20"



# laad meest actuele ziekenhuisopnames data vanaf rimv site ---------------
zknhuisopnames <- read.csv2("https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames.csv")
# selecteer veiligheidsregio
zknhuisopnames_vrmwb <- zknhuisopnames[zknhuisopnames$Security_region_code == VRMWB, ]
# selecteer benodigde variabelen
zknhuisopnames_vrmwb <- zknhuisopnames_vrmwb[, c("Date_of_statistics",
                                                 "Hospital_admission",
                                                 "Municipality_name")]
# herbenoem kolommen
names(zknhuisopnames_vrmwb) <- c("datum", "x", "GM_NAAM")
# maak er een object van met class Date
zknhuisopnames_vrmwb$datum <- ymd(zknhuisopnames_vrmwb$datum)
# selecteer enkel de rijen vanaf 1 juli 2020
zknhuisopnames_vrmwb <- zknhuisopnames_vrmwb[zknhuisopnames_vrmwb$datum >= "2020-07-01", ]
# check range
range(zknhuisopnames_vrmwb$datum)




# laad HPzone data VRMWB --------------------------------------------------
## Dit stuk moet je vervangen door je eigen HPzone data met de volgende
## variabelen: Datum.melding.aan.de.GGD en Gemeentecode. 

## Gemeentecode heb ik obv PC6 gekoppeld. Hiervoor heb ik gebruik gemaakt van
## pc6hnr20190801_gwb.csv afkomstig van het CBS. Je de file hier vinden:
## https://www.cbs.nl/nl-nl/maatwerk/2019/42/buurt-wijk-en-gemeente-2019-voor-postcode-huisnummer
## Er is sinds kort ook een bestand van 2020. 

source("./functies/get_hpzone_data.R")

OS <- "Windows"
date.start <- "2020-07-01"
date.end   <- "2021-02-12"
weekNr     <- 5
datum      <- "15 februari" 

## laad paths, dit hangt van het OS af. 
paths <- load_paths(OS = OS, datum = datum, week = weekNr)
path_data_wb  <- paths[[1]] 
path_data_hvb <- paths[[2]] 
path_shape    <- paths[[3]]
path_cluster  <- paths[[4]]

## laad HPzone data
bemonstering <- get_hpzone_data(date.start = date.start, date.end = date.end, 
                                OS = OS)

# voeg een nieuwe kolom toe om later makkelijker te sommeren.
bemonstering$freq <- 1

## some checks
table(bemonstering$GGD)
range(bemonstering$datum[bemonstering$GGD == "HvB"])
range(bemonstering$datum[bemonstering$GGD == "WB"])
sort(unique(df_subemonsteringb_codes$week))
# -------------------------------------------------------------------------


# alle datums die bij dezelfde week horen krijgen de datum van de maandag van
# die betreffende week. 
bemonstering$week <- as.Date(cut(bemonstering$datum, breaks = "week", 
                             start.on.monday = TRUE))

zknhuisopnames_vrmwb$week <- as.Date(cut(zknhuisopnames_vrmwb$datum, breaks = "week", 
                                         start.on.monday = TRUE))


# haal het aantal inwoners MWB uit de geopackgage.
# Je kan hier ook handmatig het aantal inwoners opgeven. 
GM_VRMWB <- read_sf(file.path(path_shape, "2019/MWB/gem2019MWB_cleaned.gpkg"))
GM_VRMWB$geom <- NULL
inwoners_VRMWB <- sum(GM_VRMWB$AANT_INW[GM_VRMWB$AANT_INW != -99999999]) 
#inwoners_GGDWB <- 706412

# verwijder van de gemeentecode de letter GM en maak er een numerieke variabele van.
GM_VRMWB$GM_CODE <- to_number(GM_VRMWB$GM_CODE)

# voeg gemeentenaam toe aan bemonstering data obv gemeentecode
bemonstering <- merge(bemonstering, GM_VRMWB[, c("GM_CODE", "GM_NAAM")], 
                      by.x = "Gemeente2019", 
                      by.y = "GM_CODE", all.x = TRUE)

## gemeentenamen VRMWB
GM_VRMWB <- c("Alphen-Chaam", "Altena", "Baarle-Nassau", "Bergen op Zoom",
              "Breda", "Drimmelen", "Etten-Leur", "Geertruidenberg",
              "Halderberge", "Moerdijk", "Oosterhout", "Roosendaal",
              "Rucphen", "Steenbergen", "Woensdrecht", "Zundert",
              "Dongen", "Gilze en Rijen", "Goirle", "Loon op Zand",
              "Oisterwijk", "Tilburg", "Waalwijk", "Hilvarenbeek")

# selecteer enkel de rijen uit de VRMWB
vrmwb.idx <- bemonstering$GM_NAAM %in% GM_VRMWB
bemonstering <- bemonstering[vrmwb.idx, ]

# some check
length(GM_VRMWB[!GM_VRMWB %in% bemonstering$GM_NAAM]) > 0



# hier maken we het figuur ------------------------------------------------

# styling: fontsize, legende positie, and many more...
axis.theme <- theme(
  axis.text.x  = element_text(size = 11),
  axis.title.x = element_text(size = 12, margin = margin(10,0,0,0)),
  axis.text.y  = element_text(size = 11),
  axis.title.y = element_text(size = 12, margin = margin(0,10,0,0)),
  axis.title.y.right = element_text(size = 12, margin = margin(0,0,0,10)),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.background = element_blank()
)



# some checks
# tmpy1 <- aggregate(bemonstering$freq, list(bemonstering$week),
#                    function(x) sum(x / inwoners_VRMWB * 100000))
# 
# tmpy2 <- aggregate(zknhuisopnames_vrmwb$x, list(zknhuisopnames_vrmwb$week), 
#                    function(x) sum(x / inwoners_GGDWB * 1000000))


# scale rechter y-as adhv de rivm classificaties. Op deze manier komen de 
# risico niveaus van beide y-assen overeen. 
scaleRight <- c(1/(mean(35/4, 100/16, 250/27))) #max(tmpy2$x) / max(tmpy1$x)

ggplot() +
  stat_summary(data = zknhuisopnames_vrmwb,
               aes(week, x/scaleRight, fill = "Aantal ziekenhuisopnmaes VRMWB"),
               fun = function(x) sum(x / inwoners_VRMWB * 1000000), # let op per 1 miljoen
               geom = "bar",
               size = 0.5
  ) +
  stat_summary(data = bemonstering,
               aes(week, freq, color = "Aantal positieve cases VRMWB"),
               fun = function(x) sum(x / inwoners_VRMWB * 100000), # let op per 100.000 
               geom = "line",
               size = 1.1
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W") +
  scale_y_continuous("Aantal positieve cases per 100.000 inwoners", 
                     sec.axis = sec_axis(trans = ~.*scaleRight, 
                                         name  = "Aantal ziekenhuisopnames per 1.000.000 inwoners")) +
  labs(x = "Week nummer") +
  geom_hline(yintercept = 35, linetype = "solid", color = "red", size = 0.65) +
  annotate("label", x = as.Date("2020-06-05"), y = 35, label = "Risiconiveau Zorgelijk", size = 3) + 
  geom_hline(yintercept = 100, linetype = "solid", color = "red", size = 0.65) +
  annotate("label", x = as.Date("2020-07-10"), y = 100, label = "Risiconiveau ernstig", size = 3) +
  geom_hline(yintercept = 250, linetype = "solid", color = "red", size = 0.65) +
  annotate("label", x = as.Date("2020-08-20"), y = 250, label = "Risiconiveau zeer ernstig", size = 3) +
  
  scale_colour_manual("", values = c("Aantal ziekenhuisopnmaes VRMWB" = "darkgreen", 
                                     "Aantal positieve cases VRMWB" = "orange")) +
  scale_fill_manual("", values = "darkgreen") +
  
  axis.theme# +
  #coord_cartesian(ylim = c(0, 750))


  
  
# aantal positieve cases per 100.000 inwoners -----------------------------
ggplot() +
  stat_summary(data = bemonstering,
               aes(week, freq, colour = "Aantal positieve cases VRMWB"), 
               fill = "orange",
               fun = function(x) sum(x / inwoners_VRMWB * 100000), # let op per 100.000 
               geom = "bar",
               size = 0.5
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W") +
  scale_y_continuous("Aantal positieve cases per 100.000 inwoners VRMWB") +
  labs(x = "Week nummer") +
  geom_hline(yintercept = 35, linetype = "solid", color = "black", size = 0.5) +
  annotate("label", x = as.Date("2020-07-10"), y = 35, label = "Risiconiveau Zorgelijk") + 
  geom_hline(yintercept = 100, linetype = "solid", color = "black", size = 0.5) +
  annotate("label", x = as.Date("2020-07-10"), y = 100, label = "Risiconiveau ernstig") +
  geom_hline(yintercept = 250, linetype = "solid", color = "black", size = 0.5) +
  annotate("label", x = as.Date("2020-07-10"), y = 250, label = "Risiconiveau zeer ernstig") +
  scale_color_manual(labels = c("Aantal positieve cases VRMWB"), 
                     values = c("orange")) +
  axis.theme +
  coord_cartesian(ylim = c(0, 600))
  



# aantal ziekenhuisopnames per 1.000.000 inwoners -------------------------
ggplot() +
  stat_summary(data = zknhuisopnames_vrmwb,
               aes(week, x, colour = "Aantal ziekenhuisopnmaes VRMWB"), 
               fill = "darkgreen",
               fun = function(x) sum(x / inwoners_VRMWB * 1000000), # let op per 1 miljoen
               geom = "bar",
               size = 0.5
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%W") +
  scale_y_continuous("Aantal ziekenhuisopnames per 1.000.000 inwoners VRMWB") +
  labs(x = "Week nummer") +
  geom_hline(yintercept = 4, linetype = "solid", color = "red", size = 0.5) +
  annotate("label", x = as.Date("2020-07-10"), y = 4, label = "Risiconiveau Zorgelijk") + 
  geom_hline(yintercept = 16, linetype = "solid", color = "red", size = 0.5) +
  annotate("label", x = as.Date("2020-07-10"), y = 16, label = "Risiconiveau ernstig") +
  geom_hline(yintercept = 27, linetype = "solid", color = "red", size = 0.5) +
  annotate("label", x = as.Date("2020-07-10"), y = 27, label = "Risiconiveau zeer ernstig") +
  scale_color_manual(labels = c("Aantal ziekenhuisopnmaes VRMWB"), 
                     values = c("darkgreen")) +
  axis.theme +
  coord_cartesian(ylim = c(0, 200))


