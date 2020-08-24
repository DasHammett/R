library(ggplot2)
library(dplyr)
library(sf)
library(ggrepel)
library(lubridate)

mapa <- st_read("LIMADM_MUNICIPI.shp", options = "ENCODING=ISO-8859-15")
raw <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/jj6z-iyrp/rows.csv?accessType=DOWNLOAD&sorting=true",
                header = TRUE,
                sep=",",
                fileEncoding = "UTF-8")
raw$TipusCasData <- dmy(raw$TipusCasData)
raw$ComarcaDescripcio <- trimws(raw$ComarcaDescripcio,whitespace=" ")
raw$MunicipiCodi <- stringr::str_pad(raw$MunicipiCodi, 5, side = "left", pad = 0)


#####
raw %>%
  filter(between(TipusCasData,
                 max(TipusCasData) - 7,
                 max(TipusCasData)
                 ),
         TipusCasDescripcio != "Sospitós") %>%
  group_by(MunicipiCodi,MunicipiDescripcio) %>%
  summarise(N=sum(NumCasos)) %>%
  left_join(pob[,c("CodiINE","Cens")], by = c("MunicipiCodi" = "CodiINE")) %>%
  ungroup() %>%
  mutate(Increment = N/Cens) %>%
  arrange(-Cens)



pob <- read.csv("https://analisi.transparenciacatalunya.cat/api/views/b4rr-d25b/rows.csv?accessType=DOWNLOAD&sorting=true",
                header = TRUE,
                sep = ",",
                fileEncoding = "UTF-8")

pob <- pob %>% mutate(Cens = rowSums(.[3:ncol(.)],na.rm = TRUE))
pob$CodiINE <- gsub(".{1}$", "", pob$Codi)
pob$CodiINE <- stringr::str_pad(pob$CodiINE, 5, side = "left", pad = 0)

resum <- raw %>%
  filter(TipusCasDescripcio != "Sospitós") %>%
  group_by(MunicipiCodi) %>%
  summarise(N = sum(NumCasos)) %>%
  left_join(.,pob[,c("CodiINE","Cens")], by = c("MunicipiCodi" = "CodiINE")) %>%
  mutate(N2 = (N/Cens) * 1e5) %>%
  ungroup()

mapa_N <- left_join(mapa, resum, by = c("CODIINE" = "MunicipiCodi"))
mapa_N <- mapa_N %>% mutate(lat = st_coordinates(st_centroid(mapa_N$geometry))[,1],
                            lon = st_coordinates(st_centroid(mapa_N$geometry))[,2])

ggplot(mapa_N) +
  geom_sf(aes(fill = N2)) +
 # geom_point(aes(x=lat, y = lon, shape = 21, fill = N, size = N)) +
  scale_fill_gradient(low = "limegreen", high = "red",na.value = NA) +
  #geom_sf_label_repel(data = top_n(mapa_N, 10, N2), aes(label=paste(MunicipiDescripcio,N,sep="\n"),)) +
  geom_label_repel(data = top_n(mapa_N, 10, N2),
                   aes(label=paste(NOM_MUNI,N,sep="\n"),
                       geometry = geometry),
                   stat = "sf_coordinates",
                   min.segment.length = 0) +
  theme_void()

pob %>%
  group_by(CodiINE, Literal) %>%
  summarise(Joves = sum(Total..De.0.a.14.anys),
            Total = sum(c_across(contains("anys"))),
            JovesPC = Joves/Total) %>%
  right_join(mapa, by = c("CodiINE" = "CODIINE")) %>%
  ggplot(.) +
  geom_sf(aes(fill=JovesPC, geometry = geometry)) +
  scale_fill_gradient2(low = "white") +
  theme_void()


rolling <- raw %>%
  group_by(MunicipiCodi, MunicipiDescripcio, TipusCasData) %>%
  summarise(N = sum(NumCasos)) %>%
  mutate(Rolling7 = roll_sum(N, n = 7, align = "right", fill = NA)) %>% View()


raw %>%
  filter(TipusCasDescripcio != "Sospitós") %>%
  group_by(MunicipiCodi, MunicipiDescripcio, Week = floor_date(TipusCasData,unit = "week")+1) %>%
  summarise(N = sum(NumCasos)) %>%
  mutate(Diff = N - lag(N)) %>%
  filter(Week == max(Week)) %>%
  right_join(mapa, by = c("MunicipiCodi" = "CODIINE")) %>%
  ggplot() +
  geom_sf(aes(fill = Diff, geometry = geometry)) +
  scale_fill_gradient2(low = "green", high = "red", mid = "grey80", na.value = "white") +
  geom_label_repel(data =
      raw %>%
        filter(TipusCasDescripcio != "Sospitós") %>%
        group_by(MunicipiCodi, MunicipiDescripcio, Week = floor_date(TipusCasData,unit = "week")+1) %>%
        summarise(N = sum(NumCasos)) %>%
        mutate(Diff = N - lag(N)) %>%
        ungroup() %>%
        arrange(desc(Diff)) %>%
        filter(Week == max(Week), Diff != "NA") %>%
        filter(row_number() > max(row_number()) -5 | row_number() <= 10) %>%
        left_join(mapa, by = c("MunicipiCodi" = "CODIINE")),
      aes(label = paste(MunicipiDescripcio,Diff,sep="\n"),
          geometry = geometry),
          stat = "sf_coordinates",
          min.segment.length = 0
  ) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = paste("Diferencia casos confirmats de COVID-19 de la setmana ",
                     floor_date(max(raw$TipusCasData),unit = "week")+1,
                    "\nrespecte la setmana anterior",
                    sep=" "))

