rm(list = ls())
setwd("~/PC/Magistrale/Secondo Anno/Secondo semestre/Aziendali/Dati progetto/")

library(dplyr)
library(lubridate)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(ggplot2)
library(tidyverse)

id_pv <- read.csv("pv_coordinate_geografiche_impianti.csv")
meteo_pv <- bind_rows(read.csv("PV_4045542_dati_meteo.csv"),
                      read.csv("PV_4046302_dati_meteo.csv"),
                      read.csv("PV_4045196_dati_meteo.csv"),
                      read.csv("PV_4045194_dati_meteo.csv"),
                      read.csv("PV_3402858_dati_meteo.csv"))
produzione_pv <- read.csv("produzione_impianti_pv.csv")

province_pv <- c("Rovigo","Brindisi","Viterbo","Napoli","Rovigo")
regioni_pv <- c("Veneto","Puglia","Lazio","Campania","Veneto")

id_pv$provincia <- province_pv
id_pv$regione <- regioni_pv

# Fasi Lunari --------------

lunar <- read.table("Dataset Esterni/Fasi_Lunari.txt", header = T, sep = ",")

data_luna <- paste0(lunar$year,"-",lunar$month,"-",lunar$day)
lunar$date <- as.Date(data_luna)

lunar$year <- NULL
lunar$month <- NULL
lunar$day <- NULL

meteo_pv$date <- as.Date(meteo_pv$time_prev)
meteo_pv <- inner_join(meteo_pv, lunar, by = "date")

# unisco tutto pv -----

meteo_pv$time_calc <- NULL

dati_pv <- inner_join(meteo_pv, id_pv, by = c("lat_1" = "lat_meteo",
                                              "lon_1" = "lon_meteo"))

dati_pv <- inner_join(produzione_pv, dati_pv,
                      by = c("timestamp_utc" = "time_prev" , 
                             "obj_psv" = "obj_puntoscambio"))


dati_pv <- dati_pv  %>% 
  group_by(timestamp_utc, obj_psv,regione,provincia) %>% 
  summarise_all(mean, na.rm = T)

dati_pv$lat <- NULL
dati_pv$lon <- NULL

dati_pv_timestamp_utc <- as.POSIXlt(dati_pv$timestamp_utc)

dati_pv$mese <- month(dati_pv_timestamp_utc)
dati_pv$anno <- year(dati_pv_timestamp_utc)
dati_pv$ora <- hour(dati_pv_timestamp_utc)

dati_pv <- dati_pv[!is.na(dati_pv$consuntivo),]

dati_pv$consuntivoscaled <- dati_pv$consuntivo/dati_pv$pmax_mw

dati_pv <- dati_pv[with(dati_pv, order(obj_psv, timestamp_utc)),]

dati_pv$surface_solar_radiation[is.na(dati_pv$surface_solar_radiation)] <- 0
dati_pv$total_sky_solar_radiation[is.na(dati_pv$total_sky_solar_radiation)] <- 0
dati_pv$precipitazioni_nevose[is.na(dati_pv$precipitazioni_nevose)] <- 0
dati_pv$precipitazioni[is.na(dati_pv$precipitazioni)] <- 0
# dati_pv$raffiche_vento[is.na(dati_pv$raffiche_vento)] <- 0

new_sur <- new_tot <- new_prec <- new_snow <- rep(NA, nrow(dati_pv))

h <- 1
for (i in 2:nrow(dati_pv)){
  
  if (dati_pv$obj_psv[i]==dati_pv$obj_psv[i-1]){
    
    if (hour(dati_pv$timestamp_utc[i])!=12){
      
      new_sur[i] <- dati_pv$surface_solar_radiation[i]-dati_pv$surface_solar_radiation[i-1]
      new_tot[i] <- dati_pv$total_sky_solar_radiation[i]-dati_pv$total_sky_solar_radiation[i-1]
      new_prec[i] <- dati_pv$precipitazioni[i]-dati_pv$precipitazioni[i-1]
      new_snow[i] <- dati_pv$precipitazioni_nevose[i]-dati_pv$precipitazioni_nevose[i-1]
      
      # new_raff[i] <- dati_pv$raffiche_vento[i]-dati_pv$raffiche_vento[i-1]
      
      
    }
    else {
      new_sur[i+1] <- dati_pv$surface_solar_radiation[i+1]-dati_pv$surface_solar_radiation[i]
      new_sur[i] <- mean(c(new_sur[i+1],new_sur[i-1]), na.rm = T)
      
      new_tot[i+1] <- dati_pv$total_sky_solar_radiation[i+1]-dati_pv$total_sky_solar_radiation[i]
      new_tot[i] <- mean(c(new_tot[i+1],new_tot[i-1]), na.rm = T)
      
      new_prec[i+1] <- dati_pv$precipitazioni[i+1]-dati_pv$precipitazioni[i]
      new_prec[i] <- mean(c(new_prec[i+1],new_prec[i-1]), na.rm = T)
      
      new_snow[i+1] <- dati_pv$precipitazioni_nevose[i+1]-dati_pv$precipitazioni_nevose[i]
      new_snow[i] <- mean(c(new_snow[i+1],new_snow[i-1]), na.rm = T)
      
      # new_raff[i+1] <- dati_pv$surface_solar_radiation[i+1]-dati_pv$surface_solar_radiation[i]
      # new_raff[i] <- (new_raff[i+1]+new_raff[i-1])/2
      
    }
  }
  
  if(h %% 500 == 0) cat("percentuale:", h/nrow(dati_pv) * 100, "%", "\n")
  h <- h + 1
}

dati_pv <- bind_cols(dati_pv,
                      prec = new_prec,
                      snow = new_snow,
                      tot = new_tot,
                      sur = new_sur)
                      # raff = new_raff)

dati_pv$surface_solar_radiation <- NULL
dati_pv$total_sky_solar_radiation <- NULL
dati_pv$precipitazioni_nevose <- NULL
dati_pv$precipitazioni <- NULL
dati_pv$raffiche_vento <- NULL

dati_pv <- na.omit(dati_pv)

# save(dati_pv, file = "Fotovoltaico_decumulato.RData")













