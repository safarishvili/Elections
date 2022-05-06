# Ce script permet de réaliser une simple carte des départements en R
# Les contours des départements issus d'OSM
# URL : https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/

library("rgdal")
library("rgeos")
library("maptools")
library("ggplot2")
library("dplyr")
library("stringr")
library("mapproj")
library("ggthemes")

dept <- readOGR("data/departements/", layer= "departements-20140306-5m")
dept <- fortify(dept, region = "code_insee")

dept %>% 
  filter(str_length(id) == 2) %>% 
  ggplot() + 
    geom_polygon(aes(x = long, y = lat, group = group), color = "grey", fill = "white") + 
    coord_map(projection = "mercator") + 
    theme_tufte(ticks = FALSE) + 
    theme(axis.text = element_blank(), 
          axis.title = element_blank()) + 
    labs(title = "Carte des départements de France métropolitaine en 2014")


#Importation de la BDD "ESADA_W"
ESADA_W<-read.csv(C:/Users/Irina/documents for R/ESADA_W.csv")

#les calculs nécessaires pour construire heatmap ESADA_W

h.m<-ESADA_W %>% 
  group_by(region,classe) %>% 
  summarise(N=n()) %>% 
  mutate(Sum=sum(N), prop=N/Sum)

#visualisation heatmap

h.m %>% 
  ggplot(aes(classe,region))+geom_tile(aes(fill=prop))+
  geom_text(aes(label=scales::percent(prop, accuracy = 1)))+
  theme_minimal()+
  scale_fill_gradient(low="#E6E6FA", high="#CD5C5C" )
                  
#les modifications et calculs nécessaires pour creer bubble chart

b.ch<-ESADA_W %>% 
  group_by(region,classe) %>% 
  summarise(N=n()) %>% 
  mutate(Sum=sum(N), prop=N/Sum) 
                  
#le brouillon de la visualisation de bubble chart
b.ch %>% 
  ggplot(aes(region,prop, size=prop, color=classe))+
  geom_point()+theme_minimal()+
  geom_text(aes(label=scales::percent(prop, accuracy = 1)))
