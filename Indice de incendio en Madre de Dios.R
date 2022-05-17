#  Cargamos las Librerias ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, ggplot2, ggspatial, colorspace, ggpubr, sf, elevatr, tmap, ggnewscale)

Ge            <- raster("RASTER/fwi_0.tif")
Peru              <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Per              <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
MDD_dis           <- subset(Per, NAME_1  == "Madre de Dios")
MDD           <- subset(Peru, NAME_1  == "Madre de Dios")
Geo<- projectRaster(Ge, crs = crs(Peru))
plot(Geo)

Geo_alt    <- crop(Geo, MDD)                           #   
Geo_alt   <- Geo_alt <- mask(Geo_alt, MDD)

Geo_data= rasterToPolygons(Geo_alt)

raster::shapefile(Geo_data, "SHP/IncendioMDD.shp")
Ge_data_p  = st_read("SHP/IncendioMDD.shp")
Ge_data_po <- st_transform(Ge_data_p  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))


colores<- c("white", "#55a630")
summary(Ge_data_po$fwi_0)


Mapa =ggplot()+
  geom_sf(data = Ge_data_po, aes(fill=fwi_0), size=0.2, color="gray")+
  scale_fill_gradientn(colours = colores,
                       breaks = c(0.1,6.3),
                       na.value = 'white',
                       labels = c("[Bajo] ","[Moderado]"),
                       name='')+
  geom_sf(data=Peru, fill=NA, color="black")+
  geom_sf(data=MDD_dis, fill=NA, color="gray80")+
  geom_sf(data = MDD, fill=NA, color="black")+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf_text(data = MDD_dis , aes(label = NAME_3 ), 
               family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"), 
               size = 3, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  geom_sf_text(data = Peru , aes(label = NAME_1 ), 
               family="serif", color = "black",   box.padding = unit(0.1, "lines"), 
               size = 4, face = "bold",color = 'black',
               point.padding = unit(0.9, "lines"))+
  coord_sf(xlim = c(-72.40404 ,-68.6531), ylim = c(-13.36179   ,-9.879849))+
  theme_bw()+
  theme(axis.text.x  = element_text(face="bold", color="black", size=8),
        legend.key.size = unit(2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        legend.text=element_text(size=8, angle = 90),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")+
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="black"))+
  scale_x_continuous(expand = c(0, 0), name = "Longitude (°E)")+
  scale_y_continuous(expand = c(0, 0), name = "Latitude (°N)")+
  annotate(geom = "text", x = -70, y = -10.2, hjust = 0, vjust = 1, face="bold",
           label = "Índice Meteorológico de Incendios \n                     Madre de Dios",
           size = 5, family="serif", color = "black")+
  annotate(geom = "text", x = -72.2, y = -10.6, hjust = 0, vjust = 1, face="bold",
           label = "Ing. Gorky Florez Castillo",
           size = 3, family="serif", color = "black")



ggsave(plot=Mapa,"Mapa/Incendio Mdd.tiff",units = "cm",width = 21,height = 21, dpi=2000,
       compression = c("zip"))
  

