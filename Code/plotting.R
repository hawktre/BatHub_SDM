# Try Pivot Longer Also ----------------------------------------------------

occ_long <- spp_occ %>% 
  pivot_longer(cols = possible_bats, names_to = "spp", values_to = "presence") %>% 
  dplyr::filter(presence == 1) %>% 
  dplyr::arrange(spp) %>% 
  select(-presence)

# Plot AGain --------------------------------------------------------------

occ_long_spat <- st_as_sf(occ_long, coords = c("Longitude", "Latitude"), crs = "WGS84")

ggplot()+
  geom_sf(data = pnw, alpha = 0.5)+
  geom_sf(data = occ_long_spat, aes(col = spp))+
  facet_wrap(~spp)+
  labs(title = "NABat Species Occurences (2016-2022)",
       color = "Species")+
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

## Myotis spp only
ggplot()+
  geom_sf(data = pnw, alpha = 0.5)+
  geom_sf(data = occ_long_spat %>% filter(str_detect(spp, "my")), aes(col = spp))+
  facet_wrap(~spp, nrow = 2)+
  labs(title = "Myotis Species Occurences (2016-2022)",
       color = "Species")+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())+
  guides(colour = guide_legend(nrow = 1))

## Lasiurus
ggplot()+
  geom_sf(data = pnw, alpha = 0.5)+
  geom_sf(data = occ_long_spat %>% filter(str_detect(spp, "la")), aes(col = spp))+
  facet_wrap(~spp, nrow = 1)+
  labs(title = "Lasiurus Species Occurences (2016-2022)",
       color = "Species")+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

