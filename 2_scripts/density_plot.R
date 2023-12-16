library(tidyverse)
library(usethis)
library(dataDownloader)

# get_file(node = "hk2cy",
#          file = "PFTC7_SA_cleanish_traits_2023.csv",
#          path = "C:/Users/carme/Desktop", 
#          remote_path = "trait_data") 


trait_data_raw <- read.csv("data/PFTC7_SA_cleanish_traits_2023.csv")


#cleaning out some weirdos so that the plots look niiiiice
trait_data <- trait_data_raw %>% 
  mutate(veg_height_cm = if_else(veg_height_cm>100, NA_real_, veg_height_cm),
         sla_cm2_g = if_else(sla_cm2_g>1000, NA_real_, sla_cm2_g),
         ldmc = if_else(ldmc>=1, NA_real_, ldmc),
         wet_mass_g = if_else(wet_mass_g > 2, NA_real_, wet_mass_g),
         elevation_m_asl = as.factor(elevation_m_asl))%>%
  filter(!is.na(aspect),!is.na(elevation_m_asl))


#HEIGHT 
trait_data %>% ggplot(aes(x= veg_height_cm, color= elevation_m_asl, fill= elevation_m_asl)) +
  geom_density(alpha = 0.5)+
  scale_fill_discrete(name= "site")+
  scale_color_discrete(name= "site")+
  labs(x="Vegetative height (cm)")+
  facet_wrap(~aspect)


#LEAF THICKNESS
trait_data %>% ggplot(aes(x= leaf_thickness_mm, color= elevation_m_asl, fill= elevation_m_asl)) +
  geom_density(alpha = 0.5)+
  scale_fill_discrete(name= "site")+
  scale_color_discrete(name= "site")+
  labs(x="Leaf thickness (mm)")+
  facet_wrap(~aspect)

#WET MASS
trait_data %>% ggplot(aes(x= wet_mass_g, color= elevation_m_asl, fill= elevation_m_asl)) +
  geom_density(alpha = 0.5)+
  scale_fill_discrete(name= "site")+
  scale_color_discrete(name= "site")+
  labs(x="Wet Mass (g)")+
  facet_wrap(~aspect)


#DRY MASS 
trait_data %>% ggplot(aes(x= dry_mass_g, color= elevation_m_asl, fill= elevation_m_asl)) +
  geom_density(alpha = 0.5)+
  scale_fill_discrete(name= "site")+
  scale_color_discrete(name= "site")+
  labs(x="Dry Mass (g)")+
  facet_wrap(~aspect)


#SLA 
trait_data %>% ggplot(aes(x= sla_cm2_g, color= elevation_m_asl, fill= elevation_m_asl)) +
  geom_density(alpha = 0.5)+
  scale_fill_discrete(name= "site")+
  scale_color_discrete(name= "site")+
  labs(x="SLA (cm2)")+
  facet_wrap(~aspect)

#LEAF DRY MATTER CONTENT
trait_data %>% ggplot(aes(x= ldmc, color= elevation_m_asl, fill= elevation_m_asl)) +
  geom_density(alpha = 0.5)+
  scale_fill_discrete(name= "site")+
  scale_color_discrete(name= "site")+
  labs(x="Leaf dry matter content")+
  facet_wrap(~aspect)



#################################33

trait_data %>% ggplot(aes(x= veg_height_cm, color= aspect, fill= aspect)) +
  geom_density(alpha = 0.5)+
  scale_fill_discrete(name= "site")+
  scale_color_discrete(name= "site")+
  labs(x="Vegetative height (cm)")+
  facet_wrap(~elevation_m_asl)


#######

trait_data %>% ggplot(aes(x= ldmc, y= elevation_m_asl, color= elevation_m_asl, fill= elevation_m_asl)) +
  geom_density_ridges(alpha=0.5, scale= 3)+
  scale_fill_discrete(name= "site")+
  scale_color_discrete(name= "site")+
  labs(x="Leaf dry matter content")+
  facet_wrap(~aspect)+
  theme_bw()



               