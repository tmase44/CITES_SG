library(tidyverse)
library(ggpubr)
library(gridExtra)

# this file will take the net trade of Singapore
  # per species, imports reported by SG
  # less
  # per species, exports reported by SG

#import data----
citesnet<-read_csv("CITES.csv")
view(citesnet)

#PREPARATION----
citesnet2<-citesnet %>% 
  select(Year,App.,Taxon,Importer,Exporter,
         `Importer reported quantity`,
         `Exporter reported quantity`,Purpose,Source_def)
unique(citesnet2$Taxon)

#common names added
citesnet2<-citesnet2 %>% 
  mutate(Common_Name=case_when(
    Taxon=="Cacatua goffiniana"~"Tanimbar corella",
    Taxon=="Psittacula krameri"~"Rose-ringed parakeet",
    Taxon=="Myiopsitta monachus"~"Monk parakeet",
    Taxon=="Psittacula alexandri"~"Red-breasted parakeet",
    Taxon=="Trichoglossus haematodus"~"Coconut lorikeet",
    Taxon=="Cacatua sulphurea"~"Yellow-crested cockatoo"))
citesnet2<-citesnet2 %>% 
  relocate(Common_Name,.after = Taxon) # order correctly

#factorize
citesnet2$Taxon<-as.factor(citesnet2$Taxon)
citesnet2$Importer<-as.factor(citesnet2$Importer)
citesnet2$Exporter<-as.factor(citesnet2$Exporter)
citesnet2$Origin<-as.factor(citesnet2$Origin)
citesnet2$Source_def<-as.factor(citesnet2$Source_def)
citesnet2$Purpose<-as.factor(citesnet2$Purpose)
citesnet2$App.<-as.factor(citesnet2$App.)
citesnet2$Common_Name<-as.factor(citesnet2$Common_Name)

# replace NA with ZERO----
citesnet2$`Importer reported quantity`[is.na(citesnet2$`Importer reported quantity`)] <- 0
citesnet2$`Exporter reported quantity`[is.na(citesnet2$`Exporter reported quantity`)] <- 0

# ANALYSIS----
levels(citesnet2$Purpose) 

SGIM_tibble<-citesnet2 %>% 
  filter(Importer == "SG") %>%
  group_by(Common_Name) %>% 
  summarize(Imports_SG = sum(`Importer reported quantity`)) %>% 
  arrange(desc(Imports_SG))
SGIM_tibble

SGEX_tibble<-citesnet2 %>% 
  filter(Exporter == "SG") %>%
  group_by(Common_Name) %>% 
  summarize(Exports_SG = sum(`Exporter reported quantity`)) %>% 
  arrange(desc(Exports_SG))
SGEX_tibble

SG_trade<-merge(SGEX_tibble, SGIM_tibble)  
SG_trade
SG_trade$Exports_SG<-round(SG_trade$Exports_SG)

SG_trade<-SG_trade %>%
  mutate(Net_trade = Imports_SG - Exports_SG) %>% 
  arrange(desc(Net_trade))
SG_trade


# next steps ----
  # annual exports by species
  # annual imports by species
  # annual net trade by species

SGEX_big<-citesnet2 %>% 
  filter(Exporter == "SG") %>%
  group_by(Year,Common_Name) %>% 
  summarize(Exports_SG = sum(`Exporter reported quantity`))
SGEX_big

SGIM_big<-citesnet2 %>% 
  filter(Importer == "SG") %>%
  group_by(Year,Common_Name) %>% 
  summarize(Imports_SG = sum(`Importer reported quantity`))
SGIM_big

SG_trade_big<-merge(SGIM_big, SGEX_big)  
view(SG_trade_big)

SG_trade_big<-SG_trade_big %>% 
  mutate(Net_trade = Imports_SG - Exports_SG)

# charts ----
  # reorder data first
SG_trade_big$Common_Name<-ordered(SG_trade_big$Common_Name, 
                                  levels = c("Red-breasted parakeet",
                                             "Tanimbar corella",
                                             "Yellow-crested cockatoo",
                                             "Rose-ringed parakeet",
                                             "Monk parakeet", 
                                             "Coconut lorikeet"))
# Exports /// this one/// ----
SGEX_chart<-SG_trade_big %>% 
  filter(Common_Name!='Coconut lorikeet'&Common_Name!='Yellow-crested cockatoo') %>% 
  ggplot(aes(Year,Exports_SG))+
  geom_col() +
  facet_wrap(~Common_Name, 
             strip.position = "left", 
             ncol = 1, 
             #scales = "free_y",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_pubclean()+style180+
  theme(strip.text.y = element_text(angle = 0,size=13),
        strip.placement = 'outside',
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=11))+
  labs(title = "CITES reported exports from Singapore: 1980-2020",
       y="Annual imported quantity",x="Year")
SGEX_chart

# Imports /// and this one/// ----
SGIM_chart<-SG_trade_big %>% 
  filter(Common_Name!='Coconut lorikeet'&Common_Name!='Yellow-crested cockatoo') %>% 
  ggplot(aes(Year,Imports_SG))+
  geom_col() +
  facet_wrap(~Common_Name, 
             strip.position = "left", 
             ncol = 1, 
             #scales = "free_y",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_pubclean()+style180+
  theme(strip.text.y = element_text(angle = 0,size=13),
        strip.placement = 'outside',
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=11))+
  labs(title = "CITES reported imports from Singapore: 1980-2020")
SGIM_chart

# Net trade----
SGnet_chart<-SG_trade_big %>% 
  filter(Common_Name!='Coconut lorikeet'&Common_Name!='Yellow-crested cockatoo') %>% 
  ggplot(aes(Year,Net_trade))+
  geom_col() +
  facet_wrap(~Common_Name, 
             strip.position = "left", 
             ncol = 1, 
             scales = "free_y",
             labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme_pubclean()+style180+
  theme(strip.text.y = element_text(angle = 0,size=13),
        strip.placement = 'outside',
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank())+
  labs(title = "CITES reported net trade of parrots. Singapore import and export 1980-2020")
SGnet_chart

# grid----
grid.arrange(SGIM_chart,SGEX_chart,ncol=2,nrow=1)

## styles##

style90 <-  theme(plot.title = element_text(size=40,margin = margin(0,0,25,0)),
                  axis.title.y = element_text(size=25,margin = margin(0,25,0,0)),
                  axis.title.x = element_text(size=25,margin = margin(25,0,0,0)),
                  axis.text.x = element_text(size=25,angle = 90, vjust = 0.5, hjust=1),
                  axis.text.y = element_text(size = 25))
# standard
style180 <-  theme(plot.title = element_text(size=20,margin = margin(0,0,25,0)),
                   axis.title.y = element_text(size=15,margin = margin(0,25,0,0)),
                   axis.title.x = element_text(size=15,margin = margin(25,0,0,0)),
                   axis.text.x = element_text(size=15, vjust = 0.5, hjust=1),
                   axis.text.y = element_text(size = 15))

style180Centered <-  theme(plot.title = element_text(size=20,margin = margin(0,0,25,0)),
                           axis.title.y = element_text(size=15,margin = margin(0,25,0,0)),
                           axis.title.x = element_text(size=15,margin = margin(25,0,0,0)),
                           axis.text.x = element_text(size=15),
                           axis.text.y = element_text(size = 15))

styleRA <-  theme(plot.title = element_text(size=20,margin = margin(0,0,25,0)),
                  axis.title.y = element_text(size=15,margin = margin(0,25,0,0)),
                  axis.title.x = element_text(size=15,margin = margin(25,0,0,0)))

