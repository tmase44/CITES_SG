library(tidyverse)

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
                                  levels = c("Monk parakeet", 
                                             "Rose-ringed parakeet", 
                                             "Coconut lorikeet", 
                                             "Red-breasted parakeet",
                                             "Yellow-crested cockatoo",
                                             "Tanimbar corella"))


# Exports
SGEX_chart<-SG_trade_big %>% 
  ggplot(aes(Year,`Exports_SG`,fill=Common_Name))+
  geom_col() +
  facet_wrap(~Common_Name, strip.position = "right", ncol = 1, scales = "fixed") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES reported export quantity of parrots from Singapore 1980-2020")
SGEX_chart

# Imports
SGIM_chart<-SG_trade_big %>% 
  ggplot(aes(Year,`Imports_SG`,fill=Common_Name))+
  geom_col() +
  facet_wrap(~Common_Name, strip.position = "right", ncol = 1, scales = "fixed") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES reported import quantity of parrots to Singapore 1980-2020")
SGIM_chart

# Net trade
SGnet_chart<-SG_trade_big %>% 
  ggplot(aes(Year,`Net_trade`,fill=Common_Name))+
  geom_col() +
  facet_wrap(~Common_Name, strip.position = "right", ncol = 1, scales = "fixed") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES reported net trade of parrots. Singapore import and export 1980-2020")
SGnet_chart
