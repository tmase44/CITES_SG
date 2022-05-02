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
  

