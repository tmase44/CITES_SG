# CITES SG Project
#libs----
library(tidyverse)
#directory----
getwd()
list.files()
#import data----
cites<-read_csv("CITES.csv")
#inspect---
view(cites)
dim(cites)
head(cites)
ls(cites)
class(cites)
summary(cites$Family)
#prep----
cites2 <- cites
cites2$Taxon<-as.factor(cites2$Taxon)
cites2$Importer<-as.factor(cites2$Importer)
cites2$Exporter<-as.factor(cites2$Exporter)
cites2$Origin<-as.factor(cites2$Origin)
cites2$Source_def<-as.factor(cites2$Source_def)
cites2$Year<-as.factor(cites2$Year)
cites2$App.<-as.factor(cites2$App.)
#analysis----
cites3<-cites %>% 
  select(Year,App.,Taxon,Importer,Exporter,
         `Importer reported quantity`,
         `Exporter reported quantity`,Source_def)
unique(cites3$Taxon)
#common names added
cites3<-cites3 %>% 
  mutate(Common_Name=case_when(
    Taxon=="Cacatua goffiniana"~"Tanimbar corella",
    Taxon=="Psittacula krameri"~"Rose-ringed parakeet",
    Taxon=="Myiopsitta monachus"~"Monk parakeet",
    Taxon=="Psittacula alexandri"~"Red-breasted parakeet",
    Taxon=="Trichoglossus haematodus"~"Coconut lorikeet"))
cites3<-cites3 %>% 
  relocate(Common_Name,.after = Taxon) # order correctly
# replace NA with ZERO----
cites3[is.na(cites3)] <- 0

#visualize with species split
  # exporter reports
plot_export_report<-cites3 %>% 
  ggplot(aes(Year,`Exporter reported quantity`,
             fill=Common_Name))+
  geom_col()+
  facet_wrap(~Common_Name)+
  theme_minimal()+
  theme(legend.position = c(0.85,0.25),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES reported export quantity of parrots to Singapore 1990-2020")

       
# Importer reports
plot_import_report<-cites3 %>% 
  ggplot(aes(Year,`Importer reported quantity`,
             fill=Common_Name))+
  geom_col()+
  facet_wrap(~Common_Name)+
  theme_minimal()+
  theme(legend.position = c(0.85,0.25),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES reported import quantity of parrots to Singapore 1990-2020")

# Exporters to Singapore
plot_export_report_by_country<-cites3 %>% 
  filter(`Exporter reported quantity`>=50) %>% 
  ggplot(aes(Year,`Exporter reported quantity`,
             fill=Exporter))+
  geom_bar(stat = 'identity')+
  facet_wrap(~Common_Name)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES Exports to Singapore by destination, where annual qty >= 50")
