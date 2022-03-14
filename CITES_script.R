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

#PREPARATION----
cites2<-cites %>% 
  select(Year,App.,Taxon,Importer,Exporter,
         `Importer reported quantity`,
         `Exporter reported quantity`,Purpose,Source_def)
unique(cites3$Taxon)

# replace NA with ZERO----
cites2[is.na(cites2)] <- 0

#common names added
cites2<-cites2 %>% 
  mutate(Common_Name=case_when(
    Taxon=="Cacatua goffiniana"~"Tanimbar corella",
    Taxon=="Psittacula krameri"~"Rose-ringed parakeet",
    Taxon=="Myiopsitta monachus"~"Monk parakeet",
    Taxon=="Psittacula alexandri"~"Red-breasted parakeet",
    Taxon=="Trichoglossus haematodus"~"Coconut lorikeet",
    Taxon=="Cacatua sulphurea"~"Yellow-crested cockatoo"))
cites2<-cites2 %>% 
  relocate(Common_Name,.after = Taxon) # order correctly

#factorize
cites2$Taxon<-as.factor(cites2$Taxon)
cites2$Importer<-as.factor(cites2$Importer)
cites2$Exporter<-as.factor(cites2$Exporter)
cites2$Origin<-as.factor(cites2$Origin)
cites2$Source_def<-as.factor(cites2$Source_def)
cites2$Purpose<-as.factor(cites2$Purpose)
#cites2$Year<-as.factor(cites2$Year)
cites2$App.<-as.factor(cites2$App.)
cites2$Common_Name<-as.factor(cites2$Common_Name)

# ANALYSIS----
unique(cites2$Purpose) 

# remove zoo and circus use
cites3<-cites2 %>% 
  filter(Purpose !="Zoo" & Purpose != "Circus")

# net trade
#cites3<-cites3 %>% 
  #mutate(Net_reported_trade = `Exporter reported quantity` - `Importer reported quantity`)

# CHARTING----

#visualize with species split

# exporter reports----
plot_export_report<-cites3 %>% 
  ggplot(aes(Year,`Exporter reported quantity`,fill=Common_Name))+
         geom_col(stat = "identity") +
  facet_wrap(~Common_Name, strip.position = "right", ncol = 1, scales = "fixed") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES reported export quantity of parrots to Singapore 1990-2020")
plot_export_report

# importer reports----
plot_import_report<-cites3 %>% 
  ggplot(aes(Year,`Importer reported quantity`,fill=Common_Name))+
  geom_col(stat = "identity") +
  facet_wrap(~Common_Name, strip.position = "right", ncol = 1, scales = "fixed") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES reported import quantity of parrots to Singapore 1990-2020")
plot_import_report

# Exporters to Singapore----
plot_export_report_by_country<-cites3 %>% 
  filter(`Exporter reported quantity`>=50) %>%
  ggplot(aes(Year,`Exporter reported quantity`,fill=Exporter))+
  geom_col(stat = "identity") +
  facet_wrap(~Common_Name, strip.position = "right", ncol = 1, scales = "fixed") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        legend.position = "top",
        plot.title = element_text(hjust = 0.5))+
  labs(title = "CITES Exports to Singapore by destination, where annual qty >= 50")
plot_export_report_by_country





# alternative plots----
# export report
cites3 %>% 
  ggplot(aes(Year,`Exporter reported quantity`,
             fill=Common_Name))+
  geom_col()+
  facet_wrap(~Common_Name)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
 
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
plot_import_report

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

plot_export_report
plot_import_report
plot_export_report_by_country
