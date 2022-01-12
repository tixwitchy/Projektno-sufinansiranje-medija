library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(scales)
library(viridis)
library(crosstalk)
library(lubridate)
library(writexl)
library(shinycssloaders)




###Priprema podskupova za razlicite tabove same aplikacije

##Importovanje podataka iz excel tabele

Projectmedia<-read_excel("Projektno sufinansiranje medija.xlsx")




##OPSTI PREGLED TAB

#Podskup za ukupno dodeljeno sredstava po godinama

yearstable <- Projectmedia %>%
  
  group_by(GODINA) %>%
  
  summarise(`UKUPNA SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`))

#Formatiranje sredstava

yearstable$`UKUPNA SREDSTVA U EVRIMA` <- format(yearstable$`UKUPNA SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE)

yearstable$GODINA <- format(yearstable$GODINA, digits=0)

#Podskup za bar chart- institucije/davaoci javnog novca


barchartorgani <- Projectmedia%>%
  filter(!`SREDSTVA U EVRIMA`==0)

#Stavljanje svih opstina pod zajednicki naziv lokalne samouprave

barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` [barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture" & barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`!="Pokrajinski sekretarijat za kulturu i javno informisanje"] <- "Lokalne samouprave"

#Grupisanje prema organu koji dodeljuje sredstva sumiranje sa kolone sa sredstvima u evrima
#i redjanje od najvece vrednosti ka najmanjoj

barchartorgani <- barchartorgani %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`) %>%
  
  summarise(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`),n=n())%>%
  
  arrange(desc(`SREDSTVA U EVRIMA`))

#Skracivanje naziva za pokrajinski sekretarijat u grafikonu

barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`[barchartorgani$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`=="Pokrajinski sekretarijat za kulturu i javno informisanje"] <- "Pokrajinski sekretarijat"

#dodavanje Info kolone za pop-up informacije koje ce se pojaviti kada kursor predje preko grafikona

barchartorgani <- barchartorgani %>%
  
  mutate(Info = paste('<br>', "Institucija koja raspisuje konkurs:", 
                      
                      `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`, '<br>',
                      
                      "Ukupno novca:",format(`SREDSTVA U EVRIMA`,big.mark = ","), 'eur<br>',
                      
                      "Broj projekata:",n,"<br>"))


#Podskup za linechart za sve teme za sve godine 

linechartteme <- Projectmedia

#Iskljucivanje slucajeva kada sredstva nisu dodeljena

linechartteme <- linechartteme%>%
  filter(!`SREDSTVA U EVRIMA`==0)

# Promena NA vrednosti sa Nema informacija
linechartteme$`TEMA PROJEKTA`[is.na(linechartteme$`TEMA PROJEKTA`)] <- "Nema informacija"

#Formatiranje kolone teme u faktor zbor prikazivanja na linechart-u

linechartteme$`TEMA PROJEKTA` <- as.factor(linechartteme$`TEMA PROJEKTA`)

#Grupisanje prema godini i temi, sumiranje prema broju projekata po temi i
#kreiranje Info kolone  za pop-up informacije kada se predje kursorom

linechartteme <- linechartteme %>%
  
  group_by(GODINA,`TEMA PROJEKTA`) %>%
  
  summarize(n=n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info= paste("<br>","Tema:",`TEMA PROJEKTA`,"<br>",
                     
                     "Broj projekata po temi:",n,"<br>",
                     
                     "Ukupno dodeljeno sredstava po temi:",format(`SREDSTVA U EVRIMA`,big.mark = ","),"eur<br>",
                     
                     "Godina:",GODINA,"<br>"))


#Formatiranje kolone sa godinama

linechartteme$GODINA <- as.factor(linechartteme$GODINA)


##PODNOSIOCI PROJEKTA-TAB 

#Podskup za pretrazivu tabelu-izbor kolona za prikazivanje i iskljucivanje konkursa gde sredstva nisu dodeljena

tabelapodnosioci <- Projectmedia %>%
  
  select(`PODNOSILAC PROJEKTA`,`NAZIV MEDIJA`,`NAZIV PROJEKTA`,`TEMA PROJEKTA`,`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,GODINA,`SREDSTVA U DINARIMA`,`SREDSTVA U EVRIMA`) %>%
  
  filter(!`SREDSTVA U EVRIMA` == 0)

#formatiranje kolone sa sredstvima

tabelapodnosioci$`SREDSTVA U DINARIMA` <- format( tabelapodnosioci$`SREDSTVA U DINARIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE)

tabelapodnosioci$`SREDSTVA U EVRIMA` <- format( tabelapodnosioci$`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE)

#Podskup za malu tabelu sa ukupnom kolicinom novca po podnosiocu i broju medija


podnosiocipojsuma <- Projectmedia %>%
  
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarise(`UKUPNO DODELJENO U EVRIMA`= sum(`SREDSTVA U EVRIMA`)) 


podnosiocimediji <- Projectmedia

podnosiocimediji$`NAZIV MEDIJA`[ podnosiocimediji$`NAZIV MEDIJA`== "Produkcija"] <- NA

podnosiocimediji<- podnosiocimediji %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>% 
  
  summarize( `BROJ IDENTIFIKOVANIH MEDIJA ZA KOJE JE PODNOSILAC APLICIRAO` = n_distinct(`NAZIV MEDIJA`,na.rm = TRUE))

podnosiocipojsuma <- Projectmedia %>%
  
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarise(`UKUPNO DODELJENO U EVRIMA`= sum(`SREDSTVA U EVRIMA`)) 

podnosiociorgani<- Projectmedia %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>% 
  
  summarize( `BROJ ORGANA OD KOJIH JE PODNOSILAC DOBIO SREDSTVA` = n_distinct(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`))  


podnosiocitotal <- left_join(podnosiocipojsuma, podnosiocimediji, by = "PODNOSILAC PROJEKTA")
podnosiocitotal <-left_join(podnosiocitotal, podnosiociorgani,by = "PODNOSILAC PROJEKTA")
#Formatiranje kolone sa sredstvima

podnosiocitotal$`UKUPNO DODELJENO U EVRIMA` <- format( 
  
  podnosiocitotal$`UKUPNO DODELJENO U EVRIMA`, big.mark = ',', digits = 0, nsmall=0, scientific = FALSE)



##Podskup  za top 4 podnosioca projekta za barchart za sve godine od svih davalaca javnog novca 

barcharttop4podnosioci <- Projectmedia %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`), n=n()) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4,`SREDSTVA U EVRIMA`)

#skracivanje imena zbog prikazivanja na grafikonu za top 4 podnosioca

barcharttop4podnosioci$`PODNOSILAC PROJEKTA`[barcharttop4podnosioci$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA NOVI PAZAR DOO NOVI PAZAR"] <- "RADIO TELEVIZIJA NOVI PAZAR"
barcharttop4podnosioci$`PODNOSILAC PROJEKTA`[barcharttop4podnosioci$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA BELLE AMIE DOO NIŠ"] <- "RADIO TELEVIZIJA BELLE AMIE"

barcharttop4podnosioci<- barcharttop4podnosioci %>%
  mutate(Info = paste('<br>', "Podnosilac:", 
                      
                      `PODNOSILAC PROJEKTA`, '<br>',
                      
                      "Ukupno dobijenih sredstava:", 
                      
                      format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Broj projekata:",n,"<br>"
                      
  ))



#podskup za podnosioce po godinama linechart



yearssredstvapodnosioci <- Projectmedia %>%
  
  group_by(`PODNOSILAC PROJEKTA`,`GODINA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Godina:", 
                      
                      `GODINA`, '<br>',
                      
                      "Ukupno sredstava:",
                      
                      format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>'
                      
  ))



#pie chart tema za pojedinacnog podnosioca


piecharttemepodnosiocimain <- Projectmedia 

# Promena NA vrednosti sa Nema informacija

piecharttemepodnosiocimain $`TEMA PROJEKTA`[is.na(piecharttemepodnosiocimain$`TEMA PROJEKTA`)] <- "Nema informacija"

#grupisanje prema podnosiocu i temi projekta, sumiranje i na osnovu broja projekata
#i na osnovu sredstava u EVRIMA dodavanje kolone sa informacijama za pop-up i
#dodavanje kolone sa bojama

piecharttemepodnosiocimain  <- piecharttemepodnosiocimain  %>%
  
  
  group_by(`PODNOSILAC PROJEKTA`,`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno dobijeno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dobijenim sredstvima:",round(`SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),'%<br>',
                      
                      "Ukupan broj projekata:", n, "<br>")) %>%
  
  
  
  mutate( boje = case_when (  `TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                              
                              `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                              
                              `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                              
                              `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                              
                              `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                              
                              `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                              
                              `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                              
                              `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                              
                              `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"
                              
  ))


#pie chart za sve teme podnosilaca

piecharttemepodnosall<-Projectmedia

#iskljucivanje konkursa gde nisu dodeljena sredstva

piecharttemepodnosall <- piecharttemepodnosall%>%
  filter(!`SREDSTVA U EVRIMA`==0)

# Promena NA vrednosti sa Nema informacija

piecharttemepodnosall$`TEMA PROJEKTA`[is.na(piecharttemepodnosall$`TEMA PROJEKTA`)] <- "Nema informacija"

#grupisanje  prema temi projekata, sumiranje i po broju projekata i prema sredstvima u dinarima,
#dodavanje kolone sa informacijama za pop-up i kolone sa bojama


piecharttemepodnosall <- piecharttemepodnosall %>%
  
  group_by(`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dodeljenim sredstvima:", round( `SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100, 2),"%<br>",
                      
                      "Ukupan broj projekata:", n, "<br>"
                      
  )) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"))

#Mini tabela sa medijima koji su registrovani u APR za datog izdavaca

#Importovanje podataka iz excel tabele za registrovane medije u APR-u

aprregistrovanimedijifin<- read_excel("aprregmediji2020.xlsx")

aprregistrovanimedijifin<-aprregistrovanimedijifin%>%
  select(-`MATIČNI BROJ PODNOSIOCA`)

##MINISTARSTVO KULTURE -TAB

#Podskup za sve teme  za koje je Ministarstvo kulture dalo novac 
#grupisanje prema temi, sumiranje prema broju projekata i sredstvima u EVRIMA,
#dodavanje kolone za informacije za pop-up i  kolone sa bojama

piecharttemekult <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Ministarstvo kulture") %>%
  
  group_by(`TEMA PROJEKTA`) %>%
  
  summarize(n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`))%>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF")) %>%
  
  mutate(Info=paste('<br>', "Tema:", 
                    
                    `TEMA PROJEKTA`, '<br>',
                    
                    "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                    
                    "Učešće u dobijenim sredstvima:",round(`SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),'%<br>',
                    
                    "Ukupan broj projekata:",n,"<br>"
                    
  ))


#Podskup za top 4 podnosioca projekta kojima je Ministarstvo kulture dalo novac 

barcharttop4podnosiocikult <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Ministarstvo kulture") %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`),n=n()) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4,`SREDSTVA U EVRIMA`)

#skracivanje imena zbog  lepseg prikazivanja na grafikonu

barcharttop4podnosiocikult$`PODNOSILAC PROJEKTA`[barcharttop4podnosiocikult$`PODNOSILAC PROJEKTA`=="PREDUZEĆE ZA PROIZVODNJU I DISTRIBUCIJU RTV PROGRAMA, TRGOVINU I USLUGE TV APATIN DRUŠTVO SA OGRANIČENOM ODGOVORNOŠĆU APATIN"] <- "TV APATIN DOO"
barcharttop4podnosiocikult$`PODNOSILAC PROJEKTA`[barcharttop4podnosiocikult$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA VRANJE D.O.O. VRANJE"] <- "RADIO TELEVIZIJA VRANJE"


barcharttop4podnosiocikult<-barcharttop4podnosiocikult%>%
  
  mutate(Info=paste('<br>', "Podnosilac:", 
                    
                    `PODNOSILAC PROJEKTA`, '<br>',
                    
                    "Ukupno dobijenih sredstava:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                    
                    "Broj projekata:",n,"<br>"
  ))



#podskup za linechart-koliko je para dato po temi po godinama
#biranje samo konkursa ministarstva kulture, grupisanje prema temi i godini
#sumiranje prema broju projekata i kolicini novca i dodavanja kolone za pop-up i za boje

linecharttemekult <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Ministarstvo kulture") %>%
  
  group_by(`TEMA PROJEKTA`,GODINA) %>%
  
  summarize(n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF")) %>%
  
  mutate(Info = paste("<br>","Tema:",`TEMA PROJEKTA`,"<br>",
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE),"eur<br>",
                      
                      "Broj projekata:",n,"<br>",
                      
                      "Godina:",GODINA,"<br>"))

#formatiranje kolone sa godinama

linecharttemekult$GODINA <- as.factor(linecharttemekult$GODINA)


#Podskup za pretrazivu tabelu ministarstva kulture

tabelakultura <- Projectmedia %>%
  
  select(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,`PODNOSILAC PROJEKTA`,`NAZIV MEDIJA`,`NAZIV PROJEKTA`,`TEMA PROJEKTA`,`GODINA`,`SREDSTVA U DINARIMA`,`SREDSTVA U EVRIMA`) %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Ministarstvo kulture")

#formatiranje numerickih kolona

tabelakultura$`SREDSTVA U DINARIMA` <- format( tabelakultura$`SREDSTVA U DINARIMA` , big.mark =',', digits = 0, nsmall = 0, scientific = FALSE)

tabelakultura$`SREDSTVA U EVRIMA` <- format( tabelakultura$`SREDSTVA U EVRIMA` , big.mark =',', digits = 0, nsmall = 0, scientific = FALSE)


##POKRAJINSKI SEKRETARIJAT-TAB

#Podskup sa svim  temama za koje je Pokrajinski sekretarijat dao novac 


piecharttemepoksek <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Pokrajinski sekretarijat za kulturu i javno informisanje") %>%
  
  group_by(`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dodeljenim sredstvima:", round( `SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),"%<br>",
                      
                      "Ukupan broj projekata:", n, "<br>"
                      
  )) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"))

#podskup za linechart za pokrajinski sekretarijat koliko je para dato po pojedinacnoj temi po godinama

linecharttemepoksek <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Pokrajinski sekretarijat za kulturu i javno informisanje") %>%
  
  group_by(`TEMA PROJEKTA`,GODINA) %>%
  
  summarize(n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate( boje = case_when( `TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF")) %>%
  
  mutate(Info = paste("<br>","Tema:",`TEMA PROJEKTA`,"<br>",
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE),"eur<br>",
                      
                      "Broj projekata:",n,"<br>",
                      
                      "Godina:",GODINA,"<br>"))

#Formatiranje kolone godine

linecharttemepoksek$GODINA <- as.factor(linecharttemepoksek$GODINA)


#Podskup za top 4 podnosioca kojima je Pokrajinski sekretarijat dao novac 

barcharttop4podnosiocipoksek <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Pokrajinski sekretarijat za kulturu i javno informisanje") %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`),n=n()) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4,`SREDSTVA U EVRIMA`) 

#Skracivanje imena za lepsi prikaz na grafikonu

barcharttop4podnosiocipoksek$`PODNOSILAC PROJEKTA`[barcharttop4podnosiocipoksek$`PODNOSILAC PROJEKTA`=="DNEVNIK VOJVODINA PRESS DOO PREDUZEĆE ZA IZDAVANJE I ŠTAMPANJE NOVINA, NOVI SAD"] <- "DNEVNIK VOJVODINA PRESS"

barcharttop4podnosiocipoksek$`PODNOSILAC PROJEKTA`[barcharttop4podnosiocipoksek$`PODNOSILAC PROJEKTA`=="SAVEZ GLUVIH I NAGLUVIH VOJVODINE AUDIOLOŠKI CENTAR"] <- "SAVEZ GLUVIH I NAGLUVIH VOJVODINE"


# dodavanje kolone sa informacijama za pop-up

barcharttop4podnosiocipoksek <- barcharttop4podnosiocipoksek %>%
  
  mutate(Info = paste('<br>', "Podnosilac:", 
                      
                      `PODNOSILAC PROJEKTA`, '<br>',
                      
                      "Ukupno dobijena sredstava:", format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Broj projekata:",n,"<br>"
                      
  ))


#Podskup za pretrazivu tabelu pokrajinskog sekretarijata

tabelapoksek <- Projectmedia %>%
  
  select(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,`PODNOSILAC PROJEKTA`,`NAZIV MEDIJA`,`NAZIV PROJEKTA`,`TEMA PROJEKTA`,`GODINA`,`SREDSTVA U DINARIMA`,`SREDSTVA U EVRIMA`) %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` == "Pokrajinski sekretarijat za kulturu i javno informisanje")

#Formatiranje kolone sa sredstvima

tabelapoksek$`SREDSTVA U DINARIMA` <- format( tabelapoksek$`SREDSTVA U DINARIMA` , big.mark =',', digits = 0, nsmall = 0,scientific = FALSE)
tabelapoksek$`SREDSTVA U EVRIMA` <- format( tabelapoksek$`SREDSTVA U EVRIMA` , big.mark =',', digits = 0, nsmall = 0, scientific = FALSE)


##LOKALNE SAMOUPRAVE-TAB

#Podskup za top 4 lokalne samouprave koje su dale najvise novca- mala tabela

tabela4opstine <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje" & `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4) %>%
  
  rename(`ČETIRI OPŠTINE KOJE SU DALE NAJVIŠE SREDSTAVA`=`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)


#Formatiranje kolone za sredstva

tabela4opstine$`SREDSTVA U EVRIMA` <- format( tabela4opstine$`SREDSTVA U EVRIMA` , big.mark =',', digits = 0, nsmall = 0, scientific = FALSE)


# Podskup za malu tabelu  za koliko je ukupno sredstava dato po opstini

tabelaukupnopstina <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje" & `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`) %>%
  
  summarise(`UKUPNO DODELJENO U EVRIMA`= sum(`SREDSTVA U EVRIMA`)) %>%
  
  rename(`OPŠTINA` = `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)


#Formatiranje kolone sa sredstvima

tabelaukupnopstina$`UKUPNO DODELJENO U EVRIMA` <- format( 
  
  tabelaukupnopstina$`UKUPNO DODELJENO U EVRIMA`, big.mark = ',',digits = 0, nsmall = 0, scientific = FALSE )

#Podskup za veliku pretrazivu tabelu

tabelaopstinee <- Projectmedia %>%
  
  select(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`, `PODNOSILAC PROJEKTA`, `NAZIV MEDIJA`, `NAZIV PROJEKTA`, `TEMA PROJEKTA`, `GODINA`, `SREDSTVA U DINARIMA`, `SREDSTVA U EVRIMA`) %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`!="Pokrajinski sekretarijat za kulturu i javno informisanje" & `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture")


#Podskup za teme za koje su sve lokalne samouprave dale novac 


piecharttemeloksam <- Projectmedia 

#iskljucivanje konkursa na kojima nisu dodeljena sredstva 

piecharttemeloksam <- piecharttemeloksam%>%
  filter(!`SREDSTVA U EVRIMA`==0)


# Promena NA vrednosti sa Nema informacija

piecharttemeloksam$`TEMA PROJEKTA`[is.na(piecharttemeloksam$`TEMA PROJEKTA`)] <- "Nema informacija"


piecharttemeloksam <- piecharttemeloksam %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje"& `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno dodeljeno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dobijenim sredstvima:",round(`SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),'%<br>',
                      
                      "Ukupan broj projekata:", n, "<br>")) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"))

#Podskup za linechart kako su lokalne samouprave dodeljivale novac po godinama


yearssredstvaloksam <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje"& `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,`GODINA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Godina:", 
                      
                      `GODINA`, '<br>',
                      
                      "Ukupno u evrima:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), '<br>'
                      
  ))

#Podskup za top 4 podnosioca kojima su sve lokalne samouprave dale novac

barcharttop4podnosiociloksam <- Projectmedia %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje"& `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`PODNOSILAC PROJEKTA`) %>%
  
  summarize(`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`),n=n()) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  top_n(4,`SREDSTVA U EVRIMA`) 


barcharttop4podnosiociloksam$`PODNOSILAC PROJEKTA`[barcharttop4podnosiociloksam$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA NOVI PAZAR DOO NOVI PAZAR"] <- "RADIO TELEVIZIJA NOVI PAZAR"
barcharttop4podnosiociloksam$`PODNOSILAC PROJEKTA`[barcharttop4podnosiociloksam$`PODNOSILAC PROJEKTA`=="RADIO TELEVIZIJA BELLE AMIE DOO NIŠ"] <- "RADIO TELEVIZIJA BELLE AMIE"



barcharttop4podnosiociloksam <- barcharttop4podnosiociloksam %>%
  
  mutate(Info = paste('<br>', "Podnosilac:", 
                      
                      `PODNOSILAC PROJEKTA`, '<br>',
                      
                      "Ukupno dobijena sredstava:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      "Broj projekata:",n,"<br>"
                      
  ))

#Podskup za teme po pojedinacnoj lok samoupravi

piecharttemeloksampoj <- Projectmedia


piecharttemeloksampoj  <- piecharttemeloksampoj %>%
  filter(!`SREDSTVA U EVRIMA`==0)

# Promena NA vrednosti sa Nema informacija

piecharttemeloksampoj$`TEMA PROJEKTA`[is.na(piecharttemeloksampoj$`TEMA PROJEKTA`)] <- "Nema informacija"


piecharttemeloksampoj <- piecharttemeloksampoj %>%
  
  filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Pokrajinski sekretarijat za kulturu i javno informisanje"& `ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` != "Ministarstvo kulture") %>%
  
  group_by(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,`TEMA PROJEKTA`) %>%
  
  summarize( n = n(),`SREDSTVA U EVRIMA` = sum(`SREDSTVA U EVRIMA`)) %>%
  
  arrange(desc(`SREDSTVA U EVRIMA`)) %>%
  
  mutate(Info = paste('<br>', "Tema:", 
                      
                      `TEMA PROJEKTA`, '<br>',
                      
                      "Ukupno sredstava po temi:",format (`SREDSTVA U EVRIMA`, big.mark=',', digits = 0, nsmall=0, scientific = FALSE), 'eur<br>',
                      
                      "Učešće u dodeljenim sredstvima:",round(`SREDSTVA U EVRIMA`/sum(`SREDSTVA U EVRIMA`)*100,2),'%<br>',
                      
                      "Ukupan broj projekata:", n, "<br>")) %>%
  
  mutate( boje = case_when (`TEMA PROJEKTA` == "Ekologija i zdravlje" ~ "#440154FF",
                            
                            `TEMA PROJEKTA` == "Ekonomija" ~ "#472D7BFF",
                            
                            `TEMA PROJEKTA` == "Informativni program" ~ "#3B528BFF",
                            
                            `TEMA PROJEKTA` == "Kultura i obrazovanje" ~ "#2C728EFF",
                            
                            `TEMA PROJEKTA` == "Manjinski sadržaj" ~ "#21908CFF",
                            
                            `TEMA PROJEKTA` == "Nema informacija" ~ "#27AD81FF",
                            
                            `TEMA PROJEKTA` == "Neprivilegovane grupe" ~ "#5DC863FF",
                            
                            `TEMA PROJEKTA` == "Ostalo" ~ "#AADC32FF",
                            
                            `TEMA PROJEKTA` == "Sport" ~ "#FDE725FF"))



#########KRAJ OBRADE PODATAKA SLEDI UI I SERVER DEO####


#Kreiranje izgleda aplikacije User interface-UI

ui <- 
  navbarPage(
    #glavni naslov i izbor teme
    
    "Projektno sufinansiranje medija u Srbiji", theme = shinytheme ("united"),
    
    # izgled prvog taba Opsti pregled sa tekstom i tabelama i grafikonom sa leve strane u sidebar panelu
    
    tabPanel ("Opšti pregled", 
              sidebarPanel ( h4 ("Tabela ukupno dodeljenih\n sredstava po godinama od svih organa vlasti", align="center"),
                            
                            tableOutput("tabelaukupno"),
                            
                            plotlyOutput("barchartdonor") %>% withSpinner(color="#E95420")
              ),
              
              #tekst koji ide u glavni ili desni deo sa nekim informacijama koje izvlaci iz same tabele, dodati su i linkovi
              #za kreiranje novog pasusa koristi se h4
              
              mainPanel ( h4 ("Dobrodošli na stranicu na kojoj se možete bolje upoznati sa procesom projektnog sufinansiranja medijskih
                                  
                                  sadržaja od javnog interesa u Republici Srbiji. Projekat", a(" Centra za održive zajednice",href = "https://odrzivezajednice.org/"), 
                              
                              "u okviru kojeg je izrađena ova veb aplikacija („Otvorenim podacima do kvalitetnijeg projektnog sufinansiranja medijskih sadržaja“) 
                                  
                                  podržali su ", a("Ministarstvo kulture i informisanja Republike Srbije", href = "http://www.kultura.gov.rs"),
                              
                              "i", a("Misija OEBS-a u Srbiji", href = "https://www.osce.org/mission-to-serbia")," sa ciljem da se učini transparentnim ovaj proces 
                                  
                                  koji je važan za medije, medijske profesionalce, ali i građane.", align = "justify"),
                          
                          h4 ("Veb aplikacija koja je pred vama daje mogućnost da pretražujete podatke po podnosiocu projekta,
                                  
                                  prema organu koji je dodelio sredstva, ali i prema temama koje su zastupljene u realizovanim projektima.
                                  
                                  Možete, takođe, da preuzmete podatke, metodologiju vezanu za podatke i njihovo prikupljanje, 
                                  
                                  kao i celokupnu publikaciju u kojoj se pored metodoloških napomena i preporuka, nalazi i tekst 
                                  
                                  o istorijatu konkursnog sufinansiranja u Srbiji.
                                  
                                  Podaci su takođe dostupni i na", a(" Portalu otvorenih podataka.", href = "https://data.gov.rs/sr/datasets/rezultati-konkursa-za-projektno-sufinansiranja-medijskikh-sadrzhaja/"), align ="justify"),
                          
                          h4 (paste("Na ovoj stranici imate sumirane podatke za sve godine trajanja projektnog sufinansiranja, a zastupljeni 
                                        
                                        su svi organi vlasti koji su dodeljivali novac medijima na osnovu Zakona o javnom informisanju i 
                                        
                                        medijima. Jedna od novina je zastupljenost tema koje su definisane na osnovu naslova projekata. 
                                        
                                        Tokom ovog procesa, od 2015. do  2021. godine, dodeljeno je ukupno ", format(sum(Projectmedia$`SREDSTVA U EVRIMA`), big.mark = ',', digits = 0, nsmall = 0, scientific = FALSE),
                                    
                                    "evra za ", format(NROW(tabelapodnosioci$`NAZIV PROJEKTA`), big.mark = ',', digits = 0, nsmall = 0, scientific = FALSE), "projekata koji su podneti od ukupno",
                                    
                                    format(n_distinct(tabelapodnosioci$`PODNOSILAC PROJEKTA`), big.mark = ',', digits = 0, nsmall = 0, scientific = FALSE), " podnosilaca. Predmet prikupljanja podataka 
                                        
                                        bili su konkursi Minstarstva kulture i informisanja Republike Srbije, Pokrajinskog sekretarijata za kulturu, javno informisanje i odnose sa verskim zajednicama i",
                                    
                                    n_distinct(yearssredstvaloksam$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`) ,"lokalne samouprave (gradovi i opštine). Grafikon sa vaše leve strane prikazuje dodelu sredstava prema organu vlasti koji je raspisao konkurse za sve godine, a drugi 
                                  
                                  grafikon na ovoj stranici prikazuje zastupljenost tema po godinama na osnovu broja projekata."), align = "justify"),
                          
                          h4 (strong("Važna napomena:")," pošto su grafikoni interaktivni, potrebno je samo da pređete kursorom preko njih da biste videli informacije.", align ="justify"),
                          
                          #mesto gde ce ici linechart grafikon a ispod njega logoi
                          
                          plotlyOutput("linechartteme", height = 500) %>% withSpinner(color = "#E95420"),
                          
                          fluidRow(
                            column(4, img(src="COZ logo.jpg",style="width: 200px;")),
                            
                            column(4, img(src="OEBSlog.png", style="width: 260px; margin-top: 80px;")),
                            
                            column(4, img(src="min.png"))
                          )
                          
                          
                          
                          
              )),
    
    #uredjivanje taba podnosilac projekta
    
    tabPanel("Podnosilac projekta",
             
             sidebarPanel(
               
               h4("Ovde možete da pretražite podatke vezane za",strong("podnosioce projekata"), "koji su dobili sredstva u periodu od početka 2015. do 2021. godine.", align = "justify"), 
               
               h4("Ako izaberete sve podnosioce iz padajućeg menija i pritisnete dugme ", strong("“Pretraži”"), "pojaviće vam se grafikon sa informacijama o četiri podnosioca medijskih projekata koji su
                        
                        dobili najviše novca od svih organa koji raspisuju konkurse, grafikon učešća dodeljenih sredstava po temama za sve podnosioce, kao i tabelu na kojoj će se nalaziti celokupni podaci koje možete pretraživati na različite načine.", align = "justify"),
               
               h4("Pretragom pojedinačnih podnosilaca pojaviće vam se informacije o ukupno dobijenim sredstvima koje je on povukao tokom ovog procesa, grafikon trenda dobijanja novca po godinama, učešća dobijenih sredstava po temama i tabela sa ostalim informacijama
                        
                        o izabranom podnosiocu projekta. Na samom dnu možete da vidite koji su mediji registrovani za  izabranog podnosioca u APR-u.", align = "justify"),
               
               #dodavalnje na levoj strani drop down opcije sa podnosiocima i tastera pretrazi
              
               selectizeInput("podnosilac", label = "Podnosilac:", choices = NULL,options = list(placeholder = "Ukucajte podnosioca ili SVI PODNOSIOCI")),
               
               actionBttn(inputId = "go", label = "Pretraži", icon("search"), style = "jelly", size = "sm",color = "danger"),
               
               helpText("Kliknite na Pretraži da biste videli tabelu i grafikone"),
               
               #mesto gde ce u sidebar ici koliko je ukupno dato po podnosiocu,i grafikoni
               
               tableOutput("tabelaukupnopodnosioci"),
               
               plotlyOutput("podnosiocilinebar"),
               
               plotlyOutput("chartteme"),
               
               dataTableOutput("registrovanimedijiapr")
               
             ), 
             
             #glavni panel sa velikom pretrazivom tabelom podnosilaca
             
             mainPanel ( dataTableOutput("tabelapodnosioci"))),
    
    #uredjivanje taba Ministarstva kulture
    
    tabPanel("Ministarstvo kulture i informisanja",
             
             sidebarPanel(
               
               h4("Ovde možete da pretražite podatke o konkursima", strong ("Ministarstva kulture i informisanja Republike Srbije"), "za projektno sufinansiranje medijskih sadržaja od 2015 do 2021. godine.",align= "justify"),
               
               h4("Podatke možete da pretražujete po temi projekata. Grafikoni prikazuju trend davanja novca u odnosu na određene teme. U donjem delu stranice imate i informaciju o četiri podnosioca koji su
                     
                     dobili najviše novca od Ministarstva.", align="justify"),
               
               selectizeInput("temakultura", "Izaberite temu:", options = list(placeholder = "Ukucajte temu ili Sve teme"), c("Sve teme",
                                                                  sort(unique(tabelakultura$`TEMA PROJEKTA`)))),
               
               actionBttn(inputId = "go1", label = "Pretraži", icon("search"), style = "jelly", size = "sm",color = "danger"),
               
               helpText("Kliknite na Pretraži da biste videli tabele i grafikone"),
               
               plotlyOutput("chartministarstvoteme"),
               
               plotlyOutput("barchartministarstvopodnosioci")
             ), 
             
             mainPanel ( dataTableOutput("tabelaministarstvo"))),
    
    #Uredjivanje taba pokrajinski sekretarijat 
    
    tabPanel("Pokrajinski sekretarijat",
             
             sidebarPanel(
               
               h4("Ovde možete da pretražite podatke o realizovanim projektima po konkursima ", strong("Pokrajinskog sekretarijata za kulturu, javno informisanje i odnose sa verskim zajednicama"),"za projektno sufinansiranje medijskih sadržaja 
                        
                        u periodu od 2015. do 2021. godine.", align = "justify"),
               
               h4 ("Grafikoni prikazuju trend davanja novca u odnosu na teme projekata. U donjem delu stranice se nalazi i informacija o četiri podnosioca projekata koji su dobili najviše novca od ovog organa vlasti.", align = "justify"),
               
               selectizeInput("temapoksek", "Izaberite temu:",options = list(placeholder = "Ukucajte temu ili Sve teme"), c("Sve teme",
                                                                 
                                                                 sort(unique(tabelapoksek$`TEMA PROJEKTA`)))),
               
               actionBttn(inputId = "go2",label = "Pretraži", icon("search"), style = "jelly", size = "sm",color = "danger"),
               
               helpText("Kliknite na Pretraži da biste videli tabele i grafikone"),
               
               plotlyOutput("chartpoksekteme"),
               
               plotlyOutput("barchartpoksekpodnosioci")
               
             ), 
             
             mainPanel ( dataTableOutput("tabelapoksek"))),
    
    #Uredjivanje taba Lokalne samouprave
    
    tabPanel("Lokalne samouprave",
             
             sidebarPanel(
               
               h4("Ovde možete da pretražite podatke o medijskim projektima koje su po Zakonu o javnom informisanju i medijima 
                        
                        podržale",strong("lokalne samouprave u Srbiji"), "od početka 2015. do 2021. godine.",align="justify"),
               
               h4( "Klikom na", strong("“Sve opštine”"), "dobijate tabelu sa informacijama koje su četiri opštine dodelile najviše sredstava,
                         
                         grafički prikaz dodele sredstava po temama (sve lokalne samouprave), četiri podnosioca projekata koji su zbirno 
                         
                         dobili najviše sredstava od svih lokalnih samouprava i tabelu sa svim informacijama vezanim za medijske projekte
                         
                         na nivou lokalnih samouprava.", align = "justify"),
               
               h4("Biranjem pojedinačnih opština dobićete podatke koliko je ukupno dala novca izabrana lokalna samouprava, grafikon
                        
                        učešća dodele sredstava po temama za taj grad ili opštinu, trend ukupno dodeljenih sredstava po godinama i veliku
                        
                        tabelu sa svim informacijama u vezi sa izabranom lokalnom samoupravom.", align = "justify"),
               
               selectizeInput("opstina", "Lokalna samouprava:", options = list(placeholder = "Ukucajte opštinu ili Sve opštine"), c("Sve opštine",
                                                                  
                                                                  sort(unique(tabelaopstinee$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)))),
               
               actionBttn(inputId = "go3", label = "Pretraži", icon("search"), style = "jelly", size = "sm", color = "danger"),
               
               helpText("Klikni na Pretraži da biste videli tabele i grafikone"),
               
               tableOutput("top4opstine"),
               
               plotlyOutput("barchartopstinepodnosioci"),
               
               plotlyOutput("chartopstinetemeilinechart")
               
               
             ), 
             
             mainPanel ( dataTableOutput("tabelaopstina"))),
    
    #Uredjivanje taba Podaci
    
    tabPanel("Podaci",
             
             mainPanel(
               
               h4("Ovde možete da skinete podatke prikupljene tokom trajanja ovog projekta u csv ili excel formatu.", align = "justify"),
               
               # ubacivanje tastera za skidanje podataka u csv i excel formatu
               
               downloadBttn (outputId = "downloaddatabase", label = "Preuzmi.CSV", style = "gradient", size = "sm"), 
               
               downloadBttn ("downloaddatabase1", "Preuzmi.XLSX", style = "gradient", size = "sm"),
               
               h4("Ovde možete da skinete metodologiju sa detaljima kako su prikupljeni podaci i publikaciju na srpskom i engleskom jeziku iz decembra 2020. godine." , align = "justify"),
               
               # ubacivanje tastera za skidanje metodologije i analize
               
               downloadBttn ("downloaddatadict", "Metodologija", style = "gradient", size = "sm", color = "royal"),
               
               downloadBttn ("downloadanalysis", "Publikacija 2020", style = "gradient", size = "sm", color = "royal"),
               
               downloadBttn ("downloadanalysiseng", "Publication 2020", style = "gradient", size = "sm", color = "royal"),
               
               h4(strong("Nova online verzija publikacije"), " biće uskoro dostupna.",#a(" ovde.", href = "https://projektnosufinansiranjehtmlpublikacija.netlify.app/"), 
                        " Sva rešenja koja su dobijena od lokalnih samouprava
                        
                        možete da pogledate na sledećem ", a("linku.", href = "https://docs.google.com/spreadsheets/d/1ajgnqWStLHUQ8XUA1LU0n5_1KdqcAIef1A4e4391ORI/edit#gid=0"), align = "justify"), 
               
               h4( " Kod za kreiranje same aplikacije nalazi se na sledećoj ",
                   
                   a("veb stranici.", href = "https://github.com/Centarzaodrzivezajednice/Projektno-sufinansiranje-medija"), "Sam kod i podaci mogu da se preuzmu 
                           
                           pod licencom", a("Creative Commons Zero v1.0 Universal.", href="https://creativecommons.org/publicdomain/zero/1.0/", align = "justify"), 
                   
                   " Iako je ovo najotvorenija moguća licenca, zaista bismo voleli da, ako budete koristili podatke ili kod za veb aplikaciju da nas o tome obavestite
                        
                        kako bi mogli da pratimo njenu dalju upotrebu i razvoj i za to vam se unapred zahvaljujemo. ", align = "justify"), 
               
               h4("Zbog kompleksnosti istraživanja i prikupljanja velike količine podataka, ukoliko imate neke ispravke
                                  
                                     ili sugestije pišite na sledeću", a("email adresu.", href = "mailto:medijskikonkursi@gmail.com"), align = "justify"))
    )
  )

#Rad servera i kreiranje tabela i grafikona

server <- function(input, output, session){
  
  
  ##OPSTI PREGLED TAB
  
  # kreiranje tabele dodeljenih sredstava po godinama u levom uglu 
  
  output$tabelaukupno <- renderTable({yearstable})
  
  
  # kreiranje barcharta za sve institucije koje dodeljuju sredstva u levom uglu
  
  output$barchartdonor <- renderPlotly ({
    
    #pravljenje grafikona
    
    g1<- ggplot (data = barchartorgani, aes (x = reorder(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,-`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`,label = `Info`))+
      
      geom_bar (stat = "identity",fill = c("#9F2A63FF","#F57D15FF","#FAC127FF"))+
      
      xlab ("Organ koji je raspisao konkurs")+
      
      scale_y_continuous(name="", labels = comma)+
      
      ggtitle ("Ukupno dodeljena sredstva\n od 2015-2021. godine")+
      
      theme (legend.position = "none",
             
             panel.background = element_rect (fill = "transparent"),
             
             plot.background = element_rect (fill = "transparent", color = NA),
             
             plot.title = element_text ( hjust = 0.5),
             
             axis.text.x = element_blank(),
             
             axis.ticks=element_blank(),
             
             plot.margin=unit(c(1,1,1.5,1.2),"cm"))
    
    ggplotly(g1, tooltip = "Info") %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(annotations = 
               list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima", 
                    
                    showarrow = F, xref = 'paper', yref = 'paper', 
                    
                    xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
                    
                    font = list(size = 9)))}) 
  
  #Linechart za teme u desnom delu taba
  
  output$linechartteme <- renderPlotly ({
    
    #kreinranje linecharta
    
    gg <- ggplot (linechartteme, aes(GODINA, `n`, group = `TEMA PROJEKTA`, label = Info, color =`TEMA PROJEKTA`)) +
      
      geom_line(position = position_dodge(0.1))+
      
      geom_point(position = position_dodge(0.1))+
      
      
      ggtitle("Zastupljenost tema po godinama\n na osnovu broja projekata")+
      
      scale_color_viridis(discrete = TRUE) +
      
      ylab("Broj projekata")+
      
      xlab("")+
      
      theme(legend.position = "bottom",
            
            legend.title = element_blank(),
            
            panel.background = element_rect (fill = "#f5f1ef"),
            
            plot.title = element_text(hjust = 0.5),
            
            axis.ticks.x = element_blank(),
            
            plot.margin=unit(c(1,1,1.5,1.2),"cm"),
            
            plot.caption = element_text()
      )
    
    ggplotly(gg, tooltip = "label") %>%
      
      config(displayModeBar=FALSE) %>%
      
      layout(legend = list(title = list(text = 'Teme projekata'), orientation = "h", x = 0, y =-0.3), annotations = 
               
               list(x = 1, y = -0.31, text = "*Klikom na teme u legendi ispod određujete koja će\n da se pojavi na grafikonu. Dupli klik resetuje grafikon.", 
                    
                    showarrow = F, xref='paper', yref='paper', 
                    
                    xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=10)))}) 
  
  
  
  ##PODNOSIOCI TAB
  
  # Azuriranje sa server strane podnosilaca
 
 updateSelectizeInput(session, "podnosilac",
                     choices = c("SVI PODNOSIOCI",
                                   sort(unique(tabelapodnosioci$`PODNOSILAC PROJEKTA`)))
                      , server = TRUE)

  #Kreiranje velike pretrazive tabele uz uslov sta je izabrano u dropdown meniju
  
  
  tabelapodnosiocireact <- eventReactive (input$go, {if (input$podnosilac != "SVI PODNOSIOCI") { 
    
    selected<-reactive({ tabelapodnosioci <- subset(tabelapodnosioci, `PODNOSILAC PROJEKTA` == input$podnosilac)
    
    tabelapodnosioci<-tabelapodnosioci %>%
      
      select(-`PODNOSILAC PROJEKTA`)
    
    })
    
    selected() 
    
    
    
    
  }else { tabelapodnosioci
  }   
  })
  
  output$tabelapodnosioci <- renderDataTable({
    
    tabelapodnosiocireact()
    
    
    
  }, options = list(language = list(sSearch="Pretraži celu tabelu:", sLengthMenu="Prikaži _MENU_ unosa", info="Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa",paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  #Kreiranje male tabele uz uslov sta je izabrano iz padajuceg menija
  
  totalpodnosiocireactive<- eventReactive (input$go, {if (input$podnosilac != "SVI PODNOSIOCI") {
    
    podnosilacitotal <- podnosiocitotal %>%
      
      filter(`PODNOSILAC PROJEKTA` %in% input$podnosilac)%>%
      
      select(-`PODNOSILAC PROJEKTA`)
  }})
  
  
  output$tabelaukupnopodnosioci<-renderTable(totalpodnosiocireactive(),align = "c")
  
  
  #grafikoni tab podnosioci bar chart i line chart uz uslov sta je izabrano iz padajuceg menija
  
  podnosiocichartreact <- eventReactive (input$go, { if(input$podnosilac != "SVI PODNOSIOCI")
    
    
  { 
    selected <- reactive({subset(yearssredstvapodnosioci, `PODNOSILAC PROJEKTA` == input$podnosilac)})
    
    
    #Kreiranje linechart za pojedinacne podnosioce
    
    lci <- ggplot (data = selected(), aes (x = as.factor(GODINA), y = `SREDSTVA U EVRIMA`, label = `Info`))+
      
      geom_line (aes(group = 1), colour = "#E95420")+
      
      geom_point(color = "#E95420")+
      
      xlab ("")+
      
      ylab ("")+
      
      ggtitle (paste("Ukupno dobijena sredstva\n za izabranog podnosioca \n po godinama"))+
      
      scale_y_continuous(name="", labels = comma)+
      
      theme (legend.position = "none",
             
             panel.background = element_rect (fill = "transparent"),
             
             plot.background = element_rect (fill = "transparent", color = NA),
             
             plot.title = element_text ( hjust = 0.5),
             
             axis.ticks=element_blank(),
             
             axis.text.x = element_text(angle = 30),
             
             panel.grid.minor = element_line(),
             
             plot.margin=unit(c(2,1.5,1.5,1.2),"cm"))
    
    
    ggplotly(lci, tooltip = "Info") %>%
      
      config(displayModeBar = FALSE) %>%layout(annotations = 
                                                 
                                                 list(x = 1.2, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima", 
                                                      
                                                      showarrow = F, xref = 'paper', yref = 'paper', 
                                                      
                                                      xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
    
  }else{ 
    #Kreiranje barchart za top 4 podnosioca
    
    bci <-  ggplot (data = barcharttop4podnosioci, aes (x = reorder(`PODNOSILAC PROJEKTA`,-`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`,label = `Info`))+
      
      geom_bar (stat = "identity",fill = "#E95420")+
      
      xlab ("Podnosilac projekta")+
      
      scale_y_continuous(name="", labels = comma)+
      
      ggtitle ("Četiri podnosioca koji\n su dobili najviše sredstava\n od svih organa")+
      
      theme (legend.position = "none",
             
             panel.background = element_rect (fill = "transparent"),
             
             plot.background = element_rect (fill = "transparent", color = NA),
             
             plot.title = element_text ( hjust = 0.5),
             
             axis.text.x = element_blank(),
             
             axis.ticks=element_blank(),
             
             plot.margin=unit(c(1.5,1,1.5,1.2),"cm"))
    
    ggplotly( bci, tooltip = "Info") %>%
      
      config(displayModeBar=FALSE) %>%
      
      layout(annotations = 
               list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima", 
                    
                    showarrow = F, xref = 'paper', yref = 'paper', 
                    
                    xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
  }})
  
  
  output$podnosiocilinebar <- renderPlotly ({podnosiocichartreact ()}) 
  
  
  
  
  #piechart teme za pojedinacnog podnosioca uz uslov sta je izabrano iz padajuceg menija
  
  podnosiocipiechartreact <- eventReactive (input$go, { if(input$podnosilac != "SVI PODNOSIOCI")
    
    
  {
    piecharttemepodnosiocimain <- piecharttemepodnosiocimain  %>%
      
      filter(`PODNOSILAC PROJEKTA` %in% input$podnosilac) 
    
    #kreiranje pie chart za pojedinacne podnosioce
    
    pcpoj<- piecharttemepodnosiocimain %>%
      
      plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo="label", marker = list(colors =piecharttemepodnosiocimain$boje), textinfo = "none") %>%
      
      add_pie() %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(showlegend = F,
             
             title = list(text =  "Prikaz učešća dobijenih sredstava\n po temama za izabranog podnosioca"),
             
             margin = list(t = 100),
             
             paper_bgcolor = 'rgba(0,0,0,0)',
             
             plot_bgcolor = 'rgba(0,0,0,0)',
             
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
    
    pcpoj%>%layout(margin = list(b = 40))
    
    
    
  } else{
    
    #kreiranje piechart za sve podnosioce 
    
    pcpsvi<-  piecharttemepodnosall%>%
      
      plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label", marker = list(colors = piecharttemepodnosall$boje), textinfo = "none") %>%
      
      add_pie() %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(showlegend = F,
             
             title = list(text =  "Prikaz učešća dodeljenih sredstava\n po temama za\n sve podnosioce"),
             
             margin = list(t = 100),
             
             paper_bgcolor = 'rgba(0,0,0,0)',
             
             plot_bgcolor = 'rgba(0,0,0,0)',
             
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    pcpsvi%>%layout(margin = list(b = 40)) }
    
  })
  
  output$chartteme<- renderPlotly ({podnosiocipiechartreact ()}) 
  
  
  #Tabela medija za datog podnosioca u APR-u
  
  tobelamedijiaprreactive<- eventReactive (input$go, {if (input$podnosilac != "SVI PODNOSIOCI") {
    
    aprregistrovanimedijifin <- aprregistrovanimedijifin %>%
      
      filter(`PODNOSILAC PROJEKTA` %in% input$podnosilac)%>%
      
      select(-`PODNOSILAC PROJEKTA`)
  }})
  
  
  output$registrovanimedijiapr<- 
  
  renderDataTable({
    
    tobelamedijiaprreactive()
    
    
    
  },options = list(pageLength = 5,lengthMenu =c(3,5,10),  searching=FALSE, info=0, language = list(sSearch="Pretraži celu tabelu:",sLengthMenu="Prikaži _MENU_ unosa",zeroRecords ='Za podnosioca nije nadjen registrovan medij u APR-u', infoEmpty="Nema unosa", info="Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa",paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  #MINISTARSTVO KULTURE TAB
  
  # Kreiranje velike pretrazive tabele za ministarstvo uz uslov sta je izabrano iz padajuceg menija
  
  
  tabelakulturareact <- eventReactive (input$go1, {if (input$temakultura != "Sve teme") { 
    
    tabelakultura <- tabelakultura %>%
      
      filter(`TEMA PROJEKTA`%in% input$temakultura) %>%
      
      select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
    
    tabelakultura
    
  }else {tabelakultura %>% select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
    
  }   
  })
  
  output$tabelaministarstvo <- renderDataTable({
    
    tabelakulturareact()
    
    
    
  }, options = list(language = list(sSearch = "Pretraži celu tabelu:", sLengthMenu = "Prikaži _MENU_ unosa", info = "Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa",paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  
  # Kreiranje piechart teme za ministarstvo i  koliko je para  dato pojedinacno po temi po godinama uz uslov
  #sta je izabrano iz padajuceg menija
  
  temekultchartreact <- eventReactive (input$go1, { if(input$temakultura != "Sve teme")
    
    
  { linecharttemekult <- linecharttemekult %>%
    
    filter(`TEMA PROJEKTA`%in% input$temakultura)
  
  #kreiranje linecharta za teme
  
  
  lctk <- ggplot (data = linecharttemekult, aes (x = as.factor(GODINA), y = `SREDSTVA U EVRIMA`,label = `Info`))+
    
    geom_line (aes(group = 1), color = linecharttemekult$boje)+
    
    geom_point(color = linecharttemekult$boje)+
    
    xlab ("")+
    
    ylab ("")+
    
    ggtitle (paste("Ukupno dodeljena sredstva\n za izabranu temu\n po godinama"))+
    
    scale_y_continuous(name="", labels = comma)+
    
    theme (legend.position = "none",
           
           panel.background = element_rect (fill = "transparent"),
           
           plot.background = element_rect (fill = "transparent", color = NA),
           
           plot.title = element_text ( hjust = 0.5),
           
           axis.ticks = element_blank(),
           
           axis.text.x = element_text(angle = 30),
           
           panel.grid.minor = element_line(),
           
           plot.margin = unit(c(1.5,1,1.5,1.2),"cm"))
  
  
  ggplotly(lctk, tooltip = "Info") %>%
    
    config(displayModeBar = FALSE) %>% layout(annotations = 
                                                list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                                                     
                                                     showarrow = F, xref = 'paper', yref = 'paper', 
                                                     
                                                     xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
  
  }else{ 
    
    #kreiranje piechart za sve teme
    
    pctkul<- piecharttemekult%>%
      
      plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label",marker = list(colors = piecharttemekult$boje), textinfo = 'none') %>%
      
      add_pie() %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(showlegend = F,
             
             title = list(text = paste( "Prikaz učešća dodeljenih sredstava\n po temama za Ministarstvo kulture")),
             
             margin = list(t = 100, b = 20),
             
             paper_bgcolor = 'rgba(0,0,0,0)',
             
             plot_bgcolor = 'rgba(0,0,0,0)',
             
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
    
    pctkul%>%layout(margin = list(b = 40))
    
  }})
  
  
  output$chartministarstvoteme <- renderPlotly ({temekultchartreact()}) 
  
  #bar chart top 4 podnosioca kod ministarstva kulture
  
  
  output$barchartministarstvopodnosioci <- renderPlotly( { bcik <-  ggplot (data = barcharttop4podnosiocikult, aes (x = reorder(`PODNOSILAC PROJEKTA`,-`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`, label = `Info`))+
    
    geom_bar (stat = "identity", fill = "#E95420") +
    
    xlab ("Podnosilac projekta") +
    
    scale_y_continuous(name="", labels = comma)+
    
    ggtitle ("Četiri podnosioca koji\n su dobili najviše sredstava\n od Ministarstva kulture") +
    
    theme (legend.position = "none",
           
           panel.background = element_rect (fill = "transparent"),
           
           plot.background = element_rect (fill = "transparent", color = NA),
           
           plot.title = element_text ( hjust = 0.5),
           
           axis.text.x = element_blank(),
           
           axis.ticks=element_blank(),
           
           plot.margin=unit(c(1.5,1,1.5,1.2),"cm"))
  
  
  ggplotly( bcik, tooltip = "Info") %>%
    
    config(displayModeBar=FALSE) %>%
    
    layout(annotations = 
             
             list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.",
                  
                  showarrow = F, xref = 'paper', yref = 'paper', 
                  
                  xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))})
  
  
  ##POKRAJINSKI TAB
  
  #Kreiranje pretrazive tabela uz uslov sta je izabrano iz padajuceg menija 
  
  tabelapoksekreact <- eventReactive (input$go2, {if (input$temapoksek != "Sve teme") { 
    
    tabelapoksek <- tabelapoksek %>%
      
      filter(`TEMA PROJEKTA` %in% input$temapoksek) %>%
      
      select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
    
    
    tabelapoksek
    
    
    
    
  }else {tabelapoksek %>% select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
  }   
  })
  
  output$tabelapoksek <- renderDataTable({
    
    tabelapoksekreact()
    
    
    
  }, options = list(language = list(sSearch = "Pretraži celu tabelu:", sLengthMenu = "Prikaži _MENU_ unosa", info = "Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa", paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  
  # chart top cetiri teme poksek i  koliko je para  dato pojedinacno po temi uz uslov sta je izabrano iz  padajuceg menija
  
  temepoksekchartreact <- eventReactive (input$go2, { if(input$temapoksek != "Sve teme")
    
    
  { linecharttemepoksek<- linecharttemepoksek %>%
    
    filter(`TEMA PROJEKTA` %in% input$temapoksek)
  
  
  #kreiranje line charta za pokrajinski sekretarijat za teme
  
  lptk<- ggplot (data = linecharttemepoksek, aes (x = as.factor(GODINA), y = `SREDSTVA U EVRIMA`,label = `Info`))+
    
    geom_line (aes(group = 1), color = linecharttemepoksek$boje)+
    
    geom_point(color = linecharttemepoksek$boje)+
    
    xlab ("")+
    
    ylab ("")+
    
    ggtitle (paste("Ukupno dodeljena sredstva\n za izabranu temu\n po godinama"))+
    
    scale_y_continuous(name="", labels = comma)+
    
    theme (legend.position = "none",
           
           panel.background = element_rect (fill = "transparent"),
           
           plot.background = element_rect (fill = "transparent", color = NA),
           
           plot.title = element_text ( hjust = 0.5),
           
           axis.ticks = element_blank(),
           
           axis.text.x = element_text(angle = 30),
           
           panel.grid.minor = element_line(),
           
           plot.margin = unit(c(1.8,0.7,1.5,1.2),"cm"))
  
  
  ggplotly(lptk, tooltip = "Info") %>%
    
    config(displayModeBar=FALSE) %>% layout(annotations = 
                                              
                                              list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                                                   
                                                   showarrow = F, xref = 'paper', yref = 'paper', 
                                                   
                                                   xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
  
  }else{ 
    
    
    #Kreiranje pie chart za teme za pokrajinski sekretarijat
    
    pctpoksek<-  piecharttemepoksek%>%
      
      plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label",marker = list(colors = piecharttemepoksek$boje), textinfo = "none") %>%
      
      add_pie() %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(showlegend = F,
             
             title = list(text =  "Prikaz učešća dodeljenih sredstava\n po temama za\n Pokrajinski sekretarijat"),
             margin = list(t = 100),
             
             paper_bgcolor = 'rgba(0,0,0,0)',
             
             plot_bgcolor = 'rgba(0,0,0,0)',
             
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    pctpoksek%>%layout(margin = list(b = 40))
    
    
    
  }})
  
  
  output$chartpoksekteme <- renderPlotly ({temepoksekchartreact()})
  
  #bar chart top 4 podnosioca kod pokrajinskog sekretarijata
  
  
  output$barchartpoksekpodnosioci<-renderPlotly({ 
    
    bcip<-  ggplot (data = barcharttop4podnosiocipoksek, aes (x = reorder(`PODNOSILAC PROJEKTA`,-`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`,label = `Info`))+
      
      geom_bar (stat = "identity",fill = "#E95420")+
      
      xlab ("Podnosilac projekta")+
      
      scale_y_continuous(name="", labels = comma)+
      
      ggtitle ("Četiri podnosioca koji su\n dobili najviše sredstava od\n Pokrajinskog sekretarijata")+
      
      theme (legend.position = "none",
             
             panel.background = element_rect (fill = "transparent"),
             
             plot.background = element_rect (fill = "transparent", color = NA),
             
             plot.title = element_text ( hjust = 0.5),
             
             axis.text.x = element_blank(),
             
             axis.ticks=element_blank(),
             
             plot.margin = unit(c(1.8,0.7,1.5,1.2),"cm"))
    
    ggplotly( bcip, tooltip = "Info") %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(annotations = 
               list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                    
                    showarrow = F, xref = 'paper', yref = 'paper', 
                    
                    xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))})
  
  
  ##LOKALNE SAMOUPRAVE TAB
  
  
  # kreiranje male tabele sa top 4 opstine i ukupnom sumom po opstini uz uslov sta je izabrano iz padajuceg menija
  
  tabela4opstinereact <- eventReactive (input$go3, 
                                        
                                        {if(input$opstina != "Sve opštine") {tabelaukupnopstina <- tabelaukupnopstina %>%
                                          
                                          filter(OPŠTINA %in% input$opstina)%>%
                                          
                                          select(-OPŠTINA)} else {tabela4opstine}})
  
  output$top4opstine <- renderTable(tabela4opstinereact(), align = "c")
  
  #Kreiranje velike pretrazive tabele uz uslov sta je izabrano iz padajuceg menija
  
  
  tabelaopstinereact <- eventReactive (input$go3, {if (input$opstina != "Sve opštine") { 
    
    #kreiranje podskupa za tabelu zavisno od toga koja opstina je izabrana iz padajuceg menija
    tabelaopstinee <- tabelaopstinee %>%
      
      filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` %in% input$opstina)
    
    
    if(isTRUE(sum(tabelaopstinee$`SREDSTVA U EVRIMA`) != 0)){
      
      tabelaopstinee<- tabelaopstinee %>% select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
      
      #Formatiranje kolone sredstva
      
      tabelaopstinee$`SREDSTVA U DINARIMA` <- format( tabelaopstinee$`SREDSTVA U DINARIMA` , big.mark = ',', digits = 0,nsmall = 0,scientific = FALSE)
      
      tabelaopstinee$`SREDSTVA U EVRIMA` <- format( tabelaopstinee$`SREDSTVA U EVRIMA` , big.mark = ',',digits = 0, nsmall = 0, scientific = FALSE)
      
      tabelaopstinee 
      
      #dodavanje jos jednog uslova za jos manje kolona kada sredstva nisu dodeljena nijedne godine
      
    }else{ tabelaopstinee <- tabelaopstinee %>% select(`NAZIV PROJEKTA`,`GODINA`,`SREDSTVA U DINARIMA`)}
    
    tabelaopstinee
    
  }else { 
    
    tabelaopstinee$`SREDSTVA U DINARIMA` <- format( tabelaopstinee$`SREDSTVA U DINARIMA` , big.mark = ',', digits = 0, nsmall = 0, scientific = FALSE)
    
    tabelaopstinee$`SREDSTVA U EVRIMA` <- format( tabelaopstinee$`SREDSTVA U EVRIMA` , big.mark = ',', digits = 0,nsmall = 0, scientific = FALSE)
    
    tabelaopstinee
    
    
    
  }   
  })
  
  output$tabelaopstina <- renderDataTable({
    
    tabelaopstinereact()
    
    
    
  }, options = list(language = list(sSearch = "Pretraži celu tabelu:", sLengthMenu = "Prikaži _MENU_ unosa", info = "Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa", paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  
  #pie  chart teme i line chart  godine lokalne samouprave uz uslov sta je izabrano iz padajuceg menija
  
  
  loksamchartreact <- eventReactive (input$go3, { if(input$opstina != "Sve opštine")
    
    
  {
    piecharttemeloksampoj <- piecharttemeloksampoj %>%
      
      filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` %in% input$opstina) 
    
    
    if(sum(piecharttemeloksampoj$`SREDSTVA U EVRIMA`!=0)){
      #kreiranje piechart za teme za pojedinacne samouprave i pod uslovom da su u opstini dodeljena sredstva
      
      pcloksam<- piecharttemeloksampoj%>%
        
        plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label", marker = list(colors = piecharttemeloksampoj$boje), textinfo = 'none') %>%
        
        add_pie() %>%
        
        config(displayModeBar = FALSE) %>%
        
        layout(showlegend = F,
               
               title = list(text = paste( "Prikaz učešća dodeljenih sredstava\n po temama za\n opštinu", input$opstina,"\n")),
               
               margin = list(t = 100, b = 20),
               
               paper_bgcolor = 'rgba(0,0,0,0)',
               
               plot_bgcolor = 'rgba(0,0,0,0)',
               
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
      
      pcloksam%>%layout(margin = list(b = 40))
      
      
    }}else{ 
      #kreiranje pie chart za sve lokalne samouprave
      
      pctsvesam<- piecharttemeloksam%>%
        
        plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label", marker = list(colors = piecharttemeloksam$boje), textinfo = "none") %>%
        
        add_pie() %>%
        
        config(displayModeBar = FALSE) %>%
        
        layout(showlegend = F,
               
               title = list(text = "Prikaz učešća dobijenih sredstava\n po temama za\n sve lokalne samouprave"),
               
               margin = list(t = 100),
               
               paper_bgcolor = 'rgba(0,0,0,0)',
               
               plot_bgcolor = 'rgba(0,0,0,0)',
               
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
      
      pctsvesam%>%layout(margin = list(b = 40))
      
    }})
  
  
  output$chartopstinetemeilinechart <- renderPlotly ({loksamchartreact()}) 
  
  #bar chart top 4 podnosioca kod lokalnih samouprava i line chart sredstava po godinama za opstinu uz uslov
  #sta je izabrano iz padajuceg menija
  
  loksamchartizdtemreact <- eventReactive (input$go3, { if(input$opstina != "Sve opštine")
    
    
  {
    
    yearssredstvaloksam <- yearssredstvaloksam %>%
      
      filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` %in% input$opstina) 
    
    if(sum(yearssredstvaloksam$`SREDSTVA U EVRIMA`!=0)){
      
      # Uz uslov da su dodeljena sredstva kreiranje linechart grafikona za dodeljena sredstva po godinama 
      
      lcls <- ggplot (data = yearssredstvaloksam , aes (x = as.factor(GODINA), y = `SREDSTVA U EVRIMA`, label = `Info`))+
        
        geom_line (aes(group = 1), colour = "#E95420")+
        
        geom_point(color = "#E95420")+
        
        xlab ("")+
        
        ylab ("")+
        
        ggtitle (paste("Ukupno dodeljena sredstva\n za izabranu opštinu \n po godinama"))+
        
        scale_y_continuous(name="", labels = comma)+
        
        theme (legend.position = "none",
               
               panel.background = element_rect (fill = "transparent"),
               
               plot.background = element_rect (fill = "transparent", color = NA),
               
               plot.title = element_text ( hjust = 0.5),
               
               axis.ticks=element_blank(),
               
               axis.text.x = element_text(angle = 30),
               
               panel.grid.minor = element_line(),
               
               plot.margin=unit(c(2,1.5,1.5,1.2),"cm"))
      
      
      ggplotly(lcls, tooltip = "Info") %>%
        
        config(displayModeBar = FALSE) %>%layout(annotations = 
                                                   
                                                   list(x = 1.2, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                                                        
                                                        showarrow = F, xref = 'paper', yref = 'paper', 
                                                        
                                                        xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
    }} 
    else{
      
      #kreiranje barchart za top4 podnosioca za sve lokalne samouprave
      
      bcils<-  ggplot (data = barcharttop4podnosiociloksam, aes (x = reorder(`PODNOSILAC PROJEKTA`, -`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`, label = `Info`))+
        
        geom_bar (stat = "identity",fill = "#E95420")+
        
        xlab ("Podnosilac projekta")+
        
        scale_y_continuous(name="", labels = comma)+
        
        ggtitle ("Četiri podnosioca koji su\n dobili najviše sredstava od\n svih lokalnih samouprava")+
        
        theme (legend.position = "none",
               
               panel.background = element_rect (fill = "transparent"),
               
               plot.background = element_rect (fill = "transparent", color = NA),
               
               plot.title = element_text ( hjust = 0.5),
               
               axis.text.x = element_blank(),
               
               axis.ticks=element_blank(),
               
               plot.margin=unit(c(1.5,1,1.5,1.2),"cm"))
      
      ggplotly( bcils, tooltip = "Info") %>%
        
        config(displayModeBar = FALSE) %>%
        
        layout(annotations = 
                 list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                      
                      showarrow = F, xref = 'paper', yref = 'paper', 
                      
                      xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
    }})
  
  
  
  
  output$barchartopstinepodnosioci<-renderPlotly({loksamchartizdtemreact () })
  
  
  
  ##PODACI TAB
  
  # kako se odabere excel ili csv dugme kreira se fajl u tom formatu
  
  output$downloaddatabase <- downloadHandler (
    
    filename = function(){paste("Projektno sufinansiranje.csv",sep = "")},
    
    content = function(file) {
      
      write.csv(Projectmedia, file)}
  )
  
  output$downloaddatabase1 <- downloadHandler(
    
    filename = function(){paste("Projektno sufinansiranje.xlsx", sep = "")},
    
    content = function(file) {
      
      write_xlsx(Projectmedia, path = file)})
  
  # Ukoliko se klikne na metodologiju ili analizu skida se pdf fajl koji se nalazi u folderu www
  
  output$downloaddatadict <- downloadHandler (
    
    filename = "Metodologija.pdf",
    
    content = function(file) {
      
      file.copy("www/metodologijafinal.pdf", file)
      
      
    }
  )
  
  
  output$downloadanalysis <- downloadHandler (
    
    filename = "Publikacija.pdf",
    
    content = function(file) {
      
      file.copy("www/publikacija.pdf", file)
      
      
      
      
    }
  )   
  
  
  output$downloadanalysiseng <- downloadHandler (
    
    filename = "PublicationEnglish.pdf",
    
    content = function(file) {
      
      file.copy("www/publication.pdf", file)
      
      
      
      
    }
  ) 
  
}

shinyApp(ui, server)
