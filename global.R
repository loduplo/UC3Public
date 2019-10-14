## global.R ##
library("dplyr")
library("lubridate")

#setwd("F:/SHINY-PRODUCTION/UC3-Sprint2")
####################################################################################################
# global.R
####################################################################################################
# DONNEES de production des territoires
source("data.R")

#traitement des dates pour consolider a la journee
mesData$Date <- lubridate::ymd_hms(mesData$horodate)
mesData$DateJour <- lubridate::date(mesData$Date)
mesData$DateHeure <- lubridate::floor_date(mesData$Date, unit="hour")
mesData$Month <- lubridate::floor_date(mesData$DateJour,unit="month")
mesData$Mois <- lubridate::month(mesData$DateJour)
mesData$MoisFactor <- as.factor(mesData$Mois)
mesData$HH <- lubridate::hour(mesData$Date)
mesData$HHFactor <- as.factor(mesData$HH)
#pour avoir par demi heure 0 - 0.5 - 1 - 1,5 - 2 - 2.5 etc...jusqu'à 23.5
mesData$MM <- lubridate::minute(mesData$Date)
mesData<- dplyr::mutate(mesData,HHMM=ifelse(MM==30,HH+0.5,HH))

mesData$Year <- lubridate::year(mesData$DateJour)
mesData <- dplyr::rename(mesData,production=prod)
mesData <- dplyr::rename(mesData,consommation=conso)
mesData <- dplyr::mutate(mesData,prodSurConsoCreneau=round(100*(production/consommation),2))
mesData <- mutate(mesData,prodSurConsoCreneau = ifelse(consommation<0,NA,prodSurConsoCreneau))
# pour grouper les données par semaine
mesData$week <- lubridate::week(mesData$Date)
mesData$week <- paste(mesData$Year,week(mesData$Date),sep="-")
# mois
mesData$Jour <- lubridate::day(mesData$Date)
mesData$mois <- as.factor(mesData$Mois)
levels(mesData$mois)<-c("Janvier","Fevrier","Mars","Avril","Mai","Juin",
                            "Juillet","Aout","Septembre","Octobre","Novembre","Decembre")
# #PB de quelques creneaux en mai et juin  => supprimes
# # consommations negatives
# mesData <- mutate(mesData,production = ifelse(consommation<0,0,production))
# mesData <- mutate(mesData,consommation = ifelse(consommation<0,0,consommation))

#nombre de creneaux ou la production est superieure a la consommation
mesData <- dplyr::mutate(mesData,creneauSup=ifelse(production >= consommation,1,0))
mesData <- dplyr::mutate(mesData,prodSup=ifelse(production >= consommation,"superieur","inferieur"))
mesData$prodSup <- factor(mesData$prodSup, levels=c("superieur","inferieur"))

mesData <- dplyr::mutate(mesData,trim=as.factor(
            ifelse(Month=="2017-01-01"|Month=="2017-02-01"|Month=="2017-03-01","Hiver2017",
            ifelse(Month=="2017-04-01"|Month=="2017-05-01"|Month=="2017-06-01","Printemps2017",
            ifelse(Month=="2017-07-01"|Month=="2017-08-01"|Month=="2017-09-01","Ete2017",
            ifelse(Month=="2017-10-01"|Month=="2017-11-01"|Month=="2017-12-01","Automne2017",
            ifelse(Month=="2018-01-01"|Month=="2018-02-01"|Month=="2018-03-01","Hiver2018",
            ifelse(Month=="2018-04-01"|Month=="2018-05-01"|Month=="2018-06-01","Printemps2018",
            ifelse(Month=="2018-07-01"|Month=="2018-08-01"|Month=="2018-09-01","Ete2018",
            ifelse(Month=="2018-10-01"|Month=="2018-11-01"|Month=="2018-12-01","Automne2018",
            ifelse(Month=="2019-01-01"|Month=="2019-02-01"|Month=="2019-03-01","Hiver2019",
            ifelse(Month=="2019-04-01"|Month=="2019-05-01"|Month=="2019-06-01","Printemps2019",
            ifelse(Month=="2019-07-01"|Month=="2019-08-01"|Month=="2019-09-01","Ete2019","other")))))))))))))# ajout de degres supplémentaires
mesData <- dplyr::mutate(mesData,prodSup=ifelse(production >= 2*consommation,"sup2x",
                                        ifelse(production >= consommation,"superieur",
                                        ifelse(production <= consommation/2,"inf2x",
                                        ifelse(production <= consommation,"inferieur","other")))))
mesData$prodSup <- factor(mesData$prodSup, levels=c("sup2x","superieur","inferieur","inf2x"))
#allPerHeure <- dplyr::mutate(allPerHeure,creneauSup=ifelse(production >= consommation,1,0))

##################################################################################################################################################
#consolider par heure
allPerHeure <- mesData %>% group_by(DateHeure) %>% summarise(consommation=round(sum(consommation)/1000,2),
                                                     production=round(sum(production)/1000,2),
                                                     Date=unique(DateJour),
                                                     HH=unique(HH),
                                                     HHFactor=unique(HHFactor),
                                                     creneauSup=sum(creneauSup),
                                                     tauxAuto=round((sum(creneauSup)/n())*100,2),
                                                     prodSurConsoCreneau=mean(prodSurConsoCreneau,na.rm=T),
                                                     Mois=unique(Mois),
                                                     MoisFactor=unique(MoisFactor),
                                                     Month=unique(Month),
                                                     Year=unique(Year))
allPerHeure <- dplyr::mutate(allPerHeure,prodSurConso=round(100*(production/consommation),2))
allPerHeure$Jour <- lubridate::day(allPerHeure$Date)
allPerHeure$mois <- as.factor(allPerHeure$Mois)
levels(allPerHeure$mois)
levels(allPerHeure$mois)<-c("Janvier","Fevrier","Mars","Avril","Mai","Juin",
                            "Juillet","Aout","Septembre","Octobre","Novembre","Decembre")

#Ajouter une variable avec saison octobre-mars/ saison avril-septembre
allPerHeure <- mutate(allPerHeure,saison=as.factor(ifelse((Mois<=3|Mois>=10),"octobre-mars","avril-septembre")))

# VISU
data2019 <- filter(allPerHeure,Year==2019)
data2018 <- filter(allPerHeure,Year==2018)
data2017 <- filter(allPerHeure,Year==2017)


# consolidation par mois
allPerMois <- mesData %>% group_by(Month) %>% summarise(consommation=round(sum(consommation)/1000,2),
                                                             production=round(sum(production)/1000,2),
                                                             Date=min(DateJour),
                                                        creneauSup=sum(creneauSup),
                                                        tauxAuto=round((sum(creneauSup)/n())*100,2),
                                                        prodSurConsoCreneau=mean(prodSurConsoCreneau,na.rm=T),
                                                             Mois=unique(Mois),
                                                             MoisFactor=unique(MoisFactor),
                                                             Year=unique(Year))
#prodSurConsommation est un %
allPerMois <- dplyr::mutate(allPerMois,prodSurConso=round(100*(production/consommation),2))

# consolidation par jour
allPerJour <- mesData %>% group_by(DateJour) %>% summarise(consommation=round(sum(consommation)/1000,2),
                                                            production=round(sum(production)/1000,2),
                                                            creneauSup=sum(creneauSup),
                                                            tauxAuto=round((sum(creneauSup)/n())*100,2),
                                                           prodSurConsoCreneau=mean(prodSurConsoCreneau,na.rm=T),
                                                            Date=unique(DateJour),
                                                            Mois=unique(Mois),
                                                            Month=unique(Month),
                                                            Year=unique(Year))
#prodSurConsommation est un %
allPerJour <- dplyr::mutate(allPerJour,prodSurConso=round(100*(production/consommation),2))


#Pour la prediction : consolider par semaine
allPerSemaine <- mesData %>% group_by(week) %>% summarise(consommation=round(sum(consommation)/1000,2),
                                                           production=round(sum(production)/1000,2),
                                                          creneauSup=sum(creneauSup),
                                                          tauxAuto=round((sum(creneauSup)/n())*100,2),#48*7jours
                                                          prodSurConsoCreneau=mean(prodSurConsoCreneau,na.rm=T),
                                                           Date=min(DateJour),
                                                           Year=unique(Year))
#prodSurConsommation est un %
allPerSemaine <- dplyr::mutate(allPerSemaine,prodSurConso=round(100*(production/consommation),2))

