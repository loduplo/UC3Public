## app.R ##
# 15 octobre 2019
library("shiny")
library("shinydashboard")
library("shinyTime")
library("dplyr")
library("ggplot2")
library("lubridate")
library("prophet")
library("dygraphs")
library("forecast")
library("gridExtra")
library("zoo")
library("caret")
library("gplots")
library("FactoMineR")
library("psy")
library("corrplot")
####################################################################################################
# DONNEES de production des territoires
# Provence Alpes Agglomeration ici
source("global.R")
####################################################################################################
# UI
####################################################################################################
ui <- dashboardPage(
  dashboardHeader(title = paste("UC3 :",territoireCourt)),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse des donnees", tabName = "analyse", icon = icon("dashboard")),
      menuItem("Production et Consommation", tabName = "visu", icon = icon("plug")),
      menuItem("Calendrier", tabName = "cal", icon = icon("calendar")),
      menuItem("Prediction", tabName = "prediction", icon = icon("cog"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content : ANALYSE DES DONNEES
      tabItem(tabName = "analyse",
            fluidRow(
              #Analyse des donnees
              box(background = "light-blue", width=12,
                  fluidRow(
                  column(4,h3(territoire)),
                  column(4,selectInput(inputId = "production",
                          label = h4("Choisissez :"),
                          choice = c("production","consommation","prodSurConso"))
                         ),
                  column(4,selectInput(inputId = "annee",
                              label = h4("et l'annee"),
                              choice = c("2019","2018","2017"))
                  ),
                  column(4,h4(textOutput("param")))
                  )
              ),
              box(title=paste("Consolidation mensuelle : 2017-08/2019"),status="primary",solidHeader=TRUE,width=12,
                  plotOutput("visuSeasonal")
              ),
              box(title="Consolidation jour",status="primary",solidHeader=TRUE,
                  plotOutput("prodjour")
              ),
              box(title="Consolidation mois",status="primary",solidHeader=TRUE,
                  plotOutput("prodmois")
              ),
              box(title="Boxplot horaire par mois",status="primary",solidHeader=TRUE,
                  plotOutput("visuBoxMois")
              ),
              box(title="Visualisation horaire par mois",status="primary",solidHeader=TRUE,
                plotOutput("visuPlotMois")
              ),
              box(title="Boxplot horaire par mois",status="primary",solidHeader=TRUE,width=12,
                  plotOutput("visuBoxJour")
              )
 
          ) # fluidrow #
      ), # tabItem analyse #
      
      # Second tab content : PRODUCTION ET CONSOMMATION
      tabItem(tabName = "visu",
              fluidRow(
                #Analyse des donnees 
                box(background = "light-blue", width=12,h3(territoire)
                ),
                box(title="Production et consommation horaire",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("productions")),
                box(title="Production et consommations quotidiennes",status="primary",solidHeader=TRUE,
                    plotOutput("productionsJour")),
                box(title="Production et consommations mensuelles",status="primary",solidHeader=TRUE,
                    plotOutput("productionsMois")),
                box(title="Production horaire par mois",status="primary",solidHeader=TRUE,width=12,height = 500,
                    plotOutput("prodconsoautoMois")
                ),
                box(title="Production horaire par heure",status="primary",solidHeader=TRUE,width=12,height = 500,
                    plotOutput("prodconsoautoHeure")
                ),
                box(title="Mois octobre-mars / Mois avril-septembre",status="primary",solidHeader=TRUE,width=12,
                    fluidRow(
                      column(12,plotOutput("prodSaison")),
                      column(12,plotOutput("consoSaison")),
                      column(12,plotOutput("autoSaison"))
                    )
                    
                )
              ) #fluidRow
      ), # tabItem visu #
      # 3 tab content : Calendar PRODUCTION ET CONSOMMATION
      tabItem(tabName = "cal",
              fluidRow(
                #Analyse des donnees 
                box(background = "light-blue", width=12,h3(territoire)
                ),
                box(background = "light-blue", width=12,title="Visualisation pour chaque demi-heure : Production > Consommation en vert (ou < gris et blanc)",
                    column(4,radioButtons(inputId="an", label="Annee ?", inline=TRUE,
                                          choices=c("2019","2018","2017"))
                           # column(4,radioButtons(inputId="nblevels", label="Sur 4 couleurs", inline=TRUE,
                           #            choices=c("2","4")))
                    )
                ),
                box(background = "light-blue", width=12,
                    fluidRow(
                      column(12,plotOutput("prodconsotrim1",height = 430)),#legende en plus
                      column(12,plotOutput("prodconsotrim2",height = 400)),
                      column(12,plotOutput("prodconsotrim3",height = 400)),
                      column(12,plotOutput("prodconsotrim4",height = 400))
                    )
                ),
                box(title="Production, Consommation et nombre de demi-heure de Production > Consommation",status="primary",solidHeader=TRUE,width=12,height = 1500,
                    fluidRow(
                      column(4,selectInput(inputId = "anprodconso",
                                           label = h4("choisissez l'annee"),
                                           choice = c("2017-2019","2018","2017"))
                      ),
                      column(4,selectInput(inputId = "prodtemps",
                                           label = h4("choisissez la consolidation"),
                                           choice = c("jour","semaine","mois"))
                      ),
                      column(12,plotOutput("prodconso")),
                      column(12,plotOutput("creneauauto")),
                      column(12,plotOutput("prodsurconso"))
                    )
                )
              ) #fluidRow
      ), # tabItem calendar #
      # 4 tab content
      tabItem(tabName = "prediction",
            fluidRow(
              box(background = "light-blue",title="Prediction en mois, semaines, jours, heures", width=12,
                  fluidRow(
                    column(4,h3(territoire)),
                    column(4, selectInput(inputId = "prod", 
                      label = "Choisissez :", 
                      choice = c("production","consommation","tauxAuto","prodSurConsoCreneau" ))),#"remplacer prodSurConsommation
                    column(4, selectInput(inputId = "frequence", 
                                          label = "Choisissez la frequence : en mois/semaines/jours/heures", 
                                          selected = "semaines",
                                          choice = c("mois","semaines","jours", "heures"))),
                    column(12,textOutput("parametre"))
                  )
                ),
              box(title="Prediction interactive",status="primary",solidHeader=TRUE, width=12,
                  dygraphOutput("predinteract")
              ),
              box(title="Tendance",status="primary",solidHeader=TRUE,
                  plotOutput("components")
              ),
              box(title="Erreur MAPE - Moyenne du % d'erreur",status="primary",solidHeader=TRUE,
                  plotOutput("plotcvmape")
              ),
              box(title="Prediction",status="primary",solidHeader=TRUE,width=12,
              #tableOutput("forecast"))
              checkboxGroupInput("show_vars", "Donnees disponibles :",
                                 # selection possible
                                 c("Date","yhat","yhat_lower","yhat_upper","trend","multiplicative_terms","multiplicative_terms_lower",
                                   "multiplicative_terms_upper","additive_terms","additive_terms_lower","additive_terms_upper",
                                   "daily","daily_lower","daily_upper","yearly","yearly_lower","yearly_upper"),
                                 # en montrer un sous ensemble
                                 selected = c("Date","yhat","yhat_lower","yhat_upper"),
                                 inline=TRUE),
                DT::dataTableOutput("mytable"))
      ) #fluidRow
    ) # tabItem prediction #
  ) # tabItems
  ) # dashboard body #
) # dashboard page #
####################################################################################################
# SERVER
####################################################################################################
server <- function(input, output) {
  
 
#######################################
### DASHBOARD tabname=ANALYSE         #
#######################################

  #renderText
  output$param <- renderText({
    input$production
    if (input$production == "prodSurConso"){paste("Vous avez choisi : Production / Consommation pour l'annee ", input$annee)}
    else {paste("Vous avez choisi : ", input$production, "\n"," pour l'annee ", input$annee)}
  })
  
  dataAn <- eventReactive(
    {
      input$annee
    }, 
    {
        if (input$annee == 2017){
          data <- data2017
        } else if (input$annee == 2018) {
          data <- data2018
        } else if (input$annee == 2019) { 
          data <- data2019
        } else { 
          data <- allPerHeure 
        }
      return (data)
    })
  # prodhdf : prod-conso-prodSurConso par heure
  prodhdf <- reactive({
    prod <- allPerHeure
  })
  
  # prodjdf : prod-conso-prodSurConso par jour
  prodjdf <- reactive({
    prod <- allPerJour
  })

  # prodmdf : prod-conso-prodSurConso par mois
  prodmdf <- reactive(
  {
    x <- allPerMois
    return(x)
  })
  # prodsdf : prod-conso-prodSurConso par semaine
  prodsdf <- reactive(
  {
      x <- allPerSemaine
    return(x)
  })
  
  # couleurs associees aux productions : c
  # consommation en bleu
  # production en rouge
  # prodSurConso en vert
  colProd <- eventReactive(
    {
      input$production
    }, 
    {
      if(input$production == "production"){
        col <- "red"
      } else if(input$production == "consommation"){
        col <- "blue"
      } else if(input$production == "prodSurConso"){
        col <- "green"
      }
      return (col)
    })
  #production choisie
  output$prodjour <- renderPlot({
    input$production
    if (input$production == "prodSurConso")
    {
      axey <- "% Production / Consommation"
    }
    else
    {
      axey <- paste(input$production," en MWh")
    }
    title <- paste(input$production,"consolidation par jour",sep=", ")
    plot(prodjdf()$DateJour,prodjdf()[[input$production]], type ="l", ylab=axey, xlab="jours du 01/2017 au 08/2019", main=title,col=colProd())
  })
  
  # Analyse des donnees : production consolidee au mois
  output$prodmois <- renderPlot({
    input$production
    
    if (input$production == "prodSurConso")
    {
      axey <- "% Production / Consommation"
      plot(prodmdf()$Month,prodmdf()[[input$production]], type ="p", ylab=axey, xlab="mois du 01/2017 au 08/2019", main="consolidation mois")
      lines(prodmdf()$Month,prodmdf()[[input$production]], type ="h", ylab=axey, xlab="mois du 01/2017 au 08/2019", main="consolidation mois")
    }
    else 
    {
      axey <- paste(input$production," en GWh")
      plot(prodmdf()$Month,prodmdf()[[input$production]], type ="p", ylab=axey, xlab="mois du 01/2017 au 08/2019", main="consolidation mois")
      lines(prodmdf()$Month,prodmdf()[[input$production]], type ="h", ylab=axey, xlab="mois du 01/2017 au 08/2019", main="consolidation mois")
    }
  })

  # Analyse des donnees : plot de production horaire par heure et par mois pour les annees 2017 puis 2018
  output$visuPlotMois <- renderPlot({
    input$production
    input$annee
    
    if (input$production == "prodSurConso")
    {
      unite <- "% Production / Consommation"
    }
    else
    {
      unite <- "MWh"
    }
    
    title <- paste(input$production,"horaire par mois annee", input$annee)
    visu <- ggplot(dataAn(), aes_string("HH",input$production)) +
      geom_point(colour=colProd())+labs(x = "heure", y=unite)+ facet_wrap(~ Month) +
      labs(title = title, x = "Heures", y=unite)
    return (visu)
  })
  
  # Analyse des donnees : Production horaire par Mois pour l'annee 2017 ou 2018 ou 2019 : boxplot
  output$visuBoxJour <- renderPlot({ 
    input$production
    input$annee
    
    if (input$production == "prodSurConso")
    {
      unite <- "% Production / Consommation"
    }
    else
    {
      unite <- "MWh"
    }
    title <- paste(input$production,"horaire par mois annee", input$annee)
    visu <- ggplot(dataAn(), aes_string("HHFactor",input$production,group="HHFactor")) + geom_boxplot(fill = colProd())+
      labs(title = title, x = "Heures", y=unite) + facet_wrap(~ Month) +  
      stat_summary(fun.y=mean, geom="point", shape=23, size=1,color="black")
    return (visu)
  })
  # Analyse des donnees : production mensuelle avec une courbe par annee
  output$visuSeasonal <- renderPlot({
    input$production
    
    #create a ts object
    if (input$production == "prodSurConso")
    {
      myts <- ts(prodmdf()[[input$production]], start=c(2017, 1), end=c(2019, 08), frequency=12)
      axey <- "% Production / Consommation"
    } else {
      myts <- ts(prodmdf()[[input$production]], start=c(2017, 1), end=c(2019, 08), frequency=12)
      axey <- paste(input$production,"en GWh")
    }
    visu<-seasonplot(myts,year.labels =TRUE,col=1:6, year.labels.left=TRUE, ylab= axey,xlab="",main="")
    return (visu)
  })
  # Analyse des donnees : boxplot de la Production horaire par mois
  output$visuBoxMois <- renderPlot({
    input$production
    input$annee
    if (input$production == "prodSurConso")
    {
      unite <- "% Production / Consommation"
    }
    else
    {
      unite <- "MWh"
    }
    title <- paste(input$production,"horaire par mois annee", input$annee)
    
    visu<-ggplot(dataAn(), aes_string("Mois",input$production,group="MoisFactor")) + 
      geom_boxplot(fill = colProd())+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
      labs(title = title, x = "Mois", y=unite)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=1,color="red")
    return (visu)
  })
  
  #######################################
  ### DASHBOARD tabname=CAL             #
  #######################################
  ###########################################################################################
  ## VISU PRODUCTION ET CONSOMMATION + prodSurConso
  dataprod <- eventReactive(
  {
      input$prodtemps
      input$anprodconso
  }, 
  {
      if (input$prodtemps == "mois"){
        data <- allPerMois
      } else if (input$prodtemps == "semaine"){
        data <- allPerSemaine
      } else if (input$prodtemps == "jour"){
        data <- allPerJour
      } else if (input$prodtemps == "heure"){
        data <- allPerHeure
        data <- filter(data,(Year=="2018"&Mois==1))
      }
      if (input$anprodconso == 2018)
      {
        data <- filter(data,Year=="2018")
      }
      else if (input$anprodconso == 2017)
      {
        data <- filter(data,Year=="2017")
      }
      return (data)
  })
  dataprodAn <- eventReactive(
  {
      input$anprodconso
  }, 
    {
      if (input$anprodconso == 2018){
        data <- data2018
      } else if (input$anprodconso == 2017){
        data <- data2017
      } else { 
        data <- allPerHeure 
      }
      return (data)
    })

  # Comparaison par jour, semaine ou mois
  # consommation, production 
  output$prodconso <- renderPlot({
    input$anprodconso
    input$prodtemps
    
    titlepv <- paste("Evolution de la production par",input$prodtemps,"en MWh")
    prod <- ggplot(dataprod(),aes(Date,production)) +
      geom_col(aes(y=production),colour="red",fill="red")+
      scale_x_date(date_labels="%b %y",date_breaks = "1 month") +
      labs(x = "Date", y="MWh",title=titlepv)
    titleconso <- paste("Evolution de la consommation par",input$prodtemps,"en MWh")
    conso <- ggplot(dataprod(),aes(Date,consommation)) +
      geom_col(aes(y=consommation),colour="blue",fill="blue")+
      scale_x_date(date_labels="%b %y",date_breaks = "1 month") +
      labs(x = "Date", y="MWh",title=titleconso)

    visu <- grid.arrange(prod,conso,ncol=1,nrow=2)
    return (visu)
  })
  # Comparaison par jour, semaine ou mois
  # nb de creneaux + taux prodSurConsommation 
  output$creneauauto <- renderPlot({
    input$anprodconso
    input$prodtemps
    
    titlecreneau <- paste("Evolution du nombre de creneaux Production>Consommation par",input$prodtemps)
    creneau <- ggplot(dataprod(),aes(Date,tauxAuto)) +
      geom_col(aes(y=creneauSup),colour="purple",fill="purple")+
      scale_x_date(date_labels="%b %y",date_breaks = "1 month") +
      labs(x = "Date", y="nb",title=titlecreneau)
    titleauto <- paste("Taux d'autosuffisance par",input$prodtemps,"en %")
    auto <- ggplot(dataprod(),aes(Date,tauxAuto)) +
      geom_col(aes(y=tauxAuto),colour="#62A667",fill="#62A667")+
      scale_x_date(date_labels="%b %y",date_breaks = "1 month") +
      labs(x = "Date", y="%",title=titleauto)

    visu <- grid.arrange(creneau,auto,ncol=1,nrow=2)
    return (visu)
  })
  # Comparaison par jour, semaine ou mois
  # taux prodSurConsommation 
  output$prodsurconso <- renderPlot({
    input$anprodconso
    input$prodtemps
    #PB de quelques creneaux en mai et juin 2019 => supprimes
    titleprodsurconso <- paste("Evolution du % Production sur Consommation par",input$prodtemps)
    prodsurconso <- ggplot(dataprod(),aes(Date,prodSurConsoCreneau)) +
      geom_col(aes(y=prodSurConsoCreneau),colour="yellow",fill="yellow")+
      scale_x_date(date_labels="%b %y",date_breaks = "1 month") +
      labs(x = "Date", y="%",title=titleprodsurconso)
    
    visu <- grid.arrange(prodsurconso,ncol=1,nrow=1)
    return (visu)
  })
# Visualisation par annee de l ensemble des creneaux par demi-heure
# par demi heure pour un trimestre 1
  fnvisu <- function(trimestre) {
    data <- filter(mesData,(trim==trimestre))
    
    #prodSup_levels <- c("superieur","inferieur") 
    prodSup_levels <- c("sup2x","superieur","inferieur","inf2x")
    #prodSup_colors <- c("#62A667","gray96")
    prodSup_colors <- c("green","#62A667","darkgray","gray96")
    
    visu <- ggplot(data,aes(x=HHMM,y=Jour))+
      geom_tile(aes(fill=prodSup),color="white", size=1)+#quadrillage des periodes
      scale_fill_manual(values=prodSup_colors, labels=prodSup_levels, drop = FALSE)+
      scale_y_reverse(breaks=rev(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)))+
      scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
      facet_wrap(~ mois,nrow=1)+
      labs(x="Heure",y="Jour")+#,title=theTitle)+
      theme_gray(base_size=15)+#taille de la fonte (10-14 ?)
      theme(panel.background=element_rect(fill="transparent",colour=NA),#supprime le quadrillage du fond
            axis.ticks=element_blank(),
            axis.text=element_text(size=10),#ADD
            legend.text=element_text(size=15),
            legend.position="top",
            legend.justification="right",
            legend.direction="horizontal",
            legend.key.size=unit(0.3,"cm"),
            legend.spacing.x=unit(0.2,"cm"))
    return(visu)
  }
 # Visualisation par trimestre de l ensemble des creneaux par demi-heure
# par demi heure pour un trimestre 1 avec legende
  output$prodconsotrim1 <- renderPlot({
    input$an
    if(input$an == "2018")
    {
      visu <- fnvisu("Hiver2018")
    }
    else if(input$an == "2019")
    {
      visu <- fnvisu("Hiver2019")
    }
    else
    {
      visu <- fnvisu("Hiver2017")
    }
    return (visu)
  })
  output$prodconsotrim2 <- renderPlot({
    input$an
    if(input$an == "2018")
    {
      visu <- fnvisu("Printemps2018")+theme(legend.position="none")
    }
    else if(input$an == "2019")
    {
      visu <- fnvisu("Printemps2019")
    }
    else
    {
      visu <- fnvisu("Printemps2017")+theme(legend.position="none")
    }
    
    return (visu)
  })
  output$prodconsotrim3 <- renderPlot({
    input$an
    if(input$an == "2018")
    {
      visu <- fnvisu("Ete2018")+theme(legend.position="none")
    }
    else if(input$an == "2019")
    {
      visu <- fnvisu("Ete2019")
    }
    else
    {
      visu <- fnvisu("Ete2017")+theme(legend.position="none")
    }
    
    return (visu)
  })
  output$prodconsotrim4 <- renderPlot({
    input$an
    if(input$an == "2018")
    {
      visu <- fnvisu("Automne2018")+theme(legend.position="none")
      return (visu)
    }
    else if(input$an == "2017")
    {
      visu <- fnvisu("Automne2017")+theme(legend.position="none")
      return (visu)
    }
    # pas les data 2019
  })

  # boxplot de la production de la consommation et de l'auto conso horaire par mois
  # data = allPerHeure
  output$prodconsoautoMois <- renderPlot({
    input$anprodconso
    
    prodbox <-ggplot(dataprodAn(), aes_string("Mois","production",group="MoisFactor")) +
      geom_boxplot(fill = "red")+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
      labs(title = "Production horaire", x = "Mois", y="MWh")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    consobox <-ggplot(dataprodAn(), aes_string("Mois","consommation",group="MoisFactor")) +
      geom_boxplot(fill = "blue")+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
      labs(title = "Consommation horaire", x = "Mois", y="MWh")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    autobox <-ggplot(dataprodAn(), aes_string("Mois","prodSurConso",group="MoisFactor")) +
      geom_boxplot(fill = "green")+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
      labs(title = "Production / Consommation horaire", x = "Mois", y="%")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    visu <- grid.arrange(prodbox,consobox,autobox,ncol=3,nrow=1)
    return (visu)
  })
  # boxplot de la production de la consommation et de l'auto conso horaire par heure
  # data = allPerHeure
  output$prodconsoautoHeure <- renderPlot({
    input$anprodconso
    
    prodbox <-ggplot(dataprodAn(), aes_string("HH","production",group="HHFactor")) +
      geom_boxplot(fill = "red")+
      scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
      labs(title = "Production horaire", x = "HH", y="MWh")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    consobox <-ggplot(dataprodAn(), aes_string("HH","consommation",group="HHFactor")) +
      geom_boxplot(fill = "blue")+
      scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
      labs(title = "Consommation horaire", x = "HH", y="MWh")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    autobox <-ggplot(dataprodAn(), aes_string("HH","prodSurConso",group="HHFactor")) +
      geom_boxplot(fill = "green")+
      scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
      labs(title = "Production / Consommation horaire", x = "HH", y="%")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    visu <- grid.arrange(prodbox,consobox,autobox,ncol=3,nrow=1)
    return (visu)
  })

  # Production par heure
  # separation en 2 graphiques : saison creuse a gauche et pleine a droite
  output$prodSaison <- renderPlot({
    input$anprodconso
    
    #Box Plot production
    prod <-ggplot(dataprodAn(), aes_string("HHFactor","production",group="HHFactor")) +
      geom_boxplot(fill = "red")+
      facet_wrap( ~saison,ncol=2)+
      labs(title = "Production horaire", x = "Heure", y="MWh")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    return (prod)
  })
  output$consoSaison <- renderPlot({
    input$anprodconso
    
    #Box Plot consommation
    conso <-ggplot(dataprodAn(), aes_string("HHFactor","consommation",group="HHFactor")) +
      geom_boxplot(fill = "blue")+
      facet_wrap( ~saison,ncol=2)+
      labs(title = "Consommation horaire", x = "Heure", y="MWh")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    return (conso)
  })
  output$autoSaison <- renderPlot({
    input$anprodconso
    
    #Box Plot auto
    auto <-ggplot(dataprodAn(), aes_string("HHFactor","prodSurConso",group="HHFactor")) +
      geom_boxplot(fill = "green")+
      facet_wrap( ~saison,ncol=2)+
      labs(title = "Production / Consommation horaire", x = "Heure", y="%")+
      stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")
    return (auto)
  })
  
  # ANCIENNES VISU => en bas de page
  #toutes les productions, toutes les heures
  output$productions <- renderPlot({
    # toutes les productions
    ggplot(prodhdf(),aes(DateHeure,consommation)) +
      geom_point(aes(y=production),colour="red")+
      geom_point(aes(y=consommation),colour="blue")+
      labs(x = "Date", y="MWh")
  })
  #toutes les productions consolidees par Jour
  output$productionsJour <- renderPlot({
    # toutes les productions
    ggplot(prodjdf(),aes(DateJour,consommation)) +
      geom_point(aes(y=production),colour="red")+
      geom_point(aes(y=consommation),colour="blue")+
      labs(x = "Date", y="MWh")
  })
  #toutes les productions consolidees par Mois
  output$productionsMois <- renderPlot({
    # toutes les productions
    ggplot(prodmdf(),aes(Month,consommation)) +
      geom_line(aes(y=production,group=1),colour="red")+
      geom_line(aes(y=consommation,group=1),colour="blue")+
      scale_x_date(date_labels="%m-%y",date_breaks = "6 month") +
      labs(x = "Mois", y="MWh")
  })

  #######################################
  ### DASHBOARD tabname=PREDICTION      #
  #######################################
  
  # on fixe le nb d unite predits en fonction de la frequence
  nbpredict <- eventReactive(
  {
    input$frequence
  }, 
  {
      if (input$frequence == "jours") {
        nb <- 50
      } else if(input$frequence == "semaines") {
        nb <- 6
      } else if (input$frequence == "mois") {
        nb <- 2
      } else if (input$frequence == "heures") {
        nb <- 200
      }
    return (nb)
    })
  
  #renderText
  output$parametre <- renderText({
    input$frequence
    input$prod
    paste("Parametres de la prediction :", input$prod, "du territoire", territoire ,"sur ", nbpredict(), input$frequence)
  })
  
  #renderPlot
  output$production <- renderPlot({
    input$prod
    
    if(input$prod == "production"){
      plot(production ~ DateJour, prodjdf(), type ="l", ylab="Production en MWh", xlab="du 01/2017 au 12/2018", main=paste(input$prod,"consolidation par jour",sep=", "),col=colProd())
    } else if(input$prod == "consommation"){
      plot(consommation ~ DateJour, prodjdf(), type ="l", ylab="Consommation en MWh", xlab="du 01/2017 au 12/2018", main=paste(input$prod,"consolidation par jour",sep=", "))
    } else if(input$prod == "tauxAuto"){
       plot(tauxAuto ~ DateJour, prodjdf(), type ="l", ylab="Taux creneaux autosuffisance en %", xlab="du 01/2017 au 12/2018", main=paste(input$prod,"consolidation par jour",sep=", "))
    }else {
      plot(prodSurConso ~ DateJour, prodjdf(), type ="l", ylab="% Production / Consommation", xlab="du 01/2016 au 12/2018", main=paste(input$prod,"consolidation par jour",sep=", "))
    }
  })
  
  # CHOIX des data
  # Si jour : prodjdf()
  # Si heures : mesData avec le champs Date
  statsdf <- eventReactive(
  {
      input$prod
      input$frequence
  }, 
  {
    if (input$frequence == "jours") {
      stats <- select(prodjdf(),c("DateJour",input$prod))
    } else if (input$frequence == "semaines") {
      stats <- select(prodsdf(),c("Date",input$prod))
    } else if (input$frequence == "mois") {
      stats <- select(prodmdf(),c("Date",input$prod))
    } else if (input$frequence == "heures") {
      stats <- select(dataAn(),c("DateHeure",input$prod))
    }
    colnames(stats) <- c("ds","y")
    return (stats)
  })

  # configuration de la Cross Validation
  dfcv <- eventReactive(
  {
      input$prod
      input$frequence
  }, 
  {
      if (input$frequence == "jours") {
        dfcv <- cross_validation(modeldf(), initial = 550, period = 25, horizon = 50, units = 'days')
      } else if(input$frequence == "semaines") {
        dfcv <- cross_validation(modeldf(), initial = 78, period = 3, horizon = 6, units = 'weeks')
      } else if (input$frequence == "mois") {
        dfcv <- cross_validation(modeldf(), initial = 78, period = 3, horizon = 6, units = 'weeks')
      } else if (input$frequence == "heures") {
        dfcv <- cross_validation(modeldf(), horizon =200, units='hours', initial=8000, period=150)
      }
  })
  
  #prediction de la production $production pour le nb $nb de jours/heures $frequence
  # si production : mode multiplicatif 
  # si conso : mode additif
  # si en heures : daily.seasonality=TRUE
  # si en jours : daily.seasonality=FALSE
  modeldf <- eventReactive(
    {
      input$prod
      input$frequence
    }, 
    {
      if (input$frequence == "jours")
      {
        if(input$prod == "production") {
          m <- prophet(statsdf(),yearly.seasonality=TRUE,weekly.seasonality =FALSE,seasonality.mode='multiplicative')
        } else if ((input$prod == "tauxAuto")|(input$prod == "prodSurConsoCreneau")) {
          m <- prophet(statsdf(),yearly.seasonality=TRUE,weekly.seasonality =TRUE,seasonality.mode='multiplicative')
        } else {
          m <- prophet(statsdf(),yearly.seasonality=TRUE,weekly.seasonality =TRUE,seasonality.mode='additive')
        }
      } else if(input$frequence == "semaines")# prediction en semaines
      {
        if(input$prod == "production") {
          m <- prophet(statsdf(),daily.seasonality=FALSE,yearly.seasonality=TRUE,weekly.seasonality =FALSE,seasonality.mode='multiplicative')
        } else if ((input$prod == "tauxAuto")|(input$prod == "prodSurConsoCreneau")) {
          m <- prophet(statsdf(),daily.seasonality=FALSE,yearly.seasonality=TRUE,seasonality.mode='multiplicative')
        } else {
          m <- prophet(statsdf(),daily.seasonality=FALSE,yearly.seasonality=TRUE,weekly.seasonality =FALSE,seasonality.mode='additive')
        }
      } else if(input$frequence == "mois")# prediction en mois
      {
        if(input$prod == "production") {
          m <- prophet(statsdf(),daily.seasonality=FALSE,yearly.seasonality=TRUE,weekly.seasonality =FALSE,seasonality.mode='multiplicative')
        } else if ((input$prod == "tauxAuto")|(input$prod == "prodSurConsoCreneau")) {
          m <- prophet(statsdf(),daily.seasonality=FALSE,yearly.seasonality=TRUE,seasonality.mode='multiplicative')
        } else {
          m <- prophet(statsdf(),daily.seasonality=FALSE,yearly.seasonality=TRUE,weekly.seasonality =FALSE,seasonality.mode='additive')
        }
      } else if(input$frequence == "heures")# prediction d heures
      {
        if(input$prod == "production") {
          m <- prophet(statsdf(),daily.seasonality=TRUE,yearly.seasonality=TRUE,weekly.seasonality =FALSE,seasonality.mode='multiplicative')
        } else if ((input$prod == "tauxAuto")|(input$prod == "prodSurConsoCreneau")) {
          m <- prophet(statsdf(),daily.seasonality=TRUE,yearly.seasonality=TRUE,seasonality.mode='multiplicative')
        } else {
          m <- prophet(statsdf(),daily.seasonality=TRUE,yearly.seasonality=TRUE,weekly.seasonality =TRUE,seasonality.mode='additive')
        }
      }
      return (m)
    })
  # si frequence en jour => day
  # si frequence en heures => hour
  forecastdf <- eventReactive(
  {
      input$prod
      input$frequence
  }, 
  {
    if (input$frequence == "jours")
    {
      future <- prophet::make_future_dataframe(modeldf(), periods = 50,freq='day') #50 jours
    }
    else if(input$frequence == "semaines")# prediction en semaine
    {
      future <- prophet::make_future_dataframe(modeldf(), periods = 6,freq='week') #6 semaines
    } 
    else if(input$frequence == "mois")# prediction en mois
    {
      future <- prophet::make_future_dataframe(modeldf(), periods = 2,freq='month') #2 mois
    } 
    else if(input$frequence == "heures")# prediction d heures
    {
      future <- prophet::make_future_dataframe(modeldf(), periods = 200 ,freq=3600) #200 heures cad 8 jours
    } 
    forecast <- predict(modeldf(), future)
    return (forecast)
  })
  output$prediction <- renderPlot({
    # visualisation de la prediction
    return(plot(modeldf(),forecastdf()))
  })
  #composants de la prediction
  output$components <- renderPlot({
    return (prophet_plot_components(modeldf(), forecastdf()))
  })
  # Prediction : courbe forecast 
  output$showforecast <- renderPlot({
    input$frequence
    input$production
    
    forecast <- tail(forecastdf(),nbpredict())
    if (input$production == "prodSurConso") {
      unit <- "%"
    } else if (input$frequence == "heures") {
      unit <- "Mwh"
    } else {
      unit <- "Gwh"
    }
      
    if (input$frequence == "heures")
    {
      visu <- ggplot(forecast,aes(ds,yhat_upper)) +
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper,group=1),na.rm=TRUE,fill="grey70")+
        geom_line(aes(y=yhat),colour="blue")+
        scale_x_datetime(date_labels="%H",date_breaks = "1 hour")+
        labs(x = "heures", y=unit)
    } else if (input$frequence == "semaines") {
      visu <- ggplot(forecast,aes(ds,yhat_upper)) +
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper,group=1),na.rm=TRUE,fill="grey70")+
        geom_line(aes(y=yhat),colour="blue")+
        scale_x_datetime(date_labels="%d-%m",date_breaks = "2 week")+
        labs(x = "semaines", y=unit)
    } else if (input$frequence == "mois") {
      visu <- ggplot(forecast,aes(ds,yhat_upper)) +
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper,group=1),na.rm=TRUE,fill="grey70")+
        geom_line(aes(y=yhat),colour="blue")+
        scale_x_datetime(date_labels="%d-%m",date_breaks = "1 month")+
        labs(x = "mois", y=unit)
    } else if (input$frequence == "jours") {
      visu <- ggplot(forecast,aes(ds,yhat_upper)) +
         geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper,group=1),na.rm=TRUE,fill="grey70")+
         geom_line(aes(y=yhat),colour="blue")+
         scale_x_datetime(date_labels="%d-%m",date_breaks = "2 day")+
         labs(x = "jours", y=unit)
    }
    return (visu)
  })

  # RAF : courbe forecast timeline
  # montrer J+8 jours ou H+48h
  output$showforecastimeline <- renderPlot({
    input$frequence
   
    forecast <- tail(forecastdf(),nbpredict())
    if (input$frequence == "heures")
    {
      visu <- ggplot(forecast,aes(ds,yhat_upper)) +
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper,group=1),na.rm=TRUE,fill="grey70")+
        geom_line(aes(y=yhat),colour="blue")+
        scale_x_datetime(date_labels="%H",date_breaks = "2 hour")+
        labs(x = "heures", y="MWh")
    } else if (input$frequence == "semaines") {
      visu <- ggplot(forecast,aes(ds,yhat_upper)) +
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper,group=1),na.rm=TRUE,fill="grey70")+
        geom_line(aes(y=yhat),colour="blue")+
        scale_x_datetime(date_labels="%d-%m",date_breaks = "2 week")+
        labs(x = "semaines", y="GWh")
    } else if (input$frequence == "mois") {
      visu <- ggplot(forecast,aes(ds,yhat_upper)) +
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper,group=1),na.rm=TRUE,fill="grey70")+
        geom_line(aes(y=yhat),colour="blue")+
        scale_x_datetime(date_labels="%y-%m",date_breaks = "2 month")+
        labs(x = "mois", y="GWh")
    } else if (input$frequence == "heures") {
      visu <- ggplot(forecast,aes(ds,yhat_upper)) +
        geom_ribbon(aes(ymin=yhat_lower,ymax=yhat_upper,group=1),na.rm=TRUE,fill="grey70")+
        geom_line(aes(y=yhat),colour="blue")+
        scale_x_datetime(date_labels="%d-%m",date_breaks = "2 day")+
        labs(x = "jours", y="GWh")
    }
    return (visu)
  })
  #prediction prophet interactive
  output$predinteract <- renderDygraph({
    return(dyplot.prophet(modeldf(),forecastdf()))
  })
  #table des donnees predites
  output$mytable = DT::renderDataTable({
    input$show_vars
    #table de predition
    forecast = forecastdf()
    forecast$Date <- as.character(forecast$ds)
    #forecast <- select(forecast,c("Date","yhat","yhat_lower","yhat_upper"))
    #c("Date","yhat","yhat_lower","yhat_upper","trend","additive_terms","additive_terms_lower","additive_terms_upper","yearly","yearly_lower","yearly_upper")
    # afficher seulement les jours predits => tail
    forecast <- tail(forecast,nbpredict())
    #delete row names 
    rownames(forecast) <- NULL
    DT::datatable(forecast[,input$show_vars, drop = FALSE], 
                  options = list(lengthMenu = c(5,10,15,20,25), pageLength = 10))
  })
  # table des erreurs en CV
  output$tablecv = DT::renderDataTable({

    dfp <- performance_metrics(dfcv())
    DT::datatable(dfp[,input$showcvvars, drop = FALSE], 
                          options = list(lengthMenu = c(5,10,15,20,25), pageLength = 11))
  })
  # Plot de l erreur mape
  output$plotcvmape <- renderPlot({

    visu <- plot_cross_validation_metric(dfcv(),metric="mape")
    return (visu)
  })
}
shinyApp(ui, server)