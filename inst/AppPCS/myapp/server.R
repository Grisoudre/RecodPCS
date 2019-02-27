#=====================================================
# Packages
#=====================================================

library(shiny)
library(questionr)
library(tidyverse)
library(stringr)
library(DT)


 options(shiny.maxRequestSize=30*1024^2, encoding = "UTF-8")
 
#======================================================
# Importation de la table comprenant les diff. niveaux de PCS
#======================================================

 

# N5 <- read.csv2("N5.csv", fileEncoding="UTF-8", stringsAsFactor=F)

#=========================================================
# Fichier server
#=========================================================


  shinyServer(function(input, output, session) {


  # A/ Table de recodage vide
  #======================================================
  
  
  # C/ Fonctions supports au formulaire
  #========================================================
  
  trim_string <- function(string) gsub("\\s+", " ", gsub("^\\s+|\\s+$", "", string))
  
  
  # C/ 1. Get table metadata. For now, just the fields
  # Further development: also define field types
  # and create inputs generically
  GetTableMetadata <- function() {
    fields <- c(  IdRec = "IdRec", ProfBrutRec = "ProfBrutRec",
      IntituleInsee="IntituleInsee",
      LibN4="LibN4",
      N4="N4",
      LibN3="LibN3",
      N3="N3",
      LibN2="LibN2",
      N2="N2",
      LibN1="LibN1",
      N1="N1")
    
    result <- list(fields = fields)
    return (result)
  }
  
  
  # C/ 2. Find the next ID of a new record (in mysql, this could be done by an incremental index)
  GetNextId <- function() {
    if (exists( "Recodage" ) && nrow(Recodage) > 0) {
      max(as.integer(rownames(Recodage))) + 1
    } else {
      return (1)
    }
  }
  
  # C/ 3. C = créer
  CreateData <- function(data) {
    data <- CastData(data)
    rownames(data) <- GetNextId()
    if (exists("Recodage")) {
      Recodage <<- rbind(Recodage, data)
    } else {
      Recodage <<- data
    }
  }
  
  
  # 4. R = lire
  ReadData <- function() {
    if (exists("Recodage")) {
      Recodage
    }
  }
  
  # 5. U = Mettre à jour
  UpdateData <- function(data) {
    data <- CastData(data)
    Recodage[row.names(Recodage) == row.names(data), ] <<- data
  }
  

  
  
  # 6. D = supprimer
  DeleteData <- function(data) {
    Recodage <<- Recodage[row.names(Recodage) != unname(data["IdRec"]), ]
  }
  
  
  
  # C/ Formulaire de recodage
  #========================================================
  
  # C/ 1. Cast from Inputs to a one-row data.frame
  CastData <- function(data) { 
    datar <- data.frame( ProfBrutRec = data["ProfBrutRec"], 
                         IntituleInsee=data["IntituleInsee"],
                         LibN4=data["LibN4"],
                         N4=data["N4"],
                         LibN3=data["LibN3"],
                         N3=data["N3"],
                         LibN2=data["LibN2"],
                         N2=data["N2"],
                         LibN1=data["LibN1"],
                         N1=data["N1"],
                         stringsAsFactors = FALSE)
    
    rownames(datar) <- data["IdRec"]
    return (datar)
  }
  

  
  
  # C/ 2. Return an empty, new record
  CreateDefaultRecord <- function() {
    mydefault <- CastData(list(IdRec = "0",ProfBrutRec="",IntituleInsee="",  LibN4 = "", N4="",
                               #   LibN3etdemi = "", N3etdemi="",
                               LibN3 = "", N3="",
                               LibN2 = "", N2="",
                               LibN1 = "", N1=""))
    
    return (mydefault)
  }
   
 
  # C/ 3. Mises à jour
  
  UpdateInputs <- function(data, session) {
    updateTextInput(session, "IdRec", value = unname(rownames(data)))
    updateTextInput(session, "ProfBrutRec", value = unname(data["ProfBrutRec"]))
    updateSelectInput(session, inputId= "IntituleInsee", selected=unname(data["IntituleInsee"]))
    updateSelectInput(session, inputId= "LibN4", selected=unname(data["LibN4"]))
    # updateSelectInput(session, "LibN3etdemi",selected=unname(data["LibN3etdemi"]))
    updateSelectInput(session, inputId= "LibN3", selected=unname(data["LibN3"]))
    updateSelectInput(session, inputId= "LibN2", selected=unname(data["LibN2"]))
    updateSelectInput(session, inputId= "LibN1", selected=unname(data["LibN1"]))
    
  }
  
  
  
  
  # A/ Importation du tableau de données
  #===========================================================
  
  # A/ 1. Données importées (adaptation d'explore-data, Paris Descartes) :
  output$donnees.fichier.ui <- renderUI({
    list(
      fileInput("donnees.fichier.input", "Choisir le fichier :"),
      radioButtons("donnees.fichier.header",
                   "Noms de variables en 1ère ligne :",
                   c("oui", "non")),
      radioButtons("donnees.fichier.sep",
                   "Séparateur de champs :",
                   c("point-virgule" = ";",
                     "virgule" = ",",
                     "espace" = " ",
                     "tabulation" = "\t")),
      radioButtons("donnees.fichier.dec",
                   "Séparateur de décimales :",
                   c("point" = ".", "virgule" = ",")),
      radioButtons("donnees.fichier.enc",
                   "Encodage des caractères :",
                   c("UTF-8 (par défaut sur Linux/Mac)" = "UTF-8",
                     "Windows-1252 (par défaut sur Windows)" = "WINDOWS-1252")),
      uiOutput("donnees.fichier.ok")
    )
    
  })
  # A/ 1. Données importées (adaptation d'explore-data, Paris Descartes) :
  output$donnees.fichier.ui2 <- renderUI({
    list(
     fileInput("donnees.fichier.input2", "Table de recodage sauvegardée :"),
      uiOutput("donnees.fichier.ok2")
     )
    
  })
  

  
  donnees_entree2 <-reactive({
    if (is.null(input$donnees.fichier.input2)) return (NULL)
    donnees_entree2 <- NULL
    try({
      donnees_entree2 <- read.table(
        input$donnees.fichier.input2$datapath,
        header = T,
        sep = ";",
        dec = ",",
        fileEncoding = "UTF-8",
        stringsAsFactors = FALSE)
    }, silent = TRUE)
    donnees_entree2 <- unique(donnees_entree2)
    donnees_entree2
  })
  
  
  donnees_entree <-reactive({
    if (is.null(input$donnees.fichier.input)) return (NULL)
    donnees_entree <- NULL
    try({
      donnees_entree <- read.table(
        input$donnees.fichier.input$datapath,
        header = input$donnees.fichier.header == "oui",
        sep = input$donnees.fichier.sep,
        dec = input$donnees.fichier.dec,
        fileEncoding = input$donnees.fichier.enc,
        stringsAsFactors = FALSE)
    }, silent = TRUE)
    donnees_entree <- unique(donnees_entree)
    donnees_entree
  })
  
  
  
  # A/ 2. Vérification de l'importation :
  # taille et str du tableau de départ :
  
  output$Dimensions <- renderText(
    if (is.null(input$donnees.fichier.input)) return ("")
    else {
      paste("Tableau constitué de", ncol(donnees_entree()),
            "colonnes et de", nrow(donnees_entree()),"lignes.
            Détail des variables :")
      
    })
  
  
  output$Resume <- renderTable({
    tmp <- donnees_entree()
    if (is.null(tmp)) {return (NULL)}else{
      
      Resume <- data.frame( Variable = names(tmp[1]),
                            Type = class(tmp[,1]),
                            NbreValeursDiff = nrow(unique(tmp[1])))
      for (i in (2:ncol(tmp))) {
        Resume <-rbind(Resume, data.frame( Variable = names(tmp[i]),
                                           Type = class(tmp[,i]),
                                           NbreValeursDiff = nrow(unique(tmp[i]))))
      }
      Resume
    }
  })
  
  # B/ Sélections des variables clés et table recodage intermédiaire (uniquement profession)
  #===============================================
  
  
  # B/ 1. Choix de l'identifiant :
  
  
  
  output$SelectID <- renderUI({
    selectInput("ID", "Choix de l'identifiant (doit être unique) :",
                choices=c(" ",names(donnees_entree())) , selected = " ")
  })
  
  
  # B/ 2. Sélection de la variable profession à recoder :
  
  
  
  output$SelectProfBrut <- renderUI({
    selectInput("Prof", "Choix de la variable profession à recoder :",
                choices=as.list(c(" ",names(donnees_entree()))),selected=" ")
  })
  
  
  observeEvent(input$OK,{
    
    Table <- donnees_entree()
    Ident <- input$ID
    Prof <- input$Prof
    
    validate(
      need((  Prof != " ") , "")
    )
    RecodageProfessions<- freq(Table[,Prof])
    RecodageProfessions$IdRec<- seq(1:nrow(RecodageProfessions))
    RecodageProfessions$ProfBrutRec <- row.names(RecodageProfessions)
    RecodageProfessions$ProfBrutRec <- trim_string(RecodageProfessions$ProfBrutRec)
    RecodageProfessions <- RecodageProfessions[,c("IdRec","ProfBrutRec")]
    RecodageProfessions[,"IntituleInsee"] <- ""
    RecodageProfessions[,"LibN4"] <- ""
    RecodageProfessions[,"N4"] <- ""
    RecodageProfessions[,"LibN3"] <- ""
    RecodageProfessions[,"N3"] <- ""
    RecodageProfessions[,"LibN2"] <- ""
    RecodageProfessions[,"N2"] <- ""
    RecodageProfessions[,"LibN1"] <- ""
    
    RecodageProfessions[,"N1"] <- ""
    row.names(RecodageProfessions) <- RecodageProfessions$IdRec
    RecodageProfessions$IdRec <- NULL
    RecodageProfessions
    Recodage <<- unique(rbind(Recodage, RecodageProfessions))
    
#     write.csv2(Recodage,"temp.csv", fileEncoding = "UTF-8", na= "", row.names=F)
      UpdateData(formData())
       data <- ReadData()[1, ]
       UpdateInputs(data, session)


    updateTabsetPanel(session, "tabSession",
                      selected = "panel2")
  })

  
  observeEvent(input$OK2,{
    
    Table <- donnees_entree2()
    Recodage <<- Table
    
    #     write.csv2(Recodage,"temp.csv", fileEncoding = "UTF-8", na= "", row.names=F)
   UpdateData(formData())
   data <- ReadData()[1 , ]
   UpdateInputs(data, session)
 
    updateTabsetPanel(session, "tabSession",
                      selected = "panel2")

     })
  
  
  

# B/ 4. Table avec les recodages :
#=========================================================
  
  RecodReact <- reactive ({
    
    UpdateData(formData())
    data <- ReadData()[as.integer(input$IdRec)+1, ]
    UpdateInputs(data, session)
    r <- ReadData()
    r
  })
  
TableFinale <- reactive ({
  donnees_entree <- donnees_entree()
  Prof <- input$Prof
  Recodage <- RecodReact()
  TableFinale <- merge (donnees_entree, Recodage,
                        by.x = Prof, by.y = "ProfBrutRec", all.x = T)
  TableFinale <- TableFinale [, c(2: (ncol(TableFinale)-9),1,(ncol(TableFinale)-8):ncol(TableFinale) )] 
  TableFinale
  
})


observe({
  Intitule <- input$IntituleInsee
  updateTextInput(session, "LibN4", value =  CSP4niv$LibN4[CSP4niv$Intitule==Intitule])
})
observe({
  Lib4 <- input$LibN4
  updateTextInput(session, "N4", value =  N4$N4[N4$LibN4==Lib4])
  updateTextInput(session, "LibN3", value =  CSP4niv$LibN3[CSP4niv$LibN4==Lib4])
})

observe({
  Lib3 <- input$LibN3
  updateTextInput(session, "N3", value =  N3$N3[N3$LibN3==Lib3])
  updateTextInput(session, "LibN2", value =  CSP4niv$LibN2[CSP4niv$LibN3==Lib3])
})
observe({
  Lib2 <- input$LibN2
  updateTextInput(session, "N2", value =  N2$N2[N2$LibN2==Lib2])
  updateTextInput(session, "LibN1", value =  CSP4niv$LibN1[CSP4niv$LibN2==Lib2])
}) 
observe({
  Lib1 <- input$LibN1
  updateTextInput(session, "N1", value =  N1$N1[N1$LibN1==Lib1])
})



# input fields are treated as a group
formData <- reactive({
  sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
})



# Select row in table -> show details in inputs
observeEvent(input$RecodageProfessions_rows_selected, {
  if (length(input$RecodageProfessions_rows_selected) > 0) {
    data <- ReadData()[input$RecodageProfessions_rows_selected, ]
    UpdateInputs(data, session)
    
  }
  
})



observeEvent(input$submit, {
  if (input$IdRec != "0") {
    UpdateData(formData())
    data <- ReadData()[as.integer(input$IdRec)+1, ]
    UpdateInputs(data, session)
  # write.csv2(Recodage, "data/temp.csv", fileEncoding = "UTF-8", na = "", row.names = FALSE )
    
  #  write.csv2(RecodageProfessions, "RecodageProfessions.csv", row.names = F, fileEncoding = "UTF-8", na="")
  } else {
    CreateData(formData())
    data <- ReadData()[as.integer(input$IdRec)+1, ]
    UpdateInputs(CreateDefaultRecord(), session)
  }
}, priority = 1)

observeEvent(input$delete, {
  DeleteData(formData())
  UpdateInputs(CreateDefaultRecord(), session)
}, priority = 1)



output$Finale <- DT::renderDataTable({
  
  TableFinale()
  
}, server = FALSE, selection = "single",
extensions = c('Scroller','FixedColumns'), options = list(
  deferRender = TRUE ,
  scrollX = TRUE, scrollY= 300,
  fixedColumns = TRUE,  pageLength = 50, lengthMenu = c(5,10,20,50, 100), searching = TRUE)
)



# display table
output$RecodageProfessions <- DT::renderDataTable({
  
  #update after submit is clicked
  input$submit
  input$delete
  #update after delete is clicked
  ReadData()

}, server = FALSE, selection = "single",
colnames = unname(GetTableMetadata()$fields),
extensions = c('Scroller','FixedColumns'), options = list(
  deferRender = TRUE ,
  scrollX = TRUE, scrollY= 300,
  fixedColumns = TRUE,  pageLength = 50, lengthMenu = c(5,10,20,50, 100), searching = TRUE)
)



# Télécharger la table




output$DlTable <- downloadHandler(
  
  filename=function() {
    paste0("TableAvecPCS_", Sys.Date() ,".csv")
  },
  content = function(file) {
    
    TableFinale <- TableFinale()
 
    write.csv2(TableFinale, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
  }
)


output$SauvegardeRecod <- downloadHandler(
  
  filename=function() {
    paste0("RecodagePCS_", Sys.Date() ,".csv")
  },
  content = function(file) {
    
    
    write.csv2(Recodage, file, fileEncoding = "UTF-8", na = "", row.names = FALSE )
  }
)



  })



