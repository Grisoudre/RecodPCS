
library(shinythemes)
library(shinyjs)
library(DT)

# --------------------------- TITRE -------------------------------#

shinyUI(
  navbarPage("RecodPCS",id="tabSession",

             #--------------------------- ONGLET importation et filtres -------------------------#
             
             tabPanel("1. Importation du tableau de départ", value="panel1",
                      fluidPage(
                        tags$head(tags$style(HTML(
                          "h6{ background-color: #FFF3BE ; font-size:16px;
                          font-family: calibri, Arial, sans-serif ; font-size:16px;}")
                          #,type="text/css",
                          #          ".shiny-output-error { visibility: hidden; }",
                          #           ".shiny-output-error:before { visibility: hidden; }"
                        )
                        )

                        ,theme=shinytheme("simplex"),useShinyjs(),
 
  # options(encoding = "UTF-8"),
                        (column(12,h6('ATTENTION : Avant toute action,
                                    cliquer sur "Open in browser", sans quoi les
                                    boutons et téléchargements ne fonctionneront pas.'))),

                        column(5,h4("1. Table contenant la variable à recoder (.txt ou .csv)"),
                               h6("La table brute est une base de données dont les lignes correspondent aux individus statistiques.
                                  Elle doit être en format texte (.txt ou .csv) ; Le délimitateur,
                                  l'extension du fichier et l'encodage des caractères sont précisés.
                                  Elle est importée en passant par le bouton \"browse\"."),
                               wellPanel(
                                 uiOutput("donnees.fichier.ui"))
                               ),
                        column(7,
                               fluidRow(column(12,
                                               h4("2. Variable à recoder"),
                                               h6('Attention : Même en cas de reprise d\'une table de recodage,
                                                   choisir la variable "Profession"'),
                                               uiOutput("SelectProfBrut"),
                                               h4("3. Choix des modalités à recoder"),
                                               h6("Soit utilisation des modalités de la variable sélectionnée,
                                                   soit reprise d'une table de recodage en cours"),
                                               wellPanel(
                                                 actionButton("OK", "A. Commencer avec la variable sélectionnée")),
                                                 h4("OU BIEN"),
                                               wellPanel(
                                               uiOutput("donnees.fichier.ui2"),
                                               actionButton("OK2","B. Utiliser une table de recodage sauvergardée")),
                                               
                                               
                                               h4("4. Détail du tableau importé"),
                                               h6("Vérifier que le tableau a correctement été
                                                  importé à l'aide du résumé suivant :"),
                                               textOutput("Dimensions"),
                                               tableOutput("Resume")
                                               )
                               )
                               
                        ))),


             #--------------------------- ONGLET tris à plat / croisés -------------------------#

             tabPanel("2. Recodage des professions", value= "panel2",
                      fluidPage(
                        h4("Aperçu de la table de recodage"),
                        
                        fluidRow(column(11,
                                        DT::dataTableOutput("RecodageProfessions"))),
                        h4("Modalité à recoder"), 
                        fluidRow(
                          column(10,shinyjs::disabled(textInput("ProfBrutRec", "Profession à recoder", " ", width='100%')),offset = 0)
                          ,
                          column(1,shinyjs::disabled(textInput("IdRec", "Id.", " ")))
                        ),
                        
                        h4("Recherche des professions"),
                        fluidRow(
                          column(11, selectInput("IntituleInsee", "Support site Insee : Métiers compris",
                                                 choices=as.list(t(CSP4niv$IntituleInsee)), width='100%',  selected=NULL),offset = 0)
                        ),
                        
                        h4("Catégories Insee"),
                        fluidRow(
                          column(9, selectInput("LibN4", "Libellé niveau 4",
                                                choices=as.list(t(CSP4niv$LibN4)), width='100%',  selected=NULL),offset = 0),
                          column(2,shinyjs::disabled(textInput("N4", "Code niveau 4", "", width = '100%')),offset = 0)
                        ),
                        
                        fluidRow(
                          column(9,selectInput("LibN3", "Libellé niveau 3",
                                               choices=as.list(t(CSP4niv$LibN3)), width='100%',  selected=NULL),offset = 0),
                          column(2,shinyjs::disabled( textInput("N3", "Code niveau 3", "",width='100%')),offset = 0)
                        ),
                        
                        fluidRow(
                          column(9,selectInput("LibN2", "Libellé niveau 2",
                                               choices=as.list(t(CSP4niv$LibN2)), width='100%',  selected=NULL),offset = 0),
                          column(2, shinyjs::disabled(textInput("N2", "Code niveau 2", "",width='100%')),offset = 0)
                        ),
                        fluidRow(
                          column(9,selectInput("LibN1", "Libellé niveau 1",
                                               choices=as.list(t(CSP4niv$LibN1)), width='100%',  selected=NULL),offset = 0),
                          column(2,shinyjs::disabled( textInput("N1", "Code niveau 1", "",width='100%')),offset = 0)
                        ),
                        h4("Boutons"),
                        actionButton("submit", "Enregistrer + suivant"),
                        
                        h4("Sauvegarder la table de recodage - pour plus tard"),
                        downloadButton("SauvegardeRecod", "Télécharger la table de recodage"),
                        hr()
                        
                        
                        
                        
                        
                      )
             ),
             #--------------------------- Aperçu du tableau général avec recodages -------------------------#
             
             
             tabPanel("3. Aperçu du tableau général", value="panel3",
                      fluidPage(h4("Tableau"),
                                dataTableOutput("Finale"),
                                h4("Télécharger"),
                                downloadButton("DlTable", "Télécharger la table"),
                                hr()
                      )
             ),
             
             #--------------------------- SUPPRIMER messages d'erreur -------------------------#

             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }")



  )
)
