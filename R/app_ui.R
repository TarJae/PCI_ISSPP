#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shinyjs
#' @import shiny
#' @import shinyWidgets
#' @import dplyr
#' @import tidyr
#' @importFrom DT dataTableOutput
#' @importFrom DT renderDataTable
#' @importFrom shinyWidgets alert
#' @importFrom runExample
#' @importFrom shiny actionButton tabsetPanel column
#' @noRd


# js to print screen
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

# my functions later own file
linebreaks <- function(n){HTML(strrep(br(), n))}
######
my_ids <- c("central", "right_upper", "epigastrium", "left_upper", "left_flank", "left_lower", "pelvis",
            "right_lower", "right_flank", "upper_jejunum", "lower_jejunum", "upper_ileum", "lower_ileum")

my_labels <- c("Central", "Right upper", "Epi gastrium",
               "Left upper", "Left flank", "Left lower",
               "Pelvis", "Right lower",	"Right flank",
               "Upper jejunum",	"Lower jejunum", "Upper ileum", "Lower ileum")


app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    fluidPage(
      useShinyjs(),

      tags$style(HTML("
  @media print {
    @page {
      size: landscape; /* Set the page orientation to landscape */
      color: auto; /* Ensure that colors are printed (although this is often the default setting) */
      transform: scale(0.9); /* Scale the content to fit the page better, reducing it to 90% of its original size */
    }
    body * {
      visibility: hidden; /* Hide all elements in the body by default to prevent them from printing */
    }
    #printArea, #printArea * {
      visibility: visible; /* Make only the content within the element with id='printArea' visible */
    }
    #printArea {
      position: absolute; /* Position the #printArea element absolutely relative to its nearest positioned ancestor or to the initial containing block */
      left: 0; /* Align it to the left edge of the page */
      top: 0; /* Align it to the top edge of the page */
      width: 100%; /* Stretch it to cover the full width of the page */
    }
  }
")),



      extendShinyjs(text = jsCode, functions = c("winprint")),

div(id = "printArea",  # This div contains the content you want to print

          #  css for button 0 to 3 #a2ff45 -> green
          tags$style(
            ".btn-zero {background-color: white; color: black;}",
            ".btn-zero.active {background-color: #a2ff45; color: black;}",
            ".btn-one {background-color: white; color: black;}",
            ".btn-one.active {background-color: #a2ff45; color: black;}",
            ".btn-two {background-color: white; color: black;}",
            ".btn-two.active {background-color: #a2ff45; color: black;}",
            ".btn-three {background-color: white; color: black;}",
            ".btn-three.active {background-color: #a2ff45; color: black;}"
          ),


          # positioning the widgets and avoiding overriding
          tags$head(
            tags$style(HTML("
      #container1 > .form-group {
        height: 25px;
        margin-bottom: 5px;
        font-size:25px
      }"))
          ),

      # Rechteck in dem die Summe PCI ist
      #background image living in init/app/www
      tags$img(
        src = "www/pci_score_transparent_withouttext1.png",
        #  src = "www/pci_score_transparent.png",
        style = 'position: absolute; position: absolute;
     width: 1024px; height: 768px;'
      ),

     # Zahl Summe PCI
     # PCI text
     div(style="position: absolute;left: 560px; top: 635px;",
         textOutput('txt_pci'),
     ),
     tags$head(tags$style("#txt_pci{color: red;
                                 font-size: 50px;
                                 text-align: right;
                                  font-weight: bold;
                                 }")),


     div(id = "container2",
         style="position: absolute;left: 100px; top: 15px; display: inline-block;vertical-align:middle; width: 300px;",
         dateInput("date_pci", "Date:", value = Sys.Date(), format = "dd.mm.yyyy", width = 200),
         textInput("pat_id", label="Patient AZL (ID)", width=200)
     ),


     div(id = "container3",
         style="position: absolute;left: 350px; top: 15px; display: inline-block;vertical-align:middle; width: 300px;",
         selectInput("procedure","Procedure", choices = c("HIPEC", "PIPAC", "HITOC", "PITAC", "CRS", "Exploration")),
         numericInput("ascites", label="Ascites [ml]", value=0, width = 200)
     ),

     div(id = "container4",
         style="position: absolute;left: 700px; top: 15px; display: inline-block;vertical-align:middle; width: 300px;",
         conditionalPanel(
           condition = "input.procedure ==  'HIPEC' || input.procedure == 'HITOC' || input.procedure == 'CRS' ||
            input.procedure == 'Exploration'",
           selectInput("CCS","Completeness of Cytoreduction", choices = c("n/a",
                                                                          "CC-0",
                                                                          "CC-1",
                                                                          "CC-2",
                                                                          "CC-3")),
           selectInput("sodium_thiosulfate","Sodium thiosulfate used?", choices = c("n/a", "yes", "no"))
         )
     ),

     div(id = "container5",
         style="position: absolute;left: 700px; top: 15px; display: inline-block;vertical-align:middle; width: 300px;",
         conditionalPanel(
           condition = "input.procedure ==  'PIPAC' || input.procedure == 'PITAC'",
           numericInput("flowrate", label="Flow rate", value=0, width = 200),
           selectInput("pen","Used Pen", choices = c("n/a", "Capnopharm", "Topol", "Capnomed", "Other"))
         )
     ),


     # Regions
     div(id = "container_text_region",
         style="position: absolute;left: 370px; top: 160px; display: inline-block;vertical-align:middle; width: 300px;",
         tags$h3(style="text-decoration:underline; font-weight:bold;", "Regions"),
         h4("0   Central"),
         h4("1   Right upper"),
         h4("2   Epigastrium"),
         h4("3   Left upper"),
         h4("4   Left flank"),
         h4("5   Left lower"),
         h4("6   Pelvis"),
         h4("7   Right lower"),
         h4("8   Right flank"),
         h4("9   Upper jejunum"),
         h4("10  Lower jejunum"),
         h4("11  Upper ileum"),
         h4("12  Lower ileum"),
     ),

     # PCI text
     div(id = "container_text_region",
         style="position: absolute;left: 370px; top: 630px; display: inline-block;vertical-align:middle; width: 300px;",
         tags$h1(style="font-weight:bold;", "PCI")
     ),

     div(id = "container_Lesion_size_score",
         style="position: absolute;left: 720px; top: 160px; display: inline-block;vertical-align:middle; width: 300px;",
         tags$h3(style="text-decoration:underline; font-weight:bold;", "Lesions size score"),
         h4("LS 0:   No tumor seen"),
         h4("LS 1:   Tumor up to 0.5 cm"),
         h4("LS 2:   Tumor up to 5.0 cm"),
         h4("LS 3:   Tumor > 5.0 cm", tags$br(),
            "or confluence")
     ),


     div(id = "container1_title",
         style="position: absolute;left: 520px; top: 160px; display: inline-block;vertical-align:middle; width: 300px;",
         tags$h3(style="text-decoration:underline; font-weight:bold;", "Lesion size")
     ),


     div(id = "container1",
         style="position: absolute;left: 520px; top: 170px; display: inline-block;vertical-align:middle; width: 300px;",
         radioGroupButtons(inputId = my_ids[1], label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[2], label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[3],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[4],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[5],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[6],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[7],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[8],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[9],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[10],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[11],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[12],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
         radioGroupButtons(inputId = my_ids[13],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs')
     ),


), # End of This div contains the content you want to print


     # buy me a coffee
     div(style = 'position: absolute;left: 350px; top:935px;',
         HTML('<a href="https://www.buymeacoffee.com/tarkanjaeger" target="_blank">
               <img src="https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg"
               alt="Buy me a coffee"></a>')
     ),

     # Info buy me a coffee

     div(style = 'position: absolute;left: 30px; top:955px;',
         HTML("<p>Support Tarkan J\u00E4ger fighting abdominal cancer</p>")
     ),


     # button print
     div(style="position: absolute;left: 30px; top: 660px;",
         tags$head(
           tags$style(HTML('#print{background-color:gold}'))
         ),
         actionButton("print", "PRINT"),
     ),
     div(style="position: absolute;left: 640px; top: 660px;",
         tags$head(
           tags$style(HTML('#update{background-color:orange}'))
         ),
         actionButton(inputId = "update", label = "Reset"),
     ),




     # Output: Table summarizing the values entered ----
     div(style = 'position: absolute;left: 30px; top:795px; width:950px;margin:auto', DT::DTOutput("pci_table")),


     div(style="position: absolute;left: 800px; top: 940px;",
         class = "footer",
         includeHTML(system.file("app/www/footer.Rhtml", package = "PCI"))
     )


     # eventreactive button show
     #div(style = 'position: absolute;left: 30px; top:750px; width:950px;margin:auto', actionButton(inputId = "show", label = "Show Data"))

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PCI"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
