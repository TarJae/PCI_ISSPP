#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {


  output$btn <- downloadHandler(

    filename = function(){"myreport.docx"},
    content = function(file) {

      tempReport <- file.path(tempdir(),"markdown.Rmd")
      file.copy("markdown.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render("markdown.Rmd", output_format = "word_document", output_file = file,
                        params = list(table = data()), # here I'm passing data in params
                        envir = new.env(parent = globalenv()),clean=F,encoding="utf-8"
      )


    }
  )



  # print
  observeEvent(input$print, {
    js$winprint()
  })

  #reset pat_id and  radioGroupButtons

  observeEvent(input$update, {
    updateTextInput(session = session, inputId = "pat_id", value = "")
    updateRadioGroupButtons(session = session, inputId = my_ids[1], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[2], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[3], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[4], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[5], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[6], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[7], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[8], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[9], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[10], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[11], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[12], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[13], selected = 0)
  }, ignoreInit = TRUE)

  #PCI output
  output$txt_pci <- renderText({ paste(sum(sapply(my_ids, function(x) as.numeric(input[[x]]))))
  })


  # shortened columns names vector:

  my_short_names <- paste0(rep("AR_", 13), 0:12)

  # Reactive expression to create data frame of all input values ----
  pci_data <- reactive({
    #browser()
    df <- data.frame(
      Name = my_labels,
      Value = as.character(sapply(my_ids, function(x) input[[x]])),
      stringsAsFactors = FALSE) %>%
      pivot_wider(names_from = Name, values_from = Value) %>%
      mutate(ID = input$pat_id,
             Date = as.character(input$date_pci), .before=1,
             Procedure = input$procedure,
             Ascites = input$ascites,
             CCS = input$CCS,
             `Sodium thiosulfate` = input$sodium_thiosulfate,
             `Flow rate` = input$flowrate,
             Pen = input$pen
      ) %>%
      rename_with(.fn = ~my_short_names, .cols = Central:`Lower ileum`) %>%
      mutate(PCI = sum(sapply(my_ids, function(x) as.integer(input[[x]]))
      )
      )
    return(df)
  })

  # Show the values in an HTML table in a wider format----
  output$pci_table <- DT::renderDT({
    DT::datatable(rownames = FALSE, pci_data(),
                  extensions = c('Buttons'),
                  options = list(
                    scrollX=FALSE,
                    info = FALSE,
                    bPaginate = FALSE,
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#94b659', 'color': '#fff'});",
                      "}"),
                    dom = 'Bfrtip',
                    buttons = list(list(extend = 'excel',
                                        text = 'Save table to excel',
                                        filename = paste0("pci-","id-",input$pat_id, "-", Sys.Date()))
                    ),
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '20px', targets = "_all")),
                    searching = FALSE,
                    ordering = FALSE
                  )
    )

  })
}
