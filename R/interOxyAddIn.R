#' @title Interactive add-in
#' @description Launches an interactive addin for insertion of roxygen2 comments in files.
#' Allows selection of extra parameters for \code{makeOxygen}
#' @return Nothing. Inserts roxygen2 comments in a file opened in the source editor.
#' @author Anton Grishin
#' @details Open an .R file in Rstudio's source editor.
#' Launch the add-in via Addins -> interactiveOxygen or interOxyAddIn() in the console.
#' Add-in opens in the viewer panel.
#' Select function's/dataset's name in the source editor.
#' If objects cannot be found, the addin prompts to source the file.
#' Choose parameters for \code{makeOxygen}. Click Insert.
#' Select next object's name. Rinse.Repeat. Click Quit when done with the file.
#' @examples
#' interOxyAddIn() # launches interactive add-in, alternatively,
#' Rstudio menu Addins -> 'interactiveOxygen' 
#' @export 
#' @rdname interOxyAddIn
#' @seealso \code{View(sinew:::oxygenAddin)}
#' @import rstudioapi
#' @import shiny
#' @import miniUI
interOxyAddIn <- function() {
  on.exit(detach("interOxyEnvir"))
  ui <- miniPage(
    gadgetTitleBar(textOutput("title", inline = TRUE),
                   left = miniTitleBarButton( "qt", "Quit"),
                   right = miniTitleBarButton(inputId = "insrt","Insert",
                                              primary = TRUE)),
                 miniContentPanel(
                         checkboxGroupInput(inputId = "fields",
                                            label = "include in add_fields:",
                                            choices = list("author",
                                                           "backref",
                                                           "concept",
                                                           "describeIn",
                                                           "details",
                                                           "example",
                                                           "examples",
                                                           "export",
                                                           "family",
                                                           "field",
                                                           "format",
                                                           "importClassesFrom",
                                                           "importMethodsFrom",
                                                           "include",
                                                           "inherit",
                                                           "inheritDotParams",
                                                           "inheritSection",
                                                           "keywords",
                                                           "name",
                                                           "rdname",
                                                           "references",
                                                           "section",
                                                           "slot",
                                                           "seealso",
                                                           "source",
                                                           "template",
                                                           "templateVar",
                                                           "useDynLib"),
          selected = c("examples", "details", "seealso", "export", "rdname")),
      hr(style = "border-top: 3px solid #cccccc;"),
      sliderInput(inputId = "cut", label = "cut", value = 0,
                          min = 0, max = 20, step = 1, ticks = FALSE),
      br(),
      uiOutput("cutslider"),
      br()
    )
    )
  
  server <- function(input, output, session) {
    
    output$cutslider <- renderUI({if (dir.exists("./man-roxygen")) {
      div(div(actionLink("butt", "use_dictionary",
                      icon = icon("folder-open", "glyphicons")),
                                      textOutput("dictfile")), hr())
      } else {p()}
      })
    
    robj <- reactivePoll(1000, session,
                         checkFunc = rstudioapi::getActiveDocumentContext,
                         valueFunc = rstudioapi::getActiveDocumentContext
                         )
    
    observeEvent(robj(), {
                          path <- robj()$path
                          obj <- robj()$selection[[1]]$text
    
    if (!nzchar(path)) {
      showModal(modalDialog(
       title = HTML(paste0("Open an .R file in the source editor and ",
                           "<strong><u>select</u></strong> object's name!")),
        easyClose = TRUE)
                )
    }                      
    
    if (nzchar(obj) && is.null(get0(obj)) && !"interOxyEnvir" %in% search()) {
      showModal(modalDialog(title = paste(dQuote(obj), "not found!",
                      "Do you want to source", 
                      basename(rstudioapi::getSourceEditorContext()$path),
                      " file or quit add-in?"),
        footer = tagList(actionButton("no", "Quit Add-in"),
                         actionButton("ok","Source"))
      ))}
    
    })
    
    observeEvent(input$qt, {
      if ("interOxyEnvir" %in% search()) detach("interOxyEnvir"); stopApp()})
    
    observeEvent(input$no, stopApp())
    observeEvent(input$ok, {
      nenv <- attach(NULL, name = "interOxyEnvir")
      sys.source(rstudioapi::getSourceEditorContext()$path, nenv,
                 keep.source = TRUE)
      removeModal()
    })
    
    rfile <- reactiveVal()
    observeEvent(input$butt, {
      hh <- NULL
      try(hh <- file.choose(), silent = TRUE)
    rfile(hh)})
    
    output$dictfile <- renderText({rfile()})
    output$title <- renderText({paste0("Select parameters in makeOxygen(\"",
                                       robj()$selection[[1]]$text,  "\"...)")})
    observeEvent(input$insrt, {
      obj <- robj()$selection[[1]]$text
      if (!nzchar(obj) ||
          (is.null(get0(obj)) && "interOxyEnvir" %in% search())) {
        showModal(modalDialog(
          tags$h4(style = "color: red;","Make valid object selection!"),
          size = "s", easyClose = TRUE)
        )
      } else {
        ctxt <- rstudioapi::getSourceEditorContext()
        params <- list(obj = obj,
                       add_fields = input$fields,
                       add_default = TRUE,
                       print = FALSE,
                       use_dictionary = rfile(),
                       cut = input$cut
        )
        ins_txt <- do.call(makeOxygen, params)
        rstudioapi::insertText(ctxt$selection[[c(1,1)]],
                               paste0(ins_txt, "\n",obj),
                               id = ctxt$id)
      }})
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = 450))
}
