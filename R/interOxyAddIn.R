#' @title Interactive add-in
#' @description Launches an addin for interactive selection of parameters for \code{makeOxygen}
#' @return Nothing. Inserts roxygen2 comments in a file opened in the source editor.
#' @author Anton Grishin
#' @details Add-in launches in the viewer panel of Rstudio. Select function's/dataset's name in the source editor. Choose parameters for \code{makeOxygen}. Click Insert. Select next object's name. Rinse.Repeat. Click Quit when done with the file.
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
  
  ui <- miniPage(
    gadgetTitleBar(textOutput("title", inline = TRUE),
                   left = miniTitleBarButton( "qt", "Quit"),
                   right = miniTitleBarButton(inputId = "insrt","Insert",
                                              primary = TRUE)),
    miniTabstripPanel(between = tags$p(),
                      miniTabPanel(h6("add_fields"), icon = icon("list-ul"),
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
          selected = c("examples", "details", "seealso", "export", "rdname")))),
  miniTabPanel(h6("more args"),
   icon = icon("option-horizontal", lib = "glyphicon"),
    miniContentPanel(padding = 25,
      fillPage(
        fillCol(flex = c(2,NA,1,NA,NA,NA,2), height = "100%",
                fillRow(flex = c(9,2,9),
                  radioButtons(inputId = "dflt", label = "add_default",
                               choices = list(TRUE, FALSE), selected = TRUE),
                  div(),
                  radioButtons(inputId = "print", label = "print",
                               choices = list(TRUE, FALSE), selected = FALSE)),
                
                hr(),
    div(actionLink("butt", "use_dictionary",
                   icon = icon("folder-open", "glyphicons")),
        textOutput("dictfile")),
    hr(style = "border-top: 3px solid #cccccc;"),
    tags$strong(h5(align = "center","Parameters passed to makeImport():")),
    br(),
    fillRow(sliderInput(inputId = "cut", label = "cut",value = 0,
                        min = 0,max = 20, step = 1, ticks = FALSE),
            div(),
            radioButtons("frmt", "format", list("oxygen","namespace","description"),
                         selected = "oxygen"), flex = c(9,2,9))
                      )))
                      )
    )
  )
  
  server <- function(input, output, session) {
    robj <- reactivePoll(1000, session,
                         checkFunc = rstudioapi::getSourceEditorContext,
                         valueFunc = function() {
                      rstudioapi::getSourceEditorContext()$selection[[1]]$text})
    
    observe({robj(); obj <- isolate(robj())
    
    if (nchar(obj) > 0 && is.null(get0(obj)) && search()[2] != "tmpenv") {
      showModal(modalDialog(title = paste(dQuote(obj), "not found!",
                      "Do you want to source", 
                      basename(rstudioapi::getSourceEditorContext()$path),
                      " file?"),
        footer = tagList(actionButton("no", "Cancel"), actionButton("ok","OK"))
      ))}
    })
    
    observeEvent(input$qt, {
      if (search()[2] == "tmpenv") detach("tmpenv"); stopApp()})
    
    observeEvent(input$no, stopApp())
    observeEvent(input$ok, {
      nenv <- attach(NULL, name = "tmpenv")
      sys.source(rstudioapi::getSourceEditorContext()$path, nenv,
                 keep.source = TRUE)
      removeModal()
    })
    
    rfile <- reactiveVal()
    observeEvent(input$butt, {hh <- choose.files()
    rfile(hh)})
    
    output$dictfile <- renderText({rfile()})
    output$title <- renderText({paste0("Select parameters in makeOxygen(\"",
                                       robj(),  "\"...)")})
    observeEvent(input$insrt, {
      if (nchar(robj()) == 0L ||
          (is.null(get0(robj())) && search()[2] == "tmpenv")) {
        showModal(modalDialog(
          tags$h4(style = "color: red;","Make valid object selection!"),
          size = "s")
        )
      } else {
        ctxt <- rstudioapi::getSourceEditorContext()
        params <- list(obj = robj(),
                       add_fields = input$fields,
                       add_default = input$dflt,
                       print = input$print,
                       use_dictionary = rfile(),
                       cut = input$cut,
                       format = input$frmt
        )
        ins_txt <- do.call(makeOxygen, params)
        rstudioapi::insertText(ctxt$selection[[c(1,1)]],
                               paste0(ins_txt, "\n",robj()),
                               id = ctxt$id)
      }})
  }
  
  runGadget(ui, server, viewer = paneViewer(minHeight = 450))
}
