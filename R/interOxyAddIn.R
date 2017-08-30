#' @title Interactive add-in
#' @description Launches an interactive addin for insertion of roxygen2 comments in files.
#' Allows selection of extra parameters for \code{makeOxygen}
#' @return Nothing. Inserts roxygen2 comments in a file opened in the source editor.
#' @author Anton Grishin, Jonathan Sidi
#' @details Open an .R file in Rstudio's source editor.
#' Launch the add-in via Addins -> interactiveOxygen or interOxyAddIn() in the console.
#' Add-in opens in the viewer panel.
#' Select function's/dataset's name in the source editor.
#' If objects cannot be found, the addin prompts to source the file.
#' Choose parameters for \code{makeOxygen}. Click Insert.
#' Select next object's name. Rinse.Repeat. Click Quit when done with the file.
#' @examples
#' if(interactive()) interOxyAddIn()
#' @export 
#' @rdname interOxyAddIn
#' @seealso \code{View(sinew:::oxygenAddin)}
#' @import rstudioapi
#' @import shiny
#' @import miniUI
#' @importFrom utils find
interOxyAddIn <- function() {
  
  nenv <- new.env()
  
  tweaks <- 
    list(shiny::tags$head(shiny::tags$style(shiny::HTML("
                                                        .multicol { 
                                                        height: 300px;
                                                        -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                                        -moz-column-count: 3;    /* Firefox */ 
                                                        column-count: 3; 
                                                        -moz-column-fill: auto;
                                                        -column-fill: auto;
                                                        } 
                                                        ")) 
    ))

  controls<-list(shiny::tags$h3("Select Fields to add to Oxygen Output"),
                 shiny::tags$div(align = 'left', 
                                 class = 'multicol',
                                 shiny::checkboxGroupInput(inputId = "fields",
                                                           label = '',
                                                           choices = c(names(sinew_opts$get())[-1],'seealso'),
                                                           selected = sinew_opts$get('add_fields')))
  )
  
  ui <- miniUI::miniPage(
    tweaks,
    miniUI::gadgetTitleBar(shiny::textOutput("title", inline = TRUE),
                           left = miniUI::miniTitleBarButton( "qt", "Quit"),
                           right = miniUI::miniTitleBarButton(inputId = "insrt","Insert",
                                                              primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(sidebarPanel = shiny::sidebarPanel(
        shiny::radioButtons(inputId = 'action',label = 'Action',
                            choices = c('Create','Update'),
                            selected = 'Create',inline = TRUE),
        controls,
        shiny::tags$hr(style = "border-top: 3px solid #cccccc;"),
        shiny::sliderInput(inputId = "cut", label = "cut", value = 0,
                           min = 0, max = 20, step = 1, ticks = FALSE),
        shiny::br(),
        shiny::uiOutput("cutslider"),
        shiny::br(),width = 5 
      ),
      mainPanel = shiny::mainPanel(
        shiny::verbatimTextOutput('preview'),width=7
      ))
    )
  )
  
  server <- function(input, output, session) {
    
    output$title <- shiny::renderText({paste0("Select parameters in makeOxygen(\"",robj()$selection[[1]]$text,  "\"...)")})
    shiny::observeEvent(input$no, shiny::stopApp())
    rfile <- shiny::reactiveVal()
    output$dictfile <- shiny::renderText({rfile()})
    
    output$cutslider <- shiny::renderUI({if (dir.exists("./man-roxygen")) {
      shiny::div(shiny::div(actionLink("butt", "use_dictionary",
                                       icon = icon("folder-open", "glyphicons")),
                            shiny::textOutput("dictfile")), hr())
    } else {shiny::p()}
    })
    
    robj <- shiny::reactivePoll(1000, session,
                                checkFunc = rstudioapi::getActiveDocumentContext,
                                valueFunc = rstudioapi::getActiveDocumentContext
    )
    
    shiny::observeEvent(robj(), {
      path <- robj()$path
      obj <- robj()$selection[[1]]$text
      
      searchp <- any(grepl(obj,unlist(sapply(search(),function(x) ls(x)))))

      if (!nzchar(path)&!searchp) {
        showModal(modalDialog(
          title = HTML(paste0("Open an .R file in the source editor and ",
                              "<strong><u>select</u></strong> object's name!")),
          easyClose = TRUE)
        )
      }
      
      if (nzchar(obj) && is.null(get0(obj))) {
        output$preview<-shiny::renderText({
          paste(dQuote(obj), "not found! Trying to find source")
        })
        sys.source(rstudioapi::getSourceEditorContext()$path, nenv,keep.source = TRUE)
        }
      
    })
    
    shiny::observeEvent(input$qt, {
      shiny::stopApp()
      })
    
    shiny::observeEvent(input$ok, {
      sys.source(rstudioapi::getSourceEditorContext()$path, nenv,
                 keep.source = TRUE)
      shiny::removeModal()
    })
    
    shiny::observeEvent(input$butt, {
      hh <- NULL
      try(hh <- file.choose(), silent = TRUE)
      rfile(hh)})
    
    shiny::observeEvent(input$insrt, {
      obj <- robj()$selection[[1]]$text
      if (!nzchar(obj) ||
          (is.null(get0(obj)) && "nenv" %in% ls())) {
        shiny::showModal(modalDialog(
          shiny::tags$h4(style = "color: red;","Make valid object selection!"),
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
        ins_txt <- do.call(sinew::makeOxygen, params)
        rstudioapi::insertText(ctxt$selection[[c(1,1)]],paste0(ins_txt, "\n",obj),id = ctxt$id)
      }})
    
    shiny::observeEvent(c(input$action,robj()),{
      l <- readLines(robj()$path,warn = FALSE)
      oxy_current <- paste0(grep("^#'",l,value=TRUE),collapse = '\n')
      new_fields<-switch(input$action,Update={names(get_oxy(oxy_current))},Create={sinew_opts$get('add_fields')})
      shiny::updateCheckboxGroupInput(session=session,inputId = "fields",selected=new_fields)
    })
    
    shiny::observeEvent(list(input$action,input$fields,robj(),input$cut,input$ok),{
      switch(input$action,
             Update={ 
               
               obj_name <- robj()$selection[[1]]$text

               params <- 
                 list(
                   path=robj()$path,
                   add_fields = input$fields,
                   add_default = TRUE,
                   dry.run=FALSE,
                   use_dictionary = rfile(),
                   cut = input$cut
                 )
               
               output$preview<-shiny::renderText({
                 if(nchar(params$path)>0){
                   if(length(utils::find(obj_name,mode = 'function'))>0){
                     x<-do.call(moga, params) 
                     paste(x,collapse = '\n')  
                   }
                 }
               })
             },
             Create={

              obj_name <- robj()$selection[[1]]$text
              
               output$preview<-shiny::renderText({
                 if(length(obj_name)>0){
                   test <- any(grepl(obj_name,ls(envir = nenv)))
                   if(test){
                     assign(obj_name,get(obj_name,envir = nenv))
                     eval(parse(text=sprintf('makeOxygen(%s,add_fields = input$fields,print=FALSE,cut=input$cut)',obj_name)))
                   }else{
                     if(length(find(obj_name,mode = 'function'))>0)
                       eval(parse(text=sprintf('makeOxygen(%s,add_fields = input$fields,print=FALSE,cut=input$cut)',obj_name)))                     
                   }
                 }
               })
             })
    })  
  }
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
  }