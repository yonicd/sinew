#' @title Interactive add-in
#' @description Launches an interactive addin for insertion of roxygen2 comments in files.
#' Allows selection of extra parameters for [makeOxygen][sinew::makeOxygen]
#' @return Nothing. Inserts roxygen2 comments in a file opened in the source editor.
#' @author Anton Grishin, Jonathan Sidi
#' @details Open an .R file in Rstudio's source editor.
#' 
#'  This addin requires `shiny` and `miniUI` to be installed (listed as Suggests in Description)
#' 
#'   - Launch the add-in via Addins -> interactiveOxygen or interOxyAddIn() in the console.
#'     - Add-in opens in the viewer panel.
#'   - Select function's/dataset's name in the source editor.
#'     - If objects cannot be found, the addin prompts to source the file.
#'     - Choose parameters for [makeOxygen][sinew::makeOxygen]
#'       - Click Insert
#'   - Select next object's name
#'   - Rinse/Repeat
#'   - Click Quit when done with the file.
#' @examples
#' if(interactive()) interOxyAddIn()
#' @export
#' @rdname interOxyAddIn
#' @import rstudioapi
#' @importFrom utils find
#' @concept interactive
interOxyAddIn <- function() {
  
  if(!try(requireNamespace('shiny',quietly = TRUE)))
    stop('Shiny must be installed to use this addin')
  
  if(!try(requireNamespace('miniUI',quietly = TRUE)))
    stop('miniUI must be installed to use this addin')
  
  nenv <- new.env()

  # Define checkbox layout ----
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
                                                        "))))

  controls <- list(
    shiny::tags$h3("Select Fields to add to Oxygen Output"),
    shiny::tags$div(
      align = "left",
      class = "multicol",
      shiny::checkboxGroupInput(
        inputId = "fields",
        label = "",
        choices = c(names(sinew_opts$get())[-1], "seealso"),
        selected = sinew_opts$get("add_fields")
      )
    )
  )

  # gadget UI ----
  ui <- miniUI::miniPage(
    tweaks,
    miniUI::gadgetTitleBar(
      shiny::textOutput("title", inline = TRUE),
      left = miniUI::miniTitleBarButton("qt", "Quit"),
      right = miniUI::miniTitleBarButton(
        inputId = "insrt", "Insert",
        primary = TRUE
      )
    ),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shiny::radioButtons(
            inputId = "action", label = "Action",
            choices = c("Create", "Update"),
            selected = "Create", inline = TRUE
          ),
          controls,
          shiny::tags$hr(style = "border-top: 3px solid #cccccc;"),
          shiny::sliderInput(
            inputId = "cut", label = "cut", value = 0,
            min = 0, max = 20, step = 1, ticks = FALSE
          ),
          shiny::br(),
          shiny::uiOutput("cutslider"),
          shiny::br(), width = 5
        ),
        mainPanel = shiny::mainPanel(
          shiny::verbatimTextOutput("preview"), width = 7
        )
      )
    )
  )

  # gadget Server -----
  server <- function(input, output, session) {

    # reactive ui's and helper objects ----
    output$title <- shiny::renderText({
      paste0("Select parameters in makeOxygen(\"", robj()$selection[[1]]$text, "\"...)")
    })
    shiny::observeEvent(input$no, shiny::stopApp())
    rfile <- shiny::reactiveVal()
    output$dictfile <- shiny::renderText({
      rfile()
    })

    output$cutslider <- shiny::renderUI({
      if (dir.exists("./man-roxygen")) {
        shiny::div(shiny::div(
          shiny::actionLink(
            "butt", "use_dictionary",
            icon = shiny::icon("folder-open", "glyphicons")
          ),
          shiny::textOutput("dictfile")
        ), shiny::hr())
      } else {
        shiny::p()
      }
    })

    shiny::observeEvent(input$qt, {
      shiny::stopApp()
    })

    shiny::observeEvent(input$ok, {
      sys.source(
        rstudioapi::getSourceEditorContext()$path,
        nenv,
        keep.source = TRUE
      )

      shiny::removeModal()
    })

    shiny::observeEvent(input$butt, {
      hh <- NULL
      try(hh <- file.choose(), silent = TRUE)
      rfile(hh)
    })

    # Polling ----
    robj <- shiny::reactivePoll(
      1000, session,
      checkFunc = rstudioapi::getActiveDocumentContext,
      valueFunc = function() {
        this <- rstudioapi::getActiveDocumentContext()
        obj <- this$selection[[1]]$text

        if (grepl("::", obj)) {
          check_attach(obj, nenv)
          obj <- gsub("^(.*?)::", "", obj)
        }

        this$selection[[1]]$text <- obj

        return(this)
      }
    )

    # Lookup editor text for available objects ----
    shiny::observeEvent(robj(), {
      path <- robj()$path
      obj <- robj()$selection[[1]]$text

      if (!nzchar(path)) {
        td <- file.path(tempdir(), "_sinew")

        if (!dir.exists(td)) {
          dir.create(td)
        }

        path <- file.path(td, "_tempsrc.R")

        cat(robj()$contents, file = path, sep = "\n")

        untangle(file = path, dir.out = td, keep.body = FALSE)

        FILES <- list.files(td, full.names = TRUE)

        FILES <- FILES[-grep("\\_tempsrc\\.R", FILES)]

        sapply(FILES, function(idx) sys.source(idx, envir = nenv, keep.source = TRUE))
      } else {
        sys.source(path, nenv, keep.source = TRUE)
      }

      search.env <- unlist(c(sapply(search(), function(x) ls(x)), ls(envir = nenv)))

      searchp <- any(grepl(obj, search.env))

      if (!searchp || !nzchar(obj)) {
        shiny::showModal(shiny::modalDialog(
          title = shiny::HTML(paste0(
            "Open an .R file in the source editor and ",
            "<strong><u>select</u></strong> object's name!"
          )),
          easyClose = TRUE
        ))
      }

    })

    # Insert new content above highlighted text ----
    shiny::observeEvent(input$insrt, {
      rm.oxylines(this = robj())

      obj_name <- robj()$selection[[1]]$text

      if (!nzchar(obj_name) || (is.null(get0(obj_name)) && "nenv" %in% ls())) {
        shiny::showModal(shiny::modalDialog(
          shiny::tags$h4(style = "color: red;", "Make valid object selection!"),
          size = "s", easyClose = TRUE
        ))
      } else {
        ctxt <- rstudioapi::getSourceEditorContext()

        ins_txt <- ""

        # if (nzchar(obj_name) && is.null(get0(obj_name))) {
        test <- any(grepl(obj_name, ls(envir = nenv)))
        if (test) {
          assign(obj_name, get(obj_name, envir = nenv))
          eval(parse(text = sprintf("ins_txt <- makeOxygen(%s,add_fields = input$fields,print=FALSE,cut=input$cut)", obj_name), keep.source = TRUE))
        } else {
          if (length(find(obj_name, mode = "function")) > 0) {
            eval(parse(text = sprintf("ins_txt <- makeOxygen(%s,add_fields = input$fields,print=FALSE,cut=input$cut)", obj_name), keep.source = TRUE))
          }
        }
        # }

        rstudioapi::insertText(ctxt$selection[[c(1, 1)]], paste0(ins_txt, "\n", obj_name), id = ctxt$id)

        new_ctxt <- rstudioapi::getSourceEditorContext()
        new_ctxt$selection[[1]]$range[[1]][[2]] <- ctxt$selection[[1]]$range[[1]][[2]]
        new_ctxt$selection[[1]]$range[[2]][[2]] <- ctxt$selection[[1]]$range[[2]][[2]]
        rstudioapi::setSelectionRanges(new_ctxt$selection[[1]]$range, id = new_ctxt$id)
      }
    })

    # read current fields ----
    shiny::observeEvent(c(input$action, robj()), {
      # l <- readLines(robj()$path,warn = FALSE)
      # oxy_current <- paste0(grep("^#'",l,value=TRUE),collapse = '\n')
      new_fields <- switch(input$action, Update = {
        td <- file.path(tempdir(), "_sinew")

        if (!dir.exists(td)) {
          dir.create(td)
        }

        untangle(file = robj()$path, dir.out = td)

        tf <- file.path(tempdir(), "_sinew", sprintf("%s.R", robj()$selection[[1]]$text))
        if (file.exists(tf)) {
          l <- readLines(tf, warn = FALSE)
          oxy_current <- paste0(grep("^#'", l, value = TRUE), collapse = "\n")
          unique(c(names(get_oxy(oxy_current)), sinew_opts$get("add_fields")))
        } else {
          sinew_opts$get("add_fields")
        }
      },
      Create = {
        sinew_opts$get("add_fields")
      }
      )
      shiny::updateCheckboxGroupInput(session = session, inputId = "fields", selected = new_fields)
    })

    # Create/Update ----
    shiny::observeEvent(list(input$action, input$fields, robj(), input$cut, input$ok), {
      switch(input$action,
        Update = {
          obj_name <- robj()$selection[[1]]$text
          tf <- file.path(tempdir(), "_sinew", sprintf("%s.R", obj_name))

          params <-
            list(
              path = switch(file.exists(tf), tf, robj()$path),
              add_fields = input$fields,
              add_default = TRUE,
              dry.run = FALSE,
              use_dictionary = rfile(),
              force.fields = setdiff(c(names(sinew_opts$get())[-1], "seealso"), input$fields),
              cut = input$cut
            )

          output$preview <- shiny::renderText({
            if (length(params$path) > 0) {
              if (nzchar(params$path)) {
                # if(length(utils::find(obj_name,mode = 'function'))>0){
                x <- do.call(moga, params)
                paste(x, collapse = "\n")
                # }
              }
            }
          })
        },
        Create = {
          obj_name <- robj()$selection[[1]]$text

          output$preview <- shiny::renderText({
            if (nzchar(obj_name)) {
              test <- any(grepl(obj_name, ls(envir = nenv)))
              if (test) {
                assign(obj_name, get(obj_name, envir = nenv))
                eval(parse(text = sprintf("makeOxygen(%s,add_fields = input$fields,print=FALSE,cut=input$cut)", obj_name), keep.source = TRUE))
              } else {
                if (length(find(obj_name, mode = "function")) > 0) {
                  eval(parse(text = sprintf("makeOxygen(%s,add_fields = input$fields,print=FALSE,cut=input$cut)", obj_name), keep.source = TRUE))
                }
              }
            }
          })
        }
      )
    })
  }

  # Run Gadget ----
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
  on.exit({
    junk <- sapply(nenv$toUnload, detach, unload = TRUE, character.only = TRUE)
  }, add = TRUE)
}
