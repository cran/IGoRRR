go <- function(envir=.GlobalEnv, examples=TRUE) {

  # old <- options() 
  # Following doesn't work : it restores options before the start of the application
  # on.exit(options(old)) 
  
  init(envir,examples) # libraries and global data

  # Enable access to images from html
  shiny::addResourcePath(prefix = "images",
                         directoryPath = system.file("images", package="IGoRRR"))

# UI ----------------------------------------------------------------------

  ui <- dashboardPage(
    skin = "blue",

    header = dashboardHeader(
      title = "I Go R"
      ,tags$li(class = "dropdown", em(.IGoR$Z$version))
      ,tags$li(tags$a(
                 href = 'http://www.insee.fr',
                 img(src = "images/logo_insee.png",
                     title = "insee.fr", height = "46px"),
                 style = "padding-top:2px; padding-bottom:2px;"),
               class = "dropdown")
    ),

    sidebar = dashboardSidebar(
      tags$head(
        tags$style(HTML("
                      .main-sidebar {
                      font-family : Arial;
                      }
                      .content-wrapper {
                      background-color: linen;
                      font-family : Arial;
                      }
                      "))),
      imageOutput("main.igor",height='128px'),
      tags$div(id = "loading", tags$script('$("#loading").hide()')),
      uiOutput("main.data"),
      do.call(sidebarMenu,
        append(list(id = "menu", width = "400"),
          map2(unname(.IGoR$config$menus),names(.IGoR$config$menus),
               function(l,n)
                 do.call(menuItem,
                   append(.IGoR$Z$dashboard[[n]],
                          map(l, function(x) menuSubItem(.IGoR$Z[[x]]$menu.title,tabName=x))
      ) ) )      )       )
    ),

    dashboardBody(
      div(id = "form",
        tags$script(
          'function checkifrunning() {
          var is_running = $("html").attr("class").includes("shiny-busy");
          if (is_running){
          $("#loading").show()
          } else {
          $("#loading").hide()
          }
          };
          setInterval(checkifrunning, 1000)'
        ),
        tags$style(
          "#loading {
          display: inline-block;
          border: 3px solid #f3f3f3;
          border-top: 3px solid #3498db;
          border-radius: 50%;
          width: 50px;
          height: 50px;
          animation: spin 1s ease-in-out infinite;
          }

          @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
          }"
        ),
        # Launch the user interfaces for all the pages present in config
        do.call(tabItems,
         map(unlist(.IGoR$config$menus, use.names=FALSE),
             function(x) tabItem(x, (get(paste0("page_",x))$ui)())
   ) ) ) )
  )

# Server ------------------------------------------------------------------

  server <- function(input, output,session) {
    
    # Remember the current settings
    old <- options()
    
    # The width of the display of tibble previews (should be in config?)
    options(width=200)

    # on.exit(options(old)) activates too soon
    # following code works, even if session is killed from RGUI
    session$onSessionEnded(function() {options(old); shiny::stopApp()})
    
    output$main.igor <- renderImage(list(src=..image("IGoR.jpg")),deleteFile = FALSE)
    output$main.data <- ..renderTables(input,output)

    state <- reactiveValues()
    .IGoR$state  <- state
    .IGoR$state$data <- Sys.time() # will change every time contents of current table changes
    .IGoR$state$meta <- Sys.time() # will change every time structure of current table changes
    .IGoR$state$list <- Sys.time() # will change every time the list of current tables changes

    # Launch the servers for all the pages present in config
    walk(unlist(.IGoR$config$menus, use.names=FALSE),
         function(x) (get(paste0("page_",x))$server)(input,output,session))

  }

# Launch application ------------------------------------------------------

  shiny::shinyApp(ui, server)

}
