
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# Sample Dataset
movies <- data.frame(
  ID = 1:10,
  Movie = c("Leo", "Jailer", "Vikram", "Master",
            "Beast", "Ponniyin Selvan", "Kaithi",
            "96", "Soorarai Pottru", "Doctor"),
  Genre = c("Action", "Action", "Action", "Action",
            "Action", "Drama", "Thriller", "Romance",
            "Drama", "Comedy"),
  Year = c(2023, 2023, 2022, 2021, 2022,
           2022, 2019, 2018, 2020, 2021),
  Rating = c(8.2, 7.8, 8.3, 7.9, 5.5,
             7.6, 8.5, 8.5, 8.7, 7.4)
)

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Movie Analytics Pro"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard",
               icon = icon("dashboard")),
      menuItem("Manage Movies", tabName = "manage",
               icon = icon("film"))
    ),
    
    hr(),
    
    textInput("search", "Search Movie", ""),
    selectInput("genre", "Genre", choices = "All"),
    selectInput("year", "Year", choices = "All"),
    sliderInput("rating", "Minimum Rating",
                min = 0, max = 10, value = 0, step = 0.5)
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f3f6fb; }
      .box { border-radius: 10px; }
      .small-box { border-radius: 10px; }
      .main-header .logo { font-weight: bold; }
    "))),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          valueBoxOutput("totalMovies", width = 4),
          valueBoxOutput("averageRating", width = 4),
          valueBoxOutput("topMovie", width = 4)
        ),
        
        fluidRow(
          box(title = "Movie Rating Comparison",
              width = 6, status = "primary",
              plotOutput("ratingPlot", height = 300)),
          
          box(title = "Genre Analysis",
              width = 6, status = "success",
              plotOutput("genrePlot", height = 300))
        ),
        
        fluidRow(
          box(title = "Top 5 Movies",
              width = 6, status = "warning",
              tableOutput("topFive")),
          
          box(title = "Rating Distribution",
              width = 6, status = "info",
              plotOutput("distribution", height = 300))
        ),
        
        fluidRow(
          box(title = "Movie Recommendation",
              width = 12,
              textOutput("recommendation"))
        )
      ),
      
      tabItem(
        tabName = "manage",
        
        box(
          title = "Add a New Movie",
          width = 12,
          status = "primary",
          
          textInput("newMovie", "Movie Name"),
          textInput("newGenre", "Genre"),
          numericInput("newYear", "Release Year",
                       value = 2026, min = 1900, max = 2100),
          sliderInput("newRating", "Movie Rating",
                      min = 0, max = 10, value = 7, step = 0.1),
          
          actionButton("addMovie", "Add Movie",
                       icon = icon("plus"),
                       class = "btn-success")
        ),
        
        box(
          title = "Manage Movie Records",
          width = 12,
          status = "primary",
          
          p("Select one row in the table before editing or deleting."),
          
          actionButton("editMovie", "Edit Selected",
                       icon = icon("edit")),
          actionButton("deleteMovie", "Delete Selected",
                       icon = icon("trash"),
                       class = "btn-danger"),
          
          br(), br(),
          
          downloadButton("downloadData", "Download CSV"),
          
          br(), br(),
          
          DTOutput("movieTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  movieData <- reactiveVal(movies)
  selectedID <- reactiveVal(NULL)
  
  # Update filter options when movies change
  observe({
    data <- movieData()
    
    updateSelectInput(
      session, "genre",
      choices = c("All", sort(unique(data$Genre)))
    )
    
    updateSelectInput(
      session, "year",
      choices = c("All", sort(unique(data$Year)))
    )
  })
  
  # Search and filter movies
  filteredMovies <- reactive({
    data <- movieData()
    
    if (nzchar(input$search)) {
      data <- data[
        grepl(input$search, data$Movie, ignore.case = TRUE),
      ]
    }
    
    if (input$genre != "All") {
      data <- data[data$Genre == input$genre, ]
    }
    
    if (input$year != "All") {
      data <- data[data$Year == as.numeric(input$year), ]
    }
    
    data <- data[data$Rating >= input$rating, ]
    
    data
  })
  
  # Dashboard cards
  output$totalMovies <- renderValueBox({
    valueBox(nrow(filteredMovies()), "Total Movies",
             icon = icon("film"), color = "blue")
  })
  
  output$averageRating <- renderValueBox({
    data <- filteredMovies()
    avg <- if (nrow(data)) round(mean(data$Rating), 2) else 0
    
    valueBox(avg, "Average Rating",
             icon = icon("star"), color = "green")
  })
  
  output$topMovie <- renderValueBox({
    data <- filteredMovies()
    best <- if (nrow(data)) {
      data$Movie[which.max(data$Rating)]
    } else {
      "No Movies"
    }
    
    valueBox(best, "Highest Rated Movie",
             icon = icon("trophy"), color = "purple")
  })
  
  # Movie rating chart
  output$ratingPlot <- renderPlot({
    data <- filteredMovies()
    validate(need(nrow(data) > 0, "No movies found"))
    
    ggplot(data, aes(x = reorder(Movie, Rating),
                     y = Rating, fill = Genre)) +
      geom_col() +
      coord_flip() +
      labs(x = "Movie", y = "Rating") +
      theme_minimal()
  })
  
  # Average rating by genre
  output$genrePlot <- renderPlot({
    data <- filteredMovies()
    validate(need(nrow(data) > 0, "No movies found"))
    
    genreData <- aggregate(Rating ~ Genre, data, mean)
    
    ggplot(genreData, aes(Genre, Rating, fill = Genre)) +
      geom_col() +
      labs(x = "Genre", y = "Average Rating") +
      theme_minimal()
  })
  
  # Top 5 ranking
  output$topFive <- renderTable({
    data <- filteredMovies()
    data <- data[order(-data$Rating), ]
    head(data[, c("Movie", "Genre", "Year", "Rating")], 5)
  })
  
  # Rating distribution
  output$distribution <- renderPlot({
    data <- filteredMovies()
    validate(need(nrow(data) > 0, "No movies found"))
    
    ggplot(data, aes(x = Rating)) +
      geom_histogram(binwidth = 1, color = "white") +
      scale_x_continuous(breaks = 0:10) +
      labs(x = "Rating", y = "Number of Movies") +
      theme_minimal()
  })
  
  # Recommendation
  output$recommendation <- renderText({
    data <- filteredMovies()
    
    if (nrow(data) == 0) {
      return("No matching movies. Try changing the filters.")
    }
    
    best <- data[which.max(data$Rating), ]
    
    paste0(
      "Recommended Movie: ", best$Movie,
      " | Genre: ", best$Genre,
      " | Rating: ", best$Rating,
      " | Year: ", best$Year
    )
  })
  
  # Movie table
  output$movieTable <- renderDT({
    datatable(
      filteredMovies(),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 8)
    )
  })
  
  # Add a movie
  observeEvent(input$addMovie, {
    name <- trimws(input$newMovie)
    genre <- trimws(input$newGenre)
    data <- movieData()
    
    if (name == "" || genre == "") {
      showNotification(
        "Enter both movie name and genre.",
        type = "error"
      )
      return()
    }
    
    if (any(tolower(data$Movie) == tolower(name))) {
      showNotification("Movie already exists.", type = "error")
      return()
    }
    
    newRow <- data.frame(
      ID = if (nrow(data)) max(data$ID) + 1 else 1,
      Movie = name,
      Genre = genre,
      Year = input$newYear,
      Rating = input$newRating
    )
    
    movieData(rbind(data, newRow))
    
    updateTextInput(session, "newMovie", value = "")
    updateTextInput(session, "newGenre", value = "")
    
    showNotification("Movie added successfully!")
  })
  
  # Remember selected movie
  observeEvent(input$movieTable_rows_selected, {
    rows <- input$movieTable_rows_selected
    data <- filteredMovies()
    
    if (length(rows) == 1 && nrow(data) >= rows) {
      selectedID(data$ID[rows])
    } else {
      selectedID(NULL)
    }
  })
  
  # Delete selected movie
  observeEvent(input$deleteMovie, {
    id <- selectedID()
    
    if (is.null(id)) {
      showNotification("Select a movie first.", type = "error")
      return()
    }
    
    movieData(movieData()[movieData()$ID != id, ])
    selectedID(NULL)
    showNotification("Movie deleted successfully!")
  })
  
  # Edit selected movie
  observeEvent(input$editMovie, {
    id <- selectedID()
    
    if (is.null(id)) {
      showNotification("Select a movie first.", type = "error")
      return()
    }
    
    data <- movieData()
    movie <- data[data$ID == id, ]
    
    showModal(modalDialog(
      title = "Edit Movie",
      
      textInput("editName", "Movie Name",
                value = movie$Movie),
      textInput("editGenre", "Genre",
                value = movie$Genre),
      numericInput("editYear", "Release Year",
                   value = movie$Year,
                   min = 1900, max = 2100),
      sliderInput("editRating", "Rating",
                  min = 0, max = 10,
                  value = movie$Rating, step = 0.1),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveEdit", "Save Changes",
                     class = "btn-primary")
      ),
      easyClose = FALSE
    ))
  })
  
  observeEvent(input$saveEdit, {
    id <- selectedID()
    data <- movieData()
    
    if (is.null(id) ||
        trimws(input$editName) == "" ||
        trimws(input$editGenre) == "") {
      showNotification("Enter valid movie details.",
                       type = "error")
      return()
    }
    
    otherMovies <- data[data$ID != id, ]
    
    if (any(tolower(otherMovies$Movie) ==
            tolower(trimws(input$editName)))) {
      showNotification("Another movie has this name.",
                       type = "error")
      return()
    }
    
    index <- which(data$ID == id)
    
    data$Movie[index] <- trimws(input$editName)
    data$Genre[index] <- trimws(input$editGenre)
    data$Year[index] <- input$editYear
    data$Rating[index] <- input$editRating
    
    movieData(data)
    removeModal()
    showNotification("Movie updated successfully!")
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("movie_ratings_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filteredMovies(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)