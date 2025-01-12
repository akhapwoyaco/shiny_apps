library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Snake Game"),
  sidebarLayout(
    sidebarPanel(
      actionButton("start_button", "Start Game"),
      h3("Direction"),
      radioButtons("direction", "Choose Direction:",
                   choices = c("Up", "Down", "Left", "Right"),
                   selected = NULL), # Initially disabled
      h3("Score"),
      verbatimTextOutput("score")
    ),
    mainPanel(
      plotOutput("game_board")
    )
  )
)

server <- function(input, output, session) {
  # Game variables (reactive values)
  snake_pos <- reactiveValues(x = 3, y = 2)
  snake_body <- reactiveValues(
    snake1 = c(3, 2), 
    snake2 = c(2, 2), 
    snake3 = c(1, 2) 
  ) 
  food_pos <- reactiveValues(x = sample(1:15, 1) * 10, y = sample(1:9, 1) * 10)
  food_status <- reactiveValues(status = TRUE)
  score <- reactiveValues(value = 0)
  direction <- reactiveValues(current = "RIGHT")
  game_over <- reactiveValues(is_over = FALSE)
  game_started <- reactiveValues(is_started = FALSE) 
  
  # Start Game button handler
  observeEvent(input$start_button, {
    game_started$is_started <- TRUE
    # Reset game state
    snake_pos$x <- 3
    snake_pos$y <- 2
    snake_body$snake1 <- c(3, 2)
    snake_body$snake2 <- c(2, 2)
    snake_body$snake3 <- c(1, 2)
    food_pos$x <- sample(1:15, 1) * 10
    food_pos$y <- sample(1:9, 1) * 10
    food_status$status <- TRUE
    score$value <- 0
    direction$current <- "RIGHT"
    game_over$is_over <- FALSE
  })
  
  # Update snake position based on direction
  observeEvent(input$direction, {
    if (!game_over$is_over && game_started$is_started) { 
      new_direction <- input$direction
      if (!(direction$current == "LEFT" && new_direction == "RIGHT" ||
            direction$current == "RIGHT" && new_direction == "LEFT" ||
            direction$current == "UP" && new_direction == "DOWN" ||
            direction$current == "DOWN" && new_direction == "UP")) {
        direction$current <- new_direction 
      }
    }
  })
  
  # Update snake position on timer
  timer <- reactiveTimer(100)  # Update every 100 milliseconds
  observeEvent(timer(), {
    if (!game_over$is_over && game_started$is_started) { 
      new_x <- snake_pos$x
      new_y <- snake_pos$y
      if (direction$current == "RIGHT") {
        new_x <- new_x + 10
      } else if (direction$current == "LEFT") {
        new_x <- new_x - 10
      } else if (direction$current == "UP") {
        new_y <- new_y - 10
      } else if (direction$current == "DOWN") {
        new_y <- new_y + 10
      }
      
      # Check for collisions
      if (new_x < 0 || new_x > 150 || new_y < 0 || new_y > 90 ||
          any(apply(do.call(rbind, as.list(snake_body)), 1, function(row) all(row == c(new_x, new_y))))) { 
        game_over$is_over <- TRUE 
      } else {
        # Update snake_body correctly
        snake_body$snake1 <- snake_body$snake2 
        snake_body$snake2 <- snake_body$snake3
        snake_body$snake3 <- c(new_x, new_y) 
        
        if (new_x == food_pos$x && new_y == food_pos$y) {
          food_status$status <- FALSE
          score$value <- score$value + 1 
        } 
        snake_pos$x <- new_x
        snake_pos$y <- new_y
        if (!food_status$status) { 
          food_pos$x <- sample(1:15, 1) * 10
          food_pos$y <- sample(1:9, 1) * 10
          food_status$status <- TRUE
        }
      }
    }
  })
  
  # Game board plot
  output$game_board <- renderPlot({
    if (game_started$is_started) { 
      ggplot() +
        geom_rect(aes(xmin = 0, xmax = 150, ymin = 0, ymax = 90), fill = "black") +
        geom_point(aes(x = unlist(lapply(reactiveValuesToList(snake_body), "[", 1)), 
                       y = unlist(lapply(reactiveValuesToList(snake_body), "[", 2))), 
                   color = "green", size = 5) +
        geom_point(aes(x = food_pos$x, y = food_pos$y), color = "red", size = 5) +
        coord_fixed(ratio = 1) +
        theme_void()
    }
  })
  
  # Display score
  output$score <- renderText({
    if (game_over$is_over) { 
      paste("Game Over! Score:", score$value)
    } else {
      paste("Score:", score$value)
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)