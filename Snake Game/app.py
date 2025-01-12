from shiny import App, render, ui, reactive, session
import random
import time
import plotly.graph_objects as go
import io
from PIL import Image

def server(input, output, session):
    """
    This function implements the server logic for the Snake game Shiny app.
    """

    INITIAL_SNAKE = [[3, 2], [2, 2], [1, 2]]
    GRID_WIDTH = 15
    GRID_HEIGHT = 10
    TICK_RATE = 0.1  # seconds

    snake = reactive.Value(INITIAL_SNAKE.copy())
    food = reactive.Value([random.randrange(1, GRID_WIDTH), random.randrange(1, GRID_HEIGHT)])
    score = reactive.Value(0)
    direction = reactive.Value('RIGHT')
    game_over = reactive.Value(False)
    last_update = reactive.Value(time.time())

    # Create a list to hold the snake's coordinates
    snake_x = reactive.Value([x for x, _ in snake.get()])
    snake_y = reactive.Value([y for _, y in snake.get()])

    @reactive.Effect
    def initialize_snake():
        snake.set(INITIAL_SNAKE.copy())

    @reactive.Effect
    def update_game_state():
        """
        Updates the game state.
        """
        # Move the code that initializes `snake` to a reactive effect
                    
        current_time = time.time()
        if current_time - last_update.get() >= TICK_RATE:
            if not game_over.get():
                current_snake = snake.get()
                current_food = food.get()
                current_direction = direction.get()

                # Update snake position
                new_head = list(current_snake[0])
                if current_direction == 'RIGHT':
                    new_head[0] += 1
                elif current_direction == 'LEFT':
                    new_head[0] -= 1
                elif current_direction == 'UP':
                    new_head[1] -= 1
                elif current_direction == 'DOWN':
                    new_head[1] += 1

                # Check boundaries and handle game over
                if (new_head[0] < 0 or new_head[0] >= GRID_WIDTH or
                        new_head[1] < 0 or new_head[1] >= GRID_HEIGHT):
                    game_over.set(True)
                elif new_head in current_snake[1:]:
                    game_over.set(True)
                else:
                    snake.set(current_snake[:0] + [new_head] + current_snake[1:])

                # Check food collision
                if new_head == current_food:
                    score.set(score.get() + 1)
                    food.set([random.randrange(1, GRID_WIDTH), random.randrange(1, GRID_HEIGHT)])
                else:
                    snake.set(current_snake[:0] + current_snake[1:])

            last_update.set(current_time) 

    @reactive.Effect
    @reactive.event(input.direction)
    def update_direction():
        """
        Updates the snake's direction based on user input.
        """
        if not game_over.get():  # Prevent direction changes after game over
            current_direction = direction.get()
            # Implement more robust direction change handling here
            if not (current_direction == 'RIGHT' and input.direction() == 'LEFT' or
                    current_direction == 'LEFT' and input.direction() == 'RIGHT' or
                    current_direction == 'UP' and input.direction() == 'DOWN' or
                    current_direction == 'DOWN' and input.direction() == 'UP'):
                direction.set(input.direction())

    @render.plot
    def plot_game():
        """
        Updates the Plotly plot with the current game state.
        """
        snake_x, snake_y = zip(*snake.get()) 
        return {
            "x": [x for x, _ in snake.get()], 
            "y": [y for _, y in snake.get()], 
            "type": "scatter", 
            "mode": "markers", 
            "marker": {"size": 15, "color": "green"}
        }, {
            "x": [food.get()[0]], 
            "y": [food.get()[1]], 
            "type": "scatter", 
            "mode": "markers", 
            "marker": {"size": 15, "color": "red"}
        }

    @render.text
    def score_text():
        if game_over.get():
            return f"Game Over! Score: {score.get()}"
        else:
            return f"Score: {score.get()}"

    return output


app_ui = ui.page_fluid(
    ui.panel_title("Snake Game"),
    ui.sidebar(
        ui.input_radio_buttons(
            "direction", "Direction:",
            choices = {
                "Up" : "UP", "Down": "DOWN", 
                "Left" : "LEFT", "Right" : "RIGHT"}
        ),
        ui.output_text("score_text")
    ),
    #ui.panel_main(
        ui.output_plot("plot_game")
    #)
)

app = App(app_ui, server)