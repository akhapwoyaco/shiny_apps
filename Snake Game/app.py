# Bach, [1/11/2025 12:33 AM]
from shiny import App, render, ui, reactive, session
import random
import time
import plotly.graph_objects as go

def server(input, output, session):
    """
    This function implements the server logic for the Snake game Shiny app.

    Args:
        input: Shiny input object.
        output: Shiny output object.
        session: Shiny session object.

    Returns:
        None
    """

    # Initial game state
    INITIAL_SNAKE = [[3, 2], [2, 2], [1, 2]]
    GRID_WIDTH = 15
    GRID_HEIGHT = 10
    TICK_RATE = 100  # milliseconds

    game_state = reactive.Value({
        'snake': INITIAL_SNAKE.copy(),
        'food': [random.randrange(1, GRID_WIDTH), random.randrange(1, GRID_HEIGHT)],
        'score': 0,
        'direction': 'RIGHT',
        'game_over': False
    })

    @reactive.Effect
    def update_game_state():
        """
        Runs the game logic.
        """
        while not game_state.get()['game_over']:
            current = game_state.get()

            # Update snake position
            new_head = list(current['snake'][0])
            if current['direction'] == 'RIGHT':
                new_head[0] += 1
            elif current['direction'] == 'LEFT':
                new_head[0] -= 1
            elif current['direction'] == 'UP':
                new_head[1] -= 1
            elif current['direction'] == 'DOWN':
                new_head[1] += 1

            # Check boundaries and handle game over
            if (new_head[0] < 0 or new_head[0] >= GRID_WIDTH or
                    new_head[1] < 0 or new_head[1] >= GRID_HEIGHT):
                current['game_over'] = True
                game_state.set(current)
                return

            # Check self-collision and handle game over
            if new_head in current['snake'][1:]:
                current['game_over'] = True
                game_state.set(current)
                return

            # Update snake body
            current['snake'].insert(0, new_head)

            # Check food collision
            if new_head == current['food']:
                current['score'] += 1
                current['food'] = [
                    random.randrange(1, GRID_WIDTH),
                    random.randrange(1, GRID_HEIGHT)
                ]
            else:
                current['snake'].pop()

            game_state.set(current)
            #session.send_custom_message('game_update', current)
            time.sleep(TICK_RATE / 1000) 

    @reactive.Effect
    @reactive.event(input.direction)
    def update_direction():
        """
        Updates the snake's direction based on user input.

        This function is triggered whenever the direction input changes.
        It checks if the game is not over and updates the direction in the game state.
        """
        if input.direction() and not game_state.get()['game_over']:
            current = game_state.get()
            # Prevent immediate reversal (e.g., going right then left in the same tick)
            if not (current['direction'] == 'RIGHT' and input.direction() == 'LEFT' or
                    current['direction'] == 'LEFT' and input.direction() == 'RIGHT' or
                    current['direction'] == 'UP' and input.direction() == 'DOWN' or
                    current['direction'] == 'DOWN' and input.direction() == 'UP'):
                current['direction'] = input.direction()
            game_state.set(current)

    @render.plot
    def plot_game():
        """
        Generates the Plotly plot for the Snake game.
        """
        import plotly.graph_objects as go

        # Create a blank grid
        fig = go.Figure(data=[go.Scatter(x=[], y=[])])
        fig.update_layout(
            width=GRID_WIDTH * 20,
            height=GRID_HEIGHT * 20,
            xaxis=dict(range=[0, GRID_WIDTH], showgrid=True, zeroline=True),
            yaxis=dict(range=[0, GRID_HEIGHT], showgrid=True, zeroline=True),
            plot_bgcolor='black',
            xaxis_visible=False,
            yaxis_visible=False
        )


# Draw snake
        snake_x, snake_y = zip(*game_state.get()['snake'])
        fig.add_trace(go.Scatter(x=snake_x, y=snake_y, mode='markers', marker_size=15, marker_color='green'))

        # Draw food
        fig.add_trace(go.Scatter(x=[game_state.get()['food'][0]], y=[game_state.get()['food'][1]], mode='markers', marker_size=15, marker_color='red'))

        return fig

    return output


app_ui = ui.page_fluid(
    ui.panel_title("Snake Game"),
    ui.sidebar(
        #sidebar_panel = ui.sidebar(
            ui.input_radio_buttons(
                "direction", "Direction:",
                           choices = {
                            "Up" : "UP", "Down": "DOWN", 
                            "Left" : "LEFT", "Right" : "RIGHT"}#)
        ),
        #main_panel = ui.panel_main(
            ui.output_plot("plot_game")
        #)
    )
)

app = App(app_ui, server)