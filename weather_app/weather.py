import shiny
import plotly.express as px

# Define UI
app_ui = ui.page_fluid(
    ui.input_slider("num_points", "Number of points", min=10, max=100, value=50),
    ui.output_plotly("scatter_plot")
)

# Define server logic
def server(input, output):
    @output
    @render.plotly
    def scatter_plot():
        x = np.random.randn(input.num_points())
        y = np.random.randn(input.num_points())
        fig = px.scatter(x=x, y=y)
        return fig

# Run the app
# app = shiny.App(app_ui, server)
# app.run()

