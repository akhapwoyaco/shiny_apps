import pandas as pd
from shiny import App, reactive, ui, module, render
from abc import ABC, abstractmethod


class ShinyFormTemplate(ABC):
    """This is the abstract base class that has some commonly defined and implemented ui and server logic methods, as well as abstract methods for ui and server logic methods that will be implemented by the children (FruitForm and VeggieForm)"""

    _namespace_id=None

    def __init__(self, namespace_id, *args, **kwargs): # will be inhereted by child classes
        self._namespace_id=namespace_id

    @abstractmethod
    def call_ui(self):
        pass

    @abstractmethod
    def call_server(self,input,output,session):
        pass

    def _common_button_ui(self):
        """This method gets inherited by both fruit and veggie classes, providing ui for counter button"""
        return ui.row(

            ui.column(6,ui.input_action_button('count_button',"Increment Counter")),
            ui.column(3,ui.output_text('show_counter')),
            ui.column(3),
        )
    
    def _common_button_server(self,input,output,session):
        """This method gets inherited by both fruit and veggie classes, providing server functionality for counter button"""
        counter = reactive.value(0)

        @reactive.effect
        @reactive.event(input.count_button)
        def namespace_text():
            counter.set(counter.get()+1)

        @render.text
        def show_counter():
            return str(counter())

class FruitForm(ShinyFormTemplate):
    """This is the Fruit child class providing specific UI and Server functionality for Fruits."""
    def call_ui(self):
        """This method defines the FruitForm specific ui module AND calls it at the end returning the result."""
        @module.ui
        def ui_func():
            return ui.row(
            ui.input_text("fruits",'Select a Fruit',""),
            ui.output_text("output_fruits"),
            ui.input_text("qtys",'Quantity:',""),
            ui.output_text("output_qty"),
            self._common_button_ui(), # protected method inherited from ShinyFormTemplate.  

            # Insert additional fruit specific ui here that will operate in the 'fruit' namespace
        )
        return ui.nav_panel("New Fruit", "New Fruit - Input Form",
            ui_func(self._namespace_id) # the call to establish the fruit ui has to be returned at the end of this class, so that it gets inserted into the app_ui object that is defined globally
        )

    def call_server(self,input,output,session):
        """This method defines the ui module AND calls it at the end."""
        @module.server
        def server_func(input, output, session):
            @render.text
            def output_fruits():
                return input.fruits()
            
            self.__server_fruit_addl_stuff(input, output, session) # private method for FruitForm class only
            # Insert additional Fruit specific server logic here that will operate in the 'fruit' namespace
            self._common_button_server(input,output,session) # protected method inherited from ShinyFormTemplate
            
        server_func(self._namespace_id)

    def __server_fruit_addl_stuff(self, input, output, session):
            """Here is some additional server functionality that exists only in the FruitForm class and can be called by the call_server() method"""
            @render.text
            def output_qty():
                return input.qtys()

class VeggieForm(ShinyFormTemplate):
    """This is the Veggie child class providing specific UI and Server functionality for Veggies."""

    def __init__(self, namespace_id, veggie_only_data, *args, **kwargs): # will be inhereted by child classes
        self._namespace_id=namespace_id    
        self.__veggie_only_data=veggie_only_data
    
    def call_ui(self):
        """This method defines the VeggieForm specific ui module AND calls it at the end returning the result."""
        @module.ui
        def ui_func():
            return ui.row(
                ui.row(self.__veggie_only_data).add_style("font-weight: bold;"),
                ui.input_radio_buttons("veggie","Select a Veggie:",{'Asparagus':'Asparagus','Spinach':'Spinach','Squash':'Squash','Lettuce':'Lettuce'}),
                ui.output_text("output_veggie"),
                ui.input_text("qtys",'Quantity:',""),
                ui.output_text("output_qty"),
                # Insert additional Veggie specific ui here that will operate in the 'veggie' namespace
                self._common_button_ui(),
        )
        return ui.nav_panel("New Veggie", "New Veggie - Input Form",
            ui_func(self._namespace_id)
        )


    def call_server(self,input,output,session):
        @module.server
        def server_func(input, output, session):
            @render.text
            def output_veggie():
                return input.veggie()
        
            @render.text
            def output_qty():
                return input.qtys()
            
            # Insert additional Veggie specific server logic here that will operate in the 'veggie' namespace
            self._common_button_server(input, output, session)
        
        server_func(self._namespace_id)

#Define fruit and veggie class object instances.  This allows us to also pass in other objects like specific dataframes and database models that we can use to write populated form data to PostgreSQL database
fruits = FruitForm(namespace_id='fruit') # all ui/server components will operate in the 'fruit' namespace
veggies = VeggieForm(namespace_id='veggie',veggie_only_data='This class has a veggie specific data model') # all ui/server components will operate in the 'fruit' namespace
food_forms = [fruits, veggies]

# main ui object for the app
app_ui = ui.page_fluid(
    ui.page_navbar(
        [form.call_ui() for form in food_forms],  # iterate through any number of forms - creates and runs a ui module for each
        title="Sample App",
        id="page",
    ),
    title="Basic App"
)

# main server function for the app
def server(input, output, session):
    
    for form in food_forms:
       form.call_server(input, output, session) # iterate through any number of forms - creates and runs a server module for each

app = App(app_ui, server) 