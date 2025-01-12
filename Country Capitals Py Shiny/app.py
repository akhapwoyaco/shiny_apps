"""
Purpose of this App is to create a simple quiz in shiny on various capital cities
Key areas:
    1. Use class methods in shiny App
    2. Shiny App of Python file test.file that can be run on Commmand Line as standalone
"""


from shiny import App, ui, render, reactive, req
import pandas as pd
from random import choice
import itertools

class QuizData:
    def __init__(self):
        self.country_city_dictionary = {
            "Abkhazia":"Sukhumi", "Afghanistan":"Kabul", "Akrotiri and Dhekelia":"Episkopi Cantonment", 
            "Albania":"Tirana", "Algeria":"Algiers", "American Samoa":"Pago Pago", "Andorra":"Andorra la Vella", 
            "Angola":"Luanda", "Anguilla":"The Valley", "Antigua and Barbuda":"St. John's", "Argentina":"Buenos Aires", 
            "Armenia":"Yerevan", "Aruba":"Oranjestad", "Ascension Island":"Georgetown", "Australia":"Canberra", 
            "Austria":"Vienna", "Azerbaijan":"Baku", "Bahamas":"Nassau", "Bahrain":"Manama", "Bangladesh":"Dhaka", 
            "Barbados":"Bridgetown", "Belarus":"Minsk", "Belgium":"Brussels", "Belize":"Belmopan", "Benin":"Porto-Novo", 
            "Bermuda":"Hamilton", "Bhutan":"Thimphu", "Bolivia":"Sucre", "Bolivia":"La Paz", "Bosnia and Herzegovina":"Sarajevo", 
            "Botswana":"Gaborone", "Brazil":"Brasília", "British Virgin Islands":"Road Town", "Brunei":"Bandar Seri Begawan", 
            "Bulgaria":"Sofia", "Burkina Faso":"Ouagadougou", "Burundi":"Bujumbura", "Cambodia":"Phnom Penh", "Cameroon":"Yaoundé", 
            "Canada":"Ottawa", "Cape Verde":"Praia", "Cayman Islands":"George Town", "Central African Republic":"Bangui", 
            "Chad":"N'Djamena", "Chile":"Santiago", "China":"Beijing", "Christmas Island":"Flying Fish Cove", 
            "Cocos (Keeling) Islands":"West Island", "Colombia":"Bogotá", "Comoros":"Moroni", "Cook Islands":"Avarua", 
            "Costa Rica":"San José", "Croatia":"Zagreb", "Cuba":"Havana", "Curaçao":"Willemstad", "Cyprus":"Nicosia", 
            "Czech Republic":"Prague", "Côte d'Ivoire":"Yamoussoukro", "Democratic Republic of the Congo":"Kinshasa", 
            "Denmark":"Copenhagen", "Djibouti":"Djibouti", "Dominica":"Roseau", "Dominican Republic":"Santo Domingo", 
            "East Timor (Timor-Leste)":"Dili", "Easter Island":"Hanga Roa", "Ecuador":"Quito", "Egypt":"Cairo", 
            "El Salvador":"San Salvador", "Equatorial Guinea":"Malabo", "Eritrea":"Asmara", "Estonia":"Tallinn", 
            "Ethiopia":"Addis Ababa", "Falkland Islands":"Stanley", "Faroe Islands":"Tórshavn", 
            "Federated States of Micronesia":"Palikir", "Fiji":"Suva", "Finland":"Helsinki", "France":"Paris", 
            "French Guiana":"Cayenne", "French Polynesia":"Papeete", "Gabon":"Libreville", "Gambia":"Banjul", "Georgia":"Tbilisi", 
            "Germany":"Berlin", "Ghana":"Accra", "Gibraltar":"Gibraltar", "Greece":"Athens", "Greenland":"Nuuk", 
            "Grenada":"St. George's", "Guam":"Hagåtña", "Guatemala":"Guatemala City", "Guernsey":"St. Peter Port", 
            "Guinea":"Conakry", "Guinea-Bissau":"Bissau", "Guyana":"Georgetown", "Haiti":"Port-au-Prince", "Honduras":"Tegucigalpa",
            "Hungary":"Budapest", "Iceland":"Reykjavík", "India":"New Delhi", "Indonesia":"Jakarta", "Iran":"Tehran", 
            "Iraq":"Baghdad", "Ireland":"Dublin", "Isle of Man":"Douglas",  "Israel":"Jerusalem", "Italy":"Rome", 
            "Jamaica":"Kingston", "Japan":"Tokyo",  "Jersey":"St. Helier", "Jordan":"Amman", "Kazakhstan":"Astana", 
            "Kenya":"Nairobi", "Kiribati":"Tarawa", "Kosovo":"Pristina", "Kuwait":"Kuwait City", "Kyrgyzstan":"Bishkek", 
            "Laos":"Vientiane", "Latvia":"Riga", "Lebanon":"Beirut", "Lesotho":"Maseru", "Liberia":"Monrovia", 
            "Libya":"Tripoli", "Liechtenstein":"Vaduz", "Lithuania":"Vilnius", "Luxembourg":"Luxembourg", "Macedonia":"Skopje", 
            "Madagascar":"Antananarivo", "Malawi":"Lilongwe", "Malaysia":"Kuala Lumpur", "Maldives":"Malé", "Mali":"Bamako", 
            "Malta":"Valletta", "Marshall Islands":"Majuro", "Mauritania":"Nouakchott", "Mauritius":"Port Louis", 
            "Mexico":"Mexico City", "Moldova":"Chisinau", "Monaco":"Monaco", "Mongolia":"Ulaanbaatar", "Montenegro":"Podgorica", 
            "Montserrat":"Plymouth", "Morocco":"Rabat", "Mozambique":"Maputo", "Myanmar":"Naypyidaw", 
            "Nagorno-Karabakh Republic":"Stepanakert", "Namibia":"Windhoek", "Nauru":"Yaren", "Nepal":"Kathmandu", 
            "Netherlands":"Amsterdam", "New Caledonia":"Nouméa", "New Zealand":"Wellington", "Nicaragua":"Managua", 
            "Niger":"Niamey", "Nigeria":"Abuja", "Niue":"Alofi", "Norfolk Island":"Kingston", "North Korea":"Pyongyang", 
            "Northern Cyprus":"Nicosia", "United Kingdom Northern Ireland":"Belfast",  "Northern Mariana Islands":"Saipan", 
            "Norway":"Oslo", "Oman":"Muscat", "Pakistan":"Islamabad", "Palau":"Ngerulmud", "Palestine":"Jerusalem", 
            "Panama":"Panama City", "Papua New Guinea":"Port Moresby", "Paraguay":"Asunción", "Peru":"Lima", 
            "Philippines":"Manila", "Pitcairn Islands":"Adamstown", "Poland":"Warsaw", "Portugal":"Lisbon", 
            "Puerto Rico":"San Juan", "Qatar":"Doha", "Republic of China (Taiwan)":"Taipei", 
            "Republic of the Congo":"Brazzaville", "Romania":"Bucharest", "Russia":"Moscow", "Rwanda":"Kigali", 
            "Saint Barthélemy":"Gustavia", "Saint Helena":"Jamestown", "Saint Kitts and Nevis":"Basseterre", 
            "Saint Lucia":"Castries", "Saint Martin":"Marigot", "Saint Pierre and Miquelon":"St. Pierre", 
            "Saint Vincent and the Grenadines":"Kingstown", "Samoa":"Apia", "San Marino":"San Marino", 
            "Saudi Arabia":"Riyadh", "Scotland":"Edinburgh", "Senegal":"Dakar", "Serbia":"Belgrade", 
            "Seychelles":"Victoria", "Sierra Leone":"Freetown", "Singapore":"Singapore", "Sint Maarten":"Philipsburg", 
            "Slovakia":"Bratislava", "Slovenia":"Ljubljana", "Solomon Islands":"Honiara", "Somalia":"Mogadishu", 
            "Somaliland":"Hargeisa", "South Africa":"Pretoria", "South Georgia and the South Sandwich Islands":"Grytviken", 
            "South Korea":"Seoul", "South Ossetia":"Tskhinvali", "South Sudan":"Juba", "Spain":"Madrid", 
            "Sri Lanka":"Sri Jayawardenapura Kotte", "Sudan":"Khartoum", "Suriname":"Paramaribo", "Swaziland":"Mbabane", 
            "Sweden":"Stockholm", "Switzerland":"Bern", "Syria":"Damascus", "São Tomé and Príncipe":"São Tomé", 
            "Tajikistan":"Dushanbe", "Tanzania":"Dodoma", "Thailand":"Bangkok", "Togo":"Lomé", "Tonga":"Nukuʻalofa", 
            "Transnistria":"Tiraspol", "Trinidad and Tobago":"Port of Spain", "Tristan da Cunha":"Edinburgh of the Seven Seas",  
            "Tunisia":"Tunis", "Turkey":"Ankara", "Turkmenistan":"Ashgabat", "Turks and Caicos Islands":"Cockburn Town", 
            "Tuvalu":"Funafuti", "Uganda":"Kampala",     "Ukraine":"Kiev", "United Arab Emirates":"Abu Dhabi", 
            "United Kingdom; England":"London", "United States":"Washington, D.C.", 
            "United States Virgin Islands":"Charlotte Amalie","Uruguay":"Montevideo", "Uzbekistan":"Tashkent",  
            "Vanuatu":"Port Vila", "Vatican City":"Vatican City", "Venezuela":"Caracas", "Vietnam":"Hanoi", "Wales":"Cardiff", 
            "Wallis and Futuna":"Mata-Utu", "Western Sahara":"El Aaiún", "Yemen":"Sanaá", "Zambia":"Lusaka", "Zimbabwe":"Harare"
            }
        #
        self.remaining_countries = list(self.country_city_dictionary.keys())
        self.current_country = None
        self.scores = {}
        
    # selects a random country from the list of countries
    # ensures no repeat of country as already asked country is dropped
    def get_random_country(self):
        if not self.remaining_countries:
            # if all answered, refresh list to have all countries
            self.remaining_countries = list(self.country_city_dictionary.keys())
        self.current_country = choice(list(self.remaining_countries))
        self.remaining_countries.remove(self.current_country)
        return self.current_country
    
    # ensure capital cities answers are correct, return bool if correct and answer
    def check_answer(self, country, answer):
        correct_answer = self.country_city_dictionary[country]
        is_correct = answer.strip().title() == correct_answer
        return is_correct, correct_answer
    
    # ensures each player score is distinct
    # for each records the associated values
    def add_score(self, player_name, question_number, is_correct, country, answer, correct_answer):
        if player_name not in self.scores:
            self.scores[player_name] = []

        self.total_questions = len(self.scores.get(player_name, [])) +1
        correct_count = sum(1 for score in self.scores.get(player_name,[]) if score['Correct'] == 'Correct') + (1 if is_correct else 0)
        #
        current_score = {
            'Name': player_name,
            'No.': question_number,
            'Correct': 'Correct' if is_correct else 'Wrong',
            'Question': country,
            'Answer': answer,
            'Correct Answer': correct_answer,
            'Total Score': f"{correct_count}/{self.total_questions}",
            'Mean Score': round(correct_count/self.total_questions, 2),
            'Perc. Score': round(100 * correct_count/self.total_questions, 2)
        }
        self.scores[player_name].append(current_score)
        return self.scores

    # incase you hit reset game, everything goes to default state
    def reset_data(self):
        self.remaining_countries = list(self.country_city_dictionary.keys())
        self.current_country = None
        self.scores = {}#[]

class QuizUI:
    def __init__(self):
        self.ui = ui.page_fluid(
            ui.panel_title("Country Capitals Cities Quiz"),
            ui.layout_sidebar(
                ui.sidebar( 
                    ui.input_text("player_name", "Player Name:", placeholder="Enter your name"),
                    ui.input_numeric("num_questions", "Number of Questions:", value=5, min=1, max = len(quiz_data.country_city_dictionary)-1), # questions limited to number of countries
                    ui.input_action_button("start_quiz", "Start Quiz", class_="btn-primary"),
                    ui.input_action_button("end_quiz", "End Quiz", class_="btn-secondary"),
                    ui.input_action_button("reset_game", "Reset Game", class_="btn_warning"),
                    ui.hr(),
                    ui.output_text("player_status"),
                    # my fiver 
                    ui.input_action_button(
                        'fiver', "Fiver Python/R Shiny Services", 
                        onclick ="window.open('https://www.fiverr.com/s/jj5dYam', '_blank')")
                ),
                ui.div(
                    ui.h4("Question: "),
                    ui.tags.div(
                        ui.output_ui("quiz_interface"),
                    ),
                    
                    ui.hr(),
                    ui.h4("Score History: "),
                    ui.output_ui("current_player_scores"),
                    ui.tags.div(
                        ui.output_data_frame("player_scores"), class_ = "table-responsive"
                        ),
                    ui.hr(),
                    ui.h5("All Player Scores: "),
                    ui.tags.div(
                        ui.output_data_frame("all_scores"), class_ = "table-responsive"
                        ),
                    ui.hr(),
                    ui.hr()
                    ),
                ui.card(
                    ui.h6("Other Shiny Apps"),
                    ui.p("1. [2022 General Election, mapping of tally, election winners within counties using kenyaelection](https://akhapwoyachris.shinyapps.io/presidentialelectionkenya2022/)"),
                    ui.p("2. [Kenya Election Winners](https://christopherakhapwoya.shinyapps.io/electionkenya/)"),
                    ui.p("3. [Australian Weather](https://01940e82-7881-ac23-953c-68da09cea0f0.share.connect.posit.cloud/)"),
                        ui.card_footer(
                        ui.HTML(
                            'Copyright &copy; 2025 &nbsp; Github Account: <a href="https://github.com/akhapwoyaco" target="_blank">akhapwoyaco</a>'
                        )
                )
                )
            )
        )

class QuizServer:
    def __init__(self, quiz_data):
        self.quiz_data = quiz_data
        self.current_question = 1
        self.game_active = False
       
    def server(self, input, output, session):       
        # initial states of game, no country and player, player data yet
        game_active = reactive.Value(False)
        current_question = reactive.Value(1)
        current_country = reactive.Value(None)
        player_name_rv = reactive.Value(None)
        scores_data = reactive.value()
        # this ensure as one answers, scores are displayed after each question
        player_scores_realtime = reactive.value()
        # hmmm
        # all_scores_data = reactive.Value(pd.DataFrame())
        #        
        @render.ui
        @reactive.event(input.start_quiz)
        def current_player_scores():
            # your scoreboard title with name
            return ui.h5(f"{input.player_name()} Scores")


        @reactive.Effect
        def _():
            input.start_game()
            input.submit()
        
        @output
        @render.text
        def player_status():
            if not game_active():
                return "Enter your name and number of questions to start"
            return f"Question {current_question()} of {input.num_questions()}"
        
        @output
        @render.ui
        def quiz_interface():
            if not game_active():
                return ui.div()
            
            return ui.div(
                ui.h3(f"What is the capital of {current_country()}?"),
                ui.input_text("answer", "", placeholder="Enter capital city"),
                ui.input_action_button("submit", "Submit Answer", class_="btn-success"),
                ui.output_text("feedback")
            )
        
        @output
        @render.text
        def feedback():
            return ""
        
        @reactive.Effect
        @reactive.event(input.start_quiz)
        def start_game():
            if not input.player_name():# provides name for game on
                ui.notification_show("Please enter your name first!", type="error")
                return
            
            # self.quiz_data.scores = []  # Reset 
            # at start of game react vals set to defauls
            scores_data.set({})
            game_active.set(True)
            current_question.set(1)
            new_country = self.quiz_data.get_random_country()
            current_country.set(new_country)
            self.quiz_data.current_country = new_country 
            player_name_rv.set(input.player_name())
            player_scores_realtime.set({})
        
        @reactive.effect
        @reactive.event(input.submit)
        def handle_answer():
            if not game_active() or not input.answer():
                return
            # checks answer and adds to main data
            is_correct, correct_answer = self.quiz_data.check_answer(
                current_country(),# 
                input.answer()
            )
            
            self.quiz_data.add_score(
                input.player_name(),
                current_question(),
                is_correct,
                current_country(),
                input.answer(),
                correct_answer
            )
            scores_data.set(self.quiz_data.scores)

            # Show feedback
            feedback_text = "Correct!" if is_correct else f"Wrong! The correct answer is {correct_answer}"
            ui.notification_show(feedback_text, type="success" if is_correct else "error")
            # update questions and ensure number of questions limit
            if current_question() < input.num_questions():
                current_question.set(current_question() + 1)
                new_country = self.quiz_data.get_random_country()
                current_country.set(new_country)
                self.quiz_data.current_country = new_country
            else:
                game_active.set(False)
                current_country.set(None)
                ui.notification_show("Quiz completed!", type="default")
            
            # Clear answer input
            ui.update_text("answer", value="")
            # now results printed on time
            player_scores_realtime.set(self.quiz_data.scores)
        #
        @output
        @render.data_frame
        def player_scores(): # render individual player score
            if game_active() or player_scores_realtime():
                player_scores = player_scores_realtime().get(player_name_rv())
                # player_scores = scores_data().get(player_name_rv())
                result_df = pd.DataFrame(player_scores)
                #columns_to_display = [
                #        'Name',
                #        'No.', 'Correct', 'Question', 'Answer', 
                #    'Correct Answer', 'Total Score', 'Perc. Score']           
                return render.DataGrid(
                    result_df,
                    selection_mode="none",
                    width='100%',
                    height='auto'
                )
                #else:
                 #   return None
        
        @output
        @render.data_frame
        def all_scores(): # render all player scores at quit game
            if input.end_quiz() > 0:
                if len(scores_data()) > 0:
                    #
                    all_player_scores = []
                    for player, scores in scores_data().items():
                        last_score = scores[-1]
                        last_score["Name"] = player
                        last_score = {
                            key: last_score[key] for key in ('Name', 'Total Score', 'Mean Score', 'Perc. Score')
                            }
                        all_player_scores.append(last_score)
                    result_df = pd.DataFrame(all_player_scores)
                    
                    return render.DataGrid(
                        result_df,
                        selection_mode="none",
                        width='100%',
                        height='auto'
                    )
                else:
                    return None

        @reactive.Effect
        @reactive.event(input.reset_game)
        def reset_game(): # resets game on ret_game button
            game_active.set(False)
            current_question.set(1)
            self.quiz_data.reset_data()

            new_country = self.quiz_data.get_random_country()
            current_country.set(new_country)
            self.quiz_data.current_country = new_country

            ui.update_text("player_name", value="")
            ui.update_numeric("num_questions", value = 5)
            ui.notification_show("Game Reset", type="warning") 
            #
            player_scores_realtime.set({})
            scores_data.set({})
#
quiz_data = QuizData()
quiz_ui = QuizUI()
quiz_server = QuizServer(quiz_data)
app = App(quiz_ui.ui, quiz_server.server)
#