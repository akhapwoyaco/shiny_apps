#!/usr/bin/env python
# coding: utf-8
#
# for random key selection
from random import choice
import itertools
#
# country and capital city dictionary
#
country_city_dictionary = {
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
    "South Korea":"Seoul", "South Ossetia":"Tskhinvali", "South Sudan South Sudan":"Juba", "Spain":"Madrid", 
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
# main class

class Quiz:

    def __init__(self, country_city_dictionary):
        self.country_city_dictionary = country_city_dictionary

    # add user their name
    def pla_yer_name(self):
        print()
        while True:
            try:
                self.player_name = str(input("What's your name? "))
                # replace empty spaces 
                if self.player_name.replace(' ','').isalpha and not any(x.isdigit() for x in self.player_name) and not self.player_name.replace(' ', '') == '':
                    return(self.player_name.lstrip().title())
                    break
            except ValueError:
                print("Enter a valid name: ")

    # check user input
    def country_city_input(self):
        #country_city_dictionary
        # get city 
        self.random_country = choice(
            list(self.country_city_dictionary.keys())
        )
        # question
        self.question = input("What is the capital city of city {}: ".format(self.random_country))
        # remove trailing white space
        self.city = str(self.question).lstrip().title()
        if self.city.replace(' ', '').isalpha and not any(cha_r.isdigit() for cha_r in self.city) and not self.city.replace(' ', '') == '':
            return(self.country_city_dictionary[self.random_country], self.city, self.random_country)
        else: 
            return(self.country_city_dictionary[self.random_country], 'Skipped', self.random_country)
            print("No.. input {} is not a string: ".format(question) )

    #
    def print_player_score(self, dictionary_of_scores):
        self.dictionary_of_scores = dictionary_of_scores
        self.dictionary_of_scores.sort(key = lambda x: x['Name'])
        self.group_ing = itertools.groupby(self.data_correct_wrong, lambda x: x['Name'])
        self.top_scores_by_player = [max(value, key = lambda x: x['No.']) for key, value in self.group_ing]
        #
        print()
        print("{:<30} {:<15} {:<15} {:<15}".format(
            'Name', 'Total Score', 'Mean Score', 'Percent Score'))
        for quest_response in self.top_scores_by_player:
            Na_me, N_o, Cor_rect,  Ques_tion, Ans_wer, Correct_Ans_wer, Sco_re, mean_score, Perc_core = quest_response.values()
            print("{:<30} {:<15} {:<15} {:<15} ".format(
                Na_me, Sco_re, mean_score, Perc_core))

    #
    def print_all_player_score(self, dictionary_of_scores):
        self.dictionary_of_scores = dictionary_of_scores
        print()
        print("{:<10} {:<3} {:<8} {:<35} {:<15} {:<25} {:<5} {:<5} {:<5}".format(
            'Name', 'No.', 'Correct',  'Question: Capital of','Answer','Correct Answer', 
            'Total Score', 'Mean Score', 'Perc. Score'))
        for quest_response in self.dictionary_of_scores:
            Na_me, N_o, Cor_rect,  Ques_tion, Ans_wer, Correct_Ans_wer, Sco_re, mean_score, Perc_core = quest_response.values()
            print("{:<10} {:<3} {:<8} {:<35} {:<15} {:<25} {:<5} {:<5} {:<5}".format(
                Na_me, N_o, Cor_rect, Ques_tion, Ans_wer, Correct_Ans_wer, Sco_re, mean_score, Perc_core))

    #
    def num_of_questions(self):
        while True: 
            try:
                self.player_questions = int(input("How many questions: \t"))
                return(self.player_questions)
                break
            except ValueError:
                print("Enter a valid int: ")


    # main class

    def quiz(self):
        
        # store correct and wrong
        self.data_correct_wrong = []
        self.name_player = self.pla_yer_name()
        #
        # if chosen_number_of_questions > 0:
        # 
        # number of questions
        self.number_of_questions = self.num_of_questions()
        # score
        self.__scores = 0
        # question no
        self.question_number = 1
        # while loop per question
        while self.number_of_questions > 0:
            # check answer type
            self.correct_city, self.answer_city, self.random_country = self.country_city_input()
            if self.answer_city == self.correct_city:
                #print('Correct')
                self.__scores += 1
                self.data_correct_wrong.append(
                    { 'Name': self.name_player, 'No.': self.question_number, 'Correct': 'Correct', 
                     'Question': ' {0} '.format(self.random_country), 
                     'Answer': self.answer_city, 'Correct Answer': self.correct_city, 
                     'Total Score': str(self.__scores) + '/' + str(
                        self.number_of_questions + self.question_number - 1), 
                     'Perc. Score':round(100*self.__scores/self.question_number, 2), 
                     'Mean Score': round(self.__scores/self.question_number, 2)
                    })
                
            else:
                self.__scores += 0
                self.data_correct_wrong.append(
                    { 'Name': self.name_player, 'No.': self.question_number, 'Correct': 'Wrong', 
                     'Question': '{0}'.format(self.random_country), 
                     'Answer': self.answer_city, 'Correct Answer': self.correct_city, 
                     'Total Score': str(self.__scores) + '/' + str(
                        self.number_of_questions + self.question_number -1), 
                     'Perc. Score':round(100*self.__scores/self.question_number, 2), 
                     'Mean Score': round(self.__scores/self.question_number, 2)})
                #print('Your Answer: {}, Correct Answer: {}'.format(answer_city, correct_city))    
            self.question_number += 1
            self.number_of_questions -= 1

            if self.number_of_questions != 0:
                continue
            else:
                self.print_all_player_score(self.data_correct_wrong)
                self.print_player_score(self.data_correct_wrong)
                # code to try another player
                print()
                print()
                self.try_again_option = input('Would you like to play again? (Yes/No): ')
                if self.try_again_option in ['Y', 'y', 'Yes', 'yes']:
                    self.question_number = 1
                    self.name_player = self.pla_yer_name()
                    self.number_of_questions = self.num_of_questions()
                    # get unique player name
                    self.player_unique_names = set(
                        dic_tionary['Name'] for dic_tionary in self.data_correct_wrong)
                    if self.name_player in self.player_unique_names:
                        print('You already played, allow others the chance')
                        self.name_player = self.pla_yer_name()
                else:
                    self.print_all_player_score(self.data_correct_wrong)
                    self.print_player_score(self.data_correct_wrong)
                    self.number_of_questions = 0#
#
if __name__ == '__main__':
    my_quiz = Quiz(country_city_dictionary)
    my_quiz.quiz()
#
##################### END #########################################################################################################
