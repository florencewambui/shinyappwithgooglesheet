# required libraries
library(shiny)
library(shinydashboard)
library(googlesheets)
library(dplyr)

# creating the user interface
shinyApp(ui = dashboardPage(
  skin = "black",
  dashboardHeader(title = "Data Maturity Survey"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    box(width = 12,
    h2("Organization Contacts"),
    textInput("input1", "What is your name:", width = "30%"),
    textInput("input2", "What organization are you representing?", width = "30%"),
    textInput("input3", "What is your job title in the organization?", width = "30%"),
    textInput("input4", "What email address do we send the report to?", width = "30%"),
    h2("Survey"),
    helpText("Please select the most appropriate statement for your organization"),
    h3("Data"),
    radioButtons("input5",  "Data is collected", choices = c("Rarely", "Occasionally", "Consistently")),
    radioButtons("input6", "Data is cleaned, validated and updated", choices = c("Rarely", "Occasionally", "Consistently")),
    radioButtons("input7", "Data collection is automated where possible", choices = c("No", "Yes")),
    radioButtons("input8", "Staff and volunteers are trained on data collection", choices = c("No", "Yes")),
    radioButtons("input9", "External data is sourced", choices = c("No", "Yes")),
    radioButtons("input10", "Data assets are", choices = c("Not known", "Some are known", "Known, but not recorded", "Known and recorded", "Known, recorded, ownership known , data dictionaries prepared and review periods set")),
    h3("Tools"),
    radioButtons("input11", "Data is stored", choices = c("On paper", "In basic databases and spreadsheets", "In separately managed systems such as databases, spreadsheets, CRMs",
                                                          "In appropriate databases/technologies accessible to expert users", "In singly accessible database e.g data warehouse")),
    radioButtons("input12", "Data analysis", choices = c("Is never done", "Is limited to basic tasks done using spreadsheets and basic databases",
                                                         "Is done using advanced analytical tools e.g. R, Python, SPSS, SAS in some teams or departments",
                                                         "Is done using advanced analytical tools e.g. R, Python, SPSS, SAS throughout the organization")),
    radioButtons("input13", "Reports generation is automated", choices = c("No", "Yes")),
    radioButtons("input14", "There is capacity to store, manage and analyse large volumes of data", choices = c("No", "Yes")),
    radioButtons("input15", "Data tools, systems and infrastructure", choices = c("Are not available/appropriate and there are no plans to invest in them",
                                                                                  "Limited and do not meet current needs",
                                                                                  "Allow for some inbuilt analysis and reporting",
                                                                                  "Are up to date with support and there is occasional investments in the tools",
                                                                                  "Able to support business needs and there is ongoing investments"
                                                                                  )),
    h4("Leadership"),
    radioButtons("input16", "The leadership of the organization", choices = c("Is not interested in data",
                                                                              "Is aware of data, but does not see value",
                                                                              "Know data is important, but is not entirely convinced",
                                                                              "Is engaged and supportive in the use of data",
                                                                              "Is engaged and supportive in the use of data",
                                                                              "Value and prioritise data as a vital organizational resource")),
    radioButtons("input17", "With regard to data, the leadership of the organization",
                 choices = c("Does not invest", "Invests little amounts", "Has began to plan and commit significant amounts",
                             "Continuously invests significant amounts")),
    radioButtons("input18", "In making decisions, the leadership of the organization", 
                 choices = c("Does not use data", "Uses data on what happened in the past", "Uses past and current data with some trend analysis",
                             "Uses past, current and forward-looking data")),
    radioButtons("input19", "Within the leadership of the organization",
                 choices = c("There is no data/analytics expertise",
                             "There is limited data/analytics expertise",
                             "There is adequate data/analytics expertise",
                             "There is a data champion within the management",
                             "There is a a range of people with data/analytics expertise")),
    h4("Skills"),
    radioButtons("input20", "In the organization", choices = c("There is no staff committed to data related tasks",
                                                               "Data collection and analysis done by admin and finance",
                                                               "There is dedicated person/team in charge of data",
                                                               "There are dedicated analytical roles in teams")),
    radioButtons("input21", "In the organization", choices = c("There are no staff with data skills/training",
                                                               "There are staff with basic/adequate data skills",
                                                               "There are staff with adequate data skills as part of their job roles",
                                                               "There are established data/analytic roles assigned to appropriately skilled staff")),
    radioButtons("input22", "In the organization", choices = c("There is never training on data/analytics",
                                                               "There is occasional data/analytics training",
                                                               "There is regular data/analytics training",
                                                               "There is ongoing data/analytics training")),
   
    h4("Culture"),
    radioButtons("input23", "Which statement best describes how data is perceived in the organization",
                 choices = c("Data is never considered", 
                             "Data is the responsibilty of a particular person",
                             "Data is considered important at a senior level",
                             "Data is important in the entire organization",
                             "Data is critical and highly valued in every team in the organization"
                             )),
    radioButtons("input24", "In decision making, data is",
                 choices = c("Never used",
                             "Only used to support what the organization already believes",
                             "Used to discuss and challenge past, present and future practices/notions")),
    radioButtons("input25", "Which statement best describes the state of data polices(data protection and security) in the organization",
                 choices = c("There are no data protection and security policies/practices",
                             "There are basic data protection and security policies/practices",
                             "There are data protection and security policies/practices in place. Limited understanding of data legislation by senior management",
                             "There are well established data protection and security policies/practices. Senior management understand current legistlation",
                             "There are robust data protection and security policies/practices. Senior management understand and anticipate future data legislatio")),
    h4("Uses"),
    radioButtons("input26", "Data is collected", 
                 choices = c("Only for requisite reasons such as legal, financial and funding compliance",
                             "For slightly more than for requisite legal, financial and funding reasons", 
                             "To understand/measure needs, outcomes and impacts",
                             "To understand/measure needs, outcomes and impacts and test assumptions",
                             "Used for extensive purposes - measure outcomes, predict needs, understand behaviours"
                             )),
    h4("Analysis"),
    radioButtons("input27", "Which statement best describes the highest level of analytics in the organization",
                 choices = c("Analysis is limited to financial and contractual data mainly counts",
                             "Basic analysis using spreadsheets including counts and charts",
                             "Routine and automated analysis",
                             "Real time dynamic reporting and some advanced analytics e.g A/B testing, predictive analytics",
                             "Advanced analytical approaches e.g predictive, network analysis, text analytics, deep learning")),
    actionButton("submit", "Submit")
    
  ))
),



server = function(input, output){
  user_responses = gs_key("1zw21DIjxDt4qF6fcBUkzL_0Gv1pEIkvZg1KVHrOEjQQ") # always ensure the google sheet has a column names and one row of data. otherwise gs_add_row() will output an error
  user_inputs = reactive(c(input$input1, input$input2, input$input3, input$input4, input$input5, input$input6,
                           input$input7, input$input8, input$input9, input$input10, input$input11, input$input12,
                           input$input13, input$input14, input$input15, input$input16, input$input17, input$input18,
                           input$input19, input$input20, input$input21, input$input22, input$input23, input$input24,
                           input$input25, input$input26, input$input27))
  
  
  
  observeEvent(input$submit,{
    user_responses %>%
      gs_add_row(ws = 1, input = user_inputs())
  })
               
  
}
)