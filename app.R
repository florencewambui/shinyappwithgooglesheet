# loading required libraries

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(googlesheets)
library(tidyverse)

# googlesheet authorisation
#g_auth = gs_auth()
#saveRDS(g_auth, "googlesheets_auth.rds")
gs_auth(new_user = FALSE, gs_auth(token = "googlesheets_auth.rds"))
radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
                                                 $(document).ready(function() {
                                                 setTimeout(function() {
                                                 $('input', $('#", id, "')).each(function(){
                                                 if(this.getAttribute('value') == '", choice, "') {
                                                 opts = $.extend(", options, ", {html: true});
                                                 $(this.parentElement).tooltip('destroy');
                                                 $(this.parentElement).tooltip(opts);
                                                 }
                                                 })
                                                 }, 500)
                                                 });
                                                 ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

mandatory_fields = c("name", "organization", "email")

# ui
shinyApp(ui = dashboardPage(
  skin = "black",
  dashboardHeader(title = "Data Maturity Framework App"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    textInput("name", "Name:"),
    textInput("organization", "Organization:"),
    textInput("email", "Email address:"),
    radioButtons("question1", "Rate your organization in terms of data leadership:", 
                choices = c("Unaware", "Nascent", "Learning", "Developing", "Mastering"),
                inline = T),
    #radioTooltip(id = "question1", choice = "Unaware", title = "Do not collect data blah blah blah",  placement = "bottom",  trigger = "hover")
    actionButton("submit", "Submit")

   )
),
server = function(input, output){
  user_inputs = reactive(c(input$name, input$organization, input$email, Sys.Date(), input$question1))
  data_sheet = gs_title("shinyappwithgooglesheet")
  observeEvent(input$submit,{
    data_sheet = data_sheet %>%
      gs_add_row(ws = 1, input = user_inputs())
    
  })
  
}
  
)