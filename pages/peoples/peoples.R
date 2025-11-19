source("pages/peoples/tabs/people.R")
source("pages/peoples/tabs/company.R")

server_peoples <- function(input, output) {
    server_table_people(input, output)
    server_table_company(input, output)
}

ui_peoples <- fluidPage(
    tabsetPanel(
        tabPanel("Company", ui_table_company),
        tabPanel("People", ui_table_people)
    )
)