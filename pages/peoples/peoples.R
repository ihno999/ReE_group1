source("pages/peoples/tabs/people.R")
source("pages/peoples/tabs/company.R")

server_peoples <- function(input, output, session) {
    server_table_people(input, output, session)
    server_table_company(input, output, session)
}

ui_peoples <- fluidPage(
    tabsetPanel(
        tabPanel("Company", ui_table_company),
        tabPanel("People", ui_table_people)
    )
)