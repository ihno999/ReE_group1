source("pages/peoples/tabs/people.R")
source("pages/peoples/tabs/company.R")

server_companies <- function(input, output, session, rv) {
    server_table_people(input, output, session, rv)
    server_table_company(input, output, session, rv)
}

ui_companies <- fluidPage(
    tabsetPanel(
        tabPanel("Company", ui_table_company),
        tabPanel("People", ui_table_people)
    )
)