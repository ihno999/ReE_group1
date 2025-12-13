source("pages/peoples/tabs/people.R")
source("pages/peoples/tabs/company.R")

# server that creates a per-session shared reactiveValues and passes it to both tab servers
server_companies <- function(input, output, session, rv) {
    server_table_people(input, output, session, rv)
    server_table_company(input, output, session, rv)
    observeEvent(input$companies_tabs, {
        if (input$companies_tabs == "People") {
            rv$trigger_people_refresh <- Sys.time()
        }
    })
}
# simple UI combining both tabs (adjust as your app uses)
ui_companies <- fluidPage(
    tabsetPanel(
        id = "companies_tabs",
        tabPanel("Company", ui_table_company),
        tabPanel("People", ui_table_people)
    )
)
