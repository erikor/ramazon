
library(shiny)
library(DT)

reconcile <- function(orders, 
                      items, 
                      save = FALSE,
                      file = "amazon_details.tab") {
    o <- orders
    i <- items
    ix <- match(i$Order.ID, o$Order.ID)
    url <- "https://www.amazon.com/gp/your-account/order-details/ref=ppx_yo_dt_b_order_details_o00?ie=UTF8&orderID="
    res <- data.frame( order = paste0("<a href=\"", 
                                              url, 
                                              i$Order.ID,
                                              "\" target=\"_blank\">Details</a>"),
                       order_date = i$Order.Date,
                       ship_date = i$Shipment.Date,
                       order_total = o$Total.Charged[ix],
                       item = i$Title)
    if(save) {
        write.table(res, 
                    row.names = FALSE,
                    col.names = TRUE,
                    quote = FALSE,
                    sep = "\t",
                    file = file)
    }
    colnames(res) <- c("Order Details", 
                       "Order Date",
                       "Ship Date",
                       "Order Total",
                       "Description")
    res
}

ui <- fluidPage(
#    tags$head(
#        tags$link(includeCSS("www/style.css"))
#    ),
    titlePanel("Amazon Order Details"),

    sidebarLayout(
        sidebarPanel(width = 3,
            fileInput("orders", "Orders Report"),
            fileInput("items", "Items Report")
        ),

        mainPanel(
          DT::dataTableOutput("details")
        )
    )
)

server <- function(input, output) {

    o <- reactiveVal(NULL)
    i <- reactiveVal(NULL)
    
    observeEvent(input$orders, {
        file <- input$orders
        tryCatch({
            o(read.csv(file$datapath))
            unlink(file$datapath)
        }, 
        error = function(e) {
            o(NULL)
        })
    })
    
    observeEvent(input$items, {
        file <- input$items
        tryCatch({
            i(read.csv(file$datapath))
            unlink(file$datapath)
        }, 
        error = function(e) {
            i(NULL)
        })
    })
    
    output$details <- DT::renderDataTable({ 
        if(!is.null(o()) && !is.null(i())) {
            reconcile(o(), i())
        } else {
            data.frame(order = NULL,
                       order_date = NULL,
                       ship_date = NULL,
                       item = NULL,
                       order_total = NULL)
        }
    }, escape = FALSE,
    rownames = FALSE,
    options = list(
        columnDefs = list(list(className = 'dt-center', targets = 0:2))
    ))
    
}

shinyApp(ui = ui, server = server)
