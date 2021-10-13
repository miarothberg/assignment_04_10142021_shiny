

library(shiny)
library(tidyverse)
library(DT)
library(bslib)

#load data
HF_eddyflux <- read.csv("HF_eddyflux.csv")
months_short <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
months_df <- data.frame(months, months_short)

#rename variables
# # HF_mean <- HF_mean %>%
# #   mutate(temperature_25 = ta.2.5m.filled.c) %>%
# #   mutate(temperature_27 = ta.27m.filled.c) %>%
# #   mutate(nee = nee.e.6mol.m2.s) %>%
# #   mutate(gee = gee.e.6mol.m2.s) %>%
# #   mutate(resp = resp.e.e.6mol.m2.s) %>%
# #   mutate(radiation = par.28m.filled.e.6mol.m2.s) %>%
## mutate(month = month.month) %>%
# #  mutate(year = year.year) %>%
# #   select(-ta.2.5m.filled.c, ta.27m.filled.c, nee.e.6mol.m2.s, gee.e.6mol.m2.s, resp.e.e.6mol.m2.s, par.28m.filled.e.6mol.m2.s)

HF_eddyflux <- HF_eddyflux %>% 
  left_join(months_df, by = c("month.month" = "months_short")) %>% 
  mutate(month = months) %>% 
  mutate(year = year.year) %>% 
 # group_by(month, year) %>% 
  # mutate(temperature_27 = mean(ta.27m.filled.c)) %>% 
  # mutate(nee = mean(nee.e.6mol.m2.s)) %>% 
  # mutate(gee = mean(gee.e.6mol.m2.s)) %>% 
  # mutate(resp = mean(resp.e.e.6mol.m2.s)) %>% 
  # mutate(radiation = mean(par.28m.filled.e.6mol.m2.s)) %>% 
  mutate(temperature_27 = ta.27m.filled.c) %>% 
  mutate(nee = nee.e.6mol.m2.s) %>% 
  mutate(gee = gee.e.6mol.m2.s) %>% 
  mutate(resp = resp.e.e.6mol.m2.s) %>% 
  mutate(radiation = par.28m.filled.e.6mol.m2.s) %>% 
  select(temperature_27, nee, gee, resp, radiation, month, year)

quant_vars <- HF_eddyflux %>% 
  select(-month, -year, where(is.numeric)) %>% 
  names() %>% 
  sort()

#create a dataset with averages of data between 2007 and 2017 without 2012
# mean_vals_07_17_no <- HF_eddyflux %>% 
#   filter(year.year > 2006) %>% 
#   filter(year.year < 2018) %>%
#   filter(year.year != 2012) %>% 
#   group_by(doy.day, month.month) %>% 
#   summarize(mean_radiation = mean(radiation),
#             # mean_temperature25 = mean(as.numeric(temperature_25)),
#             mean_temperature27 = mean(temperature_27),
#             mean_resp = mean(resp),
#             mean_gee = mean(gee),
#             mean_nee = mean(nee))
# 
# #create a dataset with averages of data from 2012
# HF_mean_12 <- HF_mean %>% 
#   filter(year.year == 2012) %>% 
#   mutate(temperature_27_12 = temperature_27,
#          nee_12 = nee,
#          resp_12 = resp,
#          radiation_12 = radiation) %>% 
#   select(temperature_27_12, nee_12, resp_12, radiation_12, doy.day, month.month)
# 
# #join the two datasets and calculate the differences
# HF_all <- mean_vals_07_17_no %>% 
#   left_join(HF_mean_12, by = "doy.day") %>% 
#   mutate(dif_temp = temperature_27_12-mean_temperature27,
#          dif_nee =  nee_12 - mean_nee,
#          dif_resp = resp_12 - mean_resp,
#          dif_rad = radiation_12 - mean_radiation)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  
  # Application title
  titlePanel("Harvard Forest Eddy Flux Data from 1991 to 2017"),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var", # to use in code
                  label = "Select a variable:", # how it looks in UI
                  choices = quant_vars, 
                  selected = "gee"
      ),
      selectInput(inputId = "month", # to use in code
                  label = "Month:", # how it looks in UI
                  choices = months, # I want this to be all the months
                  selected = "January"
      )
    ),
    
    # Show a plot of cumulative weight for chosen vegetable
    # Show a table beneath
    mainPanel(
      plotOutput(outputId = "dif_nee_graph"),
      dataTableOutput(outputId = "dif_nee_tbl")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  # Enclose in reactive() - makes a function
  HF_smry <- reactive(HF_eddyflux %>% 
                         filter(month == input$month) %>% 
                         group_by(year) %>% 
                        summarize(avg_var = mean(.data[[input$var]])))
                        # group_by(date) %>% 
                        # summarize(daily_wt = sum(weight)) %>% 
                        # mutate(cum_wt = cumsum(daily_wt)))
  
  # Now use that function, with no arguments.
  output$dif_nee_graph <- renderPlot({
    HF_smry() %>% 
      ggplot(aes(x = year)) +
      geom_point(aes(y = avg_var)) +
      geom_line(aes(y = avg_var)) +
      labs(title = paste("Average", input$var, "Changes Over Time in ", input$month),
           x = "",
           y = "") +
      theme_minimal()
  })
  
  output$dif_nee_tbl <- renderDataTable(HF_smry())
  
}

# Run the application 
shinyApp(ui = ui, server = server)