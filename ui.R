library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)
library(ggthemes)

shinyUI(
  dashboardPage(title = "Data Exploration",
                dashboardHeader(title = "ANALYTICS TOOL",
                                dropdownMenu(type = "message",
                                             messageItem("EG","This is an eg",icon = icon("info-sign",lib="glyphicon"))
                                ),
                                dropdownMenu(type = "notifications",
                                             notificationItem( text="This is a noti",icon=icon("info-sign",lib="glyphicon"),status = "warning")
                                ),
                                dropdownMenu(type = "tasks",
                                             taskItem(value = 80,color = "aqua","task")
                                )
                ),
                dashboardSidebar(
                  sidebarMenu(
                    sidebarSearchForm("txtsearch","button","Search") ,
                    menuItem(selectInput("bank","Select the Bank",choices = c("Wespac Bank","Emerald Bank"))),
                    menuItem("Fees",tabName = "fees"),
                    menuItem("Transaction",tabName = "Txn")
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem("fees",h1("Fees Analysis"),
                            navbarPage("Menu",
                                       navbarMenu("Distribution",
                                                  tabPanel("All Fees",
                                                           fluidRow(
                                                             box(title="Summary",verbatimTextOutput("tab11"),width = 10)
                                                           ),
                                                           fluidRow(
                                                             box(title="Histogram", status = "primary",solidHeader = TRUE,
                                                                 sliderInput("bins11","No. of bins",1,40,10),plotOutput("plot11.1")),
                                                             box(title = "Boxplot", status = "primary",solidHeader = TRUE,
                                                                 plotlyOutput("plot11.2"))
                                                             
                                                             
                                                           )
                                                  ),
                                                  tabPanel("Regions",
                                                           fluidRow(
                                                             box(uiOutput("vi"))
                                                             
                                                           ),
                                                           fluidRow(
                                                             box(title="Table", status = "primary",solidHeader = TRUE,
                                                                 tableOutput("tab12")),
                                                             box(title="Plot", status = "primary",solidHeader = TRUE,
                                                                 plotOutput("plot12"))
                                                           )
                                                           
                                                  )
                                       ),
                                       navbarMenu("Targets",
                                                  tabPanel("Fee Targets",
                                                           fluidRow(
                                                             box(status = "primary",solidHeader = TRUE,uiOutput("vj")),
                                                             box(status = "primary",solidHeader = TRUE,uiOutput("vk"),
                                                                 checkboxInput("stateinclude13","Include",FALSE))
                                                           ),
                                                           fluidRow(
                                                             box(title="Plot",status = "primary",solidHeader = TRUE,width=12,
                                                                 radioButtons("target","What to diplay?",choices = c("Target vs Actual Fee Income"="a","Short from Target Fee Income"="b")),
                                                                 plotlyOutput("plot13")
                                                             )
                                                           )
                                                  )
                                       ),
                                       navbarMenu("Customer",
                                                  tabPanel("Individual",
                                                           fluidRow(
                                                             box(status = "primary",solidHeader = TRUE,uiOutput("vl"))
                                                           ),
                                                           fluidRow(
                                                             box(title="Table",status = "primary",solidHeader = TRUE,
                                                                 column(width = 12,
                                                                        DT::dataTableOutput("tab14"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                                                 )
                                                             ),
                                                             box(title="Most Profitable Customers",status = "primary",solidHeader = TRUE,
                                                                 sliderInput("slide14","No of Customers",1,25,10),
                                                                 plotOutput("plot14"))
                                                             
                                                           )
                                                           
                                                  ),
                                                  tabPanel("Grouped",
                                                           tabsetPanel(type="tabs",
                                                                       tabPanel("Fee Amount",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,uiOutput("vm")),
                                                                                  box(status = "primary",solidHeader = TRUE,uiOutput("vn"),
                                                                                      checkboxInput("stateinclude15","Include",FALSE))
                                                                                ),
                                                                                fluidRow(
                                                                                  box(title="Table",status = "primary",solidHeader = TRUE,
                                                                                      radioButtons("group15","Select any one",choices = c("Total Fee"="a","Avg Fee"="b")),
                                                                                      tableOutput("tab15")
                                                                                  ),
                                                                                  box(title="Plot",status = "primary",solidHeader = TRUE,
                                                                                      plotOutput("plot15"))
                                                                                )
                                                                       ),
                                                                       tabPanel("Fee Count",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,uiOutput("vo")),
                                                                                  box(status = "primary",solidHeader = TRUE,uiOutput("vp"),
                                                                                      checkboxInput("stateinclude16","Include",FALSE))
                                                                                ),
                                                                                fluidRow(
                                                                                  box(title="Plot",status = "primary",solidHeader = TRUE,
                                                                                      plotOutput("plot16"),width=12)
                                                                                )
                                                                                
                                                                       )
                                                           )
                                                           
                                                  )
                                       ),
                                       navbarMenu("Products and Services",
                                                  tabPanel("Products",
                                                           fluidRow(
                                                             box(status = "primary",solidHeader = TRUE,uiOutput("vq")
                                                             ),
                                                             box(status = "primary",solidHeader = TRUE,uiOutput("vr"),
                                                                 checkboxInput("stateinclude17","Include",FALSE))
                                                           ),
                                                           fluidRow(
                                                             box(title="Table",status = "primary",solidHeader = TRUE,
                                                                 column(width = 12,
                                                                        DT::dataTableOutput("tab17"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                                                 )
                                                             ),
                                                             box(title="Most Profitable Products",status = "primary",solidHeader = TRUE,
                                                                 sliderInput("slide17","No of Products",1,25,10),
                                                                 plotOutput("plot17")
                                                             )
                                                           )
                                                  ),
                                                  tabPanel("Services",
                                                           fluidRow(
                                                             box(status = "primary",solidHeader = TRUE,uiOutput("vs")),
                                                             box(status = "primary",solidHeader = TRUE,uiOutput("vt"),
                                                                 checkboxInput("stateinclude18","Include",FALSE))
                                                           ),
                                                           fluidRow(
                                                             box(title="Table",status = "primary",solidHeader = TRUE,
                                                                 column(width = 12,
                                                                        DT::dataTableOutput("tab18"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                                                 )
                                                             ),
                                                             box(title="Most Profitable Services",status = "primary",solidHeader = TRUE,
                                                                 sliderInput("slide18","No of Services",1,25,10),
                                                                 plotOutput("plot18")
                                                             )
                                                           )
                                                  )
                                       )
                                       
                            )
                    ),
                    tabItem(tabName="Txn",h1("Transaction Analysis"),
                            navbarPage("Menu",
                                       navbarMenu("Transaction Volumes",
                                                  tabPanel("All Counts",
                                                           fluidRow(
                                                             box(status = "primary",solidHeader = TRUE,width =4,collapsible = T,
                                                                 uiOutput("v1"),checkboxInput("include0.1","Include",value = FALSE)),
                                                             box(status = "primary",solidHeader = TRUE,width =4,collapsible = T,
                                                                 uiOutput("v2"),checkboxInput("include0.2","Include",value = FALSE))
                                                             
                                                           ),
                                                           fluidRow(
                                                             tabBox(
                                                               id = "tabbox1",
                                                               tabPanel("Table",tableOutput("tab"))),
                                                             box(title="Plot",status = "primary",solidHeader = TRUE,plotOutput("bar"))
                                                           )
                                                           
                                                  ),
                                                  tabPanel("Channels",
                                                           tabsetPanel(type="tabs", 
                                                                       tabPanel("Channel Type",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,
                                                                                      uiOutput("vx")
                                                                                  ),
                                                                                  #infoBox("SALES",10000,icon=icon("thumbs-up"))
                                                                                  #infoBoxOutput("sales")
                                                                                  box(title = "Fill the plot",solidHeader = TRUE,
                                                                                      radioButtons("filling","Select from the following",choices = c("Channel Type","Service ID")))
                                                                                ),
                                                                                
                                                                                fluidRow(
                                                                                  tabBox(
                                                                                    id = "tabbox2",height = 500,
                                                                                    tabPanel("Table",tableOutput("tab2"),column(width = 12,DT::dataTableOutput("tab2.2"),style = "height:250px; overflow-y: scroll;overflow-x: scroll;")),
                                                                                    tabPanel("Percentage",tableOutput("percent2"),column(width = 12,DT::dataTableOutput("percent2.2"),style = "height:250px; overflow-y: scroll;overflow-x: scroll;")
                                                                                    )),
                                                                                  box(title="Plot",status = "primary",solidHeader = TRUE,
                                                                                      radioButtons("facet","Group by Cities",choices = c("Yes","No")),
                                                                                      plotOutput("bar2")
                                                                                  )
                                                                                )
                                                                                
                                                                       ),
                                                                       tabPanel("Channel ID",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,
                                                                                      uiOutput("vy")
                                                                                  )
                                                                                ),
                                                                                fluidRow(
                                                                                  tabBox(
                                                                                    id = "tabbox3",
                                                                                    tabPanel("Table",tableOutput("tab3")),
                                                                                    tabPanel("Percentage",tableOutput("percent3"))
                                                                                  ),
                                                                                  box(title="Plot",status = "primary",solidHeader = TRUE,
                                                                                      radioButtons("atm","Only ATMS?",choices = c("Yes","No"),selected = "No"),
                                                                                      plotOutput("bar3")
                                                                                  )
                                                                                )
                                                                                
                                                                       ),
                                                                       tabPanel("Customer",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,
                                                                                      uiOutput("vb"))
                                                                                ),
                                                                                fluidRow(
                                                                                  tabBox(
                                                                                    id = "tabbox6",
                                                                                    tabPanel("Table",
                                                                                             column(width = 12,
                                                                                                    DT::dataTableOutput("tab6"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                                                                             )
                                                                                    ),
                                                                                    tabPanel("Percentage",
                                                                                             column(width = 12,
                                                                                                    DT::dataTableOutput("percent6"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                                                                             )
                                                                                    )),
                                                                                  box(title="Plot",status = "primary",solidHeader = TRUE,
                                                                                      plotOutput("plot6")
                                                                                  )
                                                                                ))
                                                                       
                                                                       
                                                           )
                                                  ),
                                                  tabPanel("Events and Products",
                                                           tabsetPanel(type="tabs",
                                                                       tabPanel("Events",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,
                                                                                      uiOutput("vz")
                                                                                  )
                                                                                ),
                                                                                fluidRow(
                                                                                  tabBox(id = "Tabbox4",
                                                                                         tabPanel("Table",tableOutput("tab4")),
                                                                                         tabPanel("Percentage",tableOutput("percent4"))
                                                                                  ),
                                                                                  box(title = "Plot",status = "primary",solidHeader = TRUE,
                                                                                      radioButtons("facet2","Group by Cities",choices = c("Yes","No")),
                                                                                      plotOutput("bar4")
                                                                                  )
                                                                                )
                                                                       ),
                                                                       tabPanel("Products",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,
                                                                                      uiOutput("va")
                                                                                  )
                                                                                ),
                                                                                fluidRow(
                                                                                  tabBox(id = "Tabbox4",
                                                                                         tabPanel("Table",tableOutput("tab5")),
                                                                                         tabPanel("Percentage",tableOutput("percent5"))
                                                                                  ),
                                                                                  box(title = "Plot",status = "primary",solidHeader = TRUE,
                                                                                      plotOutput("bar5")
                                                                                  )
                                                                                )
                                                                       )
                                                           )
                                                  )
                                                  
                                                  
                                                  
                                       ),
                                       navbarMenu("Transaction Amounts",
                                                  tabPanel("Amount Distribution",
                                                           fluidRow(
                                                             box(status = "primary",solidHeader = TRUE,width =4,collapsible = T,
                                                                 uiOutput("vc"),checkboxInput("include1","Include",value = FALSE)),
                                                             box(status = "primary",solidHeader = TRUE,width =4,collapsible = T,
                                                                 uiOutput("vd"),checkboxInput("include2","Include",value = FALSE)),
                                                             box(status = "primary",solidHeader = TRUE,width = 4,collapsible = T,
                                                                 uiOutput("ve"),checkboxInput("include3","Include",value = FALSE))
                                                           ),
                                                           fluidRow(
                                                             box(title = "Histogram",status = "primary",solidHeader = TRUE,
                                                                 sliderInput("bins","No. of bins",1,40,10),
                                                                 plotOutput("plot7.1")
                                                                 
                                                             ),
                                                             box(title = "Boxplot",status = "primary",solidHeader = TRUE,
                                                                 plotlyOutput("plot7.2")
                                                             )
                                                           )
                                                  ),
                                                  tabPanel("Time Analysis",
                                                           fluidRow(
                                                             box(status = "primary",solidHeader = TRUE,
                                                                 uiOutput("vf")),
                                                             box(radioButtons("show","SHOW",choices = c("Total Transaction","Average Transaction")))
                                                           ),
                                                           fluidRow(
                                                             box(title="Table",status = "primary",solidHeader = TRUE,
                                                                 column(width = 12,
                                                                        DT::dataTableOutput("tab8"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                                                 )),
                                                             box(title="Plot",status = "primary",solidHeader = TRUE,
                                                                 plotlyOutput("plot8"))
                                                           )
                                                           
                                                           
                                                           
                                                  ),
                                                  tabPanel("Channels and Customers",
                                                           tabsetPanel(type="tabs",
                                                                       tabPanel("Channel Type",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,uiOutput("vg"))
                                                                                ),
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,
                                                                                      radioButtons("test","Perform T-Test",choices = c("Yes","No"),selected = "No"),
                                                                                      textOutput("testexplain"),
                                                                                      verbatimTextOutput("t.test"),
                                                                                      tableOutput("mean")
                                                                                  ),
                                                                                  box(title = "Plot",status = "primary",solidHeader = TRUE,
                                                                                      radioButtons("log","",choices = c("Linear scale","Log scale")),
                                                                                      plotOutput("plot9")
                                                                                  )
                                                                                )
                                                                       ),
                                                                       tabPanel("Customers",
                                                                                fluidRow(
                                                                                  box(status = "primary",solidHeader = TRUE,uiOutput("vh"))
                                                                                ),
                                                                                fluidRow(
                                                                                  box(title="Table",status = "primary",solidHeader = TRUE,
                                                                                      column(width = 12,
                                                                                             DT::dataTableOutput("tab10"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                                                                                      )
                                                                                  ),
                                                                                  box(title = "Top 5 Cust with most Txn Amt",status = "primary",solidHeader = TRUE,
                                                                                      plotOutput("plot10")
                                                                                  )
                                                                                )
                                                                       )
                                                           ))
                                       )
                                       
                            )
                    )
                  )
                  
                  
                )
  )
)
