library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(ggthemes)
library(DT)
library("rJava", lib.loc="~/R/win-library/3.5")
library("DBI", lib.loc="~/R/win-library/3.5")
library("RJDBC", lib.loc="~/R/win-library/3.5")
jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="C:/JDBC/ojdbc6.jar")
jdbcConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//10.180.84.34:1522/PKUBDEV", "PRICANATC", "welcome1")

fact_table=dbReadTable(jdbcConnection,'FACT_TABLE')
cust_dim=dbReadTable(jdbcConnection,'CUSTOMER_DIM')
account_dim=dbReadTable(jdbcConnection,'ACCOUNT_DIM')
branch_dim=dbReadTable(jdbcConnection,'BRANCH_DIM')
channel_dim=dbReadTable(jdbcConnection,'CHANNEL_DIM')
product_dim=dbReadTable(jdbcConnection,'PRODUCT_DIM')
service_dim=dbReadTable(jdbcConnection,'SERVICE_DIM')

library(eeptools)
cust_dim$CUST_BIRTHDATE <- as.Date(cust_dim$CUST_BIRTHDATE)
#removing -ve Age Entries
cust_dim<- cust_dim[-c(520) ];#removing faulty entry
cust_dim$CUST_BIRTHDATE <- as.Date(cust_dim$CUST_BIRTHDATE)

#DOB to AGE CONVERSION
ages <- age_calc(na.omit(cust_dim$CUST_BIRTHDATE),Sys.Date(), units = "years")
cust_dim$CUST_AGE[!is.na(cust_dim$CUST_BIRTHDATE)] <- ages

#merging
final_table<-fact_table
final_table<-final_table[complete.cases(final_table[ ,"TRANSACTION_ID"]),]
final_table<-merge(final_table,account_dim,by="ACCOUNT_ID")
final_table<-merge(final_table,branch_dim,by="BRANCH_ID")
final_table<-merge(final_table,channel_dim,by="CHANNEL_ID")
final_table<-merge(final_table,cust_dim,by="CUSTOMER_ID")
final_table<-merge(final_table,product_dim,by="PRODUCT_ID")
final_table<-merge(final_table,service_dim,by="SERVICE_ID")

final_table_txn<-subset(final_table,final_table$CHANNEL_ID!="0")

main_table_WEAU<-subset(final_table,final_table$BANK_ID=="03")
main_table_EMUS<-subset(final_table,final_table$BANK_ID=="30")

main_table_WEAU_txn<-subset(main_table_WEAU,main_table_WEAU$CHANNEL_ID!="0")
main_table_EMUS_txn<-subset(main_table_EMUS,main_table_EMUS$CHANNEL_ID!="0")


WEAU_txn_atm<-subset(main_table_WEAU_txn,main_table_WEAU_txn$CHANNEL_TYPE=="ATM")
EMUS_txn_atm<-subset(main_table_EMUS_txn,main_table_EMUS_txn$CHANNEL_TYPE=="ATM")


shinyServer(
  
  function(input, output) {
    
    bank_df<-reactive({
      if(input$bank=="Wespac Bank"){
        main_table_WEAU
      } else {
        main_table_EMUS
      }
    })
    
    bank_txn_df<-reactive({
      if(input$bank=="Wespac Bank"){
        main_table_WEAU_txn
      } else {
        main_table_EMUS_txn
      }
    })
    
    
    output$v1 <- renderUI({
      df<-bank_txn_df()
      selectInput("count0","select the country",choices =unique(df$COUNTRY))
      
    })
    
    
    output$v2 <- renderUI({
      df<-bank_txn_df()
      df<-subset(df,df$COUNTRY==input$count0)
      selectInput("state0","select the state",choices =unique(df$STATE))
      
    })
    
    
    
    output$vx <- renderUI({
      df<-bank_txn_df()
      selectInput("state","select the state",choices =unique(df$STATE))  
    })
    
    output$vy <- renderUI({
      df<-bank_txn_df()
      selectInput("pin","select the pincode",choices =unique(df$CH_PINCODE))
      
    })
    
    output$vz <- renderUI({
      df<-bank_txn_df()
      selectInput("state2","select the state",choices =unique(df$STATE))
      
    })
    
    output$va <- renderUI({
      df<-bank_txn_df()
      selectInput("state3","select the state",choices =unique(df$STATE))
      
    })
    
    
    
    output$vb <- renderUI({
      df<-bank_txn_df()
      df<-subset(df,df$CHANNEL_TYPE=="ATM")
      selectInput("pin2","select the pincode of the area",choices =unique(df$CH_PINCODE))
      
    })
    
    
    output$vc <- renderUI({
      df<-bank_txn_df()
      selectInput("count4","select the country",choices =unique(df$COUNTRY))
      
    })
    
    
    output$vd <- renderUI({
      df<-bank_txn_df()
      df<-subset(df,df$COUNTRY==input$count4)
      selectInput("state4","select the state",choices =unique(df$STATE))
      
    })
    
    
    output$ve <- renderUI({
      df<-bank_txn_df()
      df<-subset(df,df$COUNTRY==input$count4)
      df<-subset(df,df$STATE==input$state4)
      selectInput("city4","select the city",choices =unique(df$CITY))
      
    })
    
    
    output$vf <- renderUI({
      df<-bank_txn_df()
      selectInput("city5","select the city",choices =unique(df$CITY))
      
    })
    
    output$vg <- renderUI({
      df<-bank_txn_df()
      selectInput("pin6","select the pincode",choices =unique(df$CH_PINCODE))
      
    })
    
    output$vh <- renderUI({
      df<-bank_txn_df()
      selectInput("pin7","select the pincode",choices =unique(df$CH_PINCODE))
    })
    
    output$vi <- renderUI({
      df<-bank_df()
      selectInput("count12","select the country",choices =unique(df$COUNTRY))
    })
    
    output$vj <- renderUI({
      df<-bank_df()
      selectInput("count13","select the country",choices =unique(df$COUNTRY))
    })
    
    output$vk <- renderUI({
      df<-bank_df()
      df=subset(df,df$COUNTRY==input$count13)
      selectInput("state13","select the state",choices =unique(df$STATE))
      
    })
    
    output$vl <- renderUI({
      df<-bank_df()
      selectInput("city14","select the city",choices =unique(df$CITY))
      
    })
    
    output$vm <- renderUI({
      df<-bank_df()
      selectInput("count15","select the country",choices =unique(df$COUNTRY))
    })
    
    output$vn <- renderUI({
      df<-bank_df()
      df=subset(df,df$COUNTRY==input$count15)
      selectInput("state15","select the state",choices =unique(df$STATE))
      
    })
    
    output$vo <- renderUI({
      df<-bank_df()
      selectInput("count16","select the country",choices =unique(df$COUNTRY))
    })
    
    output$vp <- renderUI({
      df<-bank_df()
      df=subset(df,df$COUNTRY==input$count16)
      selectInput("state16","select the state",choices =unique(df$STATE))
      
    })
    
    output$vq <- renderUI({
      df<-bank_df()
      selectInput("count17","select the country",choices =unique(df$COUNTRY))
    })
    
    output$vr <- renderUI({
      df<-bank_df()
      df=subset(df,df$COUNTRY==input$count17)
      selectInput("state17","select the state",choices =unique(df$STATE))
      
    })
    
    output$vs <- renderUI({
      df<-bank_df()
      selectInput("count18","select the country",choices =unique(df$COUNTRY))
    })
    
    output$vt <- renderUI({
      df<-bank_df()
      df=subset(df,df$COUNTRY==input$count18)
      selectInput("state18","select the state",choices =unique(df$STATE))
      
    })
    
    
    output$bar<-renderPlot({
      df<-bank_txn_df()
      if(input$include0.1==FALSE& input$include0.2==FALSE ){
        ggplot(df,aes(x=COUNTRY))+
          geom_bar(aes(y = (..count..),fill=COUNTRY))+
          geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black")+labs(title="BANK",x="COUNTRY",y="COUNT")+ scale_fill_discrete(name = "Legend")
        #countries
      } else if(input$include0.1==TRUE & input$include0.2==FALSE ){
        theplot<-input$count0
        df<-subset(df,df$COUNTRY==input$count0)
        ggplot(df,aes(x=STATE))+
          geom_bar(aes(y = (..count..),fill=STATE))+
          geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black")+labs(title=theplot,x="STATE",y="COUNT")+ scale_fill_discrete(name = "Legend")
        #states
      } else if(input$include0.1==TRUE & input$include0.2==TRUE ){
        theplot<-input$state0
        df<-subset(df,df$COUNTRY==input$count0)
        df<-subset(df,df$STATE==input$state0)
        ggplot(df,aes(x=CITY))+
          geom_bar(aes(y = (..count..),fill=CITY))+
          geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black")+labs(title=theplot,x="CITY",y="COUNT")+ scale_fill_discrete(name = "Legend")
        #cities
      } else{
        ggplot(df)+title("INCLUDE COUNTRY AND CITY IN THE CORRECT ORDER")
      }
      
    })
    
    
    
    output$tab<- renderTable({
      df<-bank_txn_df()
      if(input$include0.1==FALSE& input$include0.2==FALSE ){
        xtabs(~COUNTRY,data = df)
        #countries
      } else if(input$include0.1==TRUE & input$include0.2==FALSE ){
        df<-subset(df,df$COUNTRY==input$count0)
        xtabs(~STATE,data = df)
        #states
      } else if(input$include0.1==TRUE & input$include0.2==TRUE ){
        df<-subset(df,df$COUNTRY==input$count0)
        df<-subset(df,df$STATE==input$state0)
        xtabs(~CITY,data = df)
        #cities
      } else{
        df<-NULL
      }
      
    })
    
    
    
    output$tab2<- renderTable({
      df<-bank_txn_df()
      df<-subset(df,df$STATE==input$state)
      tb<-as.data.frame.matrix(addmargins(xtabs(~CHANNEL_TYPE+CITY,data=subset(df)),2))
      tb
    }, include.rownames=TRUE)
    
    output$tab2.2<- renderDataTable({
      if(input$filling=="Service ID"){
        df<-bank_txn_df()
        df<-subset(df,df$STATE==input$state)
        tb<-as.data.frame.matrix(addmargins(xtabs(~SERVICE_ID+CITY,data=subset(df)),2))
        datatable(tb,options = list(paging = FALSE))
        
      }
    })
    
    output$bar2<-renderPlot({
      
      df<-bank_txn_df()
      df<-subset(df,df$STATE==input$state)
      if(input$filling=="Channel Type"){
        p<-ggplot(df,aes(CHANNEL_TYPE)) +
          geom_bar(aes(fill = CHANNEL_TYPE))+labs(title=input$state)
        if(input$facet=="Yes"){
          p+facet_wrap(~df$CITY)
        } else {
          p
        }
      } else{
        p<-ggplot(df,aes(CHANNEL_TYPE)) +
          geom_bar(aes(fill = SERVICE_ID))+labs(title=input$state)
        if(input$facet=="Yes"){
          p + facet_wrap(~df$CITY)
        } else{
          p
        }
      }
    })
    
    
    output$percent2<- renderTable({
      df<-bank_txn_df()
      df<-subset(df,df$STATE==input$state)
      tb<-as.data.frame.matrix(prop.table(addmargins(xtabs(~CHANNEL_TYPE+CITY,data=df),2),2))
      (tb)*100
    }, include.rownames=TRUE)
    
    output$percent2.2<- renderDataTable({
      if(input$filling=="Service ID"){
        df<-bank_txn_df()
        df<-subset(df,df$STATE==input$state)
        tb<-as.data.frame.matrix(prop.table(addmargins(xtabs(~SERVICE_ID+CITY,data=subset(df)),2),2))
        datatable(tb*100,options = list(paging = FALSE))
      }
    })
    
    
    output$tab3<- renderTable({
      df<-bank_txn_df()
      if(input$atm=="Yes"){
        df<-subset(df,df$CHANNEL_TYPE=="ATM")
      }
      df<-subset(df,df$CH_PINCODE==input$pin)
      addmargins(xtabs(~CHANNEL_ID,data=df))
    })
    
    
    output$percent3<- renderTable({
      df<-bank_txn_df()
      if(input$atm=="Yes"){
        df<-subset(df,df$CHANNEL_TYPE=="ATM")
      }
      df<-subset(df,df$CH_PINCODE==input$pin)
      prop.table(xtabs(~CHANNEL_ID,data=df))*100
      
    })
    
    output$bar3<-renderPlot({
      df<-bank_txn_df()
      if(input$atm=="Yes"){
        df<-subset(df,df$CHANNEL_TYPE=="ATM")
      }
      df<-subset(df,df$CH_PINCODE==input$pin)
      ggplot(df,aes(CHANNEL_ID))+
        geom_bar(aes(fill=CHANNEL_ID))+labs(title=input$pin)
      
    })
    
    output$tab4<- renderTable({
      df<-bank_txn_df()
      df<-subset(df,df$STATE==input$state2)
      tb<-as.data.frame.matrix(addmargins(xtabs(~SERVICE_ID+CITY,data=df)))
      tb 
    }, include.rownames=TRUE)
    
    output$percent4<- renderTable({
      df<-bank_txn_df()
      df<-subset(df,df$STATE==input$state2)
      tb<-as.data.frame.matrix(prop.table(addmargins(xtabs(~SERVICE_ID+CITY,data=df),2),2)*100)
      tb
    }, include.rownames=TRUE)
    
    
    output$bar4<-renderPlot({
      df<-bank_txn_df()
      df<-subset(df,df$STATE==input$state2)
      p<-ggplot(df,aes(SERVICE_ID))+geom_bar(aes(fill=SERVICE_ID))+title(input$state2)
      if(input$facet2=="Yes"){
        p+ facet_wrap(~df$CITY)
      } else{
        p
      }
    })
    
    
    output$tab5<- renderTable({
      df<-bank_txn_df()
      df<-subset(df,df$STATE==input$state3)
      tb<-as.data.frame.matrix(addmargins(xtabs(~PRODUCT_ID+CITY,data=df)))
      tb 
    }, include.rownames=TRUE)
    
    
    output$percent5<- renderTable({
      df<-bank_txn_df()
      df<-subset(df,df$STATE==input$state3)
      tb<-as.data.frame.matrix(prop.table(addmargins(xtabs(~PRODUCT_ID+CITY,data=df),2),2)*100)
      tb
    }, include.rownames=TRUE)
    
    output$bar5<-renderPlot({
      library(dplyr)
      df<-bank_txn_df()
      df <- subset(df,df$STATE==input$state3) %>%
        group_by(PRODUCT_ID) %>%
        summarise(counts = n())
      ggplot(df, aes(PRODUCT_ID, counts)) +
        geom_linerange(aes(x = PRODUCT_ID, ymin = 0, ymax = counts), color = "lightgray", size = 1.5) +
        geom_point(aes(color = PRODUCT_ID), size = 5)+ggpubr::color_palette("ucscgb");
      #upto 26 products will be displayed
    })
    
    output$tab6<- renderDataTable({
      df<-bank_txn_df()
      df<-subset(df,df$CHANNEL_TYPE=="ATM")
      df<-subset(df,df$CH_PINCODE==input$pin2)
      tb<-as.data.frame.matrix(addmargins(xtabs(~CUSTOMER_ID+CHANNEL_ID,data=df)))
      datatable(tb ,options = list(paging = FALSE))
      
    })
    
    output$percent6<- renderDataTable({
      df<-bank_txn_df()
      df<-subset(df,df$CHANNEL_TYPE=="ATM")
      df<-subset(df,df$CH_PINCODE==input$pin2)
      tb<-as.data.frame.matrix(prop.table(addmargins(xtabs(~CUSTOMER_ID+CHANNEL_ID,data=df),1),1)*100)
      datatable(tb ,options = list(paging = FALSE))
      
    })
    
    
    output$plot6<-renderPlot({
      df<-bank_txn_df()
      df<-subset(df,df$CHANNEL_TYPE=="ATM")
      df<-subset(df,df$CH_PINCODE==input$pin2)
      p <- ggplot(df, aes(CHANNEL_ID, CUSTOMER_ID))
      p + geom_jitter(aes(colour=CHANNEL_ID),width = .3,height = 0.15)
    })
    
    
    
    output$plot7.1<-renderPlot({
      df<-bank_txn_df()
      if(input$include1==FALSE& input$include2==FALSE & input$include3==FALSE){
        theplot<-"WESTPAC"
      } else if(input$include1==TRUE & input$include2==FALSE & input$include3==FALSE){
        theplot<-input$count4
        df<-subset(df,df$COUNTRY==input$count4)
      } else if(input$include1==TRUE & input$include2==TRUE & input$include3==FALSE){
        df<-subset(df,df$COUNTRY==input$count4)
        df<-subset(df,df$STATE==input$state4)
        theplot<-input$state4
      } else if(input$include1==TRUE& input$include2==TRUE & input$include3==TRUE){
        df<-subset(df,df$COUNTRY==input$count4)
        df<-subset(df,df$STATE==input$state4)
        df<-subset(df,df$CITY==input$city4)
        theplot<-input$city4
      } else{
        df<-NULL
      }
      ggplot(df,aes(df$TRANSACTION_AMOUNT))+geom_histogram(bins = input$bins,fill="darkgray",colour="black")+labs(title=theplot,x="AMOUNT")
    })
    
    
    output$plot7.2<-renderPlotly({
      df<-bank_txn_df()
      if(input$include1==FALSE& input$include2==FALSE & input$include3==FALSE){
        theplot<-"COUNTRIES"
        ggplotly(ggplot((df), aes("",TRANSACTION_AMOUNT)) + geom_boxplot() + facet_wrap(~df$COUNTRY)+labs(title=theplot))
      } else if(input$include1==TRUE & input$include2==FALSE & input$include3==FALSE){
        theplot<-"STATES"
        df<-subset(df,df$COUNTRY==input$count4)
        ggplotly(ggplot((df), aes("",TRANSACTION_AMOUNT)) + geom_boxplot() + facet_wrap(~df$STATE)+labs(title=theplot))
      } else if(input$include1==TRUE & input$include2==TRUE & input$include3==FALSE){
        df<-subset(df,df$COUNTRY==input$count4)
        df<-subset(df,df$STATE==input$state4)
        theplot<-"CITIES"
        ggplotly(ggplot((df), aes("",TRANSACTION_AMOUNT)) + geom_boxplot() + facet_wrap(~df$CITY)+labs(title=theplot))
      } else if(input$include1==TRUE & input$include2==TRUE & input$include3==TRUE){
        df<-subset(df,df$COUNTRY==input$count4)
        df<-subset(df,df$STATE==input$state4)
        df<-subset(df,df$CITY==input$city4)
        theplot<-input$city4
        ggplotly(ggplot((df), aes("",TRANSACTION_AMOUNT)) + geom_boxplot()+labs(title=theplot))
      } else{
        df<-NULL
      }
    })
    
    output$tab8<-renderDataTable({
      df<-bank_txn_df()
      df<-subset(df,df$CITY==input$city5)
      df$COMPUTE_DATE<-as.Date(df$COMPUTE_DATE)
      if(input$show=="Total Transaction"){
        q1<-aggregate(df$TRANSACTION_AMOUNT,by=list(DATE=df$COMPUTE_DATE), sum)
        q1$TxnSum<-q1$x;
      } else{
        q1<-aggregate(df$TRANSACTION_AMOUNT,by=list(DATE=df$COMPUTE_DATE), mean)
        q1$AvgTxn<-q1$x;
        q1$AvgTxn<-round(q1$AvgTxn,2)
      }
      q1$x<-NULL;
      q1$DATE<-as.Date(q1$DATE)
      datatable(q1 ,options = list(paging = FALSE))
      
    })
    
    
    output$plot8<-renderPlotly({
      options(scipen = 999)
      df<-bank_txn_df()
      
      df<-subset(df,df$CITY==input$city5)
      df$COMPUTE_DATE<-as.Date(df$COMPUTE_DATE)
      if(input$show=="Total Transaction"){
        q1<-aggregate(df$TRANSACTION_AMOUNT,by=list(DATE=df$COMPUTE_DATE), sum)
        q1$TxnSum<-q1$x;
        ytitle<-"Total Txn"
      } else{
        q1<-aggregate(df$TRANSACTION_AMOUNT,by=list(DATE=df$COMPUTE_DATE), mean)
        q1$TxnSum<-q1$x;
        ytitle<-"Average Txn"
      }
      q1$x<-NULL;
      q1$DATE<-as.Date(q1$DATE)
      ggplot(q1, aes((q1$DATE) , TxnSum)) + geom_line(colour="turquoise",size=1) +
        scale_x_date()  + ylab(ytitle)+xlab("Time")+labs(title=input$city5)
    })
    
    
    output$testexplain<-renderText({" Is it luck? or are these results repeatable?
      A t test compares the means of the two groups
      and calculates the probability of these results happening by chance.
      The Null Hypthesis is that there is no significant difference between
      the means of the two group. If the p value is less than 0.05
      we reject the null hypothesis and it implies that
      the results are statistically significant."
    })
    
    output$t.test<-renderPrint({
      df<-bank_txn_df()
      df<-subset(df,df$CH_PINCODE==input$pin6)
      if(input$test=="Yes"){
        print("P Value:")
        t.test(TRANSACTION_AMOUNT~CHANNEL_TYPE,data=df,alt="less")$p.value  
      }
    })
    
    output$mean<-renderTable({
      df<-bank_txn_df()
      df<-subset(df,df$CH_PINCODE==input$pin6)
      if(input$test=="Yes"){
        aggregate(df$TRANSACTION_AMOUNT,by=list(df$CHANNEL_TYPE),mean)                                                              
      }
    })
    
    
    output$plot9<-renderPlot({
      df<-bank_txn_df()
      df<-subset(df,df$CH_PINCODE==input$pin6)
      if(input$log=="Linear scale"){
        p <- ggplot(df, aes(CHANNEL_TYPE, TRANSACTION_AMOUNT))
      } else{
        p <- ggplot(df, aes(CHANNEL_TYPE, log(TRANSACTION_AMOUNT)))
      }
      p + geom_boxplot(colour = "grey50") + geom_jitter(aes(colour=CHANNEL_TYPE),width = .3,height = 0.2)
    })
    
    
    output$tab10<-renderDataTable({
      df<-bank_txn_df()
      df<-subset(df,df$CH_PINCODE==input$pin7)
      dfg<-aggregate(df$TRANSACTION_AMOUNT, by=list(df$CUSTOMER_ID), sum)
      dfg$Customer<-dfg$Group.1
      dfg$TotalTxn<-dfg$x
      dfg$x<-NULL
      dfg$Group.1<-NULL
      dfg<-dfg[order(-dfg[,2]), ]
      datatable(dfg ,options = list(paging = FALSE))
    })
    
    output$plot10<-renderPlot({
      df<-bank_txn_df()
      df<-subset(df,df$CH_PINCODE==input$pin7)
      dfg<-aggregate(df$TRANSACTION_AMOUNT, by=list(df$CUSTOMER_ID), sum)
      dfg$Customer<-dfg$Group.1
      dfg$TotalTxn<-dfg$x
      dfg$x<-NULL
      dfg$Group.1<-NULL
      dfg<-dfg[order(-dfg[,2]), ]
      dfg<-head(dfg,5)
      ggplot(dfg, aes(dfg$Customer, (dfg$TotalTxn))) + geom_bar(aes(fill=dfg$Customer),stat = "identity")+labs(x="Cust",y="Total Txn")
    })
    
    
    output$tab11<-renderPrint({
      df<-bank_df()
      summary(df$CHARGABLE_AMNT_ACY)
    })
    
    output$plot11.1<-renderPlot({
      df<-bank_df()
      p<-ggplot(df,aes(CHARGABLE_AMNT_ACY))
      p+geom_histogram(bins=input$bins11,fill="darkgray",colour="black")+labs(title="FEE AMOUNT ",x="AMOUNT")
      
    })
    
    output$plot11.2<-renderPlotly({
      df<-bank_df()
      ggplotly(ggplot(df, aes("",CHARGABLE_AMNT_ACY))+ geom_boxplot()+labs(x=input$bank))
    })
    
    
    output$tab12<-renderTable({
      df<-bank_df()
      df<-subset(df,df$COUNTRY==input$count12)
      q1<-aggregate(df$CHARGABLE_AMNT_ACY,by=list(df$STATE), sum)
      q1$State<-q1$Group.1
      q1$TotalFee<-q1$x
      q1$Group.1<-NULL
      q1$x<-NULL
      q1
    })
    
    
    output$plot12<-renderPlot({
      df<-bank_df()
      df<-subset(df,df$COUNTRY==input$count12)
      q1<-aggregate(df$CHARGABLE_AMNT_ACY,by=list(df$STATE), sum)
      q1$State<-q1$Group.1
      q1$TotalFee<-q1$x
      q1$Group.1<-NULL
      q1$x<-NULL
      options(scipen = 999)
      ggplot(q1, aes(State, TotalFee)) + geom_bar(aes(fill=State),stat = "identity");
    })
    
    
    output$plot13<-renderPlotly({
      df<-bank_df()
      df<-subset(df,df$COUNTRY==input$count13)
      if(input$stateinclude13){
        df<-subset(df,df$STATE==input$state13)
      }
      q1<-aggregate(df$CHARGABLE_AMNT_ACY,by=list(DATE=df$CHARGE_DATE), sum)
      q1$TotalFee<-q1$x
      q1$x<-NULL
      q1$DATE<-as.Date(q1$DATE)
      q2<-aggregate(df$AMNT_CHARGED,by=list(DATE=df$CHARGE_DATE), sum)
      q2$TotalFeeCharged<-q2$x
      q2$x<-NULL
      q2$DATE<-as.Date(q2$DATE)
      
      dff <- data.frame(as.Date(q2$DATE) , q2$TotalFeeCharged,as.Date(q1$DATE) , q1$TotalFee);
      dff$as.Date.q1.DATE.<-NULL;
      for (row in 1:nrow(dff)) {
        if(is.na(dff[row, "q2.TotalFeeCharged"])) {
          dff[row, "q2.TotalFeeCharged"]<-0;
        }
      }
      dff$Loss<- dff$q1.TotalFee-dff$q2.TotalFeeCharged
      
      if(input$target=="a"){
        p<-ggplot(dff, aes(dff$as.Date.q2.DATE., y = value,colour=Legend)) + 
          geom_line(aes(y = dff$q1.TotalFee, colour = "FeeTarget"))+
          geom_line(aes(y = dff$q2.TotalFeeCharged, colour = "FeeCharged"))+scale_x_date()+
          labs(title="ACTUAL VS TARGET FEE INCOME",x="TIME PERIOD",y="Fee")
        ggplotly(p)
      } else{
        p1<-ggplot(dff, aes(dff$as.Date.q2.DATE. , dff$Loss))
        p1<-p1 + geom_line(colour="turquoise") + 
          scale_x_date()  + ylab("AMOUNT")+xlab("TIME PERIOD")+labs(title="HOW MUCH SHORT OF TARGETTED FEE INCOME");
        ggplotly(p1)
      }
    })
    
    
    output$tab14<-renderDataTable({
      df<-bank_df()
      df<-subset(df,df$CITY==input$city14)
      dff<-aggregate(df$CHARGABLE_AMNT_ACY,by=list(CUST=df$CUSTOMER_ID), sum);
      dff$TotalFee<-dff$x
      dff$x<-NULL
      dfm<-dff[order(-dff$TotalFee),]
      dfm$CUST<-factor(dfm$CUST, levels = dfm$CUST)
      best_cust<-head(dfm,input$slide14)
      datatable(best_cust ,options = list(paging = FALSE))
    })
    
    
    output$plot14<-renderPlot({
      df<-bank_df()
      df<-subset(df,df$CITY==input$city14)
      dff<-aggregate(df$CHARGABLE_AMNT_ACY,by=list(CUST=df$CUSTOMER_ID), sum);
      dff$TotalFee<-dff$x
      dff$x<-NULL
      dfm<-dff[order(-dff$TotalFee),]
      dfm$CUST<-factor(dfm$CUST, levels = dfm$CUST)
      best_cust<-head(dfm,input$slide14)
      
      ggplot(best_cust, aes(CUST, TotalFee)) +
        geom_linerange(aes(x = CUST, ymin = 0, ymax = TotalFee), color = "lightgray", size = 1.5) + xlab("Customer")+
        geom_point(aes(color = CUST), size = 3)+ggpubr::color_palette("ucscgb")+coord_flip()
      #gove slant x axis labesl,incase too many  long labels: +theme(axis.text.x = element_text(angle=65, vjust=0.6))
    })
    
    age_group15_df<-reactive({
      df<-bank_df()
      df<-subset(df,df$COUNTRY==input$count15)
      if(input$stateinclude15){
        df<-subset(df,df$STATE==input$state15)
      }
      test1.df <- data.frame(df$FEE_ID,df$CUST_AGE,df$CHARGABLE_AMNT_ACY);
      test1.df$AGE_GROUP<-0;
      colnames(test1.df) <- c("FEE_ID", "CUST_AGE","CHARGABLE_AMNT_ACY","AGE_GROUP");
      test1.df<-na.omit(test1.df);
      test.df <- data.frame(test1.df$FEE_ID,test1.df$CUST_AGE,test1.df$CHARGABLE_AMNT_ACY,test1.df$AGE_GROUP);
      test1.df<-test.df
      colnames(test1.df) <- c("FEE_ID", "CUST_AGE","CHARGABLE_AMNT_ACY","AGE_GROUP");
      test1.df$AGE_GROUP<-0;
      
      age<-0;
      for (row in 1:nrow(test1.df)) {
        age <- test1.df[row, "CUST_AGE"];
        # print(age);
        if(age <= 20) {
          test1.df[row,"AGE_GROUP"]<-"<20";
        } else if(age>20 & age<=25) {
          test1.df[row,"AGE_GROUP"]<-"20-25";
        } else if(age>25 & age<=30) {
          test1.df[row,"AGE_GROUP"]<-"25-30";
        } else if(age>30 & age<=35) {
          test1.df[row,"AGE_GROUP"]<-"30-35";
        } else if(age>35 & age<=40) {
          test1.df[row,"AGE_GROUP"]<-"35-40";
        } else if(age>40 & age<=45) {
          test1.df[row,"AGE_GROUP"]<-"40-45";
        } else if(age>45 & age<=50) {
          test1.df[row,"AGE_GROUP"]<-"45-50";
        } else {
          test1.df[row,"AGE_GROUP"]<-"50 above";
        }
      }
      test1.df$AGE_GROUP<-factor(test1.df$AGE_GROUP);
      if(input$group15=="a"){
        test1<-aggregate(test1.df$CHARGABLE_AMNT_ACY,by=list(AgeGroup=test1.df$AGE_GROUP),sum);
        test1$TotalFee<-test1$x;
      } else{
        test1<-aggregate(test1.df$CHARGABLE_AMNT_ACY,by=list(AgeGroup=test1.df$AGE_GROUP),mean);
        test1$MeanFee<-test1$x;
      }
      test1$x<-NULL
      test1
    })
    
    
    output$tab15<-renderTable({
      age_group15_df()
    })
    
    
    output$plot15<-renderPlot({
      test1<-age_group15_df()
      if(input$group15=="a"){
        ggplot(test1, aes(AgeGroup, TotalFee)) + geom_bar(aes(fill=AgeGroup),stat = "identity");
      } else{
        ggplot(test1, aes(AgeGroup, MeanFee)) + geom_bar(aes(fill=AgeGroup),stat = "identity");
      } 
    })
    
    
    output$plot16<-renderPlot({
      df<-bank_df()
      df<-subset(df,df$COUNTRY==input$count16)
      if(input$stateinclude16){
        df<-subset(df,df$STATE==input$state16)
      }
      p<-ggplot(subset(df,df$CHARGABLE_AMNT_ACY>0),aes(CUST_AGE))
      p+geom_histogram(binwidth = 5,fill="turquoise",colour="black",alpha=0.6)+
        stat_bin(binwidth= 5, geom="text",aes(label=..count..),vjust = -0.5)
      
    })
    
    best_prod17_df<-reactive({
      df<-bank_df()
      df<-subset(df,df$COUNTRY==input$count17)
      if(input$stateinclude17){
        df<-subset(df,df$STATE==input$state17)
      }
      dff<-aggregate(df$CHARGABLE_AMNT_ACY,by=list(Prod=df$PRODUCT_ID), sum);
      dff$TotalFee<-dff$x
      dff$x<-NULL
      dfm<-dff[order(-dff$TotalFee),]
      dfm$Prod<-factor(dfm$Prod, levels = dfm$Prod)
      best_prod<-head(dfm,input$slide17)
      best_prod
    })
    
    output$tab17<-renderDataTable({
      best_prod<-best_prod17_df()
      datatable(best_prod ,options = list(paging = FALSE))
    })
    
    
    output$plot17<-renderPlot({
      best_prod<-best_prod17_df()
      ggplot(best_prod, aes(Prod, TotalFee)) +
        geom_linerange(aes(x = Prod, ymin = 0, ymax = TotalFee), color = "lightgray", size = 1.5) + xlab("Products")+
        geom_point(aes(color = Prod), size = 3)+ggpubr::color_palette("ucscgb")+coord_flip()
      #give slant x axis labesl,incase too many  long labels: +theme(axis.text.x = element_text(angle=65, vjust=0.6))
    })
    
    
    best_service18_df<-reactive({
      df<-bank_df()
      df<-subset(df,df$COUNTRY==input$count18)
      if(input$stateinclude18){
        df<-subset(df,df$STATE==input$state18)
      }
      dff<-aggregate(df$CHARGABLE_AMNT_ACY,by=list(Service=df$SERVICE_ID), sum);
      dff$TotalFee<-dff$x
      dff$x<-NULL
      dfm<-dff[order(-dff$TotalFee),]
      dfm$Service<-factor(dfm$Service, levels = dfm$Service)
      best_service<-head(dfm,input$slide18)
      best_service
    })
    
    output$tab18<-renderDataTable({
      best_service<-best_service18_df()
      datatable(best_service ,options = list(paging = FALSE))
    })
    
    
    output$plot18<-renderPlot({
      best_Service<-best_service18_df()
      ggplot(best_Service, aes(Service, TotalFee)) +
        geom_linerange(aes(x = Service, ymin = 0, ymax = TotalFee), color = "lightgray", size = 1.5) + xlab("Services")+
        geom_point(aes(color = Service), size = 3)+ggpubr::color_palette("ucscgb")+
        theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
    })
    
    
    }
    )