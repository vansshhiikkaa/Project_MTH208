library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
library(reshape)

load("NewZealand_match.Rdata")
load("India_Matches.Rdata")
load("England_Matches.Rdata")
load("WestIndies_Match.Rdata")
load("T20_WorldCup_2016_1.Rdata")


ui <- navbarPage(theme = shinytheme("united"),
  
  titlePanel(h1("T20 World Cup, 2016")),
  
  tabPanel(h3("Stadiums"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("field", "Choose which stadium",
                     choices = unique(T20_WorldCup_2016$stadium))
      ),
      
      mainPanel(
        textOutput("text4"),
        tags$head(tags$style("#text4{color: blue;
                                 font-size: 30px;
                                 }")
        ),
        tabsetPanel(type = "tab",
                    tabPanel("Data", tableOutput("data")),
                    tabPanel("Plot", plotOutput("barPlot1"), plotOutput("barPlot2")))
        
        
      )
    )
  ),
  
  tabPanel(h3("Semi-Finalist Teams"),
    sidebarLayout(
      sidebarPanel(
        selectInput("country", "Choose which semifinalists team", 
                    choices = c("India", "West Indies", "New Zealand", "England")),
        selectInput("match", "Choose which match", choices = "", selected = ""),
      ),
      
      mainPanel(
        textOutput("text1"),
        tags$head(tags$style("#text1{color: blue;
                                 font-size: 30px;
                                 }")
        ),
        tabsetPanel(type = "tab",
                    tabPanel("Data", 
                             fluidRow(
                               column(width = 3, textOutput("text50"), tableOutput("data2")),
                               column(width = 5, textOutput("text51"), tableOutput("data4"))
                              ),
                             fluidRow(
                               column(width = 3, textOutput("text52"), tableOutput("data3")),
                               column(width = 5, textOutput("text53"), tableOutput("data5"))
                             )),
                    
                    
                    tabPanel("Plot", plotOutput("piechart"), plotOutput("barchart3"), plotOutput("barchart4"),
                             textOutput("text10"),
                             tags$head(tags$style("#text10{color: purple;
                                 font-size: 20px;
                                 }")
                             ),
                             textOutput("text2"),
                             tags$head(tags$style("#text2{color: purple;
                                 font-size: 20px;
                                 }")
                             ),
                             textOutput("text3"),
                             tags$head(tags$style("#text3{color: purple;
                                 font-size: 20px;
                                 }")
                             ))),
        
      )
    )
  ),
  
  tabPanel(h3("Toss Factor"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("n_matches", "No. of matches", 1, 33, 1)
      ),
      
      mainPanel(
        plotOutput("lineDiag")
      )
    )
    
  ),
  
  tabPanel(h3("Consistency of the Bowlers"),
               plotOutput("multiLine")
  )
      
 
)


server <- function(input, output, session) {
    
    dat <- reactive({
      filter(T20_WorldCup_2016, stadium == input$field)
    })
    
    
    output$text4 <- renderText({
      input$field
    })
    
    output$data <- renderTable({
      dat() %>% select(-stadium)
    })

    output$barPlot1 <- renderPlot({
      
      
      n <- nrow(dat())
      Matches <- 1:n
      Teams <- c(rep("Team 1", n), rep("Team 2", n))
      Team_Runs <- c(dat()$Team1_Runs, dat()$Team2_Runs)
      score.bar <- data.frame(Matches, Teams, Team_Runs)
      
      df_teams <- data.frame(dat()$Team1, dat()$Team2)
      new_names <- c("ZIM", "AFG", "BAN", "IRE", "HKG", "NED", "NZ", "PAK", "ENG", "SA", "SL", "INDIA", "AUS", "SCOT", "OMA", "WI")
      for(i in 1:16)
      {
        df_teams <- replace(df_teams, df_teams == unique(c(T20_WorldCup_2016$Team1, T20_WorldCup_2016$Team2))[i], new_names[i])
      }
      
      ggplot(data = score.bar, aes(x = Matches, y = Team_Runs, fill = Teams)) +
        geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
        geom_text(aes(label = Team_Runs), fontface = "bold", vjust = 1.5, position = position_dodge(.9), size = 4) +
        labs(x = "Matches", y = "Scores", title = "Runs scored in 1st innings & 2nd innings") +
        geom_hline(yintercept = mean(dat()$Team1_Runs), color = "green", lwd = 1) +
        scale_x_discrete(limits = paste(df_teams[, 1], "VS", df_teams[, 2])) +
        theme(plot.title = element_text(hjust = 0.5, size = 20), 
              axis.title.x = element_text(face = "bold", colour = "blue", size = 14),
              axis.title.y = element_text(face = "bold", colour = "blue", size = 14),
              legend.title = element_text(face = "bold", size = 10))
      
              # theme_fivethirtyeight() + scale_fill_fivethirtyeight() +
      
      
    })
    
    output$barPlot2 <- renderPlot({
      
      
      bat.count = 0
      field.count = 0
      for(i in 1:nrow(dat()))
      {
        if(dat()$Toss_Winner[i] == dat()$Match_Winner[i])
        {
          if(dat()$Toss_decision[i] == "Bat first")
          {
            bat.count = bat.count + 1
          }
          else
          {
            field.count = field.count + 1
          }
        }
        else
        {
          if(dat()$Toss_decision[i] == "Bat first")
          {
            field.count = field.count + 1
          }
          else
          {
            bat.count = bat.count + 1
          }
        }
      }
      
      Decision <- c("Bat first", "Field first")
      Freq <- c(bat.count, field.count)
      
      df <- data.frame(Decision, Freq)
      
      ggplot(df, aes(Decision, Freq, fill = Decision)) + geom_bar(stat = "identity", position = 'dodge', alpha = 0.75, width = 0.5) +
        geom_text(aes(label = Freq), fontface = "bold", vjust = 2, size = 8) +
        labs(x = "Decision", y = "Frequency", title = "No. of matches won by batting first or fielding first") +
        theme(plot.title = element_text(hjust = 0.5, size = 20), 
              axis.title.x = element_text(face="bold", colour="red", size = 15),
              axis.title.y = element_text(face="bold", colour="red", size = 15),
              legend.title = element_text(face="bold", size = 10))
      
      
    })
    
    
    
    
    dat2 <-  reactive({
      T20_WorldCup_2016[T20_WorldCup_2016$Team1 == input$country | T20_WorldCup_2016$Team2 == input$country, ]
    })
    
    observeEvent(
      input$country,
      updateSelectInput(session, "match", "Choose which match",
                        choices = paste(dat2()$Team1, "VS", dat2()$Team2, ",", dat2()$Date))
    )
    
    k = reactive({which(paste(dat2()$Team1, "VS", dat2()$Team2, ",", dat2()$Date) == input$match)})
    m <- reactive({nrow(dat2())})
    
    dat3 <- reactive({
      if(input$country == "India")
      {
        Ind_Mat
      }
      else if(input$country == "West Indies")
      {
        Wi_Mat
      }
      else if(input$country == "New Zealand")
      {
        nz_Mat
      }
      else
      {
        eng_Mat
      }
    })
    
    opp_team <- reactive({
      if(dat2()$Team1[[k()]] == input$country)
      {
        dat2()$Team2[[k()]]
      }
      else
      {
        dat2()$Team1[[k()]]
      }
    })
    
    output$text1 <- renderText(input$match)
    
    output$data2 <- renderTable({
      output$text50 <- renderText(paste(input$country, "Batting"))
      dat3()[[k()]]
      })
    
    output$data3 <- renderTable({
      output$text51 <- renderText(paste(input$country, "Bowling"))
      dat3()[[k()+m()]]
    })
    
    output$data4 <- renderTable({
      output$text52 <- renderText(paste(opp_team(), "Batting"))
      dat3()[[k()+(2*m())]]
    })
    
    output$data5 <- renderTable({
      output$text53 <- renderText(paste(opp_team(), "Bowling"))
      dat3()[[k()+(3*m())]]
    })
    
    
    
    output$piechart <- renderPlot({
      
      par(mfrow=c(1,2))
      
      plot1 <- ggplot(dat3()[[k()]], aes(x = "", y = Runs, fill = Players)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) + theme_void() +
        geom_text(aes(label = Runs), position = position_stack(vjust = 0.5)) +
        ggtitle(paste(input$country, "Batting")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))
      
      plot2 <- ggplot(dat3()[[k()+m()]], aes(x = "", y = Runs, fill = Players)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) + theme_void() +
        geom_text(aes(label = Runs), position = position_stack(vjust = 0.5)) +
        ggtitle(paste(opp_team(), "Batting")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))
      
      grid.arrange(plot1, plot2, ncol=2)
      
    })
    
    
    output$barchart3 <- renderPlot({
      
      p <- nrow(dat3()[[k()+(2*m())]])
      
      Wickets <- subset(dat3()[[k()+(2*m())]], select = c(1,2))
      p1 <- ggplot(data = Wickets, aes(x = players, y = wickets)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.5) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      Economy <- subset(dat3()[[k()+(2*m())]], select = c(1,3))
      p2 <- ggplot(data = Economy, aes(x = players, y = ECON)) +
        geom_bar(stat = "identity", fill = "yellow", color = "black", width = 0.5) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      n_0 <- subset(dat3()[[k()+(2*m())]], select = c(1,4))
      p3 <- ggplot(data = n_0, aes(x = players, y = `0's`)) +
        geom_bar(stat = "identity", fill = "green", color = "black", width = 0.5) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      boundaries <- dat3()[[k()+(2*m())]]$`4's` + dat3()[[k()+(2*m())]]$`6's`
      players <- dat3()[[k()+(2*m())]]$players
      n_bound <- data.frame(players, boundaries)
      p4 <- ggplot(data = n_bound, aes(x = players, y = boundaries)) +
        geom_bar(stat = "identity", fill = "red", color = "black", width = 0.5) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      (p1 + p2) / (p3 + p4) +
        plot_annotation(paste(input$country, "Bowling")) &
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))
      
      
    })
    
    output$barchart4 <- renderPlot({
      
      p <- nrow(dat3()[[k()+(3*m())]])
      
      Wickets <- subset(dat3()[[k()+(3*m())]], select = c(1,2))
      p1 <- ggplot(data = Wickets, aes(x = players, y = wickets)) +
        geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.5) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      Economy <- subset(dat3()[[k()+(3*m())]], select = c(1,3))
      p2 <- ggplot(data = Economy, aes(x = players, y = ECON)) +
        geom_bar(stat = "identity", fill = "yellow", color = "black", width = 0.5) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      n_0 <- subset(dat3()[[k()+(3*m())]], select = c(1,4))
      p3 <- ggplot(data = n_0, aes(x = players, y = `0's`)) +
        geom_bar(stat = "identity", fill = "green", color = "black", width = 0.5) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      boundaries <- dat3()[[k()+(3*m())]]$`4's` + dat3()[[k()+(3*m())]]$`6's`
      players <- dat3()[[k()+(3*m())]]$players
      n_bound <- data.frame(players, boundaries)
      p4 <- ggplot(data = n_bound, aes(x = players, y = boundaries)) +
        geom_bar(stat = "identity", fill = "red", color = "black", width = 0.5) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      (p1 + p2) / (p3 + p4) +
        plot_annotation(paste(opp_team(), "Bowling")) &
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20))
      
    })
    
    output$text10 <- renderText({
      paste("Winner Of the Toss - ", dat2()$Toss_Winner[[k()]])
    })
    
    output$text2 <- renderText({
      paste("Winner Of the Match - ", dat2()$Match_Winner[[k()]])
    })
    
    output$text3 <- renderText({
      paste("Player Of the Match - ", dat2()$POM[[k()]])
    })
    
    
    output$lineDiag <- renderPlot({
      
      Toss.Match <- as.numeric(length(33))
      for(i in 1:input$n_matches)
      {
        if(T20_WorldCup_2016$Toss_Winner[i] == T20_WorldCup_2016$Match_Winner[i])
        {
          Toss.Match[i] <- 1
        }
        else
        {
          Toss.Match[i] <- 0
        }
      }
      
      win.prob <- as.numeric(length(33))
      count = 0
      sum.1 = 0
      for(i in 1:input$n_matches)
      {
        count = count + 1
        if(Toss.Match[i] == 1)
        {
          sum.1 <- sum.1 + 1
        }
        win.prob[i] <- sum.1/count
      }
      
      Matches <- 1:input$n_matches
      winning <- data.frame(Matches, Toss.Match, win.prob)
      ggplot(data = winning, aes(Matches, win.prob, group = 1)) + geom_line(color = "blue") + 
        geom_point() + xlim(1, 33) + geom_hline(yintercept = c(0.5, 1), color = "red") +
        labs(x = "Number of Matches", y = "Relative Frequency")
      
    })
    
    output$multiLine <- renderPlot({
      Ashwin <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(Ind_Mat[[11+i]]$players == "Ravichandran Ashwin")
        Ashwin[i+1] = Ind_Mat[[11+i]]$ECON[a]
      }
      
      Nehra <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(Ind_Mat[[11+i]]$players == "Ashish Nehra")
        Nehra[i+1] = Ind_Mat[[11+i]]$ECON[a]
      }
      
      Bumrah <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(Ind_Mat[[11+i]]$players == "Jasprit Bumrah")
        Bumrah[i+1] = Ind_Mat[[11+i]]$ECON[a]
      }
      
      Jadeja <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(Ind_Mat[[11+i]]$players == "Ravindra Jadeja")
        Jadeja[i+1] = Ind_Mat[[11+i]]$ECON[a]
      }
      
      Pandya <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(Ind_Mat[[11+i]]$players == "Hardik Pandya")
        Pandya[i+1] = Ind_Mat[[11+i]]$ECON[a]
      }
      
      India_Bowling <- data.frame(Ashwin, Nehra, Bumrah, Jadeja, Pandya)
      
      India_Bowling_2 <- data.frame(x = seq_along(India_Bowling[, 1]), India_Bowling)
      India_Bowling_2 <- melt(India_Bowling_2, id.vars = "x")
      
      g1 <- ggplot(India_Bowling_2, aes(x = x, y = value, color = variable)) +
        geom_line(lwd = 1.1) +
        guides(color = guide_legend(title = "Bowlers")) +
        scale_color_discrete(labels = c("Ravichandran Ashwin", "Ashish Nehra", "Jasprit Bumrah", "Ravindra Jadeja", "Hardik Pandya")) +
        labs(x = "Matches", y = "Economy", title = "Consistency of India Bowlers")
      
      
      
      Corey <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(nz_Mat[[11+i]]$players == "Corey Anderson")
        Corey[i+1] = nz_Mat[[11+i]]$ECON[a]
      }
      
      Mitchell <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(nz_Mat[[11+i]]$players == "Mitchell Santner")
        Mitchell[i+1] = nz_Mat[[11+i]]$ECON[a]
      }
      
      Grant <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(nz_Mat[[11+i]]$players == "Grant Elliott")
        Grant[i+1] = nz_Mat[[11+i]]$ECON[a]
      }
      
      Ish <- as.numeric(length(5))
      for(i in 0:4)
      {
        a <- which(nz_Mat[[11+i]]$players == "Ish Sodhi")
        Ish[i+1] = nz_Mat[[11+i]]$ECON[a]
      }
      
      NZ_Bowling <- data.frame(Corey, Mitchell, Grant, Ish)
      
      NZ_Bowling_2 <- data.frame(x = seq_along(NZ_Bowling[, 1]), NZ_Bowling)
      NZ_Bowling_2 <- melt(NZ_Bowling_2, id.vars = "x")
      
      g2 <- ggplot(NZ_Bowling_2, aes(x = x, y = value, color = variable)) +
        geom_line(lwd = 1.1) +
        guides(color = guide_legend(title = "Bowlers")) +
        scale_color_discrete(labels = c("Corey Anderson", "Mitchell Santner", "Grant Elliott", "Ish Sodhi")) +
        labs(x = "Matches", y = "Economy", title = "Consistency of New Zealand Bowlers")
      
      
      David <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(eng_Mat[[13+i]]$players == "David Willey")
        David[i+1] = eng_Mat[[13+i]]$ECON[a]
      }
      
      Chris <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(eng_Mat[[13+i]]$players == "Chris Jordan")
        Chris[i+1] = eng_Mat[[13+i]]$ECON[a]
      }
      
      Ben <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(eng_Mat[[13+i]]$players == "Ben Stokes")
        Ben[i+1] = eng_Mat[[13+i]]$ECON[a]
      }
      
      Adil <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(eng_Mat[[13+i]]$players == "Adil Rashid")
        Adil[i+1] = eng_Mat[[13+i]]$ECON[a]
      }
      
      Eng_Bowling <- data.frame(David, Chris, Ben, Adil)
      
      Eng_Bowling_2 <- data.frame(x = seq_along(Eng_Bowling[, 1]), Eng_Bowling)
      Eng_Bowling_2 <- melt(Eng_Bowling_2, id.vars = "x")
      
      g3 <- ggplot(Eng_Bowling_2, aes(x = x, y = value, color = variable)) +
        geom_line(lwd = 1.1) +
        guides(color = guide_legend(title = "Bowlers")) +
        scale_color_discrete(labels = c("David Willey", "Chris Jordan", "Ben Stokes", "Adil Rashid")) +
        labs(x = "Matches", y = "Economy", title = "Consistency of England Bowlers")
      
      
      Samuel <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(Wi_Mat[[13+i]]$players == "Samuel Badree")
        Samuel[i+1] = Wi_Mat[[13+i]]$ECON[a]
      }
      
      Andre <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(Wi_Mat[[13+i]]$players == "Andre Russell")
        Andre[i+1] = Wi_Mat[[13+i]]$ECON[a]
      }
      
      Dwayne <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(Wi_Mat[[13+i]]$players == "Dwayne Bravo")
        Dwayne[i+1] = Wi_Mat[[13+i]]$ECON[a]
      }
      
      Sulieman <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(Wi_Mat[[13+i]]$players == "Sulieman Benn")
        Sulieman[i+1] = Wi_Mat[[13+i]]$ECON[a]
      }
      
      Carlos <- as.numeric(length(6))
      for(i in 0:5)
      {
        a <- which(Wi_Mat[[13+i]]$players == "Carlos Brathwaite")
        Carlos[i+1] = Wi_Mat[[13+i]]$ECON[a]
      }
      
      WI_Bowling <- data.frame(Samuel, Andre, Dwayne, Sulieman)
      
      WI_Bowling_2 <- data.frame(x = seq_along(WI_Bowling[, 1]), WI_Bowling)
      WI_Bowling_2 <- melt(WI_Bowling_2, id.vars = "x")
      
      g4 <- ggplot(WI_Bowling_2, aes(x = x, y = value, color = variable)) +
        geom_line(lwd = 1.1) +
        guides(color = guide_legend(title = "Bowlers")) +
        scale_color_discrete(labels = c("Samuel Badree", "Andre Russell", "Dwayne Bravo", "Sulieman Benn", "Carlos Brathwaite")) +
        labs(x = "Matches", y = "Economy", title = "Consistency of West Indies Bowlers")
      
      (g1 + g2) / (g3 + g4)
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
