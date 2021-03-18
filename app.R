library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(shiny)
library(formattable)
library(shinybusy)
area <- "com"
app_id <- "920283951177e08eabca9aa7acf0adb6"
getId <- function(roughName) {
  r <- GET(
    paste0("https://api.wotblitz.", area, "/wotb/account/list/"),
    #"https://api.wotblitz.com/wotb/account/list/",
    query = list(
      application_id = app_id,
      search = roughName,
      fields = "account_id, nickname",
      type = "startswith",
      limit = "10"
    )
  )
  stop_for_status(r)
  ret <- fromJSON(content(r, as = "text"))
  return(ret$data$account_id)
}

nameArr <- c()
idArr <- c()
ret_df <- data.frame()
vehicle_df <- data.frame()
getTankIdLst <- function(lower, upper, lst) {
  ret = ""
  for (i in (lower + 1):upper) {
    ret <- paste0(ret, ",", lst[i])
  }
  return(paste0(lst[lower], ret))
}

nameInit = ""
areaInit = "com"
getLocalId <- function(name, area) {
  if (name == nameInit && area == areaInit) {
    return(idArr[which(nameArr == name)])
  }
  nameInit <<- name
  areaInit <<- area
  cur_id <- idArr[which(nameArr == name)]
  #cur_id <- 2009800556
  r <- GET(
    paste0("https://api.wotblitz.", area, "/wotb/account/info/"),
    #"https://api.wotblitz.com/wotb/account/info/",
    query = list(
      application_id = app_id,
      account_id = cur_id
    )
  )
  stop_for_status(r)
  ret <- fromJSON(content(r, as = "text"))
  ret_df <<- ret$data[[1]]
  
  
  r <- GET(
    paste0("https://api.wotblitz.", area, "/wotb/tanks/stats/"),
    #"https://api.wotblitz.com/wotb/tanks/stats/",
    query = list(
      application_id = app_id,
      account_id = cur_id
    )
  )
  stop_for_status(r)
  ret <- fromJSON(content(r, as = "text"))
  temp <- ret$data[[1]]
  temp <- temp %>% 
    arrange(tank_id)
  
  tier <- c()
  type <- c()
  tankId <- c()
  for (i in 1:ceiling(length(temp$tank_id) / 100)) {
    n = length(temp$tank_id)
    #i <- 1
    lower = (i - 1) * 100 + 1
    upper = lower + 99
    if (upper > n) {
      upper = n
    }
    r <- GET(
      paste0("https://api.wotblitz.", area, "/wotb/encyclopedia/vehicles/"),
      #"https://api.wotblitz.com/wotb/encyclopedia/vehicles/",
      query = list(
        application_id = app_id,
        tank_id = getTankIdLst(lower, upper, temp$tank_id)#paste0(a$tank_id[1], ",",a$tank_id[2])
      )
    )
    stop_for_status(r)
    ret <- fromJSON(content(r, as = "text"))
    for (j in 1:(upper - lower + 1)) {
      if (length(ret$data[[j]]) != 0) {
        rawData <- ret$data[[j]]
        tier <- c(tier, rawData$tier)
        type <- c(type, rawData$type)
        tankId <- c(tankId, rawData$tank_id)
      } else {
        tier <- c(tier, NA)
        type <- c(type, NA)
        tankId <- c(tankId, NA)
      }
      
    }
  }
  typeof(temp)
  temp <- as.tibble(temp)
  additional <- tibble(tier, type, tankId)
  vehicle_df <<- temp %>% 
    left_join(additional, by = c("tank_id" = "tankId")) %>% 
    filter(!is.na(type))

  return(cur_id)
}



getOverviewTable <- function() {
  info_all <- ret_df$statistics$all
  total_battle <- info_all$battles
  table <- rbind(
    cbind("Player", ret_df$nickname),
    cbind("Total Battles", total_battle), 
    cbind("Winrate", as.character(percent(info_all$wins / total_battle))),
    cbind("Average Damage", round(info_all$damage_dealt / total_battle, digits = 2)),
    cbind("Damage Ratio", round(info_all$damage_dealt / info_all$damage_received, digits = 2)),
    cbind("Kills/deaths", round(info_all$frags / (total_battle - info_all$survived_battles), digits = 2)),
    cbind("Spots", round(info_all$spotted / total_battle, digits = 2)),
    cbind("Survival Rate", as.character(percent(info_all$survived_battles / total_battle, digits = 2)))
    )
  
  colnames(table) <- c("Catrgories", "Data")
  return(table)
}
getWinrate <- function() {
  print("Winrate:", ret_df$statistics$all$wins)
  return(paste("Winrate:", ret_df$statistics$all$wins))
}

getPlotByTier <- function(typeChoice, categoryChoice) {
  if (typeChoice == "all") {
    local_df_tier <- vehicle_df
    title_type <- "all types of tanks"
  } else if (typeChoice == "Light Tank") {
    local_df_tier <- vehicle_df %>% 
      filter(type == "lightTank")
    title_type <- "all light tanks"
  } else if (typeChoice == "Medium Tank") {
    local_df_tier <- vehicle_df %>% 
      filter(type == "mediumTank")
    title_type <- "all medium tanks"
  } else if (typeChoice == "Heavy Tank") {
    local_df_tier <- vehicle_df %>% 
      filter(type == "heavyTank")
    title_type <- "all heavy tanks"
  } else {
    local_df_tier <- vehicle_df %>% 
      filter(type == "AT-SPG")
    title_type <- "all tank destroyers"
  }
  
  title_by_tier <- paste0("Histogram of ", nameInit, "'s ", tolower(categoryChoice), " statistic of ", title_type, " by tiers")
  if (categoryChoice == "Total Battles") {
    ggplot(data = local_df_tier %>% 
             group_by(tier) %>% 
             summarise(`Total Battles` = sum(all$battles)), 
           aes(x = factor(tier, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), y = `Total Battles`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Tier") +
      ggtitle(title_by_tier)
  } else if (categoryChoice == "Winrate") {
    ggplot(data = local_df_tier %>% 
             group_by(tier) %>% 
             summarise(totalWin = sum(all$wins), totalGame = sum(all$battles)) %>% 
             mutate(winrate = round(totalWin / totalGame, digits = 2)), 
           aes(x = factor(tier, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), y = winrate)) +
      geom_bar(stat = "identity") +
      xlab("Tank Tier") +
      ggtitle(title_by_tier)
  } else if (categoryChoice == "Average Damage") {
    ggplot(data = local_df_tier %>% 
             group_by(tier) %>% 
             summarise(totalDamage = sum(all$damage_dealt), totalGame = sum(all$battles)) %>% 
             mutate(`Average Damage` = round(totalDamage / totalGame, digits = 2)), 
           aes(x = factor(tier, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), y = `Average Damage`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Tier") +
      ggtitle(title_by_tier)
  } else if (categoryChoice == "Damage Ratio") {
    ggplot(data = local_df_tier %>% 
             group_by(tier) %>% 
             summarise(totalDamageDealt = sum(all$damage_dealt), totalDamageReceived = sum(all$damage_received)) %>% 
             mutate(`Damage Ratio` = round(totalDamageDealt / totalDamageReceived, digits = 2)), 
           aes(x = factor(tier, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), y = `Damage Ratio`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Tier") +
      ggtitle(title_by_tier)
  } else if (categoryChoice == "Kills/deaths") {
    ggplot(data = local_df_tier %>% 
             mutate(death = all$battles - all$survived_battles) %>% 
             group_by(tier) %>% 
             summarise(totalKill = sum(all$frags), totalDeath = sum(death)) %>% 
             mutate(`Kills/deaths` = round(totalKill / totalDeath, digits = 2)), 
           aes(x = factor(tier, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), y = `Kills/deaths`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Tier") +
      ggtitle(title_by_tier)
  } else if (categoryChoice == "Spots") {
    ggplot(data = local_df_tier %>% 
             group_by(tier) %>% 
             summarise(spot = sum(all$spotted), totalGame = sum(all$battles)) %>% 
             mutate(`Spots` = round(spot / totalGame, digits = 2)), 
           aes(x = factor(tier, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), y = Spots)) +
      geom_bar(stat = "identity") +
      xlab("Tank Tier") +
      ggtitle(title_by_tier)
  } else { # Survival Rate
    ggplot(data = local_df_tier %>% 
             group_by(tier) %>% 
             summarise(survive = sum(all$survived_battles), totalGame = sum(all$battles)) %>% 
             mutate(`Survival Rate` = round(survive / totalGame, digits = 2)), 
           aes(x = factor(tier, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), y = `Survival Rate`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Tier") +
      ggtitle(title_by_tier)
  }
}

# "Total Battles", "Winrate", "Average Damage", "Damage Ratio", "Kills/deaths", "Spots", "Survival Rate"
getPlotByType <- function(tierChoice, categoryChoice) {
  if (tierChoice == "all") {
    local_df <- vehicle_df
    title_tier <- "all tiers"
  } else {
    local_df <- vehicle_df %>% 
      filter(tier == tierChoice)
    title_tier <- paste0("tier ", tierChoice)
  }
  
  title_by_type <- paste0("Histogram of ", nameInit, "'s ", tolower(categoryChoice), " statistic of ", title_tier, " by types")
  
  if (categoryChoice == "Total Battles") {
    ggplot(data = local_df %>% 
             group_by(type) %>% 
             summarise(`Total Battles` = sum(all$battles)) %>% 
             arrange(factor(type, levels = c("lightTank", "mediumTank", "heavyTank", "AT-SPG"))), 
           aes(x = factor(type, levels = c("lightTank", "mediumTank", "heavyTank", "AT-SPG")), y = `Total Battles`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Type") +
      ggtitle(title_by_type)
  } else if (categoryChoice == "Winrate") {
    ggplot(data = local_df %>% 
             group_by(type) %>% 
             summarise(totalWin = sum(all$wins), totalGame = sum(all$battles)) %>% 
             mutate(winrate = round(totalWin / totalGame, digits = 2)), 
           aes(x = factor(type, levels = c("lightTank", "mediumTank", "heavyTank", "AT-SPG")), y = winrate)) +
      geom_bar(stat = "identity") +
      xlab("Tank Type") +
      ggtitle(title_by_type)
  } else if (categoryChoice == "Average Damage") {
    ggplot(data = local_df %>% 
             group_by(type) %>% 
             summarise(totalDamage = sum(all$damage_dealt), totalGame = sum(all$battles)) %>% 
             mutate(`Average Damage` = round(totalDamage / totalGame, digits = 2)), 
           aes(x = factor(type, levels = c("lightTank", "mediumTank", "heavyTank", "AT-SPG")), y = `Average Damage`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Type") +
      ggtitle(title_by_type)
  } else if (categoryChoice == "Damage Ratio") {
    ggplot(data = local_df %>% 
             group_by(type) %>% 
             summarise(totalDamageDealt = sum(all$damage_dealt), totalDamageReceived = sum(all$damage_received)) %>% 
             mutate(`Damage Ratio` = round(totalDamageDealt / totalDamageReceived, digits = 2)), 
           aes(x = factor(type, levels = c("lightTank", "mediumTank", "heavyTank", "AT-SPG")), y = `Damage Ratio`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Type") +
      ggtitle(title_by_type)
  } else if (categoryChoice == "Kills/deaths") {
    ggplot(data = local_df %>% 
             mutate(death = all$battles - all$survived_battles) %>% 
             group_by(type) %>% 
             summarise(totalKill = sum(all$frags), totalDeath = sum(death)) %>% 
             mutate(`Kills/deaths` = round(totalKill / totalDeath, digits = 2)), 
           aes(x = factor(type, levels = c("lightTank", "mediumTank", "heavyTank", "AT-SPG")), y = `Kills/deaths`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Type") +
      ggtitle(title_by_type)
  } else if (categoryChoice == "Spots") {
    ggplot(data = local_df %>% 
             group_by(type) %>% 
             summarise(spot = sum(all$spotted), totalGame = sum(all$battles)) %>% 
             mutate(`Spots` = round(spot / totalGame, digits = 2)), 
           aes(x = factor(type, levels = c("lightTank", "mediumTank", "heavyTank", "AT-SPG")), y = Spots)) +
      geom_bar(stat = "identity") +
      xlab("Tank Type") +
      ggtitle(title_by_type)
  } else { # Survival Rate
    ggplot(data = local_df %>% 
             group_by(type) %>% 
             summarise(survive = sum(all$survived_battles), totalGame = sum(all$battles)) %>% 
             mutate(`Survival Rate` = round(survive / totalGame, digits = 2)), 
           aes(x = factor(type, levels = c("lightTank", "mediumTank", "heavyTank", "AT-SPG")), y = `Survival Rate`)) +
      geom_bar(stat = "identity") +
      xlab("Tank Type") +
      ggtitle(title_by_type)
  }
  
}

getNames <- function(roughName) {
  r <- GET(
    paste0("https://api.wotblitz.", area, "/wotb/account/list/"),
    #"https://api.wotblitz.com/wotb/account/list/",
    query = list(
      application_id = app_id,
      search = roughName,
      fields = "account_id, nickname",
      type = "startswith",
      limit = "10"
    )
  )
  stop_for_status(r)
  ret <- fromJSON(content(r, as = "text"))
  retData <- ret$data
  ret$data$nickname
  nameArr <<- retData$nickname
  idArr <<- retData$account_id
  
  return(retData$nickname)
}
categoryArr <- c("Total Battles", 
                 "Winrate", 
                 "Average Damage", 
                 "Damage Ratio",
                 "Kills/deaths",
                 "Spots",
                 "Survival Rate")
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "roughName",
                label = "Player Search (at least three letters)"),
      selectInput(inputId = "nameChoice", label = "Choose a Player", c("-")),
      selectInput(inputId = "serverChoice", label = "Choose a Server", c("North America", "Asia", "Russia", "Europe")),
      actionButton("getData", label = "Search")
    ),
    
    mainPanel(
      tabsetPanel(     
        tabPanel(title = "Description",
                 textOutput("description1"),
                 textOutput("description2"),
                 textOutput("description3"),
                 textOutput("description4"),
                 textOutput("description5"),
                 textOutput("description6"),
                 textOutput("description7")
        ),
        tabPanel(title = "Overall Statistics",
                 add_busy_spinner(spin = "fading-circle"),
                 textOutput("playerName"),
                 column(5, tableOutput("overviewTable")),
                 column(7, textOutput("overall1"),
                        textOutput("overall2"),
                        textOutput("overall3"),
                        textOutput("overall4"),
                        textOutput("overall5"),
                        textOutput("overall6"),
                        textOutput("overall7"),
                        textOutput("overall8")
                        )
                 
        ),
        tabPanel(title = "Statistics by Type",
                 add_busy_spinner(spin = "fading-circle"),
                 column(4, selectInput(inputId = "tierChoice", label = "Choose a Tier", c("all", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))),
                 column(4, selectInput(inputId = "categoryChoice", label = "Choose a Category", categoryArr)),
                 plotOutput("StatisticsByType")
        ),
        tabPanel(title = "Statistics by Tier",
                 add_busy_spinner(spin = "fading-circle"),
                 column(4, selectInput(inputId = "typeChoice", label = "Choose a Type", c("all", "Light Tank", "Medium Tank", "Heavy Tank", "AT-SPG"))),
                 column(4, selectInput(inputId = "categoryChoiceII", label = "Choose a Category", categoryArr)),
                 plotOutput("StatisticsByTier")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$description1 <- renderText({
    "This project is inspired by BlitzStars (https://www.blitzstars.com)"
  })
  
  output$description2 <- renderText({
  "World of Tanks Blitz is a free millitary game that can be played on various of platforms including 
    phones and computers (https://na.wotblitz.com)."
  })
  
  output$description3 <- renderText({
    "From this project, we can check a player's overall statistics (in the overall statistics tab), and statistics by different catrgories 
    (in the statistics by type and statistics by tier tabs)"
  })
  
  output$description4 <- renderText({
    "There are ten tiers and four types of tanks in the game. Tiers range from 1 to 10, and types include light tanks, medium tanks, heavy 
    tanks and tank destroyer (AT-SPG)"
  })
  
  output$description5 <- renderText({
    "In order to search for a specific player, we need to first choose a server that the player belongs to, and then type the player's name
    in the \"Player Search\" area (at lease three letters are required). Then, in the \"Choose a Player\" area, at most ten players that have similar
    names as the one in the \"Player Search\" area will be shown, so that we can choose the player we want."
  })
  
  output$description6 <- renderText({
    "For reference, we can search the creator's account, \"Lei_47\" in the North American and the Asian server, \"Lei_74\" in the Asian server, and \"KVSeries\" in 
    the European server."
  })
  
  output$description7 <- renderText({
    "If there are any errors showed up, please press the search buttom again or refresh the page :)."
  })
  
  observe({
    roughName <- input$roughName
    updateSelectInput(session, "nameChoice",
                      choices = getNames(roughName)
    )
  })
  
  observe({
    serverChoice <- input$serverChoice
    # "North America", "Asia", "Russia", "Europe"
    if (serverChoice == "North America") {
      area <<- "com"
    } else if (serverChoice == "Asia") {
      area <<- "asia"
    } else if (serverChoice == "Russia") {
      area <<- "ru"
    } else {
      area <<- "eu"
    }
    
    updateTextInput(session, "roughName", value = "")
    
    updateSelectInput(session, "nameChoice",
                      choices = getNames("abc"))
    
  })
  
  observeEvent(input$getData, {
    output$playerName <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        show_spinner() # show the spinner
        getLocalId(nameChoice, area)
        hide_spinner()
      }
    })
    output$overviewTable <- renderTable({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        getOverviewTable()
      }
    })
    
    output$overall1 <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        "\"Player\" is the name of the player."
      }
    })
    
    output$overall2 <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        "\"Total Battles\" is the total number of battles the player has played."
      }
    })
    
    output$overall3 <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        "\"Winrate\" is calculated by dividing the number of victories by total battles."
      }
    })
    
    output$overall4 <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        "\"Average Damage\" is the average damage that a player dealt per game."
      }
    })
    
    output$overall5 <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        "\"Damage Ratio\" is calculated by dividing the overall damage dealt by the overall damage received
        from enemies."
      }
    })
    
    output$overall6 <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        "\"Kills/deaths\" is calculated by dividing the overall number of destruction from the player by the 
        number of times that the player has been destructed."
      }
    })
    
    output$overall7 <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        "\"Spots\" is the average number of tanks that the player can find before other teammates per game."
      }
    })
    
    output$overall8 <- renderText({
      nameChoice <- isolate(input$nameChoice)
      if (nameChoice != "-") {
        "\"Survival Rate\" is calculated by dividing the number of battle that the player has survived by the total
        number of battles."
      }
    })
    
    output$StatisticsByType <- renderPlot({
      nameChoice <- isolate(input$nameChoice)
      getData <- input$getData
      tierChoice <- input$tierChoice
      categoryChoice <- input$categoryChoice
      if (nameChoice != "-") {
        getLocalId(nameChoice, area)
        show_spinner()
        getPlotByType(tierChoice, categoryChoice)
      }
    })
    
    output$StatisticsByTier <- renderPlot({
      nameChoice <- isolate(input$nameChoice)
      getData <- input$getData
      typeChoice <- input$typeChoice
      categoryChoice <- input$categoryChoiceII
      if (nameChoice != "-") {
        getLocalId(nameChoice, area)
        show_spinner()
        getPlotByTier(typeChoice, categoryChoice)
      }
    })
    
  })
}

shinyApp(ui = ui, server = server)