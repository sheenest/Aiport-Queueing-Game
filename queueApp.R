#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list=ls())
library(shiny)
library(jsonlite)
library(stringr)
library(tidyverse)

source( "sprites_df.R" )
source( "passenger_df.R" )

numbers_only <- function(x) str_detect(x, "^[:digit:]+$")

ui <- fluidPage(

  # h1(em("Airline check in counter")),
  
  sidebarLayout(
    sidebarPanel(
      # column(2,
      tags$head(tags$style('h1 {color:blue;}')),
      titlePanel(title = span(img(src = "MyImage.jpg", height = 100), "Check In!")),
      
      actionButton('start','Start Game'),
      actionButton("restart","Restart"),
      hr(), 
      #need to cut down on words
      h3(em('Game Objective:')),
      p('To maximise profits earned in a span of 3 hours.'),
      h3(em("Controls:")),
      p("Drag passengers to their respective check in counters with minimum wait time. You may add new counters with a 90% probability and a cost of $10."),
      h2(em('Rules:')),
      h3('Normal Passenger:'),
      p("Passengers in the normal queue can only join the priority counter if there is a empty priority counter and no priority passengers waiting'. "),
      p("Respective revenue earned for different group sizes and waiting times below:"),
      p("1 passenger (counter time: 3 mins) | < 15 mins: $5 | < 30 mins: $2 | > 30 mins: $0"),
      p("2 passengers (counter time: 5 mins) | < 15 mins: $10 | < 30 mins: $5 | > 30 mins: $3"),
      p("3 passengers (counter time: 7 mins) | < 15 mins: $15 | < 30 mins: $6 | > 30 mins: $4"),
      p("4 passengers (counter time: 8 mins) | < 15 mins: $20 | < 30 mins: $8 | > 30 mins: $5"),
      h3("Priority passenger:"),
      p("Can only join priority counters"),
      p("Respective revenue earned for different group sizes and waiting times below:"),
      p("1 passenger (counter time: 2 mins) | < 15 mins: $7 | < 30 mins: $3 | > 30 mins: $0"),
      p("2 passengers (counter time: 3 mins) | < 15 mins: $14 | < 30 mins: $6 | > 30 mins: $0"),
      p("3 passengers (counter time: 4 mins) | < 15 mins: $21 | < 30 mins: $9 | > 30 mins: $0"),
      p("4 passengers (counter time: 5 mins) | < 15 mins: $28 | < 30 mins: $12 | > 30 mins: $0"),
      width = 2
      
      
    ),
    mainPanel(
      fluidRow(
        h2( textOutput('GameTime') )
      ),
      fluidRow(
        tags$script(src="ShinySprites.js"),
        tags$div(id="playingfield"),
        tags$script(src="https://d3js.org/d3.v3.min.js"),
        # Server side has control over the style of sprite label and border
        tags$style(".spritelabel {fill:yellow;} .spriteborder {stroke:yellow;stroke-width:2} "),
        # tags$br()
        
      ),
      
      fluidRow(
        h3("Game Progress"),
        column( 6 , 
                h4(textOutput('OverallProfit')),
                h4(textOutput('normalqueue'))
                ),
        
        column( 6,
                h4(textOutput("priorityqueue")),
                h4(textOutput("Avgwaitingtime"))
                )

      )
      
    )
    
  )
)

resetGame <-function(){
  
  ls <- init_df()
  prior_id <- ls$prior_id
  norm_id <- ls$norm_id
  
  profit <- ls$profit
  
  norm_queue <- ls$norm_queue
  prior_queue <- ls$prior_queue
  
  norm_counter <- ls$norm_counter
  prior_counter <- ls$prior_counter
  
  completed <- ls$completed
  profit <- ls$profit
  
  rm(ls)
  
  game_df <- list()
  game_df$prior_id <- prior_id
  game_df$norm_id <- norm_id

  game_df$profit <- profit

  game_df$norm_queue <- norm_queue
  game_df$prior_queue <- prior_queue

  game_df$norm_counter <- norm_counter
  game_df$prior_counter <- prior_counter

  game_df$completed <- completed
  
  
  nqueue <- data.frame(list(nx = c(1017,917,817,717,617,517,417,317,217,117),
                            ny = rep(425,10) ))
  pqueue <- data.frame(list(nx = c(1017,917,817,717,617,517,417,317,217,117),
                            ny = rep(282,10) ))
  pn_counter <- data.frame( list( nx = rep( 1124 , 4 ),
                                  ny = c( 234 , 344 , 454 , 564 ) ))
  new_counter <- list( nx = 1450 , ny = 760 )

  ncounter <- as.data.frame(list(id=c("nc1","nc2"), # must be unique for each sprite
                                 type = rep("ncounter" , 2 ),
                                 img=c( "box.png","box.png" ), # any image file in www
                                 x_pct=pn_counter$nx[2:3] , # the x location of the playing field width
                                 y_pct=pn_counter$ny[2:3] , # the y location of the playing field height
                                 label=c("",""), # the label to display below the sprite (could be blank)
                                 x_scalepct=c( 64 , 64 ), # the width of the sprite as a percentage of the playing field width
                                 y_scalepct=c( 80 , 80 ), # the height of the sprite as a percentage of the playing field height
                                 showBorder=c(TRUE,TRUE))) # do you wish to display a border around the sprite?
  
  pcounter <- as.data.frame(list(id=c("pc1"), # must be unique for each sprite
                                 type = rep("pcounter" ),
                                 img=c( "box.png" ), # any image file in www
                                 x_pct=c(pn_counter$nx[1]) , # the x location of the playing field width
                                 y_pct=c(pn_counter$ny[1]) , # the y location of the playing field height
                                 label=c(""), # the label to display below the sprite (could be blank)
                                 x_scalepct=c( 64 ), # the width of the sprite as a percentage of the playing field width
                                 y_scalepct=c( 80 ), # the height of the sprite as a percentage of the playing field height
                                 showBorder=c( TRUE ))) # do you wish to display a border around the sprite?
  
  add_counter <- as.data.frame(list(id=c("plus"), # must be unique for each sprite
                                    type =c("add" ),
                                    img=c( "box.png" ), # any image file in www
                                    x_pct=c( 1194 ) , # the x location of the playing field width
                                    y_pct=c( 542 ) , # the y location of the playing field height
                                    label=c(""), # the label to display below the sprite (could be blank)
                                    x_scalepct=c( 118 ), # the width of the sprite as a percentage of the playing field width
                                    y_scalepct=c( 107 ), # the height of the sprite as a percentage of the playing field height
                                    showBorder=c( TRUE ))) # do you wish to display a border around the sprite?
  
  sprites_cname <- c( 'id' , 'type' , 'img' , 'x_pct' , 'y_pct' , 'label' , 'x_scalepct' , 'y_scalepct' , 'showBorder' ) 
  
  p_pqueue <- data.frame( matrix(ncol = 9 , nrow = 0 ))
  colnames(p_pqueue) <- sprites_cname
  
  p_nqueue <- data.frame( matrix(ncol = 9 , nrow = 0 ))
  colnames(p_nqueue) <- sprites_cname
  
  p_pcounter <- as.data.frame( matrix(ncol = 9 , nrow = 0 ) )
  colnames(p_pcounter) <- sprites_cname
  
  p_ncounter <- as.data.frame( matrix(ncol = 9 , nrow = 0 ) )
  colnames(p_ncounter) <- sprites_cname
  
  #type codes, have to be arranged in the following specific order
  # ncounter => normal counter
  # pcounter => priority counter
  # p_ncounter => passengers in counter
  # p_nqueue => passengers in queue
  # p_pcounter => passengers in priority counter
  # p_pqueue => passengers in priority queue
  
  sprites_ls <- list(ncounter = ncounter,
                     pcounter = pcounter,
                     p_ncounter = p_ncounter, 
                     p_nqueue = p_nqueue,
                     p_pcounter = p_pcounter,
                     p_pqueue = p_pqueue )
  
  sprites <- comb_df( sprites_ls )
  
  sprites <- rbind( sprites , add_counter )
  
  # print("sprites")
  # print( sprites )
  out <- list()
  out$sprites <- sprites
  
  out$game_df <- game_df
  
  return( out )
}

server <- function(input, output, session) {
  
  overallprofit <- reactiveVal(0)
  active <- reactiveVal(FALSE)
  
  normqueue <- reactiveVal(0)
  priorqueue<- reactiveVal(0)
  waittime <-reactiveVal(0)
  
  timer <- reactiveVal(0)
  
  
  output$OverallProfit <- renderText({
    paste("Overall Profit= $", overallprofit())
  })
  
  output$normalqueue <- renderText({
    paste("Number of passengers in normal queue: ", normqueue())
  })
  
  output$priorityqueue <- renderText({
    paste("Number of passengers in priority queue: ", priorqueue())
  })
  
  output$Avgwaitingtime <- renderText({
    paste("Average Waiting Time: ", waittime())
  })
  
  output$GameTime <- renderText({
    paste("Game Time:", timer() , "minutes" )
  })
  
  ls <- init_df()
  prior_id <- ls$prior_id
  norm_id <- ls$norm_id
  
  profit <- ls$profit
  
  norm_queue <- ls$norm_queue
  prior_queue <- ls$prior_queue
  
  norm_counter <- ls$norm_counter
  prior_counter <- ls$prior_counter
  
  completed <- ls$completed
  profit <- ls$profit
  
  rm(ls)
  
  game_df <- reactiveValues( prior_id = prior_id , norm_id=norm_id , profit=profit , norm_queue=norm_queue , prior_queue=prior_queue,
                             norm_counter=norm_counter , prior_counter=prior_counter , completed = completed , profit=profit )
  
  
  
  # Compose data for display
  newdata <- list(divid="playingfield",imgname="background.jpg",width=1344,height=672)
  newdata <- as.data.frame(newdata)
  # Convert the R object to a JSON string
  var_json <- toJSON(newdata)
  
  # Print this to the console for debugging
  #print(var_json)
  # Use the session variable to send the data to the client;
  # We give the message a name, "initPlayingField", so the client can know what to do with it.
  # The client code is found SpriteAnimation.js"
  session$sendCustomMessage(type="initPlayingField",var_json) 
  
  # Initialize the dynamic game sprites
  
  
  nqueue <- data.frame(list(nx = c(1017,917,817,717,617,517,417,317,217,117),
                            ny = rep(425,10) ))
  
  pqueue <- data.frame(list(nx = c(1017,917,817,717,617,517,417,317,217,117),
                            ny = rep(282,10) ))
  
  pn_counter <- data.frame( list( nx = rep( 1124 , 4 ),
                                 ny = c( 234 , 344 , 454 , 564 ) ))
  
  new_counter <- list( nx = 1450 , ny = 760 )
  
  sprites_cname <- c( 'id' , 'type' , 'img' , 'x_pct' , 'y_pct' , 'label' , 'x_scalepct' , 'y_scalepct' , 'showBorder' ) 
  
  ncounter <- as.data.frame(list(id=c("nc1","nc2"), # must be unique for each sprite
                                 type = rep("ncounter" , 2 ),
                                 img=c( "box.png","box.png" ), # any image file in www
                                 x_pct=pn_counter$nx[2:3] , # the x location of the playing field width
                                 y_pct=pn_counter$ny[2:3] , # the y location of the playing field height
                                 label=c("",""), # the label to display below the sprite (could be blank)
                                 x_scalepct=c( 64 , 64 ), # the width of the sprite as a percentage of the playing field width
                                 y_scalepct=c( 80 , 80 ), # the height of the sprite as a percentage of the playing field height
                                 showBorder=c(TRUE,TRUE))) # do you wish to display a border around the sprite?
  
  pcounter <- as.data.frame(list(id=c("pc1"), # must be unique for each sprite
                                 type = rep("pcounter" ),
                                 img=c( "box.png" ), # any image file in www
                                 x_pct=c(pn_counter$nx[1]) , # the x location of the playing field width
                                 y_pct=c(pn_counter$ny[1]) , # the y location of the playing field height
                                 label=c(""), # the label to display below the sprite (could be blank)
                                 x_scalepct=c( 64 ), # the width of the sprite as a percentage of the playing field width
                                 y_scalepct=c( 80 ), # the height of the sprite as a percentage of the playing field height
                                 showBorder=c( TRUE ))) # do you wish to display a border around the sprite?
  
  add_counter <- as.data.frame(list(id=c("plus"), # must be unique for each sprite
                                 type =c("add" ),
                                 img=c( "box.png" ), # any image file in www
                                 x_pct=c( 1194 ) , # the x location of the playing field width
                                 y_pct=c( 542 ) , # the y location of the playing field height
                                 label=c(""), # the label to display below the sprite (could be blank)
                                 x_scalepct=c( 118 ), # the width of the sprite as a percentage of the playing field width
                                 y_scalepct=c( 107 ), # the height of the sprite as a percentage of the playing field height
                                 showBorder=c( TRUE ))) # do you wish to display a border around the sprite?
  
  p_pqueue <- data.frame( matrix(ncol = 9 , nrow = 0 ))
  colnames(p_pqueue) <- sprites_cname
  
  p_nqueue <- data.frame( matrix(ncol = 9 , nrow = 0 ))
  colnames(p_nqueue) <- sprites_cname
  
  p_pcounter <- as.data.frame( matrix(ncol = 9 , nrow = 0 ) )
  colnames(p_pcounter) <- sprites_cname
  
  p_ncounter <- as.data.frame( matrix(ncol = 9 , nrow = 0 ) )
  colnames(p_ncounter) <- sprites_cname
  
  #type codes, have to be arranged in the following specific order
  # ncounter => normal counter
  # pcounter => priority counter
  # p_ncounter => passengers in counter
  # p_nqueue => passengers in queue
  # p_pcounter => passengers in priority counter
  # p_pqueue => passengers in priority queue
  
  sprites_ls <- list(ncounter = ncounter,
                     pcounter = pcounter,
                     p_ncounter = p_ncounter, 
                     p_nqueue = p_nqueue,
                     p_pcounter = p_pcounter,
                     p_pqueue = p_pqueue )
  
  sprites <- comb_df( sprites_ls )
  
  sprites <- rbind( sprites , add_counter )
  # print("sprites")
  # print(sprites[1])

  
  vals <- reactiveValues( sprites = sprites , animationDuration=200 , sprite_click=0 , nq = c("NA",0) ,  pq = c("NA",0) , field_click=c(0,0) )
  
  session$sendCustomMessage(type="displaySprites",toJSON(sprites))
  
  
  
  nprob <- reactiveVal(0.25)
  pprob <- reactiveVal(0.16)
  
  observe({
    invalidateLater(1000, session)
    isolate({
      
      if( active() ){
        
        if( timer() <= 60 ){
          nprob( 0.25)
          pprob( 0.16)
        }
        else if( timer() <= 120 ){
          nprob( 0.4 )
          pprob( 0.2 )
        }
        else{
          nprob( 0.25)
          pprob( 0.16)
        }
        
        print("game_df$prior_counter")
        print( game_df$prior_counter )
        print("game_df$norm_queue")
        print( game_df$norm_queue )
        
        if( vals$nq[1] != "NA" || vals$pq[1] != "NA" ){

          if( vals$nq[1] == "p_nqueue"){
            
            p_nqueue <- subset( vals$sprites , type == "p_nqueue")
            rownames( p_nqueue ) <- NULL
            
            # print("rownames(p_nqueue)")
            # print( nrow(p_nqueue) )
            # 
            p_id <- vals$nq[2]
            
            p_index <- nrow(p_nqueue)
            # print("p_id")
            # print(p_id)
            # print("p_index")
            # print(p_index)
            
            index <- match( p_id , vals$sprites$id )
            # index = nrow( vals$sprites ) 
            
            #may not always be the last one,
            # #normal and priority passengers can come at the same time
            #
            vals$sprites$x_pct[index] = nqueue$nx[p_index]
            
            vals$nq <- c( "NA" , 0 )
          }
          
          if( vals$pq[1] == "p_pqueue"){
            p_pqueue <- subset( vals$sprites , type == "p_pqueue")
            rownames( p_pqueue ) <- NULL
            
            
            p_id <- vals$pq[2]
            p_index <- nrow(p_pqueue)
            # print("p_id")
            # print(p_id)
            # print("p_index")
            # print(p_index)
            
            index <- match( p_id , vals$sprites$id )
            
            #may not always be the last one,
            # #normal and priority passengers can come at the same time
            #
            
            vals$sprites$x_pct[index] = pqueue$nx[p_index]
            vals$pq <- c( "NA" , 0 )
          }
          
          
          
          
        }
        
        #arrival of normal passengers
        if( runif(1) < nprob()  ){

          p_nqueue <- subset( vals$sprites , type == "p_nqueue" )
          rownames( p_nqueue ) <- NULL
          
          # print( "p_nqueue")
          # print( p_nqueue )
          num_normal <- sum( vals$sprites$type == "p_nqueue")  

          if ( num_normal < 10 ){
            
            # print("enqueue 1 passenger to normal queue")
            game_df$norm_queue <- na.omit(game_df$norm_queue)
            # print("norm_queue")
            # str(norm_queue)
            # print("norm_id")
            
            # str(norm_queue)
            ls <- enqueue( game_df$norm_queue , game_df$norm_id )
            game_df$norm_queue <- ls$queue
            game_df$norm_queue <- na.omit(game_df$norm_queue)
            
            game_df$norm_id <- ls$global_id
            
            # print("norm_queue")
            # print(game_df$norm_queue)
            # print("norm_id")
            # print(game_df$norm_id)
            rm(ls)
            
            
            ls <- enqueue_sprite( TRUE , vals$sprites , game_df$norm_id )
            # print("ls")
            # print( ls )
            
            new_p <- ls$new_p
            #
            
            vals$nq <- c( ls$nq , new_p$id[1] )
            #
            vals$sprites <- rbind( vals$sprites , new_p )
            rownames( vals$sprites ) <- NULL
            

            # print("p_nqueue2")
            # print( p_nqueue )
            
            # print("enqueueing passenger")
            # print(vals$sprites)
            
            #
            # print("new passenger")
            # print( vals$sprites )
            # p_nqueue <- subset( vals$sprites , type == "p_nqueue")
            # rownames( p_nqueue ) <- NULL
            # p_id <- tail(p_nqueue$id , 1)
            # # p_id <- p_nqueue$id[1]
            # p_index <- nrow(p_nqueue)
            # # print("p_id")
            # # print(p_id)
            # # print("p_index")
            # # print(p_index)
          }
          
        }
        
        #arrival of priority passengers
        if( runif(1) < pprob()  ){
          
          
          # arrive(TRUE)
          # ls <- enqueue( queue , global_id )
          # queue <- ls$queue
          # global_id <- ls$global_id
          # rm(ls)
          # print( "vals$sprites:)")
          # print( vals$sprites )
          p_pqueue <- subset( vals$sprites , type == "p_pqueue" )
          rownames( p_pqueue ) <- NULL
          
          # print( "p_pqueue")
          # print( p_pqueue )
          num_prior <- sum( vals$sprites$type == "p_pqueue")  
          
          if ( num_prior < 10 ){
            
            #enqueue priority passenger
            # if( runif(1) <= 0.5 ){
            print("enqueue 1 passenger to priority queue")
            
            game_df$prior_queue <- na.omit(game_df$prior_queue)
            # str(prior_queue)
            ls <- enqueue( game_df$prior_queue , game_df$prior_id )
            game_df$prior_queue <- ls$queue
            game_df$prior_queue <- na.omit(game_df$prior_queue)
            
            
            game_df$prior_id <- ls$global_id
            
            # print("prior_queue")
            # print(game_df$prior_queue)
            # print("prior_id")
            # print(game_df$prior_id)
            
            rm(ls)
            
            
            ls <- enqueue_sprite( FALSE , vals$sprites , game_df$prior_id)
            # print("ls")
            # print( ls )
            
            new_p <- ls$new_p
            #
            
            vals$pq <- c( ls$nq , new_p$id[1] )
            # 
            vals$sprites <- rbind( vals$sprites , new_p  )
            rownames( vals$sprites ) <- NULL
            
            # p_nqueue <- subset( vals$sprites , type == fixed("p_nqueue") )
            # print("p_nqueue2")
            # print( p_nqueue )
            
            # vals$sprites[nrow(vals$sprites)] <- nrow(p_nqueue)
            
            # new_p$id <- nrow( p_nqueue)
            
            
            # print("enqueueing passenger")
            # print(vals$sprites)
            
            #
            # print("new passenger")
            # print( vals$sprites )
            # p_nqueue <- subset( vals$sprites , type == "p_nqueue")
            # rownames( p_nqueue ) <- NULL
            # p_id <- tail(p_nqueue$id , 1)
            # # p_id <- p_nqueue$id[1]
            # p_index <- nrow(p_nqueue)
            # # print("p_id")
            # # print(p_id)
            # # print("p_index")
            # # print(p_index)
          }
        }
        

        #increment the waiting time of the passengers in the queue and counter
        
        ls <- incre_time( game_df$norm_queue , game_df$norm_counter )
        game_df$norm_queue <- ls$queue
        game_df$norm_counter <- ls$counter
        rm(ls)
        # print("incre_time for normal counter and queue")
        # print( "norm_counter" )
        # print( game_df$norm_counter )
        # print("game_df$norm_queue")
        # print( game_df$norm_queue )
        
        ls <- incre_time( game_df$prior_queue , game_df$prior_counter )
        game_df$prior_queue <- ls$queue
        game_df$prior_counter <- ls$counter
        rm(ls)
        # print("incre_time for prior counter and queue")    
        # print( "prior_counter" )
        # print( game_df$prior_counter )
        # print("game_df$prior_queue")
        # print( game_df$prior_queue )
        
        
        #check complete for passengers at counters
        # print("check_complete for normal counters before")
        # print("norm_counter")
        # print( norm_counter )
        # print("completed")
        # print( completed )
        
        # print( "old game_df$norm_counter" )
        # print(  game_df$norm_counter  )
        # 
        # print("old game_df$completed")
        # print( game_df$completed )
        
        # print( paste0( "test" , timer() ) )
        
        ls <- check_complete( game_df$norm_counter , game_df$completed )
        game_df$norm_counter <- ls$counter
        game_df$completed <- ls$completed
        rm_id <- ls$id
        revenue <- ls$revenue
        
        game_df$profit <- game_df$profit + revenue
        
        if( length(rm_id) >= 1){

          for ( i in 1:length(rm_id) ){
            # index <- match( id , vals$sprites$id )
            rm_index <- match( rm_id[i] , vals$sprites$id )
            vals$sprites$type[rm_index] <- "completed"
            vals$sprites$x_pct[rm_index] <- 1408
          }
        }
        rm(ls)
        rm( rm_id )
        
        # print( "new game_df$norm_counter" )
        # print(  game_df$norm_counter  )
        # 
        # print("new game_df$completed")
        # print( game_df$completed )
        # 
        # 
        # print("rm_id")
        # print( rm_id )
        # 
        # print( "revenue" )
        # print( revenue )
        # print("check_complete for normal counters")
        # print("norm_counter")
        # print( norm_counter )
        # print("completed")
        # print( completed )
        
        
        # print("prior_counter")
        # print( prior_counter )
        # print("completed")
        # print( completed )
        
        ls <- check_complete( game_df$prior_counter , game_df$completed )
        game_df$prior_counter <- ls$counter
        game_df$completed <- ls$completed
        
        rm_id <- ls$id
        revenue <- ls$revenue
        
        
        game_df$profit <- game_df$profit + revenue
        
        if( length(rm_id) >= 1){
          for( i in 1:length(rm_id) ){
            # index <- match( id , vals$sprites$id )
            rm_index <- match( rm_id[i] , vals$sprites$id )
            vals$sprites$type[rm_index] <- "completed"
            vals$sprites$x_pct[rm_index] <- 1408
          }
        }
        rm(ls)
        rm( rm_id )
        
        #calculate and assignn Game Vales
        overallprofit( game_df$profit )
        
        
        p_nqueue <- subset( vals$sprites , type == "p_nqueue")
        p_nqueue <- subset( p_nqueue , x_pct != -80 )
        rownames(p_nqueue) <- NULL
        num_nqueue <- nrow( p_nqueue )
        
        p_pqueue <- subset( vals$sprites , type == "p_pqueue")
        p_pqueue <- subset( p_pqueue , x_pct != -80 )
        rownames(p_pqueue) <- NULL
        num_pqueue <- nrow( p_pqueue )
        
        normqueue(num_nqueue)
        priorqueue(num_pqueue)
        
        
        #calculate avg waiting time 
        if( "completed" %in% vals$sprites$type ){
          
          rev_list <- game_df$completed$w_time
          avg_time <- mean( rev_list )
          
          avg_time <- round( avg_time , digits = 2)
          waittime( avg_time )
          
        }

        
        
        # print("check_complete for prior counters")
        # print("prior_counter")
        # print( prior_counter )
        # print("completed")
        # print( completed )
        
        # game_df$profit <- Reduce('+', game_df$completed$revenue)
        
        
        json_sprites <- toJSON( vals$sprites )
        #print(json_sprites)  # for debug
        # Update sprite display
        session$sendCustomMessage(type="displaySprites",json_sprites)
        # Double the animationDuration
        
        # Our convention is to send all messages as data frames, even something as simple as a number
        duration <- as.data.frame(list(animationDuration=c(vals$animationDuration)))
        json_duration <- toJSON(duration)
        # #print(json_duration)
        # # Here is how to change the animation speed
        session$sendCustomMessage(type="setAnimationDuration",json_duration)
        
        print("vals$sprites")
        print( vals$sprites )
        
        timer( timer() + 1 )
        
        if( timer() >= 180 ){
          active(FALSE)
          ls<- resetGame()
          vals$sprites <- ls$sprites
          
          vals$sprite_click = 0 
          vals$nq = c("NA" , 0 )
          vals$pq = c("NA ", 0 )
          
          game_df$norm_id <- ls$game_df$norm_id
          game_df$prior_id <- ls$game_df$prior_id
          
          game_df$profit <- ls$game_df$profit
          
          game_df$norm_queue <- ls$game_df$norm_queue
          game_df$prior_queue <- ls$game_df$prior_queue
          
          game_df$norm_counter <- ls$game_df$norm_counter
          game_df$prior_counter <- ls$game_df$prior_counter
          
          game_df$completed <- ls$game_df$completed
          
          rm(ls)
          
          # print( "vals$sprites")
          # print( vals$sprites )
          # 
          # print("game_df")
          # print( game_df )
          
          json_sprites <- toJSON( vals$sprites )
          #print(json_sprites)  # for debug
          # Update sprite display
          session$sendCustomMessage(type="displaySprites",json_sprites)
          
          overallprofit(0)
          normqueue(0)
          priorqueue(0)
          waittime( 0 )
          timer(0)
        }
        
        
        
      }


      
      
    })
  })
  
  observeEvent(input$spriteClick,{
    # print( "as.data.frame(input$spriteClick)" )
    # print(as.data.frame(input$spriteClick))
    id <- input$spriteClick$id
    index <- match( id , vals$sprites$id )
    # showNotification(paste0("You clicked sprite with id: ",id))
    
    # showNotification(paste0(index ) 
    #check if a passenger is clicked
    
    
    
    ncounter <- subset( vals$sprites , type == "ncounter")
    rownames(ncounter) <- NULL
    
    # print("ncounter")
    # print( ncounter )
    pcounter <- subset( vals$sprites , type == "pcounter")
    rownames(pcounter) <- NULL
    
    p_nqueue <- subset( vals$sprites , type == "p_nqueue")
    rownames( p_nqueue ) <- NULL
    p_pqueue <- subset( vals$sprites , type == "p_pqueue")
    rownames( p_pqueue ) <- NULL
    
    # print("id")
    # print(id)
    # print("index")
    # print(index)
    # print("vals$sprites$type[index]")
    # print(vals$sprites$type[index])
    # print("id")
    # print(id)
    
    # when player clicks on a sprite, check if its a passenger and save vals$sprite_click
    if( numbers_only(as.integer(id)) &&  ( vals$sprites$type[index] == "p_nqueue" || vals$sprites$type[index] == "p_pqueue")  ){
      # print("test vals$sprites" )
      if( vals$sprites$type[index] == "p_nqueue" ) {
        
        p_index <- match( id , p_nqueue$id )
        # print("p_index")
        # print(p_index)
        if( p_index == 1 ){
          
          #sprite_click saves the id, not the index
          vals$sprite_click <- id
          # showNotification( paste0( id ) )
        }
        else{
          vals$sprite_click <- 0 
        }
        
      }
      
      else if( vals$sprites$type[index] == "p_pqueue" ) {
        
        p_index <- match( id , p_pqueue$id )
        if( p_index == 1 ){
          #sprite_click saves the id, not the index
          vals$sprite_click <- id
          # showNotification( paste0( id ) )
        }
        else{
          vals$sprite_click <- 0 
        }
        
      }
      
      # print("vals$sprite_click")
      # print( vals$sprite_click )
    }
    # print("vals$sprite_click")
    # print( vals$sprite_click )
    # print("vals$sprites")
    # print( vals$sprites )
    # index <- match( vals$sprite_click , vals$sprite$id )
    #how to determine that the passenger (represented by a row) is first in the queue?
    
    # sprites_c <- vals$sprites[ vals$sprites$type == 'counter', ]
    
    # print("id")
    # print( id )
    # print("ncounter$id")
    # print(ncounter$id)
    # print("vals$sprite_click")
    # print(vals$sprite_click)
    # print("vals$sprite_click != 0  && (id %in% ncounter$id || id %in% pcounter$id  )")
    # print( (vals$sprite_click != 0  && (id %in% ncounter$id || id %in% pcounter$id  )) )
    
    # dequeue the passenger to the counter
    if( vals$sprite_click != 0  && (id %in% ncounter$id || id %in% pcounter$id  ) ){
      
      # print("test vals$sprite_click != 0  && (id %in% ncounter$id || id %in% pcounter$id  )" )
      # sprites_ls <- split_df( vals$sprites )
      # print("sprites_ls")
      # print(sprites_ls)
      
      
      
      p_nqueue <- subset( vals$sprites , type == "p_nqueue")
      rownames( p_nqueue ) <- NULL
      p_pqueue <- subset( vals$sprites , type == "p_pqueue")
      rownames( p_pqueue ) <- NULL
      
      # print("p_nqueue")
      # print(p_nqueue)
      # print("p_pqueue$id")
      # print(p_pqueue$id)
      # print("p_ncounter")
      # print(p_ncounter)
      # print("ncounter")
      # print(ncounter)
      
      
      #index of counter
      # index <- match(id , vals$sprite$id)
      
      #id of the clicked passenger, is an integer
      new_id <- vals$sprite_click
      # print("new_id")
      # print(new_id)
      #index of passenger
      new_index <- match( new_id , vals$sprites$id )
      
      #so long as its not a priority passenger going into a normal queue
      # print( " id %in% ncounter$id" )
      # print( id %in% ncounter$id)
      #
      # print("new_id %in% p_pqueue$id" )
      # print( new_id %in% p_pqueue$id )
      #
      # print("!(( id %in% ncounter$id) && ( new_id %in% p_pqueue$id ))")
      # print( !(( id %in% ncounter$id) && ( new_id %in% p_pqueue$id )) )
      
      
      # check that its not a priority passenger going into a normal queue
      if( !(( id %in% ncounter$id) && ( new_id %in% p_pqueue$id )) ){
        vals$sprite_click <- 0
        # print("test not a priority passenger going into a normal queue" )
        #
        # print("vals$sprite$type")
        # print( vals$sprites$type )
        
        #update the type of the passenger
        if ( vals$sprites$type[index] == "ncounter" ){
          # print(" test ncounter")
          vals$sprites$type[ new_index ] <- "p_ncounter"
          
          q_c <- dequeue( game_df$norm_queue , game_df$norm_counter , id )
          
          if( q_c$queue$id[1] != "NULL"){
            game_df$norm_queue <- q_c$queue
          }
          else{
            game_df$norm_queue <- game_df$norm_queue[0,]
          }

          game_df$norm_counter <- q_c$counter
          rm(q_c)
          
        }
        else if ( vals$sprites$type[index] == "pcounter" ){
          
          # priority passenger gg to priority counter
          if( as.integer(new_id) > 1000 ){
            # print("test pcounter")
            vals$sprites$type[ new_index ] <- "p_pcounter"
            
            q_c <- dequeue( game_df$prior_queue , game_df$prior_counter , id )
            
            if( q_c$queue$id[1] != "NULL"){
              game_df$prior_queue <- q_c$queue
            }
            else{
              game_df$prior_queue <- game_df$prior_queue[0,]
            }
            game_df$prior_counter <- q_c$counter
            rm(q_c)
          }
          
          # normal passenger gg to priority counter 
          else{
            # print(" test pcounter")
            vals$sprites$type[ new_index ] <- "p_pcounter"
            
            q_c <- dequeue( game_df$norm_queue , game_df$prior_counter , id )
            
            if( q_c$queue$id[1] != "NULL"){
              game_df$norm_queue <- q_c$queue
            }
            else{
              game_df$norm_queue <- game_df$norm_queue[0,]
            }
            
            game_df$prior_counter <- q_c$counter
            rm(q_c)
            
            # print( )
          }

          
        }
        

        
        #update position of passenger
        vals$sprites$x_pct[ new_index ] <-  vals$sprites$x_pct[ index ]
        vals$sprites$y_pct[ new_index ] <-  vals$sprites$y_pct[ index ]
        
        # if its a normal passenger, the passengers in the normal queue will shift
        if( as.integer(new_id) < 1000 ){
          
          # print("test normal passenger")
          
          #update p_nqueue
          p_nqueue <- subset( vals$sprites , type == "p_nqueue")
          
          # if it is a normal queue, all passengers in the normal queue will shift
          if( nrow(p_nqueue)>0 ){
            for ( i in 1:nrow(p_nqueue) ){
              # print("moving normal passenger")
              #get id of other passengers in the queue
              # print("new_id")
              # print( new_id )
              # print("i")
              # print(i)
              p_id <- as.integer(new_id) + i
              # print("p_id")
              # print(p_id)
              #get index of other passengers in the queue
              p_index <- match( as.character(p_id) , vals$sprites$id )
              # print("vals$sprites$id")
              # print(vals$sprites$id)
              # print("as.character(p_id)")
              # print(as.character(p_id))
              # print("p_index")
              # print(p_index)
              vals$sprites$x_pct[ p_index ] <-  nqueue$nx[i]
              
            }
          }
          
        }
        
        #else its a priority passenger, the passenger in the priority queue will shift
        else{
          if( nrow(p_pqueue)>0){
            for ( i in 1:nrow(p_pqueue) ){
              
              #get id of other passengers in the queue
              p_id <- as.integer(new_id) + i
              #get index of other passengers in the queue
              p_index <- match( as.character(p_id) , vals$sprites$id )
              
              vals$sprites$x_pct[ p_index ] <-  pqueue$nx[i]
              
            }
          }
        }
        
        # print( paste0("test dequeue" , timer() ))
      }
      
      
    }
    
    
    # ncounter <- as.data.frame(list(id=c("nc1","nc2"), # must be unique for each sprite
    #                                type = rep("ncounter" , 2 ),
    #                                img=c( "box.png","box.png" ), # any image file in www
    #                                x_pct=pn_counter$nx[2:3] , # the x location of the playing field width
    #                                y_pct=pn_counter$ny[2:3] , # the y location of the playing field height
    #                                label=c("",""), # the label to display below the sprite (could be blank)
    #                                x_scalepct=c( 64 , 64 ), # the width of the sprite as a percentage of the playing field width
    #                                y_scalepct=c( 80 , 80 ), # the height of the sprite as a percentage of the playing field height
    #                                showBorder=c(TRUE,TRUE))) # do you wish to display a border around the sprite?
    
    #add new counter
    if( id == "plus" ){
      
      if( game_df$profit > 50 && !'counter' %in% vals$sprites$type ){

        # remove border for the plus sprite
        counter_index <- match( "plus" , vals$sprites$id )
        vals$sprites$showBorder[counter_index] <- FALSE
        
        #sprite for the placeholder
        new_counter <- data.frame( id = "nc3" , 
                                   type = "ncounter",
                                   img = "box.png" ,
                                   x_pct = pn_counter$nx[4],
                                   y_pct = pn_counter$ny[4],
                                   label= "",
                                   x_scalepct = 64 , 
                                   y_scalepct = 80 ,
                                   showBorder =  TRUE )
        vals$sprites <- rbind( vals$sprites , new_counter )
        
        #sprite for the actual counter png
        new_c <- data.frame( id = "counter" ,
                             type = "counter" ,
                             img = "counter.png",
                             x_pct = 1194 ,
                             y_pct = 542 ,
                             label = "" ,
                             x_scalepct = 118 ,
                             y_scalepct = 107 ,
                             showBorder = FALSE )

        vals$sprites <- rbind( vals$sprites , new_c )
        
        # add normal counter to norm_counter df
        # columns <- c( "c_id" , "counter_num","occupied","id","w_time" , "c_time" , "fixed_time") 
        new_norm_counter <- data.frame( c_id = "nc3",
                                        counter_num = 3 ,
                                        occupied = FALSE ,
                                        id = 0 ,
                                        w_time = 0,
                                        c_time = 0,
                                        fixed_time = 5 )
        game_df$norm_counter <- rbind( game_df$norm_counter , new_norm_counter )
        
        game_df$profit <- game_df$profit - 5
      }
      
      else{
        showNotification( "Cannot add counter, you have not earned enough profit yet." )
      }
    }
    
    overallprofit( game_df$profit )
    
  })
  
  observeEvent(input$start, {
    
    active(TRUE)
    
    
    })
  
  observeEvent(input$restart, {
    active(FALSE)
    
    
    ls<- resetGame()
    vals$sprites <- ls$sprites
    
    vals$sprite_click = 0 
    vals$nq = c("NA" , 0 )
    vals$pq = c("NA ", 0 )
    
    game_df$norm_id <- ls$game_df$norm_id
    game_df$prior_id <- ls$game_df$prior_id
    
    game_df$profit <- ls$game_df$profit
    
    game_df$norm_queue <- ls$game_df$norm_queue
    game_df$prior_queue <- ls$game_df$prior_queue
    
    game_df$norm_counter <- ls$game_df$norm_counter
    game_df$prior_counter <- ls$game_df$prior_counter
    
    game_df$completed <- ls$game_df$completed
    
    rm(ls)
    
    # print( "vals$sprites")
    # print( vals$sprites )
    # 
    # print("game_df")
    # print( game_df )
    
    json_sprites <- toJSON( vals$sprites )
    #print(json_sprites)  # for debug
    # Update sprite display
    session$sendCustomMessage(type="displaySprites",json_sprites)
    
    overallprofit(0)
    normqueue(0)
    priorqueue(0)
    waittime(0)
    
    timer(0)

    
    
    
    })

}

shinyApp(ui = ui, server = server)
