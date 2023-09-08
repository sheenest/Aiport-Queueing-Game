# sprites_q <- as.data.frame(list(id=c( "q1" , "q2" , "q3" ), # must be unique for each sprite
#                             type = rep("blank queue",3) , 
#                             img=c( "box.png","box.png","box.png" ), # any image file in www
#                             x_pct=c( 1000,900,800 ), # the x location of the playing field width
#                             y_pct=c( 500,500,500  ), # the y location of the playing field height
#                             label=c( "","","" ), # the label to display below the sprite (could be blank)
#                             x_scalepct=c( 80,80,80), # the width of the sprite as a percentage of the playing field width
#                             y_scalepct=c( 100,100,100 ), # the height of the sprite as a percentage of the playing field height
#                             showBorder=c(TRUE,TRUE,TRUE))) # do you wish to display a border around the sprite?
# 
# 
# sprites_p <- as.data.frame(list(id=c(1,2,3), # must be unique for each sprite
#                           type = rep("p_nqueue",3) , # 
#                           img=c( "Asset 13.png","Asset 14.png","Asset 15.png" ), # any image file in www
#                           x_pct=sprites_q$x_pct, # the x location of the playing field width
#                           y_pct=sprites_q$y_pct, # the y location of the playing field height
#                           label=c("","",""), # the label to display below the sprite (could be blank)
#                           x_scalepct=c( 80,80,80), # the width of the sprite as a percentage of the playing field width
#                           y_scalepct=c( 100,100,100 ), # the height of the sprite as a percentage of the playing field height
#                           showBorder=c(FALSE,FALSE,FALSE))) # do you wish to display a border around the sprite?
# 
# sprites_c <- as.data.frame(list(id=c("c1","c2"), # must be unique for each sprite
#                               type = rep("ncounter" , 2 ),
#                               img=c( "box.png","box.png" ), # any image file in www
#                               x_pct=c( 1200, 1200 ) , # the x location of the playing field width
#                               y_pct=c( 400 , 600 ) , # the y location of the playing field height
#                               label=c("",""), # the label to display below the sprite (could be blank)
#                               x_scalepct=c( 80,80), # the width of the sprite as a percentage of the playing field width
#                               y_scalepct=c( 100,100 ), # the height of the sprite as a percentage of the playing field height
#                               showBorder=c(FALSE,FALSE))) # do you wish to display a border around the sprite?

#also need to priority queue and priority counter in helper functions

#type codes, have to be arranged in the following specific order
# ncounter => normal counter
# pcounter => priority counter
# p_ncounter => passengers in counter
# p_nqueue => passengers in queue
# p_pcounter => passengers in priority counter
# p_pqueue => passengers in priority queue

split_df <- function( sprites ){

  out <- list()

  out$ncounter   <- subset( sprites , type == "ncounter")
  rownames(out$ncounter) <- NULL
  
  out$pcounter   <- subset( sprites , type == "pcounter")
  rownames(out$pcounter) <- NULL
  
  out$p_ncounter <- subset( sprites , type == "p_ncounter")
  rownames(out$p_ncounter) <- NULL
  
  out$p_nqueue   <- subset( sprites , type == "p_nqueue")
  rownames( out$p_nqueue) <- NULL
  
  out$p_pcounter <- subset( sprites , type == "p_pcounter")
  rownames(out$p_pcounter) <- NULL
  
  out$p_pqueue   <- subset( sprites , type == "p_pqueue")
  rownames(out$p_pqueue) <- NULL

  return(out)

}

comb_df<- function( input ){
  #input is a list of dataframes of each sprite type
  
  
  # sprites1 <- data.frame(matrix(ncol = 9, nrow = 0))
  # colnames(sprites1) <- c("id" , "type" , "img" , "x_pct" , "y_pct" ,
  #                        "label" , "x_scalepct" , "y_scalepct" , "showBorder")
  #
  # sprites2 <- data.frame(matrix(ncol = 9, nrow = 0))
  # colnames(sprites2) <- c("id" , "type" , "img" , "x_pct" , "y_pct" ,
  #                        "label" , "x_scalepct" , "y_scalepct" , "showBorder")
  
  sprites <- data.frame(matrix(ncol = 9, nrow = 0))
  colnames(sprites) <- c("id" , "type" , "img" , "x_pct" , "y_pct" ,
                         "label" , "x_scalepct" , "y_scalepct" , "showBorder")
  
  if( nrow(input$ncounter) > 0 ){
    sprites <- rbind( sprites , input$ncounter )
  }
  if( nrow(input$pcounter) > 0 ){
    sprites <- rbind( sprites , input$pcounter )
  }
  if( nrow(input$p_ncounter) > 0){
    sprites <- rbind( sprites , input$p_ncounter )
  }
  if( nrow(input$p_nqueue) > 0){
    sprites <- rbind( sprites , input$p_nqueue )
  }
  if( nrow(input$p_pcounter) > 0){
    sprites <- rbind( sprites , input$p_pcounter )
  }
  if( nrow(input$p_pqueue) > 0){
    sprites <- rbind( sprites , input$p_pqueue )
  }
  
  
  # rownames(sprites1) <- NULL
  # rownames(sprites2) <- NULL
  rownames(sprites) <- NULL
  
  # sprites <- list( sprites1 , sprites2 )
  
  return(sprites)
}


nqueue <- data.frame(list(nx = c(1017,917,817,717,617,517,417,317,217,117),
                          ny = rep(425,10) ))

pqueue <- data.frame(list(nx = c(1017,917,817,717,617,517,417,317,217,117),
                          ny = rep(282,10) ))

pqcounter <- data.frame( list( nx = rep( 1365 , 4 ),
                               ny = c( 390 , 522 , 651 , 784 ) ))

new_counter <- list( nx = 1450 , ny = 760 )



enqueue_sprite<- function ( normal , sprites , id ){
  #normal = TRUE, enqueue normal passenger
  #normal = FASLE, qneueu priority passenger
  
  
  pic_index = as.integer(runif( 1,1,30 )) #generate 1 number between 1 and 30 
  pic_string = paste0("indv_",pic_index,".png")
  # print(pic_string)
  
  out <- list()
  
  if( normal ){ #if it is a normal passenger
    type = "p_nqueue"
    y = nqueue$ny[1]
    
    # if( "p_nqueue" %in% sprites$type ){
    #   p_nqueue <- subset( sprites , type == fixed("p_nqueue"))
    #   rownames( p_nqueue ) <- NULL
    #   
    #   id <- as.integer(tail(p_nqueue$id , 1)) + 1
    # }
    # else{
    #   id <- 1 
    # }

    # id = nrow(p_nqueue)+1 #new id 
    
    out$nq <- "p_nqueue"
  }
  else{
    type = "p_pqueue"
    y = pqueue$ny[1]
    
    # if( "p_pqueue" %in% sprites$type ){
    #   
    #   p_pqueue <- subset( sprites , type == fixed("p_pqueue"))
    #   rownames( p_pqueue ) <- NULL
    #   
    #   id <-  as.integer(tail(p_pqueue$id , 1)) + 1
    # 
    # }
    # else{
    #   id <- 1001
    # }

    out$nq <- "p_pqueue"
    
    
  }

  
  out$new_p <- as.data.frame( list( id = id,
                              type = type,
                              img = pic_string,
                              x_pct = -80,
                              y_pct = y, 
                              label = "",
                              x_scalepct = 64,
                              y_scalepct = 80,
                              showBorder = FALSE ))
  
 
  return( out )
}

