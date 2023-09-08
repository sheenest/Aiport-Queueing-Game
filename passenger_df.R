
library(dplyr)

# rm(list=ls())

# variables dataframes
# norm_id  
# prior_id
# profit => total profit
# norm_queue => id, w_time
# prior_queue => id, w_time
# norm_counter => id, w_time , c_time
# prior_counter => id, w_time , c_time
# completed => => id, w_time , c_time , revenue

#initialize dataframes
init_df <- function(){
  
  # created vector with 5 characters
  # columns= c("id","queue","counter","completed") 
  
  # pass this vector length to ncol parameter
  # and nrow with 0
  # out = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  
  # assign column names
  # colnames(out) = columns
  
  out <- list()

  norm_id <- 0 
  out$norm_id <- norm_id
  
  prior_id <- 1000 
  out$prior_id <- prior_id
  
  profit <- 0
  out$profit <- profit
  
  
  # Queue
  columns <- c("id","w_time") 
  # pass this vector length to ncol parameter
  # and nrow with 0
  
  norm_queue = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  # assign column names
  colnames(norm_queue) = columns
  # display
  out$norm_queue <- norm_queue
  
  prior_queue = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  # assign column names
  colnames(prior_queue) = columns
  # display
  out$prior_queue <- prior_queue
  
  
  # Counter
  columns <- c( "c_id" , "counter_num","occupied","id","w_time" , "c_time" , "fixed_time") 
  # pass this vector length to ncol parameter
  # and nrow with 0
  
  k = 2 #starts with 2 counters
  
  norm_counter <- data.frame(matrix(nrow = k, ncol = length(columns))) 
  
  # assign column names
  colnames(norm_counter) = columns
  for ( i in 1:nrow(norm_counter) ){
    norm_counter$c_id[i] = paste0("nc",i)
    norm_counter$counter_num[i] = i 
    norm_counter$occupied[i] = FALSE 
    norm_counter$id[i] = 0 
    norm_counter$w_time[i] = 0
    norm_counter$c_time[i] = 0
    norm_counter$fixed_time[i] = 5
  }
  out$norm_counter <- norm_counter
  
  #priority
  columns2 <- c( "c_id" , "counter_num" , "occupied" , "id" , "w_time" , "c_time" , "fixed_time" ) 
  # pass this vector length to ncol parameter
  # and nrow with 0
  
  k = 1 #starts with 1 priority counter
  
  prior_counter = data.frame(matrix(nrow = k, ncol = length(columns2))) 
  
  # assign column names
  colnames(prior_counter) = columns2
  for ( i in 1:nrow(prior_counter) ){
    prior_counter$c_id[i] = paste0("pc",i)
    prior_counter$counter_num[i] = i 
    prior_counter$occupied[i] = FALSE 
    prior_counter$id[i] = 0 
    prior_counter$w_time[i] = 0
    prior_counter$c_time[i] = 0
    prior_counter$fixed_time[i] = 3
  }
  out$prior_counter <- prior_counter
  #not sure of prior counter can use same completed function
  
  # completed
  columns <- c("id","w_time" , "c_time" , "revenue") 
  # pass this vector length to ncol parameter
  # and nrow with 0
  completed = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  # assign column names
  colnames(completed) = columns
  # display
  out$completed <- completed
  
  profit <- Reduce('+', completed$revenue)
  
  return(out)
}

inc <- function(x) {eval.parent(substitute(x <- x + 1))}

#enqueue
enqueue <- function( queue , global_id ){
  
  # str(queue)
  global_id <- global_id + 1 
  last_id <- global_id 

  id = global_id
  c_time = 0
  # queue <- rbind( queue , cbind( id , c_time)

  queue[nrow(queue) + 1,] = c(id, c_time)
  print("enqueue queue")
  print( queue )
  out<- list()
  out$global_id <- global_id
  out$queue <- queue
  return( out )
  
}

check_counter_available<- function( counter ){
  available <- FALSE
  index <- list()
  # print(index)
  for( i in 1:nrow(counter) ){
    if( counter$occupied[counter$counter_num == i] == FALSE ){
      available <- TRUE
      #get length of list
      len <- length(index)
      #append value to end of list
      index[[len+1]] <- i 
      # print( i)
      
    }
  }
  
  out <- list()
  out$index <- index
  out$available <- available 
  return(out)
  
}

#dequeue 1 passenger to a specific counter
dequeue <- function( queue , counter , counter_id ){
  
  #check if counter is empty
  index <- match( counter_id , counter$c_id )
  # print("index")
  # print(index)
  # print("counter$occupied[ index ]")
  # print( counter$occupied[ index ] )
  if( counter$occupied[ index ] == FALSE ){
    # counter is not occupied
    
    #save data of first passenger in queue
    passenger = queue[1 , ]
    
    #remove passenger from queue
    
    if( nrow(queue) >= 2 ){
      queue <- queue %>% slice(-1)
    }
    else{
      queue <- data.frame( id = "NULL" , w_time = 0 )
    }
    # print( "dequeue queue")
    # print( queue )

    # index <- counter$counter_num == counter_num

    
    #update counter
    counter$occupied[ index ] = TRUE
    counter$id[ index ] = passenger$id
    counter$w_time[ index ] = passenger$w_time
    counter$c_time[ index ] = 0 
    
    
  }
  # else{
    # the counter is occupied
    # show error
    # print("Can't do that, counter is occupied")
  # }
  
  print( "dequeue queue")
  print( queue )
  print("nrow(queue)")
  print(nrow(queue))
  
  out<- list()
  # if( nrow(queue) >= 1 ){
  out$queue <- queue
  # }
  # else{
  #   # columns <- c("id","w_time") 
  #   # pass this vector length to ncol parameter
  #   # and nrow with 0
  #   
  #   out$queue <- data.frame( id = "NA" , w_time = 0 )
  # }
  out$counter <- counter
  return(out)
} 


#increment waiting time in the game
incre_time <- function( queue , counter ){
  
  #increment waiting time to all passengers in queue
  for (i in 1:nrow(queue)){
    queue[i,'w_time'] <- as.numeric(queue[i,'w_time']) +1
  }
  
  #increment waiting time to all passengers in the counter
  for (i in 1:nrow(counter)){
    
    if( counter$occupied[i] == TRUE ){
      counter[i,'c_time'] <- as.numeric(counter[i,'c_time']) +1
      
    }
  }
  
  out = list()
  out$queue = queue
  out$counter = counter
  return(out)
}



# check passengers at counters if they are complete and move them to completed df

calculate_revenue <- function ( w_time , c_time , normal ){
  
  
  if ( normal ){
    
    if( w_time <= 5 ){
      revenue = 5
    }
    else if ( w_time <= 10 ){
      revenue = 2
    }
    else{
      revenue = 0 
    }
    
  }
  else{
    if( w_time <= 5 ){
      revenue = 7
    }
    else if ( w_time <= 10 ){
      revenue = 3
    }
    else{
      revenue = 0 
    }
  }

  return(revenue)
}

# calculate_revenue(30)

#check if passenger at counter has completed and moves the passenger to completed
check_complete <- function( counter , completed ){
  
  id <- list()
  total_rev <- 0 
  for( i in 1:nrow(counter)){
    
    # print( "test" )
    if( counter$occupied[i] == TRUE ){
      #whehter the passenger completed checking the luggage can depend on:
      #1. fixed timing
      #2. probability cdf based on c_time
      if( counter$c_time[i] >= counter$fixed_time[i] ){
        # print( paste("removing passenger ", counter$id[i] , " from counter ", counter$counter_num[i]) )
        passenger <- counter[i,]
        # print("i" , i )
        # counter[i,] <- NULL
        
        counter$id[i] <- -1
        counter$w_time[i] <- -1
        counter$c_time[i] <- -1
        counter$occupied[i] <- FALSE
        # view(counter)
        
        print("passenger")
        print( passenger )

        id <- append( id , passenger$id )
        
        print("id")
        print( id )
        
        # normal = TRUE means its a normal passenger, 
        # normal = FALSE means its a priority passenger
        if( passenger$id[1] <1000 ){
          normal <- TRUE
        }
        else{
          normal <- FALSE
        }
        revenue <- calculate_revenue( passenger$w_time[1] , passenger$c_time[1] , normal )
        
        print("revenue")
        print( revenue )
        total_rev <- total_rev + revenue
        passenger$revenue <- revenue 
        # print( passenger)
        # passenger
        
        completed[nrow(completed) + 1,] <- c(passenger$id , passenger$w_time , passenger$c_time , passenger$revenue )
        # print(completed)
        
      }
    }
  }
  
  # print( "counter from check_complete" )
  # print(counter)
  # print( "completed from check_complete")
  # print(completed)
  
  out <- list()
  out$counter <- counter
  out$completed <- completed
  out$id <- id 
  out$revenue <- total_rev
  return(out)
  # return(passenger)
}

#adding counter
addcounter <- function( counter,profit ){
  # adding <- sample(c(1, 0), size = 1, replace = TRUE, prob = c(0.9, 0.1))
  # 
  # if (adding == 1){
  if( runif(1) <= 0.9 ){
    # columns <- c("counter_num","occupied","id","w_time" , "c_time" , "fixed_time") 
    # counter = data.frame(matrix(nrow = k, ncol = length(columns)))
    # print("counter_num = nrow(counter)+1")
    # print( nrow(counter)+1 )
    
    
    new_counter = data.frame( list(c_id = paste0( "nc" , nrow(counter)+1 ),
                              counter_num = c(nrow(counter)+1) ,
                              occupied = c(FALSE) ,
                              id = c(0) , 
                              w_time = c(0),
                              c_time = c(0),
                              fixed_time = c(5) ))
    
    # print("counter")
    # print(counter)
    # print("new_counter")
    # print(new_counter)
    counter <- rbind( counter , new_counter )
    
    # counter <- counter[nrow(counter)+1]
    # profit <- profit - 10 #this not sure
  }
  
  profit <- profit - 10 #still gets deducted

  
  out <- list()
  out$counter <- counter
  out$profit <- profit
  return(out)
}
  
test <- function(){
  rm(list=ls())
  
  #call init_df and retrieve dataframes
  # print("init_df")
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
  
  # print("norm_id")
  # str(norm_id)
  # 
  # print("prior_id")
  # str(prior_id)
  # 
  # print("norm_queue")
  # str(norm_queue)
  # print("norm_counter")
  # str(norm_counter)
  # 
  # print("prior_queue")
  # str(prior_queue)
  # print("prior_counter")
  # str(prior_counter)
  # 
  # print("completed")
  # str(completed)
  
  #testing enqueue and counter
  print("run for 20 iterations")
  
  for (i in 1:10){
    print(paste("iteration ",i) )
    

    #enqueue normal passenger 
    # if( runif(1) <= 0.5 ){
    # print("enqueue 1 passenger to normal queue")

    norm_queue <- na.omit(norm_queue)
    # print("norm_queue")
    # str(norm_queue)
    # print("norm_id")

    # str(norm_queue)
    ls <- enqueue( norm_queue , norm_id )
    norm_queue <- ls$queue
    norm_queue <- na.omit(norm_queue)

    norm_id <- ls$global_id

    # print("norm_queue")
    # print(norm_queue)
    # print("norm_id")
    # print(norm_id)
    rm(ls)
      
    # }
    
    #enqueue priority passenger
    # if( runif(1) <= 0.5 ){
    # print("enqueue 1 passenger to priority queue")

    prior_queue <- na.omit(prior_queue)
    # str(prior_queue)
    ls <- enqueue( prior_queue , prior_id )
    prior_queue <- ls$queue
    prior_queue <- na.omit(prior_queue)

    prior_id <- ls$global_id

    # print("prior_queue")
    # print(prior_queue)
    # print("prior_id")
    # print(prior_id)
    rm(ls)
    # }
    
    #check counter available
    # print("check counter available")
    ls = check_counter_available( norm_counter )
    n_available = ls$available
    n_index = ls$index
    rm(ls)
    
    # print("n_available")
    # print(n_available)
    # print("n_index")
    # print(n_index)
    
    ls = check_counter_available( prior_counter )
    p_available = ls$available
    p_index = ls$index
    rm(ls)
    
    # print("p_available")
    # print(p_available)
    # print("p_index")
    # print(p_index)
    
    # add counter
    # print("norm_counter")
    # print( norm_counter )
    # print( "nrow(norm_counter)")
    # print(nrow(norm_counter))
    # 
    # print("prior_counter")
    # print( prior_counter )
    # print( "nrow(prior_counter)")
    # print(nrow(prior_counter))
    
    if( i == 5 ){

      # print( "adding counter")
      ls = addcounter( norm_counter , profit  )
      profit <- ls$profit
      norm_counter <- ls$counter
      rm(ls)
      
      print("norm_counter")
      print(norm_counter)
      # print("profit")
      # print( profit )

    }
    # print("n_available")
    # print( n_available )
    # print("norm_queue")
    # print( norm_queue )
    # print("n_index")
    # print(n_index)
    # print('length(n_index)')
    # print( length(n_index) )
    # 
    # print("n_index[1]")
    # print(n_index[1])
    # print("n_index[2]")
    # print(n_index[2])
    
    if (n_available == TRUE & nrow(norm_queue) >= 1 ){
      #there is a counter available, dequeue passenger to counter
      # # print("dequeue normal passenger")
      # print("nrow(norm_counter)")
      # print(nrow(norm_counter))
      # print("norm_queue")
      # print( norm_queue )
      # print( "min(length(n_index) , nrow(norm_queue))" )
      # print(min(length(n_index) , nrow(norm_queue)) )
      
      for (i in 1: nrow(norm_counter) ){

        # print("norm_counter$occupied[i]")
        # print( norm_counter$occupied[i] )

        if( norm_counter$occupied[i] == FALSE && nrow(norm_queue) >= 1 ){
          # print("dequeueing")
          #testing dequeue
          # print( paste("dequeue 1 passenger to counter " , n_index[i] ) )

          q_c <- dequeue( norm_queue , norm_counter , norm_counter$c_id[i] )
          norm_queue <- q_c$queue
          norm_counter <- q_c$counter
          rm(q_c)

        }

      }
      # print("norm_queue")
      # print(norm_queue)
      # print("norm_counter")
      # print(norm_counter)
    }
    
    # print("p_available")
    # print( p_available )
    # print("prior_queue")
    # print( prior_queue )
    # print("p_index")
    # print(p_index)
    # print('length(p_index)')
    # print( length(p_index) )
    # print("p_index[1]")
    # print(p_index[1])
    # 
    # print( "p_available == TRUE & nrow(prior_queue) >= 1")
    # print( p_available == TRUE & nrow(prior_queue) >= 1 )
    
    if (p_available == TRUE & nrow(prior_queue) >= 1 ){
      #there is a counter available, dequeue passenger to counter
      # print("dequeue priority passenger")
      
      for (i in 1:nrow(prior_counter) ){

        if( prior_counter$occupied[i] == FALSE && nrow(prior_queue) >= 1 ) {
          # print("dequeueing")
          #testing dequeue
          # print( paste("dequeue 1 passenger to counter " , p_index[i] ) )

          q_c <- dequeue( prior_queue , prior_counter , prior_counter$c_id[i] )
          prior_queue <- q_c$queue
          prior_counter <- q_c$counter
          rm(q_c)
        }


      }
      
      # print("prior_queue")
      # print(prior_queue)
      # print("prior_counter")
      # print(prior_counter)
    }
    
    ls <- incre_time( norm_queue , norm_counter )
    norm_queue <- ls$queue
    norm_counter <- ls$counter
    rm(ls)
    # print("incre_time for normal counter and queue")
    # print( "norm_counter" )
    # print( norm_counter )
    # print("norm_queue")
    # print( norm_queue )
    
    ls <- incre_time( prior_queue , prior_counter )
    prior_queue <- ls$queue
    prior_counter <- ls$counter
    rm(ls)
    # print("incre_time for prior counter and queue")    
    # print( "prior_counter" )
    # print( prior_counter )
    # print("prior_queue")
    # print( prior_queue )
    
    # print("check_complete for normal counters before")
    # print("norm_counter")
    # print( norm_counter )
    # print("completed")
    # print( completed )
    
    ls <- check_complete( norm_counter , completed )
    norm_counter <- ls$counter
    completed <- ls$completed 
    rm(ls)
    # print("check_complete for normal counters")
    # print("norm_counter")
    # print( norm_counter )
    # print("completed")
    # print( completed )

    # print("check_complete for prior counters before")
    # print("prior_counter")
    # print( prior_counter )
    # print("completed")
    # print( completed )
    
    ls <- check_complete( prior_counter , completed )
    prior_counter <- ls$counter
    completed <- ls$completed 
    rm(ls)
    # print("check_complete for prior counters")
    # print("prior_counter")
    # print( prior_counter )
    # print("completed")
    # print( completed )
    

    
    profit <- Reduce('+', completed$revenue)
    # print("profit")
    # print(profit)
    

  }
  #check completed
  
  
  
  # print("profit")
  # print(profit)
  # 
  # print("completed")
  # print(completed)
  
}


# test()
# df <- data.frame( id = "NA" , w_time = 0 )
# df1 <- subset( df , id == 1 )
# df<- rbind( df1 , df )
# 
# nrow(df)
# df$id
# df$w_time





