## server.R

# load libraries
library(recommenderlab)

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(idx = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   rating = unlist(as.character(value_list)))
  dat = dat[!is.null(rating) & !is.na(idx)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (idx = as.numeric(idx), rating = as.numeric(rating))]
  dat = dat[rating > 0]
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

# load pre-computed values
system1.res = readRDS("rds/system1.res.RDS")
popular.120 = readRDS("rds/popular.120.RDS")
popular.120 = match(unlist(popular.120), movies$MovieID)
recommender.IBCF = readRDS("rds/recommender.IBCF.RDS")


shinyServer(function(input, output, session) {
  
  # System I
  
  # Give recommendations when the submit button is clicked
  df1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      user_predicted_ids = unlist(system1.res[input$slct1])
      user_predicted_idx = match(user_predicted_ids, movies$MovieID)
      recom_results <- data.table(idx = user_predicted_idx)
      
    }) # still busy
  }) # clicked on button
  
  # display the recommendations
  output$results1 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$idx[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$idx[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  
  # System II
  
  # Show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[popular.120[(i - 1) * num_movies + j]], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[popular.120[(i - 1) * num_movies + j]])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the submit button is clicked
  df2 <- eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        
        newdata = matrix(rep(NA, 3706), nrow = 1)
        for(i in user_ratings$idx){
          newdata[popular.120[i]] = user_ratings$rating[i]
        }
        newdata = as(newdata, "realRatingMatrix")
        
        p.IBCF = predict(recommender.IBCF, newdata, type="ratings")
        p.IBCF = as.numeric(as(p.IBCF, "matrix"))
        
        num.non.na = sum(!is.na(p.IBCF))
        n = 10
        m = 0
        if(num.non.na < n){
          m = n - num.non.na
          n = num.non.na
        }
        user_predicted_idx = tail(order(p.IBCF, decreasing = FALSE, na.last = FALSE), n)
        if(m > 0){
          user_predicted_idx = c(user_predicted_idx, popular.120[!popular.120 %in% user_predicted_idx][1:m])
        }
        recom_results <- data.table(idx = user_predicted_idx)
        
    }) # still busy
  }) # clicked on button
  

  # display the recommendations
  output$results2 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df2()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
          div(style = "text-align:center", 
              a(img(src = movies$image_url[recom_result$idx[(i - 1) * num_movies + j]], height = 150))
             ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies$Title[recom_result$idx[(i - 1) * num_movies + j]])
             )
          
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function