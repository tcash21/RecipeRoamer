
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(tm)
library(DT)
library(networkD3)
library(shiny)
library(stringi)
library(igraph)
library(stringr)
library(shinyBS)
library(shinyjs)
library(scales)

google_img_head <- 'https://www.google.com/search?q='
google_img_tail <- '&safe=off&source=lnms&tbm=isch'

## flatten out the ingredients list for performance/search reasons
flat_ingredients <-lapply(r$ingredients, function(x) paste(x, collapse=" "))

## Set color scale, needs to be tweaked
group <- data.frame(rating = c('Perfect', 'Best', 'Above Average', 'Average', 'Below Average', 'Worst', 'Inedible'), value= c(5, 4, 3.8,3.6, 3, 2, 0))
group <- group[order(group$value),]

## Lots of these values are missing, set to 0 so we don't lose the recipes
r$calories[which(is.na(r$calories))] <- 0
r$sodium[which(is.na(r$sodium))] <- 0
r$fat[which(is.na(r$fat))] <- 0

shinyServer(function(input, output) {

  df <- reactiveValues()
  ingredients_clicked <- reactiveValues()
  fridge_ingredients <- reactiveValues()
  the_recipes <- reactiveValues()
  counter <- reactiveValues(n = 0)
  
  observeEvent(input$add_btn, {
    counter$n <- counter$n + 1
    if(counter$n > 1){
      fridge_ingredients$ingredients[counter$n] <- eval(parse(text=paste0('input$textin', counter$n)))
    }
  })
  
  textboxes <- reactive({
    n <- counter$n
    if(n == 1){
      fridge_ingredients$ingredients[n] <- input$ingredient1
      lapply(seq_len(n), function(i) {
        textInput(inputId = paste0("textin", i+1),
                  label = paste0("Ingredient", i+1),
                  value = '')})
      } else if (n > 1){
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("textin", i+1),
                    label = paste0("Ingredient", i+1),
                    value = fridge_ingredients$ingredients[-1][i])})
      }
   })
  
  output$textbox_ui <- renderUI({ textboxes() })
  
  output$recipes2 <- DT::renderDataTable({
    if(input$ingredient1 == '' | input$add_btn == 0){
      return()
    }
    m <- sapply(fridge_ingredients$ingredients, function(x) stri_detect_regex(flat_ingredients, x))
    i <- apply(m, 1, all)
    the_recipes_df2 <- r[i,]
    the_recipes_df2$id <- 1:nrow(the_recipes_df2)
    the_recipes_df2 <<- the_recipes_df2
    df<-data.frame(Title=the_recipes_df2$title, Rating = the_recipes_df2$rating, Calories = the_recipes_df2$calories,
                   Fat = the_recipes_df2$fat, Sodium = the_recipes_df2$sodium,
                   recipe = paste0("<button id='button_", the_recipes_df2$id, "' type=\'button\' class=\'btn btn-default action-button\' onclick=\'Shiny.onInputChange(&quot;select_button2&quot;, this.id)\'>View Recipe</button>"))
    #df <- df[order(df$Rating, decreasing=TRUE),]
    df
    
  }, server = TRUE, rownames=FALSE, escape = FALSE, selection = 'single', options = list(order = list(list(2, 'desc')))
  )
  
  observeEvent(input$select_button2, {
    selectedRow <- as.numeric(strsplit(input$select_button2, "_")[[1]][2])
    the_selected_recipe <- the_recipes_df2[selectedRow,]
    ing <- unlist(the_selected_recipe$ingredients)
    directions <- unlist(the_selected_recipe$directions)
    the_header <- "<h3>Ingredients</h3><ul><li>"
    
    showModal(modalDialog(
      title = the_selected_recipe$title,
      HTML(paste0(the_header, paste0(ing, collapse='<li>'), '</ul><h3>Directions</h3><ul><li>', paste0(directions, collapse="<li>"), "</ul>"))
    ))
    
  })
  
  observeEvent(input$reset2, {
    fridge_ingredients$ingredients <- NULL
    the_recipes$recipes <- NULL
    df$the_df <- NULL
    counter$n <- 0
    reset('ingredient1')
    
  })
  
  
  output$About <- renderText(HTML("<br>Data Source: <a href = 'http://www.epicurious.com/', target='_blank'>epicurious</a>"))
    
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    inputs <- sapply(1:length(inputs), function(i) as.character(FUN(paste0(id, i), ...)))
    inputs
  }
  
  output$ingredients <- renderUI({
    if(input$go == 0){
      return()
    }
    the_string <- paste0("<br><br>You are viewing recipes consisting of: <ul><li>", ingredients_clicked$ingredients[1], "</li>")
    if(length(ingredients_clicked$ingredients) >= 2){
      for(i in 2:length(ingredients_clicked$ingredients)){
        the_string <- paste0(the_string, "<li>", ingredients_clicked$ingredients[i], "</li>")
      }
    }
    HTML(the_string)
  })

  observeEvent(input$reset, {
    ingredients_clicked$ingredients <- NULL
    the_recipes$recipes <- NULL
    df$the_df <- NULL
    reset('ingredient')
  })
  
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    the_selected_recipe <- the_recipes_df[selectedRow,]
    ing <- unlist(the_selected_recipe$ingredients)
    directions <- unlist(the_selected_recipe$directions)
    the_header <- "<h3>Ingredients</h3><ul><li>"
    
    showModal(modalDialog(
      title = the_selected_recipe$title,
      HTML(paste0(the_header, paste0(ing, collapse='<li>'), '</ul><h3>Directions</h3><ul><li>', paste0(directions, collapse="<li>"), "</ul>"))
    ))
    
    })
  
  observeEvent(input$go, {
    hide(id = "instructions", anim = TRUE)
    validate(
      need(input$ingredient != "", "Please type an ingredient.")
    )
    if(input$go > 1){
      ingredients_clicked$ingredients <- NULL
      the_recipes$recipes <- NULL
      df$the_df <- NULL
    }
    the_ingredients <- getAssociatedIngredients(input$ingredient)
    ingredients_clicked$ingredients <- c(ingredients_clicked$ingredients, input$ingredient)
    the_ingredients_p <- paste0(c(as.character(input$ingredient), as.character(the_ingredients$ingredients)), ".*")
    m <- sapply(the_ingredients_p, function(x) stri_detect_regex(flat_ingredients, x))
    i_ratings <- apply(m, 2, function(x) mean(r[x,]$rating, na.rm=TRUE))
    
    df$the_df <- data.frame(c(as.character(input$ingredient), as.character(the_ingredients$ingredients)), avg_rating=i_ratings, cor=c(1,the_ingredients$cors))
    df$the_df$src <- isolate(input$ingredient)
    colnames(df$the_df)[1] <- 'target'
    unique(df$the_df[,c('target', 'avg_rating')])
    if(any(duplicated(df$the_df$target))){
      df$the_df <- df$the_df[-which(duplicated(df$the_df$target)),]  
    }
    if(any(is.nan(df$the_df$avg_rating))){
      df$the_df <- df$the_df[-which(is.nan(df$the_df$avg_rating)),]  
    }
  })
  
  observeEvent(input$node, {
    if(is.null(input$node)){
      return()
    }
    
    clicked_node <- input$node
    ingredient <- strsplit(clicked_node, ":")[[1]][1]
    ingredients_clicked$ingredients <- c(ingredients_clicked$ingredients, ingredient)
    the_ingredients <- getAssociatedIngredients(ingredient)
    
    the_ingredients_p <- paste0(the_ingredients$ingredients, ".*")
    m <- sapply(the_ingredients_p, function(x) stri_detect_regex(flat_ingredients, x))
    i_ratings <- apply(m, 2, function(x) mean(r[x,]$rating, na.rm=TRUE))
    
    adf <- data.frame(the_ingredients$ingredients, avg_rating=i_ratings, cor=the_ingredients$cors)
    adf$src <- ingredient
    colnames(adf)[1] <- 'target'
    df$the_df <- isolate(rbind(df$the_df, data.frame(target=ingredient, cor=1, avg_rating=mean(adf$avg_rating), src=ingredient), adf))
    if(any(duplicated(df$the_df$target))){
      df$the_df <- df$the_df[-which(duplicated(df$the_df$target)),]  
    }
    if(any(is.nan(df$the_df$avg_rating))){
      df$the_df <- df$the_df[-which(is.nan(df$the_df$avg_rating)),]  
    }

  })

  ## get associated ingredients
  getAssociatedIngredients <- function(ingredient){
    f<-findAssocs(dtms, ingredient, corlimit=0) 
    words <- names(f[[1]])[1:10]
    cors <- f[[1]][1:10]
    return(data.frame(ingredients=c(words), cors = c(cors)))
  }
  

  output$force <- renderForceNetwork({
    if(input$go == 0 | is.null(df$the_df)){
      return()
    }
    
    g <- graph.data.frame(df$the_df[c('src', 'target')], directed=F)
    V(g)$name<-1:length(V(g)$name)
    links<-as.data.frame(get.edgelist(g)) #set name to number so calculate link correctly
    links$V1<-as.numeric(links$V1)
    links$V2<-as.numeric(links$V2)
    colnames(links)<-c("source","target")
    el <- data.frame(from=as.numeric(factor(links$source))-1, 
                     to=as.numeric(factor(links$target))-1, Value = rescale(df$the_df$cor, 0,10))
    nl <- cbind(idn=factor(df$the_df$target, levels=unique(df$the_df$target)), df$the_df)
    nl$group <- group[findInterval(nl$avg_rating, group$value),]$rating
    nl$idn <- paste0(nl$idn, ":", as.character(round(nl$avg_rating, 2)))
    nl$nodesize <- rescale(nl$avg_rating, to=c(1,40))
    forceNetwork(Links = el, Nodes = nl,Source = "from", Target = "to", NodeID = "idn",Group = "group", opacity=0.9,
                 colourScale = "d3.scale.category10()", charge=-320, linkDistance = 200, zoom=TRUE, width=800, height=1000, 
                 legend = TRUE, fontSize = 18, opacityNoHover = 0.6, clickAction='myclick(d)', Nodesize='nodesize')
  })
  
  output$recipes <- DT::renderDataTable({
    if(input$go == 0 | is.null(ingredients_clicked$ingredients)){
      return()
    }
    m <- sapply(ingredients_clicked$ingredients, function(x) stri_detect_regex(flat_ingredients, x))
    i <- apply(m, 1, all)
    the_recipes_df <- r[i,]
    the_recipes_df <- the_recipes_df[which(the_recipes_df$calories <= input$calories & 
                                    the_recipes_df$sodium <= input$sodium &
                                    the_recipes_df$fat <= input$fat),]  
    the_recipes_df$id <- 1:nrow(the_recipes_df)
    the_recipes_df <<- the_recipes_df
    df<-data.frame(Title=the_recipes_df$title, Rating = the_recipes_df$rating, Calories = the_recipes_df$calories,
                   Fat = the_recipes_df$fat, Sodium = the_recipes_df$sodium,
                   recipe = paste0("<button id='button_", the_recipes_df$id, "' type=\'button\' class=\'btn btn-default action-button\' onclick=\'Shiny.onInputChange(&quot;select_button&quot;, this.id)\'>View Recipe</button>"))
    df

  }, server = TRUE, rownames=FALSE, escape = FALSE, selection = 'single', options = list(order = list(list(2, 'desc')))
  )
  
  output$about <- renderUI(
    HTML("Blah")
  )

})
