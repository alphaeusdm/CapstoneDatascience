library(shiny)
library(tm)
library(slam)
library(RWeka)
library(e1071)
library(dplyr)
library(magrittr)
library(stringr)

bigram <- readRDS("C:/Users/alphaeus/Desktop/Data Science Coursera/CapstoneDatascience/bigram.RData")
trigram <- readRDS("C:/Users/alphaeus/Desktop/Data Science Coursera/CapstoneDatascience/trigram.RData")
quadgram <- readRDS("C:/Users/alphaeus/Desktop/Data Science Coursera/CapstoneDatascience/quadgram.RData")

# Define my function to take raw text input and return the predicted next word
Predict <- function(raw) 
{
    sentence <- tolower(raw) %>%
        removePunctuation %>%
        removeNumbers %>%
        stripWhitespace %>%
        str_trim %>%
        strsplit(split=" ") %>%
        unlist
    
    length(sentence)
    
    if(length(sentence)>=3)
    {
        gram <- paste(tail(sentence, 3), collapse=" ")
        pred <- quadgram[which(paste(quadgram$V1,quadgram$V2,quadgram$V3)==gram),]$V4[1]
        
        if(is.na(pred))
        {
            gram <- paste(tail(sentence, 2), collapse=" ")
            pred <- trigram[ which(paste(trigram$V1,trigram$V2)==gram),]$V3[1]
            
            if(is.na(pred))
            {
                gram <- paste(tail(sentence, 1), collapse=" ")
                pred <- bigram[ which(paste(bigram$V1)==gram),]$V2[1]
                
                if(is.na(pred))
                {
                    return("sorry")
                }
                else
                {
                    return(pred)
                }
            }
            else
            {
                return(pred)
            }
        }
        else
        {
            return(pred)
        }
    }
    
    if(length(sentence)==2)
    {
        gram <- paste(tail(sentence, 2), collapse=" ")
        pred <- trigram[ which(paste(trigram$V1,trigram$V2)==gram),]$V3[1]
        
        if(is.na(pred))
        {
            gram <- paste(tail(sentence, 1), collapse=" ")
            pred <- bigram[ which(paste(bigram$V1)==gram),]$V2[1]
            
            if(is.na(pred))
            { 
                return("sorry")
            }
            else
            {
                return(pred)
            }
        }
        else
        {
            return(pred)
        }
    }
    
    if(length(sentence)==1)
    {
        gram <- paste(tail(sentence, 1), collapse=" ")
        pred <- bigram[ which(paste(bigram$V1)==gram),]$V2[1]
        
        if(is.na(pred))
        { 
            return("sorry")
        }
        else
        {
            return(pred)
        }
    }
    
    else
    {
        return("empty")
    }
    
}


# Define the Shiny Server
shinyServer(function(input, output)
{
    
    observe({
        p <- ""
        output$textPred <- renderText(
            { 
                p <-  Predict(input$textIn)
                paste("", p)
            })
        output$textOut <- renderText(
            { 
                paste(input$textIn, Predict(input$textIn))
            })
        
    })
    
})