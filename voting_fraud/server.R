#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(rootSolve)
library(ggplot2)


pope_v=function(Voters){
  losing=pbinom(Voters-2, size=InnerPop, prob=PVote) 
  tieing=dbinom(Voters-1, size=InnerPop, prob=PVote) 
  winning=1-pbinom(Voters, size=InnerPop, prob=PVote) 
  return( losing*Loss+tieing*Tie+winning*Win-Cost)
}


pope_nv=function(Voters){
  losing=pbinom(Voters-1, size=InnerPop, prob=PVote) 
  tieing=dbinom(Voters, size=InnerPop, prob=PVote) 
  winning=1-pbinom(Voters+1, size=InnerPop, prob=PVote) 
  return (losing*Loss+tieing*Tie+winning*Win)
}

VotersOptimum<-function(InnerPop, Cost, Win, Loss, Tie){
  InnerPop<<-InnerPop
  Tie<<-Tie
  Loss<<-Loss
  Win<<-Win
  Cost<<-Cost
  x<-seq(0,InnerPop-1)
  return(sapply(x,shapingVoters))
}


shapingVoters<-function(Voters){
  
  #we need a function that will return p under which E_V==E_NV if we provide it with
  #voters number, population size and cost
  
  
  
  e_v=function(PVote){
    losing=pbinom(Voters-2, size=InnerPop, prob=PVote) 
    tieing=dbinom(Voters-1, size=InnerPop, prob=PVote) 
    winning=1-pbinom(Voters, size=InnerPop, prob=PVote) 
    return( losing*Loss+tieing*Tie+winning*Win-Cost)
  }
  
  e_nv=function(PVote){
    losing=pbinom(Voters-1, size=InnerPop, prob=PVote) 
    tieing=dbinom(Voters, size=InnerPop, prob=PVote) 
    winning=1-pbinom(Voters+1, size=InnerPop, prob=PVote) 
    return (losing*Loss+tieing*Tie+winning*Win)
  }
  
  voting_decision<-function(PVote){
    return(e_v( PVote)-e_nv(PVote))
  }
  
  
  roots<-uniroot.all(voting_decision, c(0, 1))
  if (length(roots)<1) {
    #print(paste('no roots for voters=',(Voters)))
    opt<-optimize(voting_decision,c(0,1),maximum = T)
    maxvotingprofit<-opt$objective
    maxprob<-opt$maximum
    if (maxvotingprofit>0){
      return(maxprob)
      
    } else {
      return (0)
    }
  } 
  return(roots[which.max(sapply(roots,e_v))])
  
}
Population<-22
Cost<-1
Loss<-0
Win<-10
Tie<-(Loss+Win)/2


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {
  observe({
    win <- input$Win
    updateSliderInput(session, "Loss", max=win-1)
    updateSliderInput(session, "Cost", max=win)
    updateSliderInput(session, "Tie", max=win-1,value = (win+input$Loss)/2)
    
  })
  output$distPlot <- renderPlot({
    Population=input$Pop
    Cost=input$Cost
    Loss=input$Loss
    Tie=input$Tie
    
    x<-seq(from=0,to=input$Pop-1)
    
    
    y<-VotersOptimum(Population, Cost, Win, Loss, Tie)
    # y<-sapply(x,myfun)
    qplot(x, y)+geom_line() +  geom_segment(aes(x = 0, y = 0, xend = Population, yend = 1)) +
      geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1),arrow = arrow(length = unit(0.5, "cm"))) +
      geom_segment(aes(x = 0, y = 0, xend = Population, yend = 0),arrow = arrow(length = unit(0.5, "cm")))+
      ggtitle('Optimum mixed strategy (P. to vote)')+
      xlab('Number of other party voters')+
      ylab('Probability to vote')
    
    # hist(x, breaks = bins, col = 'orange', border = 'white'),
    
  }
  )
  
  output$distPlot2 <- renderPlot({
    Population<<-input$Pop
    Cost<<-input$Cost
    Loss<<-input$Loss
    Tie<<-input$Tie
    PVote<<-input$Prob
    x<-seq(from=0,to=input$Pop-1)
    xxx<-sapply(x,pope_v)
    
    yyy<-sapply(x,pope_nv)
    
    
    test_data<-as.data.frame(cbind(x,xxx,yyy))
    ggplot(test_data, aes(x)) + 
      geom_line(aes(y = xxx, colour = "Vote")) + 
      geom_line(aes(y = yyy, colour = "Abstain"))+
      ggtitle('Comparing profits of voting and abstain')+
      xlab('Number of other party voters')+
      ylab('Profit')+  scale_colour_discrete(name  ="Profits",
                                             labels=c("Abstain", "Vote"))
    
    
    
  }
  )
  
  
  
})