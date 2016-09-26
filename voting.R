library(rootSolve)
Population<-25

Cost=1
Loss=0
Win<-10
Tie<-(Loss+Win)/2
x<-seq(from=0,to=Population-1)
y<-VotersOptimum(Population, Cost, Win, Loss, Tie)
y<-sapply(x,shapingVoters)
x<-seq(from=0,to=Population-1)
difvoters<-x-y
which.min(difvoters[2:length(difvoters)])

# plot(x,y)
qplot(x, y)+geom_line() +  geom_segment(aes(x = 0, y = 0, xend = Population, yend = 1)) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1),arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = 0, y = 0, xend = Population, yend = 0),arrow = arrow(length = unit(0.5, "cm"))) 
  
lines(x,x)
VotersOptimum(125,1,10,0,5)
Population<-25
VotersOptimum(Population, Cost, Win, Loss, Tie)
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

#what if we go to vote
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
prob2vote<-roots[which.max(sapply(roots,e_v))]

return(prob2vote)

}

PVote=0.6
# InnerPop=10
# x
InnerPop<-25
Loss<-0
Cost<-1
Tie<-5
Win<-10
xxx<-sapply(x,pope_v)
x
yyy<-sapply(x,pope_nv)
par(col="black")
plot(x,xxx,type='l')
par(col="red")
lines(x,yyy)
test_data<-as.data.frame(cbind(x,xxx,yyy))
ggplot(test_data, aes(x)) + 
  geom_line(aes(y = xxx, colour = "Vote")) + 
  geom_line(aes(y = yyy, colour = "Abstain"))+
  ggtitle('asdf')+
  xlab('Number of other party voters')+
  ylab('Profit')+  scale_colour_discrete(name  ="Profits",
                                      
                                         labels=c("Abstain", "Vote"))

df<-as.data.frame(rbind(cbind(rep('Vote',InnerPop),x,xxx),cbind(rep('Not Vote',InnerPop),x,yyy)))

df1<-df[df$V1=='Vote',]
ggplot() + geom_area(aes(y = xxx, x = x, fill = V1), data = df1)
# y axis as density value
p + geom_area(aes(y = ..density..), stat = "bin")
hist(xxx)
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

set.seed(1234)
df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5),
                 rnorm(200, mean=65, sd=5)))
)
head(df)
p <- ggplot(df, aes(x=weight))
# Basic area plot
p + geom_area(stat = "bin")
# y axis as density value
p + geom_area(aes(y = ..density..), stat = "bin")
# Add mean line
p + geom_area(stat = "bin", fill = "lightblue")+
  geom_vline(aes(xintercept=mean(weight)),
             color="blue", linetype="dashed", size=1)
library(plyr)
mu <- ddply(df, "sex", summarise, grp.mean=mean(weight))
head(mu)
ggplot(df, aes(x=weight, fill=sex)) +
  geom_area(stat ="bin")
# Use semi-transparent fill
p<-ggplot(df, aes(x=weight, fill=sex)) +
  geom_area(stat ="bin", alpha=0.6) +
  theme_classic()
p
# Add mean lines
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")

data("diamonds")
p <- ggplot(diamonds, aes(x = price, fill = cut))
head(diamonds)
p + geom_bar(stat = "bin")
with(df,
      plot(x,xxx))
df$xxx
lines(x,yyy)
plot(x,xxx)
dat <- with(density(df$xxx), data.frame(x, y))
ggplot(data = dat, mapping = aes(x = x, y = y)) +
  geom_line()
  geom_area(mapping = aes(x = ifelse(x>65 & x< 70 , x, 0)), fill = "red") +
  xlim(30, 80)
