#load the data

nomvdata <- read.csv("dataimputed.csv")
head(nomvdata)
colnames(nomvdata)
ncol(nomvdata)

#removing the irrelevant columns

colremove <- c("Height","Weight","Age")
nomvdata <- nomvdata[, ! names(nomvdata) %in% colremove, drop = F]
ncol(nomvdata)
View(nomvdata)
colnames(nomvdata)

columns1 <- c(colnames(nomvdata))
music1 <- columns1[1:19]
#music
movies1 <- columns1[20:31]
#movies
hobbies1 <- columns1[32:63]
#hobbies
phobias1 <- columns1[64:73]
#phobias
lifestyle1 <- columns1[74:76]
#lifestyle
demographics1 <- columns1[141:147]
#demographics
spendinghabits1 <- columns1[134:140]
#spendinghabits
generalviewstraits1 <- columns1[77:133]

musicdf1 <- data.frame(nomvdata[music1])
moviesdf1 <- data.frame(nomvdata[movies1])
hobbiesdf1 <- data.frame(nomvdata[hobbies1])
phobiasdf1 <- data.frame(nomvdata[phobias1])
lifestyledf1 <- data.frame(nomvdata[lifestyle1])
demographicsdf1 <- data.frame(nomvdata[demographics1])
spendinghabitsdf1 <- data.frame(nomvdata[spendinghabits1])
generalviewstraitsdf1 <- data.frame(nomvdata[generalviewstraits1])

library(poLCA)
#library(e1071)
#m1 <- lca(generalviewstraitsdf1,3,matchdata = T,verbose = F)

# v <- c()
# gencol <- colnames(generalviewstraitsdf1)
# gencol
# for (i in 1:57){
#     v[i] <- gencol[i]
#   }
#l1<-toupper(l)
#typeof(l1)

#getting the data ready

f <- cbind(Daily.events,                 
              Prioritising.workload,        
              Writing.notes,                 
             Workaholism,                   
              Thinking.ahead,                
             Final.judgement,             
              Reliability,                   
              Keeping.promises,              
              Loss.of.interest,              
              Friends.versus.money,          
              Funniness,                     
              Fake,                          
              Criminal.damage,               
              Decision.making,               
              Elections,                     
             Self.criticism,                
             Judgment.calls,                
             Hypochondria,                 
             Empathy,                       
              Eating.to.survive,             
              Giving,              
              Compassion.to.animals,        
              Borrowed.stuff,                 
              Loneliness,               
              Cheating.in.school,            
              Health,                        
              Changing.the.past,             
              God,              
              Dreams,                        
              Charity,                       
              Number.of.friends,             
              Punctuality,              
               Lying,                         
              Waiting,                     
              New.environment,              
              Mood.swings,                   
              Appearence.and.gestures,       
             Socializing,                   
             Achievements,                 
             Responding.to.a.serious.letter,
              Children,                     
             Assertiveness,                 
              Getting.angry,                 
              Knowing.the.right.people,      
              Public.speaking,               
              Unpopularity,                  
              Life.struggles,                
              Happiness.in.life,             
              Energy.levels,                 
              Small...big.dogs,              
               Personality,                   
             Finding.lost.valuables,      
              Getting.up,                    
             Interests.or.hobbies,          
              Parents_.advice,               
              Questionnaires.or.polls,       
              Internet.usage) ~ 1

#summary(m1)
#View(election)

#fitting the models, first with no predictors to find inherent groups if any
m1 <- poLCA(f,generalviewstraitsdf1,nclass = 2,maxiter = 1000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

m2 <- poLCA(f,generalviewstraitsdf1,nclass = 3,maxiter = 1000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

m3 <- poLCA(f,generalviewstraitsdf1,nclass = 4,maxiter = 1000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

#----------------------------------------------------------------------------------------------------------------------------------------


traitsgenderdf <- data.frame(generalviewstraitsdf1,nomvdata$Gender)
ncol(traitsgenderdf)

f1 <- cbind(Daily.events,                 
            Prioritising.workload,        
            Writing.notes,                 
            Workaholism,                   
            Thinking.ahead,                
            Final.judgement,             
            Reliability,                   
            Keeping.promises,              
            Loss.of.interest,              
            Friends.versus.money,          
            Funniness,                     
            Fake,                          
            Criminal.damage,               
            Decision.making,               
            Elections,                     
            Self.criticism,                
            Judgment.calls,                
            Hypochondria,                 
            Empathy,                       
            Eating.to.survive,             
            Giving,              
            Compassion.to.animals,        
            Borrowed.stuff,                 
            Loneliness,               
            Cheating.in.school,            
            Health,                        
            Changing.the.past,             
            God,              
            Dreams,                        
            Charity,                       
            Number.of.friends,             
            Punctuality,              
            Lying,                         
            Waiting,                     
            New.environment,              
            Mood.swings,                   
            Appearence.and.gestures,       
            Socializing,                   
            Achievements,                 
            Responding.to.a.serious.letter,
            Children,                     
            Assertiveness,                 
            Getting.angry,                 
            Knowing.the.right.people,      
            Public.speaking,               
            Unpopularity,                  
            Life.struggles,                
            Happiness.in.life,             
            Energy.levels,                 
            Small...big.dogs,              
            Personality,                   
            Finding.lost.valuables,      
            Getting.up,                    
            Interests.or.hobbies,          
            Parents_.advice,               
            Questionnaires.or.polls,       
            Internet.usage) ~ nomvdata.Gender

m4 <- poLCA(f1,traitsgenderdf,nclass = 4,maxiter = 1000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

pidmat <- cbind(1,c(1,2))
#m4$coeff
exb <- exp(pidmat %*% m4$coeff)
a <- (cbind(1,exb)/(1+rowSums(exb)))

# Maximum entropy (if all cases equally dispersed)
log(prod(sapply(m4$probs,ncol)))

# Sample entropy ("plug-in" estimator, or MLE)

p.hat <- m4$predcell$observed/m4$N
p.hat
H.hat <- -sum(p.hat * log(p.hat))
H.hat
m4$N
m4$llik
m4$P.se
m4$probs.se
m4$attempts
m4$x
m4$y
m4$coeff.V
m4$resid.df
m4$probs.start
m4$probs
m4$predclass
#m4$bic
#-------------------------------------------------------------------------------------------------------------------------------------------

musicgenderdf <- data.frame(musicdf1,nomvdata$Gender)
noquote(colnames(musicgenderdf))

f2 <- cbind(Music,Slow.songs.or.fast.songs,Dance,Folk,Country,
            Classical.music,Musical,Pop,Rock,Metal.or.Hardrock,Punk,
            Hiphop..Rap,Reggae..Ska,Swing..Jazz,Rock.n.roll,Alternative,
            Latino,Techno..Trance,Opera) ~ nomvdata.Gender


m5 <- poLCA(f2,musicgenderdf,nclass = 4,maxiter = 1000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

#m5a <- poLCA(f2,musicgenderdf,nclass = 3,maxiter = 1000,graphs=F,tol=1e-10,
#             na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T) 
            
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

moviesgenderdf <- data.frame(moviesdf1,nomvdata$Gender)
noquote(colnames(moviesgenderdf))

f3 <- cbind(Movies,Horror,Thriller,Comedy,Romantic,Sci.fi,War,
            Fantasy.Fairy.tales,Animated,
            Documentary,Western,Action) ~ nomvdata.Gender

m6 <- poLCA(f3,moviesgenderdf,nclass = 4,maxiter = 1000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

hobbiesgenderdf <- data.frame(hobbiesdf1,nomvdata$Gender)
noquote(colnames(hobbiesgenderdf))

f4 <- cbind( History,Psychology,Politics,Mathematics,Physics,Internet,PC,
             Economy.Management,Biology,Chemistry,Reading,Geography,
             Foreign.languages,Medicine,Law,Cars,Art.exhibitions,
             Religion,Countryside..outdoors,Dancing,Musical.instruments,Writing,
             Passive.sport,Active.sport,Gardening,Celebrities,
             Shopping,Science.and.technology,Theatre,
             Fun.with.friends,Adrenaline.sports,Pets) ~ nomvdata.Gender

m7 <- poLCA(f4,hobbiesgenderdf,nclass = 4,maxiter = 1000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

spendhabgenderdf <- data.frame(spendinghabitsdf1,nomvdata$Gender)
noquote(colnames(spendhabgenderdf))

f5 <- cbind(Finances,Shopping.centres,Branded.clothing,Entertainment.spending,Spending.on.looks,         
            Spending.on.gadgets,Spending.on.healthy.eating) ~ nomvdata.Gender

m8 <- poLCA(f5,spendhabgenderdf,nclass = 4,maxiter = 2000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

phobiagenderdf <- data.frame(phobiasdf1,nomvdata$Gender)
noquote(colnames(phobiagenderdf))

f6 <- cbind(Flying,Storm,Darkness,Heights,Spiders,Snakes,
            Rats,Ageing,Dangerous.dogs,Fear.of.public.speaking) ~ nomvdata.Gender

m9 <- poLCA(f6,phobiagenderdf,nclass = 4,maxiter = 2000,graphs=F,tol=1e-10,
            na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lifestylgenderdf <- data.frame(lifestyledf1,nomvdata$Gender)
noquote(colnames(lifestylgenderdf))

f7 <- cbind(Smoking,Alcohol,Healthy.eating) ~ nomvdata.Gender

m10 <- poLCA(f7,lifestylgenderdf,nclass = 4,maxiter = 2000,graphs=F,tol=1e-10,
      na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

interestsdf <- data.frame(musicdf1 <- data.frame(nomvdata[music1]),
                          moviesdf1 <- data.frame(nomvdata[movies1]),
                          hobbiesdf1 <- data.frame(nomvdata[hobbies1]),
                          spendinghabitsdf1)
colnames(interestsdf)
ncol(interestsdf)
noquote(colnames(interestsdf))

f8 <- cbind(Music,Slow.songs.or.fast.songs,Dance,                    
             Folk,Country,Classical.music,           
             Musical,Pop,Rock,                      
             Metal.or.Hardrock,Punk,Hiphop..Rap,               
             Reggae..Ska,Swing..Jazz,Rock.n.roll,              
             Alternative,Latino,Techno..Trance,           
             Opera,Movies,Horror,                    
             Thriller,Comedy,Romantic,                  
             Sci.fi,War,Fantasy.Fairy.tales,      
             Animated,Documentary,Western,                
             Action,History,Psychology,                
             Politics,Mathematics,Physics,                   
             Internet,PC,Economy.Management,        
             Biology,Chemistry,Reading,                   
             Geography,Foreign.languages,Medicine,                  
             Law,Cars,Art.exhibitions,          
             Religion,Countryside..outdoors,Dancing,                   
             Musical.instruments,Writing,Passive.sport,             
             Active.sport,Gardening,Celebrities,               
             Shopping,Science.and.technology,Theatre,                   
             Fun.with.friends,Adrenaline.sports,Pets,                      
             Finances,Shopping.centres,Branded.clothing,          
             Entertainment.spending,Spending.on.looks,Spending.on.gadgets,       
             Spending.on.healthy.eating) ~ 1

m11 <- poLCA(f8,interestsdf,nclass = 3,maxiter = 2000,graphs=F,tol=1e-10,
             na.rm=F,probs.start=NULL,nrep=1,verbose=T,calc.se=T)

#---------------------------------------------------------------------------------------------------------------------------------------------------------



















