###USATT rating calculation based on USATT rating explanation ####
###Author: Yunhu Wan #####
###Update: Jun-11-2019 #####


cat("***********This script can preview your tournament rating************\n")
cat("The first pass should use the pre-tournament rating.\n")
cat("The second pass should use the adjusted rating if they have.\n")
cat("Disclaimer: \n1. Not associated with USATT. Cannot guarantee to match with official USATT rating.\n")
cat("2. Play for honor. Play for winning. Play for fun. Don't play solely for rating increase.\n")
cat("3. Make sure everyone's rating current in order to provide correct calculation")
cat("**********************************************************************\n")


##Process sequence orders: Update on May 24 2019
# 1. find any player adjusted rating
# 2. Assign 0 player rating if it has 0 player
# 3. Recalculate any player's adjusted rating if it has 0 player
# 4. Rerun all players rating with new adjusted rating.


##Update Jun 11 2019
#Change 0 rating person all won adjusted rating.



cat("The pre-tournament rating: ")
conn <- file("stdin")
old <- readLines(conn,n=1)
old <- as.integer(old)

cat("Win: ")
won<- readLines(conn,n=1)
won <- as.integer(unlist(strsplit(won,",")))
#print(won)

cat("Lost: ")
lost<- readLines(conn,n=1)
lost <- as.integer(unlist(strsplit(lost,",")))
#print(lost)

close(conn)

#Test case #1
#old=0
#won=c(1758,1398,1336)
#lost=c()

#Note 1: The first pass using opponent pre-adjust rating.

#Test case #2
#old = 1673
#won = c(1451,1411,1347,1208,1897,1579,1525,1320,1260,1317,1675,1724,1710,1607,1734)
#lost = c(1846,1735,1934,1629)
result = vector(mode="numeric",length=length(won)+length(lost))

cat("Old rating is ",old,"\n")

#cat("Begin pass1\n")

#Do somthing about unrated player
if( old == 0 )
{
  if (length(won) > 0 & length(lost) > 0)
  {
    adjust = floor((max(won)+min(lost))/2)
    
  }
  
  else if(length(won) > 0 )
  {
    adjust = max(won)
  }
  
  #Not sure about this case
  else if(length(lost) > 0 )
  {
    adjust = 75
  }
  cat("Adjust rating for 0 player: ", adjust,"\n")
  old = adjust
}
  
if(old > 0 & length(won) > 0 )
{
 for(i in 1:length(won))
 {
   change = old-won[i]
   
   if(change >= 0 & change <= 12)
   { result[i] = 8}
   else if (change >=13 & change <= 37)
   { result[i] = 7}
   else if (change >=38 & change <= 62)
   { result[i] = 6}
   else if (change >=63 & change <= 87)
   { result[i] = 5}
   else if (change >=88 & change <= 112)
   { result[i] = 4}
   else if (change >=113 & change <= 137)
   { result[i] = 3}
   else if (change >=138 & change <= 187)
   { result[i] = 2}
   else if (change >=188 & change <= 237)
   { result[i] = 1}
   else if (change >=238)
   { result[i] = 0}
   
   else if(change >= -12 & change <= 0 )
   {result[i] = 8}
   else if (change >= -37 & change <= -13)
   {result[i] = 10}
   else if (change >= -62 & change <= -38)
   {result[i] = 13}
   else if (change >= -87 & change <= -63)
   {result[i] = 16}
   else if (change >= -112 & change <= -88)
   {result[i] = 20}
   else if (change >= -137 & change <= -113)
   {result[i] = 25}
   else if (change >= -162 & change <= -138)
   {result[i] = 30}
   else if (change >= -187 & change <= -163)
   {result[i] = 35}
   else if (change >= -212 & change <= -188)
   {result[i] = 40}
   else if (change >= -237 & change <= -213)
   {result[i] = 45}
   else if (change <= -238)
   {result[i] = 50}
   
   #cat(i,"\t",old,"\t",won[i],"\t",result[i],"\n")  
 }
}

if(old > 0 & length(lost) > 0 )
{
 for( i in 1:length(lost))
 {
  current = length(won)+i
  
  
  change = old - lost[i]
  
  if(change >= 0 & change <= 12)
  { result[current] = -8}
  else if (change >=13 & change <= 37)
  { result[current] = -10}
  else if (change >=38 & change <= 62)
  { result[current] = -13}
  else if (change >=63 & change <= 87)
  { result[current] = -16}
  else if (change >=88 & change <= 112)
  { result[current] = -20}
  else if (change >=113 & change <= 137)
  { result[current] = -25}
  else if (change >=138 & change <= 162)
  { result[current] = -30}
  else if (change >= 163 & change <= 187)
  { result[current] = -35 }
  else if (change >=188 & change <= 212)
  { result[current] = -40 }
  else if (change >= 213 & change <= 237)
  { result[current] = -45 }
  else if (change >=238)
  { result[current] = -50 }
  
  else if(change >= -12 & change <= 0 )
  {result[current] = -8}
  else if (change >= -37 & change <= -13)
  {result[current] = -7}
  else if (change >= -62 & change <= -38)
  {result[current] = -6}
  else if (change >= -87 & change <= -63)
  {result[current] = -5}
  else if (change >= -112 & change <= -88)
  {result[current] = -4}
  else if (change >= -137 & change <= -113)
  {result[current] = -3}
  else if (change >= -162 & change <= -138)
  {result[current] = -2}
  else if (change >= -187 & change <= -163)
  {result[current] = -2}
  else if (change >= -212 & change <= -188)
  {result[current] = -1}
  else if (change >= -237 & change <= -213)
  {result[current] = -1}
  else if (change <= -238)
  {result[current] = 0}
  }
}


if(length(won) > 0)
{
 for(i in 1:length(won))
 {
  cat(i,"\t",old,"\t",won[i],"\t",result[i],"\n")  
 }
}

if(length(lost) > 0)
{
  for( i in 1:length(lost))
  {
  current = length(won)+i
  cat(current,"\t",old,"\t",lost[i],"\t",result[current],"\n")  
  }
}



if(length(won) > 0 & length(lost) > 0  )
{
cat("Rating change is ",sum(result),"\n")
if(sum(result)<50)
{
  new = sum(result)+old
  cat("New rating is ",new,"\n")
}else if (sum(result)>=50 & sum(result)< 75)
{
  pass1 = sum(result)+old
  cat("Adjust scenario 1 is ",pass1,"\n")
  old=pass1
  
}else if (sum(result) >= 75)
{
  pass1 = sum(result) + old
  adjust = (pass1+(max(won)+min(lost))/2)/2
  #adjust = (old+(max(won)+min(lost))/2)/2
  adjust = floor(adjust)
  #cat("Adjust scenario 2 (new algorithm) is ",adjust,"\n")
  cat("Adjust scenario 2 is ",floor(adjust),"\n")
  old = adjust
}
}else{
  new=sum(result) + old
  cat("New rating is ",new,"\n")
}

if(length(won) > 0 & length(lost) == 0 )
{
  cat("Rating change is ",sum(result),"\n")
  if(sum(result)<50)
  {
    new = sum(result)+old
    cat("New rating is ",new,"\n")
  }else if (sum(result)>=50 & sum(result)< 75)
  {
    pass1 = sum(result)+old
    cat("Adjust scenario 1 is ",pass1,"\n")
    old=pass1
    
  }else if (sum(result) >= 75)
  {
    pass1 = sum(result) + old
    adjust = median(won)
    #adjust = (old+(max(won)+min(lost))/2)/2
    adjust = floor(adjust)
    #cat("Adjust scenario 2 (new algorithm) is ",adjust,"\n")
    cat("Adjust scenario 2 is ",floor(adjust),"\n")
    old = adjust
  }else{
    new=sum(result) + old
    cat("New rating is ",new,"\n")
  }
}


###Second pass only for rating change more and equal than 50

#Note 2: using opponent adjusted rating
if(sum(result)>=50 & length(won) > 0 ) #& length(lost) > 0 )  
{
  
  cat("Do you have any person rating needs to be adjusted? (Y or N) :")
  conn <- file("stdin")
  answer <- readLines(conn,n=1)
  
  if (answer == "Y")
  {
    cat("Win: ")
    won<- readLines(conn,n=1)
    won <- as.integer(unlist(strsplit(won,",")))
    #print(won)
    
    cat("Lost: ")
    lost<- readLines(conn,n=1)
    lost <- as.integer(unlist(strsplit(lost,",")))
    
  }
  
  close(conn)
 # won = c(1451,1411,1347,1208,1897,1579,1525,1320,1260,1317,1675,1724,1710,1607,1734)
  #lost = c(1846,1735,1934,1739)
 # cat("Begin pass2\n")
  #won=c(2339,2186)
  #lost=c(2596)
  
  result = vector(mode="numeric",length=length(won)+length(lost))
  
  for(i in 1:length(won))
  {
    change = old-won[i]
    
    if(change >= 0 & change <= 12)
    { result[i] = 8}
    else if (change >=13 & change <= 37)
    { result[i] = 7}
    else if (change >=38 & change <= 62)
    { result[i] = 6}
    else if (change >=63 & change <= 87)
    { result[i] = 5}
    else if (change >=88 & change <= 112)
    { result[i] = 4}
    else if (change >=113 & change <= 137)
    { result[i] = 3}
    else if (change >=138 & change <= 187)
    { result[i] = 2}
    else if (change >=188 & change <= 237)
    { result[i] = 1}
    else if (change >=238)
    { result[i] = 0}
    
    else if(change >= -12 & change <= 0 )
    {result[i] = 8}
    else if (change >= -37 & change <= -13)
    {result[i] = 10}
    else if (change >= -62 & change <= -38)
    {result[i] = 13}
    else if (change >= -87 & change <= -63)
    {result[i] = 16}
    else if (change >= -112 & change <= -88)
    {result[i] = 20}
    else if (change >= -137 & change <= -113)
    {result[i] = 25}
    else if (change >= -162 & change <= -138)
    {result[i] = 30}
    else if (change >= -187 & change <= -163)
    {result[i] = 35}
    else if (change >= -212 & change <= -188)
    {result[i] = 40}
    else if (change >= -237 & change <= -213)
    {result[i] = 45}
    else if (change <= -238)
    {result[i] = 50}
    
    #cat(i,"\t",old,"\t",won[i],"\t",result[i],"\n")  
  }
  
  if(length(lost > 0 ))
  {
  for( i in 1:length(lost))
  {
    current = length(won)+i
    
    
    change = old - lost[i]
    
    if(change >= 0 & change <= 12)
    { result[current] = -8}
    else if (change >=13 & change <= 37)
    { result[current] = -10}
    else if (change >=38 & change <= 62)
    { result[current] = -13}
    else if (change >=63 & change <= 87)
    { result[current] = -16}
    else if (change >=88 & change <= 112)
    { result[current] = -20}
    else if (change >=113 & change <= 137)
    { result[current] = -25}
    else if (change >=138 & change <= 162)
    { result[current] = -30}
    else if (change >= 163 & change <= 187)
    { result[current] = -35 }
    else if (change >=188 & change <= 212)
    { result[current] = -40 }
    else if (change >= 213 & change <= 237)
    { result[current] = -45 }
    else if (change >=238)
    { result[current] = -50 }
    
    else if(change >= -12 & change <= 0 )
    {result[current] = -8}
    else if (change >= -37 & change <= -13)
    {result[current] = -7}
    else if (change >= -62 & change <= -38)
    {result[current] = -6}
    else if (change >= -87 & change <= -63)
    {result[current] = -5}
    else if (change >= -112 & change <= -88)
    {result[current] = -4}
    else if (change >= -137 & change <= -113)
    {result[current] = -3}
    else if (change >= -162 & change <= -138)
    {result[current] = -2}
    else if (change >= -187 & change <= -163)
    {result[current] = -2}
    else if (change >= -212 & change <= -188)
    {result[current] = -1}
    else if (change >= -237 & change <= -213)
    {result[current] = -1}
    else if (change <= -238)
    {result[current] = 0}
  }
  }
  
  if(length(won) > 0)
  {
    for(i in 1:length(won))
    {
      cat(i,"\t",old,"\t",won[i],"\t",result[i],"\n")  
    }
  }
  
  if(length(lost) > 0)
  {
    for( i in 1:length(lost))
    {
      current = length(won)+i
      cat(current,"\t",old,"\t",lost[i],"\t",result[current],"\n")  
    }
  }
  
  new = sum(result)+old
  cat("New rating is ",new,"\n")
  
}

#View(ratings
#library(PlayerRatings)
#afl <- aflodds[,c(2,3,4,7)]
