# Agriculture_game
A single player game programmed in Shiny (R studio)


# Results from the ENDORSE game showed that social equity is affecting the willingness to cooperate, especially distributional equity. But how and how much is still not explored. Therefore my study is building on these discoveries from the ENDORSE game to further investigate how equity affects farmers' willingness to cooperate. We hypothesise that if the livelihood is at stake, it might affect the way to cooperate.

## How this will be done
A single player game, estimated time to play should be around 10 minutes (max 15). 

I have programmed in 2 different parts, one with R code and one with the Shiny app. This application will manipulate the players perception of equity. When the player starts the game he or she is assigned to end up in the top (out of 10 players) or in the bottom. Also how much variation in scores will be manipulated to see how much equity is important or at what level.

The point is the player is playing against digital neighbours where they can check how well they are doing, compared to their neighbours. The players land management strategy is evolving for each season and we want to see if, and how, the players might change their strategy depending on how well they are doing compared to neighbours.

## My game: 
The player will manage a field with crops and decide what pest control to use for each square when the pest arrives to eat the crops. The player will treat the crops 2 times for each season and the game will last for 3 seasons.

# Players' options and implications
Synthetic control 
Biological control
Recommended biological control
No control

# Implications are
Chemical control - is bad and comes with a cost 
Biological control - is better, but still does not help with the lack of diversity
Recommended biological control - makes sure that farmers uses different pest control to slow down the evolution of pest resistance to pest control. Sometimes the recommended control will be very effective and sometimes less
No control - good

# Prediction:
If the player is doing poorly this might affect the willingness to cooperate for next season, whereas when the player is winning it might stick with the same strategy as before
How much individual loss one is willing to sacrifice affects the strategy of the player (how spread out the variations in score are)



## What I need help with
1. Now all the coding are separate parts, I need to put the R code in to the shiny app and make it more interactive and more easy to play
2. For each season I need the table with the players score to be updated, but the player should still stay in the top/bottom and scores should be either scattered or not
3. I need a way for player to make choices for each square, maybe 2 times per season for 3 seasons and all the choices the player makes needs to be stored 
4. Need some kind of pest that come and attack the crops
