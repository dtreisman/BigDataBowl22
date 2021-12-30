# Big Data Bowl 2022

My submission for the [2022 NFL Big Data Bowl](https://www.kaggle.com/c/nfl-big-data-bowl-2022).  
Read the full submission [here](https://www.kaggle.com/danitreisman/quantifying-punt-return-decision-making)

## Background
Special teams is a component of American football that is not explored nearly as extensively as offense and defense. While the frequency of the decision to punt is declining slightly, punts remain an integral part of the game. They offer the returning team the opportunity gain additional yardage and better field position and the kicking team an opportunity to back their opponents deep into their own territory. The job of the punt returner is to try and gain as many yards as possible with the help of their teammates' blocking and is generally what spectators pay attention to. However, the role is unique relative to others since it is the only position where a significant part of their role on the field is to make a decision mid-play about whether to end the play on the spot or attempt to run with the ball. There are three main types of decisions on punt returns:

* Return the punt
* Call a fair catch
* Let it drop
  1. Downed
  2. Touchback

A majority of punt plays are not actually returned, which is why it is important to evaluate not just returns, but all other punt play results. In this analysis, I leverage tracking data to evaluate punt returners while adjusting for this decision-making component and quantify the decisions themselves.

## Summary 
In this analysis I created three metrics:

* Return Yards Over Expected (RYOE)
  * This measures returner skill on returning punts.
* Adjusted Return Yards Over Expected (RYOE<sub>adj</sub>) 
  * This is a more comprehensive measure of punt returner value that attributes value to other types of punt play decisions.
* Decision Weighted Return Yards Over Expected (dRYOE)
  * This serves as a proxy to quantify the value of punt play decisions made by punt returners.

While athletic ability is generally the main focus of evaluation for most aspects of football, punt plays offer a strategy component that is at least equally as important. This is further augmented by the lack of stability for all the metrics I attempted to create in this work. None of the three metrics presented above are stable from year to year at the player or team level. Other metrics I attempted to create that were not presented were also not stable. The challenge with punt returns is that the punt return personnel changes often from year to year and there are not many players with a qualifying number of punt plays in all three years. Therefore, evaluating punt returner skill is descriptive, not predictive. What is predictive, is the value of punt play decisions in general. Returns offer a chance at high RYOE but on average they under-perform and are not that much more valuable than fair catches. Letting the ball bounce offers the chance at a touchback, but doing so in front of the 10 yard line almost always results in a large loss of yardage. While we cannot necessarily determine which players should be chosen as a punt returner based on these metrics, it is clear that teams that avoid downed punts will be putting themselves in better positions to win.
