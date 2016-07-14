globals [color-mode pop] ;; 0 = default, 1 = source, 2 = times heard, 3 = popularity

turtles-own [popularity trendy? trend-setter? adoption-liklihood trend-category trend-source times-heard]
;; popularity is a number value representing the degree of each turtle
;; trendy? is a boolean that is true if the person follows the trend
;; trend-setter? is a boolean that is true if the person is seeded the trend (i.e. the overall trend-starter)
;; adoption-liklihood is an integer representing the type of things the person is interested in
;; trend-category is an integer that represents the inherent type of thing a trend is
;; (corresponds with same values as adoption-liklihood) if the person is carrying a trend.
;; trend-source tells whether the turtle followed a trend from a friend, from the media, or both.
;; times-heard counts how many times a turtle has heard a meme

patches-own [category]
;; category is an integer that represents the inherent type of thing a trend is
;; (corresponds with same values as trend-category of a person)

;; Create people and links.
to setup
  ca
  set color-mode 0 ;; default
  set-default-shape turtles "person"
  make-node nobody ;; first node, unattached
  make-node turtle 0 ;; second node, attached to first node
  ask patches [
    set category -1 ;; -1 corresponds with "no trend", i.e. this patch is not a TV
  ]
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;;Network Procedures;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Network and layout procedures incorporated from Preferential Attachment example model
to create-network
  make-node find-partner ;; find partner and use it as attachment
  tick
  if layout? [layout]
  if count turtles = population [ ;; We want the network to have POPULATION turtles, as set by the slider
    reset-ticks ;; reset the ticks at 0 so we can observe them for the meme-spreading section
    stop
  ]
end

;; used for creating a new node
to make-node [old-node]
  crt 1
  [

    set adoption-liklihood random-poisson 3 ;; an "interest type" category corresponding to one of 0-9
    ifelse adoption-liklihood >= 7
    [set color red] ;;those propogating anti-idea?
    [set color (105 - 10 * adoption-liklihood) ]

    set trend-category -1 ;; -1 corresponds with "no trend"
    set times-heard 0
    if old-node != nobody
      [ create-link-with old-node
        ;; position new node near its partner
        move-to old-node
        fd 8
      ]
  ]
  end

;; Main preferential attachment mechanism. The more connections a node already has, the more likely
;; it is to gain another connection.
to-report find-partner
  report [one-of both-ends] of one-of links
end

;;;;;;;;;;;;;;;;;;;;;;;
;;;Layout Procedures;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; resize nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [set size sqrt count link-neighbors ]
  ]
  [
    ask turtles [set size 1]
  ]
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end

;;;;;;;;;;;;;;;;;;;;;
;;;Meme Procedures;;;
;;;;;;;;;;;;;;;;;;;;;

;; seed a trend to one random person
to seed-trend
  ask turtles [
    set popularity count my-links
  ]

  ask one-of turtles with [(popularity > mean [popularity] of turtles with [adoption-liklihood > 5] ) and (adoption-liklihood > 5) ] [
    set color red
    set trendy? true
    set trend-setter? true
    set trend-category adoption-liklihood ;; trend is given a "type" corresponding to the turtle's interest
    set times-heard 1
    set size 1.5 ;; distinguish the trend-setter
  ]
end

;; run the model
to go


  ask turtles with [trendy? = true][ ;; ask the trendy turtles to spread the trend
    spread-trend
  ]

  ;; if media is turned on, every  ticks a TV will flash in a patch with a turtle on it.
  if media?[
    ;; a TV will appear once every MEDIA-FREQUENCY ticks
    if ticks mod (media-frequency / 2) = 0 [
      ask one-of patches with [count turtles-here > 0] [
        media-trend
      ]
    ]
    ;; reset TV patches to black
    if ticks mod media-frequency = 0 [
      ask patches with [pcolor = white][
        set category -1
        set pcolor black
      ]
    ]
  ]
  ;; fit to the chosen color-mode
  recolor
  ;; if all of the turtles now follow the trend, stop. The model is over.
  if all? turtles [trendy? = true] [stop]
  tick
end

;; spreading the trend
to spread-trend
  ;; turtles try to spread the trend to one of their linked neighbors
  let target nobody
  set target one-of link-neighbors
  if target != nobody [
    ask target [
      ;; diff represents the difference between the turtle's interest category and the category of the trend.
      ;; the smaller the difference, the higher the probability that the trend will be passed to that turtle.
      ;; (i.e. if difference = 0, random number is chosen between 0 and 9. if difference = 9, random number is chosen between 0 and 99
      ;;let diff (adoption-liklihood - [trend-category] of myself)
      ;;if 0 = random (10 * (1 + (abs diff))) [
              if adoption-liklihood > 7 or (adoption-liklihood > 3.5 and adoption-liklihood > (7 - times-heard * 0.25) ) [
          set color red
          set trendy? true
          ;; if the turtle has already adopted the trend from media,
          ;; leave it be. otherwise, set its source to "friend"
          if trend-source != "media" [
            set trend-source "friend"
          ]
          set trend-category [trend-category] of myself
      ]
      set times-heard times-heard + 1
    ]
    ;; if a trend spreads between 2 turtles, turn the link between them red
    ;; (or blue or green-83 depending on the color-mode)
    ask links [
      if all? both-ends [trendy? = true]
        [ ifelse color-mode = 0 or color-mode = 1
          [set color red]
          [ifelse color-mode = 2
            [set color blue]
            [set color 83]
          ]
        ]
    ]
  ]
end

;; when media? is true, patches run this procedure, representing TV-watching.
;; assume that a turtle will accept a trend regardless of category with media exposure
to media-trend
  set pcolor white ;; the turtle watches TV
  ;; the TV broadcasts the current trend
  set category [trend-category] of one-of turtles with [trend-setter? = true]
  ;; the media will try to influence one of the turtles on the TV patch
  let watcher one-of turtles-here
  if watcher != nobody [
    ask watcher[
      if adoption-liklihood > 7 or (adoption-liklihood > 3.5 and adoption-liklihood > (7 - times-heard * 0.1) ) [
      ;; in the 3%
        set color red
        set trendy? true

        ;; if the turtle has already adopted the trend from a friend,
        ;; leave it be. otherwise, set its source to "media"
        if trend-source != "friend" [
          set trend-source "media"
        ]
        ;; they now follow the trend, therefore housing the same TREND-CATEGORY as every other TRENDY? turtle
        set trend-category [category] of myself
      ]
      set times-heard times-heard + 1

    ]
  ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Recoloring Procedures;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; procedure to recolor to the default scheme -- color-mode = 0
;; red = trendy, blue = not trendy
to recolor-default
  ;ask turtles [
  ;  ifelse trendy? = true
  ;    [set color red]
  ;    [set color blue]
  ;]
  ask patches with [category = -1] [set pcolor black]
  ask links with [color = 83 or color = blue] [set color red]
end

;; procedure to recolor to show source (friend vs media) -- color-mode = 1
;; "friend" = red, "media" = yellow, "not trendy" = blue
to recolor-by-source
  ask patches with [category = -1] [set pcolor black]
  ;ask turtles with [trend-source = "media"] [set color yellow]
  ;ask turtles with [trend-source = "friend"] [set color red]
  ;ask turtles with [trend-source = 0] [set color blue]
  ask links with [color = 83 or color = blue] [set color red]
end

;; procedure to recolor to display the number of times heard -- color-mode = 2
;; lighter = more times heard, darker = fewer times heard
to recolor-by-times-heard
  ask patches with [category = -1] [set pcolor 3]
  ;ask turtles [set color scale-color green times-heard 0 world-width * 2]
  ask links with [color = red or color = 83] [set color blue]
end

;; procedure to recolor to show popularity levels -- color-mode = 3
;; lighter = higher popularity (i.e. degree), darker = lower popularity
to recolor-by-popularity
  ask patches with [category = -1] [set pcolor 3]
 ; ask turtles [set color scale-color violet popularity 0 world-width * 2]
  ask links with [color = red or color = blue] [set color 83]
end

;; procedure to recolor while the "go" function is running
to recolor
  ifelse color-mode = 0
    [recolor-default]
    [ ifelse color-mode = 1
      [recolor-by-source]
      [ifelse color-mode = 2
        [recolor-by-times-heard]
        [recolor-by-popularity]
      ]
    ]
end
@#$#@#$#@
GRAPHICS-WINDOW
375
10
958
614
25
25
11.24
1
10
1
1
1
0
1
1
1
-25
25
-25
25
1
1
1
ticks
30.0

SLIDER
31
66
336
99
population
population
0
1000
300
1
1
NIL
HORIZONTAL

BUTTON
38
108
93
151
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
110
299
197
342
spread trend
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
205
298
353
343
Popularity of Trend-Setter
[popularity] of one-of turtles with [trend-setter? = true]
17
1
11

MONITOR
1010
40
1088
85
% Trendy
100 * (count turtles with [trendy? = true]) / (count turtles)
3
1
11

PLOT
973
358
1317
572
Adoption Liklihood (0-9)
Interest Category
Trend-Followers
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Turtle Interests" 1.0 1 -2674135 true "" "histogram [adoption-liklihood] of turtles with [trendy? = true]"
"Trend Category" 1.0 1 -16777216 true "" "histogram [trend-category] of turtles with [trend-setter? = true]"

SWITCH
29
256
146
289
media?
media?
1
1
-1000

SLIDER
153
256
352
289
media-frequency
media-frequency
2
20
6
1
1
ticks
HORIZONTAL

MONITOR
1092
40
1177
85
% From Friend
100 * count turtles with [trend-source = \"friend\"] / count turtles
3
1
11

MONITOR
1181
40
1264
85
% From Media
100 * count turtles with [trend-source = \"media\"] / count turtles
3
1
11

MONITOR
973
304
1100
349
Trend Category (0-9)
[trend-category] of one-of turtles with [trend-setter? = true]
17
1
11

PLOT
975
91
1314
288
Number of Trendy People
Ticks
Trend-Followers
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"All" 1.0 0 -16777216 true "" "plot count turtles with [trendy? = true]"
"From Friend" 1.0 0 -2674135 true "" "plot count turtles with [trend-source = \"friend\"]"
"From Media" 1.0 0 -1184463 true "" "plot count turtles with [trend-source = \"media\"]"

BUTTON
13
401
184
434
recolor-default
set color-mode 0\nrecolor-default
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
13
444
185
477
recolor-by-times-heard
set color-mode 2\nrecolor-by-times-heard
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
191
444
361
477
recolor-by-popularity
set color-mode 3\nrecolor-by-popularity
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
191
401
361
434
recolor-by-trend-source
set color-mode 1\nrecolor-by-source
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1106
304
1317
349
Mode Interest of Trendy Turtles (0-9)
modes [adoption-liklihood] of turtles with [trendy? = true]
17
1
11

BUTTON
26
299
103
342
seed-trend
if not any? turtles with [trend-setter? = true] [seed-trend]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
98
107
224
151
NIL
create-network
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
231
114
334
147
layout?
layout?
0
1
-1000

BUTTON
64
160
166
199
redo layout
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
172
160
285
199
NIL
resize-nodes
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
67
24
296
61
     Create the Social Network\n(Preferential Attachment Method)
14
0.0
1

TEXTBOX
116
228
241
246
Spread the Meme
14
0.0
1

TEXTBOX
102
373
252
391
Choose a Color Mode
14
0.0
1

@#$#@#$#@
## WHAT IS IT?

The purpose of this model is to examine how a meme spreads across a social network, and to explore the effects of different influences on this spread.

Influence factors include:
<li>Friends
<li>Media
<li>Inherent "category" of meme and "interest category" of a person
<li>Popularity of the "trend-setter", the person who starts the meme
</ul>

In this model, the "turtle" agents are people, the "link" agents represent relationships between the people, and the "patches" act as TV/media influences.

## HOW IT WORKS

First, a social network must be created. To do this, we use the "Preferential Attachment" method. In this method, we start with two nodes connected by an edge. Then, at each step, a node is added. The new node chooses to connect to an existing node randomly, but with a bias based on the number of connections, or "degree", the existing node already has. So the higher the degree of an existing node, the more likely a new node will connect to it. A new node "prefers" to connect to an existing node with a higher number of connections. <i>(See the "Preferential Attachment" Sample Model.)</i>

When the network is generated, each "node", or <i>person</i> in this model, is given an "INTEREST-CATEGORY", an integer ranging between 0 and 9, that represents the sort of subject matter that person is interested in. The person is also initialized as blue to show that they are not currently following the meme (i.e. they are not TRENDY). A person's "POPULARITY" is the exact value of their degree, or how many links they have to other people in the social network.

Next, it's time to start a meme. First, a TREND-SETTER must be seeded. This person will turn red, indicating that they follow the meme (i.e. they are TRENDY). A meme will have a "TREND-CATEGORY", an integer ranging between 0 and 9 that directly corresponds with the categories given by the "INTEREST-CATEGORY" values of people. The current meme's "TREND-CATEGORY" will be the exact value of the TREND-SETTER's "INTEREST-CATEGORY".

Finally, we must spread this meme across the social network. At each clock tick, if a person is following the meme (i.e. they are TRENDY), then they will try to spread it to one of their linked neighbors. The success of the spread depends on the INTEREST-CATEGORY of the target neighbor. The closer the TREND-CATEGORY of the meme is to the INTEREST-CATEGORY of the neighbor, the more likely that neighbor is to follow the meme (and become TRENDY). So if a meme is about a subject matter that a person is interested in, they will be more open to accepting and spreading it.

The media can also help spread the meme across the social network if the MEDIA? switch is turned on (see below in the "HOW TO USE IT" section). In this case, a random patch with people on it will flash white at a given frequency of ticks, emulating the act of people watching a TV. This patch will select one of the people on it as its target, spreading the meme to them if they are not already TRENDY. If the target becomes TRENDY, then it will follow the typical turtle behavior outlined in the previous paragraph, trying to spread the meme to its linked neighbors.

## HOW TO USE IT

<b>I. Setting Up the Network</b>

Use the POPULATION slider to select the number of people you want to exist in the social network.

The SETUP button provides a starting point for the network (two people connected by a link).

Click the CREATE-NETWORK button to allow the preferential attachment network to fully form. It will stop when the POPULATION number of people is reached, resetting ticks to 0 and releasing the button.

The LAYOUT? switch controls whether or not the layout procedure is run. This procedure intends to make the network structure easier to see by moving the nodes around. You can also use the REDO-LAYOUT button to fix the layout after the network is created.

The RESIZE-NODES button will make the people take on a physical size that represents their degree distribution. The larger the person, the higher the degree. Press the button again to return the nodes to equal size.

<b>II. Spreading the Meme</b>

The MEDIA? switch controls whether or not the meme-spreading procedure will use media as a factor. If the switch is turned on, use the MEDIA-FREQUENCY slider to decide how often a patch becomes a TV. This will occur every MEDIA-FREQUENCY ticks.

Use the SEED-TREND button to randomly create a TREND-SETTER, the person who creates the meme.

Press the SPREAD TREND button to spread the trend across the network. You can stop and start the process by pressing the button, or it will automatically end once all people follow the meme.

The POPULARITY OF TREND-SETTER monitor displays the POPULARITY, or degree, of the person who started the meme in the SEED-TREND command.

<b>III. Recoloring the View</b>

You can recolor the view in four different ways:

DEFAULT: Red = following the meme; Blue = not following the meme.

TREND-SOURCE: Red = followed the meme from a friend; Yellow = followed the meme from the media; Blue = not following the meme.

TIMES-HEARD: Lighter = more times heard; Darker = fewer times heard.

POPULARITY: Lighter = higher popularity; Darker = lower popularity.


## THINGS TO NOTICE

While the model runs, keep an eye on the monitors and plots in the Interface tab. The first plot and the monitors above it keep track of the percentage of people who are TRENDY and follow the meme over time. They also show the percentage of people who started following the meme because a friend spread it to them, and the percentage who started following it because they learned it from the media.
<li> How does varying the MEDIA-FREQUENCY affect this plot?
<li> Which influence factor seems to be the most prominent?
</ul>

The second plot and the monitors above it display the TREND-CATEGORY of the current meme, as well as the distribution of INTEREST-CATEGORY values of the TRENDY people in the form of a histogram. The histogram plots the number of TRENDY people with each INTEREST-CATEGORY in red, as well as a black marker for the current TREND-CATEGORY for comparison.
<li> Notice that when MEDIA? is turned on, there is a larger variance of interests from the TREND-CATEGORY earlier in the run.
<li> Generally, in earlier ticks, a larger number of TRENDY turtles tend to have INTEREST-CATEGORY values nearest to the TREND-CATEGORY.
<li> If you let the model run to completion, all people will be TRENDY; therefore, this histogram will become the distribution of INTEREST-CATEGORY values of all people.
</ul>

## THINGS TO TRY

Try creating networks with different POPULATION numbers of people (with the LAYOUT? switch turned on). Does this affect the network shape, or does the degree distribution appear to be the same across all population sizes?

Try varying the MEDIA-FREQUENCY slider to see how the "Number of Trendy People" plot is affected.

Explore the different color modes. See if you can find any similarities between the TIMES-HEARD and the TRENDY status of a person. Does the TREND-SOURCE view reveal anything interesting about the distribution of TRENDY turtles? What about POPULARITY?

## EXTENDING THE MODEL

As the model currently stands, if a person watches TV, they accept the broadcasted meme. The model could be made more accurate by finding some sort of probability of being influenced by the media instead of immediately accepting the meme. There could be a MEDIA-INFLUENCE slider that the user could vary to decide the probability of a person accepting a meme from the media.

It would be quite interesting to see how a meme <i>changes</i> over time. Could a meme mutate as it gets farther and farther away from the source? Could it start shifting to fit other INTEREST-CATEGORY values, and therefore appeal to different people over time?

What would happen if we introduced competing memes into the network? Would one defeat the other, or would they coexist in balanced harmony?

## NETLOGO FEATURES

People are turtle agents and the relationships between people are link agents. The model uses the ONE-OF primitive to choose a random link, as well as the BOTH-ENDS primitive to select the two people attached to that link. It also uses BOTH-ENDS to color the link between the people a certain color if both people sharing the link are TRENDY, visually showing the path of the meme.

The ONE-OF primitive is also used to randomly seed the TREND-SETTER, as well as for each person to choose which of its neighbors it wants to spread the meme to at each tick.

The LAYOUT method, incorporated from the "Preferential Attachment" sample NetLogo model, uses the <code>layout-spring</code> primitive to place the nodes as if the links are springs and the people are repelling each other. This makes the network much easier to visualize and examine.

NetLogo provides a network extension that comes with many network primitives. It is not used in this model, but it is a great tool for analyzing network features.

## RELATED MODELS

This model uses the network-building technique found in the "Preferential Attachment" example in the "Networks" folder of the "Sample Models".

This model is similar to the "Rumor Mill" sample model in the "Social Sciences" folder, a model in which patches are the primary agents working to spread a rumor spatially across the view.

## CREDITS AND REFERENCES

This model and additional related files can be found at its page on the Modeling Commons website: http://modelingcommons.org/browse/one_model/4424

Wilensky, U. (2005). NetLogo Preferential Attachment model. http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Media Influence" repetitions="3" runMetricsEveryStep="false">
    <setup>setup
while [count turtles &lt; population] [
create-network
]
seed-trend</setup>
    <go>if any? turtles with [trend-setter? = true] [go]</go>
    <exitCondition>all? turtles [trendy? = true]</exitCondition>
    <metric>count turtles</metric>
    <metric>count turtles with [trendy? = true]</metric>
    <metric>count turtles with [trend-source = "media"]</metric>
    <enumeratedValueSet variable="media?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="media-frequency" first="2" step="1" last="20"/>
    <enumeratedValueSet variable="population">
      <value value="300"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Media Influence Control" repetitions="3" runMetricsEveryStep="false">
    <setup>setup
while [count turtles &lt; population] [
create-network
]
seed-trend</setup>
    <go>if any? turtles with [trend-setter? = true] [go]</go>
    <exitCondition>all? turtles [trendy? = true]</exitCondition>
    <metric>count turtles</metric>
    <metric>count turtles with [trendy? = true]</metric>
    <metric>count turtles with [trend-source = "media"]</metric>
    <enumeratedValueSet variable="media?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="300"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Trend Categories vs Interests" repetitions="5" runMetricsEveryStep="true">
    <setup>setup
while [count turtles &lt; population] [
create-network
]
seed-trend</setup>
    <go>if any? turtles with [trend-setter? = true] [go]</go>
    <final>export-plot "Interest of Trend-Followers (0-9)" "histogram.csv"</final>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>count turtles with [trendy? = true]</metric>
    <metric>[trend-category] of one-of turtles with [trend-setter? = true]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 0]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 1]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 2]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 3]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 4]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 5]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 6]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 7]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 8]</metric>
    <metric>count turtles with [trendy? = true and interest-category = 9]</metric>
    <enumeratedValueSet variable="media?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="300"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Popularity of Trend-Setters" repetitions="20" runMetricsEveryStep="true">
    <setup>setup
while [count turtles &lt; population] [
create-network
]
seed-trend</setup>
    <go>if any? turtles with [trend-setter? = true] [go]</go>
    <timeLimit steps="4000"/>
    <metric>count turtles</metric>
    <metric>count turtles with [trendy? = true]</metric>
    <metric>[popularity] of one-of turtles with [trend-setter? = true]</metric>
    <enumeratedValueSet variable="media?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="300"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Popularity of Trend-Setters 2" repetitions="3" runMetricsEveryStep="false">
    <setup>setup
while [count turtles &lt; population] [
create-network
]
ask one-of turtles with [popularity = pop] [
set trendy? true
set trend-category interest-category
set trend-setter? true
]</setup>
    <go>if any? turtles with [trend-setter? = true] [go]</go>
    <metric>count turtles</metric>
    <metric>count turtles with [trendy? = true]</metric>
    <metric>[popularity] of one-of turtles with [trend-setter? = true]</metric>
    <enumeratedValueSet variable="media?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="300"/>
    </enumeratedValueSet>
    <steppedValueSet variable="pop" first="0" step="1" last="15"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
