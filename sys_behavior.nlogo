;;;Each robot has its own copy of these variable
;;;We should really be able to remove the radius of the circles from here!
globals[nest? r1 r2]
turtles-own [x y T-x-Delta T-y-Delta NN-x-Delta NN-y-Delta G-x-Delta G-y-Delta]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a Random population outside of the inner radius r1
to random-pop
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set-default-shape turtles "circle"
  setup-patches
  crt population
  [
    set size 1 setxy random-xcor random-ycor        ;; easier to see
    while [(( sqrt(abs(xcor) ^ 2  + (abs(ycor)) ^ 2) < r1)) or (count turtles-here > 1)]
    [
     setxy random-xcor random-ycor
    ]
  ]
  ask turtles[
     collision-monitor ;double check that each turtle is already not collided
  ]
  setup-patches

end

;create a population that is radially linear; all points collinear to center
to collinear-rad-pop

  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set-default-shape turtles "circle"
  setup-patches
  crt population
  [

    set size 1 setxy 0 random-ycor        ;; easier to see
    while [(abs(xcor) < r1) and (abs(ycor) < r1) or (count turtles-here > 1)]
    [setxy 0 random-ycor]
  ]
  ask turtles[
   collision-monitor ;double check that each turtle is already not collided
  ]
  setup-patches
end

;create a population that is linear, but not collinear to the center
to linear-pop
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set-default-shape turtles "circle"
  setup-patches
  crt population
  [
    set size 1 setxy random-xcor 20        ;; easier to see
    while [(abs(xcor) < r1) and (abs(ycor) < r1) or (count turtles-here > 1)]
    [setxy random-xcor 20]
  ]
  ask turtles
  [collision-monitor
  ]
  setup-patches

end

to box-pop

  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set-default-shape turtles "circle"
  setup-patches
  crt population
  [

  set size 1 setxy random-xcor random-ycor        ;; easier to see
  while [(xcor < x1) or (xcor > x2) or (ycor < y1) or (ycor > y2) or (count turtles-here > 1)]
  [
   setxy random-xcor random-ycor
  ]
  ]
   ask turtles[
      collision-monitor ;double check that each turtle is already not collided
   ]
  setup-patches

end

;Visualize the links between all the turtles, aides in visualization, slows things down
to on-links
 ask turtles[
   create-links-with other turtles
  ]
end

;Random setup of
to setup-patches
  ask patches
  [ setup-nest
  set r1 8
  set r2 16
  recolor-patch ]

end

to setup-nest  ;; patch procedure
  ;; set nest? variable to true inside the nest, false elsewhere
  set nest? (distancexy 0 0) < 3
end

to recolor-patch  ;; patch procedure
  ;; give color to nest and food sources
  if (distancexy 0 0) < r2
    [set pcolor blue]
  if (distancexy 0 0) < r1
    [set pcolor black]
  if nest?
  [ set pcolor violet ]
  if (distancexy 0 0) abs 2

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Procedures that matter;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;;; Go procedures ;;;
;;;;;;;;;;;;;;;;;;;;;

to go  ;; forever button
  ask turtles
  [trial
    set-current-plot "plot1"
    create-temporary-plot-pen (word who)
    set-plot-pen-color color
    set-plot-pen-mode 2
    plotxy g-x-delta g-y-delta
    ] ;execute the trial code
  tick
end


to trial
  set x xcor
  set y ycor
  ;face the target, this allows movement quickly
  facexy 0 0
  ;too close, move away
  if  (( ((r1 + r2) / 2) - (distancexy 0 0 )) > .5) ; was .5 now r1
  [
        ;new check before movement insures no one is at the location we're moving to
    if (not any? turtles-on patch-ahead (- centRule) or collisionDetector = false)
    [fd (- centRule)]
  ]
  ;too far away, move closer
  if ((((r1 + r2) / 2) - (distancexy 0 0 )) < -.5) ; was -.5 now -r2
  [
    ;new check before movement insures no one is at the location we're moving to
    if (not any? turtles-on patch-ahead centRule or collisionDetector = false)
    [fd centRule]
  ]

  collision-monitor ;check for collisions after move

  ;only do this if more than one turtle exists
  if count turtles > 1
  ;face the second closest turtle in the list, 1st closest is myself, 2nd is nearest neighbor
  [face max-one-of min-n-of 2 turtles  [distance myself] [distance myself]
    ;if (not any? turtles-on patch-ahead -1 or collisionDetector = false)
    right rotationAmt
    if (patch-ahead (- DisperseRule) != nobody)[
            if (not any? (turtles-on patch-ahead (- DisperseRule)) or collisionDetector = false)
            [
              fd (- DisperseRule) ;if DisperseRule = centRule deadlock can occur
              collision-monitor  ;check for collisions after move
            ]
    ]
  ]
  set G-x-Delta abs (x - xcor)
  set G-y-Delta abs (y - ycor)
end


to collision-monitor
   ;we check the current patch where a turtle is
   if any? turtles-on patch-here
   [ ;there will be atleast one turtles (us)
     if count turtles-here > 1  ;but if there is more than one, we have a collision!
     [ print "Collision Alert!"
     show count turtles-here
     print turtles-here
     write xcor
     write ycor
     print ""
     set pcolor red ;visually see where the collisions occured
     ]
   ]

end

to-report average-x-delta
  ifelse any? turtles with [g-x-delta > 0]
  [report mean [g-x-delta] of turtles]
  [report 0]
end
to-report average-y-delta
  ifelse any? turtles with [g-y-delta > 0]
  [report mean [g-y-delta] of turtles]
  [report 0]
end

to-report min-x-delta
  ifelse any? turtles with [g-x-delta > 0]
  [report min [g-x-delta] of turtles]
  [report 0]
end
to-report min-y-delta
  ifelse any? turtles with [g-y-delta > 0]
  [report min [g-y-delta] of turtles]
  [report 0]
end

to-report max-x-delta
  ifelse any? turtles with [g-x-delta > 0]
  [report max [g-x-delta] of turtles]
  [report 0]
end
to-report max-y-delta
  ifelse any? turtles with [g-y-delta > 0]
  [report max [g-y-delta] of turtles]
  [report 0]
end

to-report stddev-x-delta
  ifelse any? turtles with [g-x-delta > 0]
  [report  standard-deviation [g-x-delta] of turtles]
  [report 0]
end
to-report stddev-y-delta
  ifelse any? turtles with [g-y-delta > 0]
  [report standard-deviation [g-y-delta] of turtles]
  [report 0]
end

to-report med-x-delta
  ifelse any? turtles with [g-x-delta > 0]
  [report  median [g-x-delta] of turtles]
  [report 0]
end
to-report med-y-delta
  ifelse any? turtles with [g-y-delta > 0]
  [report median [g-y-delta] of turtles]
  [report 0]
end
@#$#@#$#@
GRAPHICS-WINDOW
257
10
693
467
35
35
6.0
1
10
1
1
1
0
0
0
1
-35
35
-35
35
1
1
1
ticks
30.0

BUTTON
28
356
219
397
Start/Stop
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
7
18
102
51
population
population
0
500
4
1
1
NIL
HORIZONTAL

BUTTON
151
470
247
503
View Paths
pd
NIL
1
T
TURTLE
NIL
NIL
NIL
NIL
1

BUTTON
151
507
239
540
Paths Off
pu
NIL
1
T
TURTLE
NIL
NIL
NIL
NIL
1

BUTTON
10
507
139
540
NIL
clear-drawing
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
3
469
139
502
subject pd
if subject != nobody\n[ ask subject [ pd ] ]
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
61
548
202
581
NIL
reset-perspective
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
415
478
482
511
Start
movie-start(\"run.mov\")\n\n
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
568
478
632
511
Stop
movie-close
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
489
478
558
511
Record 20
repeat 20 [\n  movie-grab-view \n  go\n]\n
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
91
411
156
444
View Links
on-links
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
321
487
471
505
Video Controls
11
0.0
1

BUTTON
6
60
73
93
Random
random-pop
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
81
61
185
94
Radially Linear
collinear-rad-pop
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
193
62
248
95
Linear
linear-pop
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
92
170
155
203
Box
box-pop
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
32
149
94
211
x1
-5
1
0
Number

INPUTBOX
155
154
216
215
x2
5
1
0
Number

INPUTBOX
94
203
155
263
y1
20
1
0
Number

INPUTBOX
93
111
156
171
y2
34
1
0
Number

SWITCH
108
18
247
51
collisionDetector
collisionDetector
0
1
-1000

INPUTBOX
10
281
75
341
CentRule
2
1
0
Number

INPUTBOX
80
281
157
341
DisperseRule
1.3
1
0
Number

INPUTBOX
167
281
243
341
rotationAmt
60
1
0
Number

PLOT
714
15
1218
530
plot1
NIL
NIL
0.0
5.0
0.0
5.0
true
false
"" ""
PENS

PLOT
260
533
1098
683
plot 1
NIL
NIL
0.0
5.0
0.0
5.0
true
true
"" ""
PENS
"avg x delta" 1.0 2 -8862290 true "" "plot average-x-delta"
"avg y delta" 1.0 0 -955883 true "" "plot average-y-delta"

@#$#@#$#@
## WHAT IS IT?

Coming Soon!

## HOW TO USE IT

Coming Soon!

## THINGS TO NOTICE

Coming Soon!

## THINGS TO TRY

Coming Soon!

## CREDITS AND REFERENCES

Copyright 2008 Mike Borowczak. Digital Design Environments Lab  All rights reserved.
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
need-to-manually-make-preview-for-this-model
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="5" runMetricsEveryStep="true">
    <setup>random-pop</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-x-delta</metric>
    <metric>average-y-delta</metric>
    <metric>min-x-delta</metric>
    <metric>min-y-delta</metric>
    <metric>max-x-delta</metric>
    <metric>max-y-delta</metric>
    <metric>stddev-x-delta</metric>
    <metric>stddev-y-delta</metric>
    <metric>med-x-delta</metric>
    <metric>med-y-delta</metric>
    <steppedValueSet variable="rotationAmt" first="15" step="90" last="15"/>
    <enumeratedValueSet variable="population">
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CentRule">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collisionDetector">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="DisperseRule">
      <value value="1.5"/>
    </enumeratedValueSet>
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
