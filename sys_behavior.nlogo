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
