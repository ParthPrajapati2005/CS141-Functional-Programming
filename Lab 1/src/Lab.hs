--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Getting started                                                       --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

import Hatch

--------------------------------------------------------------------------------

animation t =     offset (-400)   200  (dogwheel t)
              <@> offset (-400) (-200) (gooseChase t)
              <@> offset  400    200   (dogwheel t)
              <@> offset  0      200   (rotate (t) dog)
              <@> offset (-300)  0     (text "Welcome to CS141!")

-- A load of dogs spinning around
dogwheel t = superimposeAll [spinningDog x | x <- [t, t + 45 .. t + 315 ] ]

-- One single rotating dog
spinningDog t = rotate t (offset 100 0 dog)

-- The bottom of the screen goose chase
gooseChase t = offset (t * 4) 0 (duck <|> duck <|> scale 10 (mirror goose))

--------------------------------------------------------------------------------