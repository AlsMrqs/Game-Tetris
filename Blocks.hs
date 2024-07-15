module Blocks where

import Plane (Elem(..))

b0 = 
    [ Elem (0,1+8-1) '#', Elem (1,1+8-1) '#'
    , Elem (0,0+8-1) '#', Elem (1,0+8-1) '#' ]

b1 = 
    [ Elem (-1,1+8-1) '#' 
    , Elem (-1,0+8-1) '#', Elem (0,0+8-1) '#'
                        , Elem (0,-1+8-1) '#' ]
b2 = 
    [                    Elem (1,1+8-1) '#' 
    , Elem (1,0+8-1) '#', Elem (0,0+8-1) '#' 
    , Elem (0,-1+8-1) '#' ]

b3 = 
    [ Elem (0,2+8-1-1) '#'
    , Elem (0,1+8-1-1) '#'
    , Elem (0,0+8-1-1) '#'
    , Elem (0,-1+8-1-1) '#' ]

b4 = 
    [ Elem (-1,1+8-1) '#', Elem (0,1+8-1) '#', Elem (1,1+8-1) '#'
                        , Elem (0,0+8-1) '#']

b5 = 
    [ Elem (-1,0+8) '#', Elem (0,0+8) '#', Elem (1,0+8) '#'
                                           , Elem (1,-1+8) '#']

b6 = 
    [ Elem (-1,0+8) '#', Elem (0,0+8) '#', Elem (1,0+8) '#'
    , Elem (-1,-1+8) '#']

