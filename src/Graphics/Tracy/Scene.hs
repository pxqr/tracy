module Graphics.Tracy.Scene
       ( Scene(..)
       ) where

import Data.Default

import Graphics.Tracy.Color
import Graphics.Tracy.Light
import Graphics.Tracy.Material
import Graphics.Tracy.Prim
import Graphics.Tracy.V3


data Scene = Scene
  { backgroundColor   :: Color
  , ambientColor      :: Color
  , bulbs             :: [Light]
  , objects           :: [Object]
  }


instance Default Scene where
  def = Scene
    { backgroundColor = from256 (210 * 3) (220 * 3) (250 * 3)
    , ambientColor    = black
    , bulbs = concat
      [ lightLine 1 (Light 3000 (V3 (-1000) 1000 0) white)
                                (V3 (-1000) 1000 10)
      ]

            , objects =
                [ Object { -- ground plane
                    mat = def { diffuse = gray }
                  , obj = Primitive $ Plane (V3 0 1 0) 5
                  }

                , Object { -- central ball
                    mat = def { diffuse   = red
                              , specularK = 0.001
                              }
                  , obj = Primitive $ Sphere (V3 0 0 25) 5
                  }

                , Object { -- left ball
                    mat = def { diffuse = yellow }
                  , obj = Primitive $ Sphere (V3 (-16) 6 26) 12
                  }

                , Object { -- light ball
                    mat = def { diffuse = white
                              , luminosity = gray
                              }
                  , obj = Primitive $ Sphere (V3 5 (-4) 20) 1
                  }


                , Object { -- big ball
                    mat = def { diffuse = red }
                  , obj = Primitive $ Sphere (V3 14 3 25) 8
                  }

                , Object { -- big ball
                    mat = def { diffuse = green }
                  , obj = Primitive $ Sphere (V3 5 11 45) 16
                  }


                , Object { -- nearest ball
                    mat = def { diffuse = magenta }
                  , obj = Primitive $ Sphere (V3 (-2) (-3.8) 12)  1.2
                  }

                ]
            }
