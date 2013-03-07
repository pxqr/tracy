module Graphics.Tracy.Scene
       ( Scene(..)
       , defaultScene
       ) where

import Graphics.Tracy.V3
import Graphics.Tracy.Prim
import Graphics.Tracy.Material
import Graphics.Tracy.Light


data Scene = Scene { backgroundColor   :: Color
                   , ambientColor      :: Color
                   , bulbs             :: [Light]
                   , objects           :: [Object]
                   }

defaultScene :: Scene
defaultScene = Scene {
              backgroundColor = from256 (210 * 3) (220 * 3) (250 * 3)
            , ambientColor    = V3 0.0  0.0  0.0
            , bulbs = concat
                [ lightLine 1 (Light 3000 (V3 (-1000) 1000 0) (V3 1.0 1.0 1.0))
                                (V3 (-1000) 1000 10)
                ]

            , objects =
                [ Object { -- ground plane
                    mat = defaultMaterial { diffuse = from256 100 100 100 }
                  , obj = Primitive $ Plane (V3 0 1 0) 5
                  }

                , Object { -- central ball
                    mat = defaultMaterial { diffuse = from256 252 152 133
                                          , specularK = 0.001
                                          }
                  , obj = Primitive $ Sphere (V3 0 0 25) 5
                  }

                , Object { -- left ball
                    mat = defaultMaterial { diffuse = from256 252 255 100 }
                  , obj = Primitive $ Sphere (V3 (-16) 6 26) 12
                  }

                , Object { -- light ball
                    mat = defaultMaterial { diffuse = from256 252 255 255
                                          , luminosity = V3 20 20 20 }
                  , obj = Primitive $ Sphere (V3 5 (-4) 20) 1
                  }


                , Object { -- big ball
                    mat = defaultMaterial { diffuse = from256 255 147 187 }
                  , obj = Primitive $ Sphere (V3 14 3 25) 8
                  }

                , Object { -- big ball
                    mat = defaultMaterial { diffuse = from256 140 300 187 }
                  , obj = Primitive $ Sphere (V3 5 11 45) 16
                  }


                , Object { -- nearest ball
                    mat = defaultMaterial { diffuse = from256 254 219 142 }
                  , obj = Primitive $ Sphere (V3 (-2) (-3.8) 12)  1.2
                  }

                ]
            }
