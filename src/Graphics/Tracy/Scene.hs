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
              backgroundColor = from256 (219 * 3) (225 * 3) (243 * 3)
            , ambientColor    = V3 0.0  0.0  0.0
            , bulbs = concat
                [ lightLine 1 (Light 1000 (V3 (-1000) 1000 0) (V3 1.0 1.0 1.0))
                                (V3 (-1000) 1000 10)
                ]

            , objects =
                [ Object { -- ground plane
                    mat = defaultMaterial -- { luminosity = V3 1 1 1 }
                  , obj = Primitive $ Plane (V3 0 1 0) 5
                  }

                , Object { -- central ball
                    mat = defaultMaterial { diffuse = from256 252 152 133 }
                  , obj = Primitive $ Sphere (V3 0 0 20) 5
                  }

                , Object { -- big ball
                    mat = defaultMaterial { diffuse = from256 255 147 187 }
                  , obj = Primitive $ Sphere (V3 14 3 20) 8
                  }


                , Object { -- nearest ball
                    mat = defaultMaterial { diffuse = from256 254 219 142 }
                  , obj = Primitive $ Sphere (V3 (-2) (-3.8) 10)  1.2
                  }

                ]
            }
