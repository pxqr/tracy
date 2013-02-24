{-# LANGUAGE Rank2Types #-}
module Main (main) where

import Codec.Picture.Bitmap
import Codec.Picture.Types

import System.Environment
import System.Console.GetOpt
import System.FilePath

import Paths_tracy (version)
import Data.Version (showVersion)
import Data.List
import Data.Maybe

import Graphics.Tracy.V3
import Graphics.Tracy.Tracer
import Graphics.Tracy.Scene


data Options = Options {
      optVersion   :: Bool
    , optHelp      :: Bool
    , optInput     :: Maybe FilePath
    , optOutput    :: FilePath
    , optImageSize :: (Int, Int)
    } deriving (Show, Eq)

defaultOptions = Options {
                   optVersion = False
                 , optHelp    = False
                 , optInput   = Nothing
                 , optOutput  = "out.bmp"
                 , optImageSize = (400, 400)
                 }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['V', '?'] ["version"] (NoArg (\opts -> opts { optVersion = True }))
                 "Print the version number."
    , Option ['h']      ["help"]    (NoArg (\opts -> opts { optHelp = True }))
                 "Print this message."

    , Option ['i']      ["input"]  (ReqArg (\path opts -> opts { optInput = Just path }) "")
                "Path to scene config."

    , Option ['o']      ["output"]  (ReqArg (\path opts -> opts { optOutput = path }) "")
                 "Path to result image."

    , Option ['s']      ["size"]    (ReqArg (\size opts -> opts { optImageSize = readDim size } ) "")
                 "Width and height of image in format `%width%x%height%'."
    ]
        where
          readDim xs = let (a, b) = splitAt (fromJust (findIndex (=='x') xs)) xs
                       in (read a, read (tail b))



parseArgs :: [String] -> IO Options
parseArgs argv =
    case getOpt Permute options argv of
      (o, n, []) -> if null n
                    then return $ foldl (flip id) defaultOptions o
                    else ioError $ userError $ "Unable to handle options: " ++ concat n
      (_, _, e)  -> ioError $ userError $ concat e ++ usage
    where
      usage = usageInfo "" options


handleArgs :: Options -> IO ()
handleArgs opts
    | optVersion opts = putStrLn (showVersion version)
    | optHelp    opts = putStrLn (usageInfo "" options)
    |    otherwise    = do
      let bitmap = runTracer (optImageSize opts) defaultScene
      writeBitmap (optOutput opts) bitmap


main :: IO ()
main = getArgs >>= parseArgs >>= handleArgs


runTracer (w, h) scene = let view  = View w h (-1) in
    generateImage (\x y -> vecToColor (tracePixel scene view 1 x y)) w h
  where
    toLDR :: Double -> Double
    toLDR x = ((1 / (1 + exp (-x))) - 0.5) * 2

    vecToColor :: Color -> PixelRGB8
    vecToColor (V3 r g b) = PixelRGB8 (conv r) (conv g) (conv b)
        where conv x = floor (255 * toLDR x)
