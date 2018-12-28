{-# LANGUAGE ScopedTypeVariables #-}
module Demo.IpeToSvg where

import           Control.Lens
import qualified Data.ByteString.Lazy as B
import           Data.Data
import           Data.Geometry.Ipe (readSinglePageFile, IpePage, content)
import           Data.Geometry.Svg (svgO, toSvgXML)
import qualified Data.List.NonEmpty as NonEmpty
import           Options.Applicative

--------------------------------------------------------------------------------

data Options = Options { _inPath    :: FilePath
                       , _outFile   :: FilePath
                       }
               deriving Data


options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Reads an ipe file and converts it into a svg file."
               <> header   "IpeToSvg"
               )
  where
    parser = Options
          <$> strOption (help "Input file (in ipe7 xml format)"
                         <> short 'i'
                        )
          <*> strOption (help "Output File (in svg format)"
                         <> short 'o'
                        )

mainWith                          :: Options -> IO ()
mainWith (Options inFile outFile) = do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> print err
      Right (page :: IpePage Rational) -> do
            let content' = map svgO $ page^.content
                out = svgO content'
            B.writeFile outFile $ toSvgXML out
