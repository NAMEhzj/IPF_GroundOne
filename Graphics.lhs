> module Graphics where
>
> import Data.List
>
> import Text.LaTeX
> import Text.LaTeX.Packages.Inputenc
> import Text.LaTeX.Packages.TikZ 
> import Text.LaTeX.Base.Commands
> import Text.LaTeX.Base.Writer
> import Text.LaTeX.Base.Render
> import Text.LaTeX.Base.Syntax
> import Text.LaTeX.Packages.TikZ.Simple



> savePath = "IPFGraphResults/graphicIPF2.tex"
>
> axisMarkLen = 0.05
> axisArrowSiz = 0.1
> yStart :: Int
> yStart = -12
> yEnd :: Int
> yEnd = 10
> xStart :: Int
> xStart = 0
> xEnd :: Int
> xEnd = 10
> yAxisOffset = 12

> {- The functions in this module produce a LaTeX File that will compile to a document(article)
> consisting of an introduction and a tikzPicture. The graphic shows the width of the intervals
> ,i.e. the entries in an IPF matrix depending on the number of iterations.
> The interface to the outside is the function buildGraphicOne.
> -}
>
> 
>
> buildGraphicTwo :: FilePath -> [[(Double,Double)]] -> IO()
> buildGraphicTwo path list = (execLaTeXT
>                      (thePreambleTwo >> document (graphicTypeTwo list) ))
>                      >>= renderFile path
> 
> --constants--
>
> -- mkPoint :: (Int, Double) -> TPoint
> -- mkPoint (a,b) = pointAtXY (fromIntegral a) $ if (abs (b*20000)) > 10 then 10*signum(b) else (b*20000)
>
> mkPointLog :: (Int, Double) -> TPoint
> mkPointLog (a,b) = pointAtXY (fromIntegral a) (logScale b)
>                      
> oldscale :: Double -> Double
> oldscale b = signum b * 20 * (1/ (max (-(log (abs b))) 1))
>
>
>
>
> logScale :: Double -> Double
> logScale x = signum x * (max ((logBase 10 (abs x) + yAxisOffset)) 0) 
>
>
> calcax :: Double -> Double
> calcax x = -(abs x)/ (log 10)
> 
>{- 
> y: abs(y) = 10^(x-12) -> sig(y)*x    
> -}
>
>
>
> orange :: TikZColor
> orange = RGBColor 255 128 0  
>
> gray :: TikZColor
> gray = RGBColor 102 51 0
>
> violet :: TikZColor
> violet = RGBColor 102 0 102
> 
> brown :: TikZColor
> brown = RGBColor 128 128 128 
> 
> allcolours :: [TikZColor]
> allcolours = [BasicColor Red, BasicColor Green, BasicColor Blue, BasicColor Yellow,
>               BasicColor Magenta, BasicColor Cyan, orange, gray, violet, brown]
>
> -- frame :: TikZ
> -- frame = draw (Rectangle (Start $ pointAtXY 0 0) (pointAtXY 12 11))
>
> {-draw x and y axis, x shows number of iteration, y shows interval upper and lower deviations on a logarithmic scale-}
>
> drawAxes :: TikZ
> drawAxes =  (drawAxis True (fromIntegral xStart) (fromIntegral xEnd) axisMarkLen) 
>             ->> (drawAxis False (fromIntegral yStart) (fromIntegral yEnd) axisMarkLen)
>
> drawAxis :: Bool -> Double -> Double -> Double -> TikZ 
> drawAxis isX start end markLen = (axdescrip isX start end (defaultRender isX) (defaultSpacing isX)) ->> foldl (->>)  
>                    (draw (Start (dynTPoint isX (start-0.5) 0) ->- dynTPoint isX (end+0.5) 0))
>                    [draw (Start (dynTPoint isX x (-markLen)) ->- dynTPoint isX x markLen)| x <- [start..end]]  
>                    ->> arrowHead isX (end+0.5) 0 

> axdescrip :: Bool -> Double -> Double -> (Double -> LaTeX) -> (Double -> Double) -> TikZ
> axdescrip isX start end render spacing = foldl (->>)  (draw (Node (Start (dynTPoint isX (end+0.5) (-2))) (defaultDesc isX) ))
>                          [draw (Node (Start (dynTPoint isX x (- spacing x)))
>                                 (render x) )| x <- [start..end]]
>             
>
> dynTPoint :: Bool -> Double -> Double -> TPoint
> dynTPoint True x y  = pointAtXY x y
> dynTPoint False y x = pointAtXY x y
>
> dynPoint :: Bool -> Double -> Double -> Point
> dynPoint True x y  = (x, y)
> dynPoint False y x = (x, y)

> 

>
> sciNot :: String -> Double -> LaTeX
> sciNot sign n = TeXMath Dollar $  (fromString (sign ++ "10")) <> TeXComm ("textsuperscript") [FixArg (fromString $ show (floor n))]  
>
> defaultRender :: Bool -> Double -> LaTeX
> defaultRender True n = fromString $ show (floor n)
> defaultRender False n | n > 0     = (sciNot "" (n-yAxisOffset))
>                       | n == 0    = (sciNot "" (-yAxisOffset)) <> TeXMath Dollar (TeXCommS "geq y " <> TeXCommS "geq ") <> (sciNot "-" (-yAxisOffset))
>                       | otherwise = (sciNot "-" (-n-yAxisOffset))
>
> defaultSpacing :: Bool -> Double -> Double
> defaultSpacing True _ = 0.6
> defaultSpacing False n | n > 0     = 0.9
>                        | n == 0    = 1.8
>                        | otherwise = 1.0
>
> defaultDesc :: Bool -> LaTeX
> defaultDesc True  = fromString "iterations"
> defaultDesc False = fromString "bound deviations"

> arrowHead :: Bool -> Double -> Double -> TikZ
> arrowHead isX x y = figuretikz $ PolygonFilled [dynPoint isX x (y+axisArrowSiz), dynPoint isX (x+2*axisArrowSiz) y, dynPoint isX x (y-axisArrowSiz)]
>
>


> 
> -- production of preamble with title and remarks
> 
> thePreambleTwo :: LaTeXT IO ()
> thePreambleTwo = do
>    {documentclass [] article;
>     usepackage [utf8] inputenc;
>     usepackage [] tikz;
>     author(fromString ("Eva Richter"));
>     title (fromString ("Relative deviations of columnsum from marginals depending on iterations"))
>    }
> {--
> the second part of the document contains only an error message if the input for the local function
> drawGraphs in readDrawIPFTypeTwo from main-module  is Nothing, i.e. saveIPF does not yield a correct array
> -}
> 
> errorTexFileTwo :: IO ()
> errorTexFileTwo = (execLaTeXT (thePreambleTwo >> document (maketitle >> fromString ("There was nothing to draw")) ))
>                      >>= renderFile savePath
>
> graphicTypeTwo :: [[(Double, Double)]] -> LaTeXT IO() 
> graphicTypeTwo list = do
>  maketitle
>  fromString ("The graphic below shows the changes in relative error of the column sum.")
>  fromString ("Each column is represented by one color, the x axis shows the number of iterations.The y-axis has a logarithmic scale")
>  newline
>  center $ tikzpicture $
>    foldl (->>) drawAxes [graphicTypeTwoSingleRow rowColour (unzip rowData)
>                        | (rowColour, rowData) <- zip allcolours (transpose list)]
>

> graphicTypeTwoSingleRow :: TikZColor -> ([Double], [Double]) -> TikZ
> graphicTypeTwoSingleRow color ((u:uppers), (l:lowers)) = scope [TColor color, TWidth (Pt 1)]
>                      (draw (bpath (mkPointLog (0,u)) (mapM_ (line.mkPointLog) (zip [1..] uppers)))
>                       ->> draw (bpath  (mkPointLog (0,l)) (mapM_ (line.mkPointLog) (zip [1..] lowers))))
>

> 
>
