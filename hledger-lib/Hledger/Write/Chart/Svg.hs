{- |
Draw a 'Chart' as SVG, a self-contained graphical file viewable in web browsers and many image viewers.

Charts over time (periods or dates) are drawn with time along the x axis:
bar series as vertical bars (stacked, with a 2px gap between segments), line series as 2px lines,
and totals as ink-coloured line marks across each period's bars, or as an ink-coloured line.
Charts of categories (eg the accounts of a single-period report) are drawn as horizontal bars,
with labels on the left and an aligned value column on the right.
There is one panel per commodity, one above the other, and a legend when a panel has several series (or an average line).
Each mark has a tooltip (an SVG title) giving its x label, series and value.

The SVG includes its own styles, with a colour-blind-safe palette and a dark-mode variant
(used when the viewer prefers a dark colour scheme). Text widths are estimated, since fonts are not known.
Coordinates are rounded to one decimal place, so the output is deterministic.

A page embedding a chart can draw it with 'SvgOptions', eg without its styles, which the page then supplies
('chartStylesheet').
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Hledger.Write.Chart.Svg (
  chartAsSvg,
  chartAsSvgWith,
  chartSvgMarkup,
  SvgOptions(..),
  defaultSvgOptions,
  chartSvgMarkupWith,
  chartStylesheet,
  tests_Hledger_Write_Chart_Svg,
) where

import Control.Monad (forM_, unless, when)
import Data.Char (isDigit, isUpper)
import Data.Function (on)
import Data.List (find, groupBy, nub, sortOn, transpose)
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import Data.Map qualified as M
import Data.Array qualified as Arr
import Data.Decimal (realFracToDecimal)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Time (Day, addDays, diffDays, fromGregorian, toGregorian)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Safe (atMay, headMay, lastMay, maximumDef, minimumDef)
import Text.Blaze ((!?), customAttribute, toValue)
import Text.Blaze.Svg.Renderer.Text (renderSvg)
import Text.Blaze.Svg11 ((!), Svg)
import Text.Blaze.Svg11 qualified as S
import Text.Blaze.Svg11.Attributes qualified as A
import Text.DocLayout qualified as DL
import Text.Printf (printf)

import Hledger.Data.Amount (nullamt, oneLineNoCostFmt, showAmountWith)
import Hledger.Data.Dates (showDate)
import Hledger.Data.Types (AccountType(..), Amount(..), AmountPrecision(..), AmountStyle(..), Quantity)
import Hledger.Utils.Test
import Hledger.Write.Chart
import Hledger.Write.Chart.Palette (colourHex)


-- | Render a chart as a complete SVG document, with a dark variant for viewers preferring a dark colour scheme.
chartAsSvg :: Chart -> TL.Text
chartAsSvg = chartAsSvgWith True

-- | Render a chart as SVG, with or without the dark variant
-- (eg without, when embedding it in a page that is always light).
-- It has one element per line, for readability.
chartAsSvgWith :: Bool -> Chart -> TL.Text
chartAsSvgWith dark = (<> "\n") . TL.replace "><" ">\n<" . renderSvg . chartSvgMarkup dark

-- | A chart as SVG markup, which can also be embedded in HTML; with or without the dark variant.
chartSvgMarkup :: Bool -> Chart -> Svg
chartSvgMarkup dark = chartSvgMarkupWith defaultSvgOptions{svgDark = dark}

-- | How to draw a chart as SVG.
data SvgOptions = SvgOptions
  { svgStyles     :: Bool    -- ^ include the chart's styles ? A page embedding the chart can leave them out and supply
                             --   'chartStylesheet' itself, eg one whose Content-Security-Policy allows no inline styles.
  , svgDark       :: Bool    -- ^ include the styles' dark variant, for viewers preferring a dark colour scheme ?
  , svgPlotHeight :: Double  -- ^ the height of each plot of values over time or of categories as columns, in pixels,
                             --   not counting its labels; also deciding how many y ticks it has (see 'plotIntervals').
                             --   (Horizontal bars are as tall as their rows.)
  , svgPointLink  :: Int -> Maybe Text  -- ^ a link for each x position (by index), if any: lines' tooltip targets there link to it,
                                        --   eg to a page's row for that transaction. They are left out of the tab order, being many.
  , svgDateScale  :: Bool    -- ^ give each plot of values by date its date scale, for a page's script (eg to select dates by
                             --   dragging across it): a transparent rectangle over the plot (class plot), with its first and last
                             --   dates (data-d0, data-d1) and their x positions (data-x0, data-x1), between which dates are spaced evenly ?
  , svgToday      :: Maybe Day  -- ^ today's date, if each plot of values by date should be shaded as of it: the future, after it,
                                --   and the past where values are below zero (eg an overdrawn account), with the zero line in red.
  }

-- | A self-contained chart: with its styles, including their dark variant, plots 260px high, no links, no date scale, and no shading.
defaultSvgOptions :: SvgOptions
defaultSvgOptions = SvgOptions
  { svgStyles     = True
  , svgDark       = True
  , svgPlotHeight = 260
  , svgPointLink  = const Nothing
  , svgDateScale  = False
  , svgToday      = Nothing
  }

-- | A chart as SVG markup, which can also be embedded in HTML, drawn with these options.
chartSvgMarkupWith :: SvgOptions -> Chart -> Svg
chartSvgMarkupWith opts@SvgOptions{svgStyles, svgDark} c@Chart{chTitle, chXAxis, chPanels} =
  S.svg
    ! customAttribute "xmlns" "http://www.w3.org/2000/svg"
    ! A.class_ "hledger-chart"
    ! A.width (toValue $ num chartWidth)
    ! A.height (toValue $ num totalheight)
    ! A.viewbox (toValue $ "0 0 " <> num chartWidth <> " " <> num totalheight)
    ! customAttribute "role" "img"
    !? (not $ T.null chTitle, customAttribute "aria-label" (toValue chTitle))
    $ do
      unless (T.null chTitle) $ S.title (S.toMarkup chTitle)
      when svgStyles $ S.style (S.toMarkup $ chartStylesheet svgDark)
      S.rect ! A.class_ "bg" ! A.width "100%" ! A.height "100%"
      unless (T.null chTitle) $
        S.text_ ! A.class_ "title" ! A.x (toValue $ num pad) ! A.y (toValue $ num (pad + 17)) $ S.toMarkup chTitle
      sequence_ panelmarkups
      when (null chPanels) $
        S.text_ ! A.class_ "label" ! A.x (toValue $ num pad) ! A.y (toValue $ num (pad + titleheight + 16)) $ "No data"
  where
    titleheight = if T.null chTitle then 0 else 38
    showheadings = length chPanels > 1
    (panelsbottom, panelmarkups) = foldl stackPanel (pad + titleheight, []) chPanels
    totalheight = if null chPanels then panelsbottom + 14 + pad else panelsbottom
    stackPanel (top, ms) p =
      let (h, m) = panelSvg opts chXAxis (chartColouring c) showheadings top p
      in (top + h + panelGap, ms ++ [m])

-- Layout constants, in pixels. (The plots' height is in 'SvgOptions'.)
chartWidth, pad, panelGap, legendRowHeight, rowHeight :: Double
chartWidth      = 880
pad             = 16
panelGap        = 16
legendRowHeight = 24
rowHeight       = 28   -- height of a row in a horizontal bar chart

-- | How many intervals a plot this high (in pixels) divides its y axis into, roughly: about one per 50px, from 2 to 5.
plotIntervals :: Double -> Int
plotIntervals h = max 2 $ min 5 $ floor (h / 50)

-- | Estimated rendered width of some text, in the 14px system sans font (fonts are not known),
-- from the widths of kinds of character (measured with SF Pro; Segoe UI and others are similar or narrower).
textWidth :: Text -> Double
textWidth = T.foldl' (\w c -> w + glyphWidth c) 0
  where
    glyphWidth c
      | DL.charWidth c == 2               = 14
      | DL.charWidth c == 0               = 0
      | isDigit c                         = 8.7  -- tabular digits, as in ticks and values
      | c `elem` ("mwMW" :: String)       = 12
      | isUpper c                         = 9.5
      | c `elem` ("fijlrt" :: String)     = 4.2
      | c `elem` (" .,:;'|!()" :: String) = 3.8
      | otherwise                         = 7.9

-- | A series to draw, with its CSS class and the values it draws.
data DrawSeries = DrawSeries
  { dsClass  :: Text
  , dsSeries :: Series
  , dsValues :: SeriesValues
  }

-- | Draw one panel with its top at the given y; return its height and markup.
panelSvg :: SvgOptions -> XAxis -> Colouring -> Bool -> Double -> Panel -> (Double, Svg)
panelSvg opts xaxis colouring showheading top Panel{pnUnit, pnSeries, pnGroupNames, pnScale, pnLayout, pnSummary} =
  (headingh + legendh + bodyh + summaryh, do
    when (headingh > 0) $
      S.text_ ! A.class_ "heading" ! A.x (toValue $ num pad) ! A.y (toValue $ num (top + 16)) $ S.toMarkup (acommodity pnUnit)
    legendm
    bodym
    when (summaryh > 0) $
      S.text_ ! A.class_ "label" ! A.x (toValue $ num pad) ! A.y (toValue $ num (top + headingh + legendh + bodyh + 16)) $ S.toMarkup summarytext)
  where
    headingh = if showheading && not (T.null $ acommodity pnUnit) then 28 else 0
    -- the summary values, on a line below the plot, eg "Total 11, Average 0.9"
    summaryh = if null pnSummary then 0 else 26
    summarytext = T.intercalate ", " [suLabel su <> " " <> showSummary su | su <- pnSummary]
    showSummary = T.pack . showAmountWith oneLineNoCostFmt . suValue
    -- the average, drawn as a line across a chart of periods
    maverage = case xaxis of
      XPeriods _ -> find ((== SummaryAverage) . suKind) pnSummary
      _          -> Nothing
    -- The series drawn as marks: items and "other" (bars or lines), and the total (dots or a line). Other markers are not drawn yet.
    items = [s | s <- pnSeries, seRole s /= SeriesTotal, seMark s /= MarkMarker]
    mtotal = find ((== SeriesTotal) . seRole) pnSeries
    -- The total, if it's to be marked: not when it is always just the top of the stack, ie with bars in one stack
    -- (not side by side) and no x position having values of both signs (eg counts, or time logged).
    mtotalmark = if onestack && all ((== MarkBar) . seMark) items && not mixedsigns then Nothing else mtotal
    onestack = not (pnLayout == Grouped && length pnGroupNames > 1)
    mixedsigns = or [any (> 0) qs && any (< 0) qs | qs <- map catMaybes $ transpose $ map seValues items]
    drawn = zipWith (\c s -> DrawSeries (colourClass c) s (seriesValues s)) (seriesColours colouring items) items
    (legendh, legendm) = legendSvg (top + headingh) [(dsClass d, seMark (dsSeries d), lbFull $ seLabel $ dsSeries d) | d <- drawn]
                           (if length items > 1 then mtotalmark else Nothing) hascontra (suLabel <$> maverage)
    hascontra = or [isContra s q | s <- items, seMark s == MarkBar, Just q <- seValues s]
    (bodyh, bodym) = case xaxis of
      XCategories ls -> categoriesSvg (svgPlotHeight opts) (top + headingh + legendh) pnScale pnUnit drawn mtotal ls
      XPeriods ps    -> timeSvg opts (top + headingh + legendh) pnScale pnLayout pnUnit drawn mtotalmark averageline (EvenScale $ map snd ps)
      XDates ds      -> timeSvg opts (top + headingh + legendh) pnScale pnLayout pnUnit drawn mtotalmark Nothing (DateScale ds)
    averageline = (\su -> (suLabel su <> " · " <> showSummary su, aquantity $ suValue su)) <$> maverage

-- | A legend: a swatch and label for each series, flowing left to right, when there are several series.
-- If there are contra amounts, a hatched swatch is shown for them too.
-- If an average line is drawn (with this label), a dashed swatch is shown for it, even when there is only one series.
legendSvg :: Double -> [(Text, Mark, Text)] -> Maybe Series -> Bool -> Maybe Text -> (Double, Svg)
legendSvg top entries mtotal hascontra maveragelabel
  | null allentries = (0, mempty)
  | otherwise = (fromIntegral (length rows) * legendRowHeight + 8, mapM_ drawRow (zip [0 ..] rows))
  where
    allentries
      | length entries < 2 = averageentry
      | otherwise = entries
          ++ [("total", if seMark t == MarkLine || all (\(_, mk, _) -> mk == MarkLine) entries then MarkLine else MarkMarker, lbFull (seLabel t)) | Just t <- [mtotal]]
          ++ averageentry
          ++ [(contraClass, MarkBar, "contra") | hascontra]
    averageentry = [(averageClass, MarkLine, lbl) | Just lbl <- [maveragelabel]]
    contraClass = "contra"
    averageClass = "average"
    itemWidth (_, _, lbl) = 20 + textWidth lbl + 18
    rows = reverse $ map reverse $ foldl place [] allentries
    place [] e = [[e]]
    place (r:rs) e
      | sum (map itemWidth r) + itemWidth e <= chartWidth - 2 * pad = (e : r) : rs
      | otherwise = [e] : r : rs
    drawRow :: (Int, [(Text, Mark, Text)]) -> Svg
    drawRow (i, row) = do
      let y = top + fromIntegral i * legendRowHeight + 10
          xs = scanl (+) pad (map itemWidth row)
      forM_ (zip xs row) $ \(x, (cls, mk, lbl)) -> do
        case mk of
          MarkLine | cls == averageClass ->  -- a dashed line, a little longer so that it ends with a dash
                          S.line ! A.class_ "average" ! A.x1 (toValue $ num x) ! A.x2 (toValue $ num (x + 14))
                          ! A.y1 (toValue $ num y) ! A.y2 (toValue $ num y)
          MarkLine   -> S.line ! A.class_ ("line " <> toValue cls) ! A.x1 (toValue $ num x) ! A.x2 (toValue $ num (x + 12))
                          ! A.y1 (toValue $ num y) ! A.y2 (toValue $ num y)
          MarkMarker -> S.line ! A.class_ ("mark " <> toValue cls) ! A.x1 (toValue $ num x) ! A.x2 (toValue $ num (x + 12))
                          ! A.y1 (toValue $ num y) ! A.y2 (toValue $ num y)
          MarkBar | cls == contraClass -> do  -- a neutral swatch, hatched
                          swatch x y ! A.class_ ("fill " <> toValue (colourClass NeutralColour))
                          swatch x y ! A.fill (toValue $ "url(#" <> hatchId <> ")")
          MarkBar    -> swatch x y ! A.class_ ("fill " <> toValue cls)
        S.text_ ! A.class_ "label" ! A.x (toValue $ num (x + 20)) ! A.y (toValue $ num (y + 5)) $ S.toMarkup lbl
    swatch x y = S.rect ! A.x (toValue $ num x) ! A.y (toValue $ num (y - 6)) ! A.width "12" ! A.height "12" ! A.rx "3"

-- | How x positions are placed along a time axis: evenly, with these labels (eg report periods),
-- or by date, on a time scale (eg a register's transactions; lines are then drawn as steps).
data XScale = EvenScale [Text] | DateScale [Day]

-- | Draw series over time, along the given x scale:
-- vertical stacked bars (with the grouped layout, a stack per group, side by side), lines, and total marks or lines.
timeSvg :: SvgOptions -> Double -> Maybe Quantity -> BarLayout -> Amount -> [DrawSeries] -> Maybe Series -> Maybe (Text, Quantity) -> XScale -> (Double, Svg)
timeSvg SvgOptions{svgPlotHeight = plotheight, svgPointLink, svgDateScale, svgToday} top mscale layout unit drawn mtotal maverage xscale = (plotheight + 32, do
    -- the plot's area with its date scale, if wanted, beneath the rest so that their tooltips and links still work
    when (svgDateScale && isJust datearray) $
      S.rect ! A.class_ "plot"
        ! A.x (toValue $ num plotleft) ! A.y (toValue $ num plottop) ! A.width (toValue $ num (plotright - plotleft)) ! A.height (toValue $ num plotheight)
        ! customAttribute "data-d0" (toValue $ showDate d0) ! customAttribute "data-d1" (toValue $ showDate d1)
        ! customAttribute "data-x0" (toValue $ num $ xofday d0) ! customAttribute "data-x1" (toValue $ num $ xofday d1)
    -- the shading as of today, if wanted: the past below zero, and the future, divided halfway between today and tomorrow
    forM_ mtoday $ \today -> do
      let xnow = clamp plotleft plotright $ xofdays (fromIntegral (diffDays today d0) + 0.5)
          yzero = yof 0
          shade cls x y w h = when (w > 0 && h > 0) $ S.rect ! A.class_ ("shade " <> cls)
            ! A.x (toValue $ num x) ! A.y (toValue $ num y) ! A.width (toValue $ num w) ! A.height (toValue $ num h)
      shade "past-negative" plotleft yzero (xnow - plotleft) (plotbottom - yzero)
      shade "future" xnow plottop (plotright - xnow) (yzero - plottop)
      shade "future-negative" xnow yzero (plotright - xnow) (plotbottom - yzero)
    -- gridlines and y tick labels
    forM_ ticks $ \t -> do
      S.line ! A.class_ (if t == 0 then (if isJust mtoday then "axis zero" else "axis") else "grid")
        ! A.x1 (toValue $ num plotleft) ! A.x2 (toValue $ num plotright) ! A.y1 (toValue $ num (yof t)) ! A.y2 (toValue $ num (yof t))
      S.text_ ! A.class_ "tick" ! A.x (toValue $ num (plotleft - 8)) ! A.y (toValue $ num (yof t + 5)) ! A.textAnchor "end" $ S.toMarkup (ticklabel t)
    -- x labels: periods' labels, thinned to avoid overlap; or ticks at calendar boundaries
    forM_ (placeXLabels shownxlabels) $ \(x, lbl) ->
      S.text_ ! A.class_ "tick" ! A.x (toValue $ num x) ! A.y (toValue $ num (plotbottom + 21)) ! A.textAnchor "middle" $ S.toMarkup lbl
    -- bars: the stacks side by side (2px apart), each with its positive segments stacked upward and negative ones downward
    forM_ (zip [0 ..] xlabels) $ \(j, lbl) ->
      forM_ (zip [0 :: Int ..] stacks) $ \(k, stk) -> do
        let x = xcenter j - groupwidth / 2 + fromIntegral k * (barwidth + 2)
        stack lbl x (contraFirst [(d, q) | d <- stk, Just q <- [valueAt (dsValues d) j], q > 0]) 1
        stack lbl x (contraFirst [(d, q) | d <- stk, Just q <- [valueAt (dsValues d) j], q < 0]) (-1)
    -- a hatching pattern, for contra amounts
    when hascontra $ S.defs $ S.pattern ! A.id_ (toValue hatchId) ! A.width "5" ! A.height "5" ! A.patternunits "userSpaceOnUse" ! A.patterntransform "rotate(45)" $
      S.line ! A.class_ "hatch" ! A.x1 "0" ! A.y1 "0" ! A.x2 "0" ! A.y2 "5"
    -- with a fixed scale, lines are clipped at the plot's edges
    when (isJust mscale) $ S.defs $ S.clippath ! A.id_ (toValue clipid) $
      S.rect ! A.x (toValue $ num plotleft) ! A.y (toValue $ num plottop) ! A.width (toValue $ num (plotright - plotleft)) ! A.height (toValue $ num plotheight)
    let clipLines = if isJust mscale then S.g ! A.clipPath (toValue $ "url(#" <> clipid <> ")") else id
    -- lines
    clipLines $ forM_ lines_ $ \d -> lineSvg (dsClass d) (dsSeries d)
    -- the total: as a line, or as a mark (a thin horizontal line) across each period's bars
    forM_ mtotal $ \t ->
      if seMark t == MarkLine || null bars then clipLines $ lineSvg "total" t
      else let tvals = seriesValues t in
        forM_ (zip [0 ..] xlabels) $ \(j, lbl) -> forM_ (valueAt tvals j) $ \q -> when (hasbars j) $ withTooltip (tooltip lbl t q) $ do
          let y = toValue $ num (yof $ toD q)
              halfw = groupwidth / 2 + 4
              x1 = toValue $ num (xcenter j - halfw)
              x2 = toValue $ num (xcenter j + halfw)
          S.line ! A.class_ "mark total" ! A.x1 x1 ! A.x2 x2 ! A.y1 y ! A.y2 y
    -- the average, if any: a dashed line across the plot
    forM_ maverage $ \(tip, q) -> withTooltip tip $ do
      let y = toValue $ num (yof $ toD q)
      S.line ! A.class_ "average" ! A.x1 (toValue $ num plotleft) ! A.x2 (toValue $ num plotright) ! A.y1 y ! A.y2 y
  )
  where
    xlabels = case xscale of
      EvenScale ls -> ls
      DateScale ds -> map showDate ds
    n = length xlabels
    -- the x labels shown: for periods, every k'th label, with the smallest k that leaves them all room
    -- (keeping them evenly spaced, even when the end ones are moved inward)
    shownxlabels = case xscale of
      EvenScale _ -> fromMaybe [] $ find fitsall [everykth k | k <- [labelevery .. max labelevery n]]
      DateScale _ -> [(xofday d, lbl) | (d, lbl) <- dateTicks d0 d1]
    everykth k = [(xcenter j, lbl) | (j, lbl) <- zip [0 :: Int ..] xlabels, j `mod` k == 0]
    fitsall ls = length (placeXLabels ls) == length ls
    -- the date scale, if any: its dates (as an array, for fast lookup), first and last dates, and a date's x position (inset a little from the plot's edges)
    datearray = case xscale of
      DateScale ds -> Just $ Arr.listArray (0, length ds - 1) ds
      EvenScale _  -> Nothing
    (d0, d1) = case xscale of
      DateScale ds@(d:_) -> (minimumDef d ds, maximumDef d ds)
      _                  -> (fromGregorian 2000 1 1, fromGregorian 2000 1 1)
    xofday d = xofdays $ fromIntegral (diffDays d d0)
    xofdays k = plotleft + 8 + (plotright - plotleft - 16) * k / fromIntegral (max 1 (diffDays d1 d0))
    -- today, if the plot is to be shaded as of it
    mtoday = if isJust datearray then svgToday else Nothing
    bars   = [d | d <- drawn, seMark (dsSeries d) == MarkBar]
    lines_ = [d | d <- drawn, seMark (dsSeries d) == MarkLine]
    -- the bar series in stacks: one, or with the grouped layout, one per group (see 'seGroup')
    stacks = case layout of
      Grouped   -> [ [d | d <- bars, seGroup (dsSeries d) == k] | k <- nub $ map (seGroup . dsSeries) bars ]
      Diverging -> [bars]
    -- the extent of the bars in each direction: each stack's sum, at each x position
    stackedsums sign = [ sum [toD q | d <- stk, Just q <- [valueAt (dsValues d) j], signum q == sign] | j <- [0 .. n - 1], stk <- stacks ]
    hasbars j = any (\d -> maybe False (/= 0) $ valueAt (dsValues d) j) bars
    hascontra = or [isContra (dsSeries d) q | d <- bars, Just q <- seValues (dsSeries d)]
    -- contra amounts are drawn nearest the axis
    contraFirst = sortOn (\(d, q) -> not $ isContra (dsSeries d) q)
    pointvalues = [toD q | s <- map dsSeries lines_ ++ maybe [] pure mtotal, Just q <- seValues s]
    datamax = maximumDef 0 (stackedsums 1 ++ pointvalues)
    datamin = minimumDef 0 (stackedsums (-1) ++ pointvalues)
    (lo, hi, ticks, tickdecimals) = panelScale (plotIntervals plotheight) mscale (min 0 datamin) (max 0 datamax)
    ticklabel = tickLabel unit tickdecimals ticks
    plotleft = pad + maximumDef 0 (map (textWidth . ticklabel) ticks) + 12
    plotright = chartWidth - pad
    plottop = top + 6
    plotbottom = plottop + plotheight
    -- a value's y position, within the plot (for bars and marks), or anywhere (for lines, which are clipped)
    yof = clamp plottop plotbottom . yofline
    yofline v = plottop + plotheight * (hi - v) / (hi - lo)
    -- an id for the plot's clipping path, unique within the chart
    clipid = "hledger-chart-clip-" <> num top
    band = (plotright - plotleft) / fromIntegral (max 1 n)
    xcenter j = case datearray of
      Just a | Arr.inRange (Arr.bounds a) j -> xofday (a Arr.! j)
      _ -> plotleft + band * (fromIntegral j + 0.5)
    nbars = max 1 $ length stacks
    -- each bar's width, and the width of a group of side-by-side bars (2px apart)
    barwidth
      | band < 4  = max 1 (band * 0.9 / fromIntegral nbars)
      | otherwise = max 1 $ min 56 ((band * 0.75 - 2 * fromIntegral (nbars - 1)) / fromIntegral nbars)
    groupwidth = fromIntegral nbars * barwidth + 2 * fromIntegral (nbars - 1)
    labelevery = max 1 $ ceiling ((maximumDef 0 (map textWidth xlabels) + 12) / band) :: Int
    stack :: Text -> Double -> [(DrawSeries, Quantity)] -> Double -> Svg
    stack lbl x segs = vstack (lo, hi) yof x barwidth [(d, q, tooltip lbl (dsSeries d) q) | (d, q) <- segs]
    lineSvg :: Text -> Series -> Svg
    lineSvg cls s = do
      -- runs of consecutive values, so that gaps break the line
      let pts = [ fmap (\q -> (j, xcenter j, yofline (toD q), q)) (valueAt vals j) | let vals = seriesValues s, j <- [0 .. n - 1] ]
      forM_ (runs pts) $ \run ->
        S.polyline ! A.class_ ("line " <> toValue cls)
          ! A.points (toValue $ T.unwords [num x <> "," <> num y | (x, y) <- thinPoints $ (if isJust datearray then steps else id) [(x, y) | (_, x, y, _) <- run]])
      -- invisible larger targets, for tooltips, and links if any
      forM_ (hitPoints $ catMaybes pts) $ \(j, x, y, q) -> linked j $ withTooltip (tooltip (fromMaybe "" $ xlabels `atMay` j) s q) $
        S.circle ! A.class_ "hit" ! A.cx (toValue $ num x) ! A.cy (toValue $ num y) ! A.r "6"
    -- some markup for the given x position, as a link if it has one
    linked j m = case svgPointLink j of
      Just url -> S.a ! customAttribute "href" (toValue url) ! customAttribute "tabindex" "-1" $ m
      Nothing  -> m
    -- The points of a line given tooltip targets: all of them, or beyond 400 x positions, the last point in each 4px column,
    -- which keeps the SVG small while the targets (12px across) still cover the whole line.
    hitPoints ps
      | n <= 400  = ps
      | otherwise = M.elems $ M.fromList [(floor (x / 4) :: Integer, p) | p@(_, x, _, _) <- ps]
    tooltip :: Text -> Series -> Quantity -> Text
    tooltip lbl s q = T.intercalate " · " (filter (not . T.null) [lbl, lbFull (seLabel s), showValue unit q])

-- | Place x axis labels: centred at their x positions, but kept within the chart,
-- leaving out any that would overlap the previous one (eg after being moved inward from the chart's edge).
placeXLabels :: [(Double, Text)] -> [(Double, Text)]
placeXLabels = reverse . foldl' place []
  where
    place kept (x, lbl) =
      let halfw = textWidth lbl / 2
          x' = max (pad + halfw) $ min (chartWidth - pad - halfw) x
      in case kept of
           (px, plbl) : _ | x' - halfw < px + textWidth plbl / 2 + 12 -> kept
           _ -> (x', lbl) : kept

-- | Draw series of categories (eg accounts): as vertical bars, or as horizontal bars when there are more than 'maxColumns' categories.
-- Categories with no values at all (eg accounts without this commodity) are left out.
-- With a single series, each category's bar has its own account type's colour, if known.
categoriesSvg :: Double -> Double -> Maybe Quantity -> Amount -> [DrawSeries] -> Maybe Series -> [ChartLabel] -> (Double, Svg)
categoriesSvg plotheight top mscale unit drawn mtotal labels
  | length rows <= maxColumns = columnsSvg plotheight top mscale unit rows rowValue
  | otherwise                 = barsSvg top mscale unit rows rowValue
  where
    rows = [ (l, j, [(categoryColoured l d, q) | d <- drawn, Just q <- [valueAt (dsValues d) j]])
           | (j, l) <- zip [0 ..] labels, not $ all (isNothing . flip valueAt j . dsValues) drawn ]
    categoryColoured lbl d = case (drawn, lbType lbl) of
      ([_], Just at) -> d{dsClass = colourClass $ TypeColour (mainAccountType at) 0}
      _              -> d
    mtotalvals = seriesValues <$> mtotal
    -- the value to show for each category: the total's, or the single series'
    rowValue j = case (mtotal, drawn) of
      (Just _, _) -> mtotalvals >>= flip valueAt j
      (_, [d])    -> valueAt (dsValues d) j
      _           -> Nothing

-- | The most categories to draw as vertical bars; beyond this, they are drawn as horizontal bars (rows), which can show many more names.
maxColumns :: Int
maxColumns = 30

-- | Draw categories as vertical bars (stacked if several series), with their names below (angled if they don't fit),
-- and their values above the bars (when they fit), in a plot this high.
columnsSvg :: Double -> Double -> Maybe Quantity -> Amount -> [(ChartLabel, Int, [(DrawSeries, Quantity)])] -> (Int -> Maybe Quantity) -> (Double, Svg)
columnsSvg plotheight top mscale unit rows rowValue = (plotheight + 22 + negroom + labelheight, do
    -- gridlines and y tick labels
    forM_ ticks $ \t -> do
      S.line ! A.class_ (if t == 0 then "axis" else "grid")
        ! A.x1 (toValue $ num plotleft) ! A.x2 (toValue $ num plotright) ! A.y1 (toValue $ num (yof t)) ! A.y2 (toValue $ num (yof t))
      S.text_ ! A.class_ "tick" ! A.x (toValue $ num (plotleft - 8)) ! A.y (toValue $ num (yof t + 5)) ! A.textAnchor "end" $ S.toMarkup (ticklabel t)
    forM_ (zip [0 :: Int ..] rows) $ \(i, (lbl, j, segs)) -> do
      let x = xcenter i
          name = shortName lbl
          ly = plotbottom + negroom + 16
      -- the category's name, below; angled (reading upward to the bar) if the names don't fit
      if angled
      then S.text_ ! A.class_ "label" ! A.x (toValue $ num (x + 4)) ! A.y (toValue $ num ly) ! A.textAnchor "end"
             ! A.transform (toValue $ "rotate(-45 " <> num (x + 4) <> " " <> num ly <> ")") $ S.toMarkup name
      else S.text_ ! A.class_ "label" ! A.x (toValue $ num x) ! A.y (toValue $ num ly) ! A.textAnchor "middle" $ S.toMarkup name
      -- the bar
      let tip d q = T.intercalate " · " $ nub [lbFull lbl, lbFull (seLabel $ dsSeries d), showValue unit q]
          bx = x - barwidth / 2
      vstack (lo, hi) yof bx barwidth [(d, q, tip d q) | (d, q) <- segs, q > 0] 1
      vstack (lo, hi) yof bx barwidth [(d, q, tip d q) | (d, q) <- segs, q < 0] (-1)
      -- the value, above the bar (or below, if negative), if it fits
      forM_ (rowValue j) $ \q -> do
        let v = showValue unit q
            pos = sum [toD a | (_, a) <- segs, a > 0]
            neg = sum [toD a | (_, a) <- segs, a < 0]
            cliproom = if pos > hi || neg < lo then 6 else 0  -- room for a clip mark
            vy = if q >= 0 then yof pos - 6 - cliproom else yof neg + 14 + cliproom
        when (textWidth v <= band - 2) $
          S.text_ ! A.class_ "value" ! A.x (toValue $ num x) ! A.y (toValue $ num vy) ! A.textAnchor "middle" $ S.toMarkup v
  )
  where
    n = length rows
    stackedsums sign = [sum [toD q | (_, q) <- segs, signum q == sign] | (_, _, segs) <- rows]
    (lo, hi, ticks, tickdecimals) = panelScale (plotIntervals plotheight) mscale (minimumDef 0 $ stackedsums (-1)) (maximumDef 0 $ stackedsums 1)
    ticklabel = tickLabel unit tickdecimals ticks
    -- the plot's left edge: after the tick labels, and far enough right that angled names don't extend beyond the chart's left edge
    ticksright = pad + maximumDef 0 (map (textWidth . ticklabel) ticks) + 12
    plotleft
      | angled    = max ticksright $ maximumDef 0 [pad + 0.71 * textWidth (shortName l) - 4 - bandwith ticksright * (fromIntegral i + 0.5) | (i, (l, _, _)) <- zip [0 :: Int ..] rows]
      | otherwise = ticksright
    plotright = chartWidth - pad
    plottop = top + 20  -- leaving room for values above the bars
    plotbottom = plottop + plotheight
    yof v = plottop + plotheight * (hi - clamp lo hi v) / (hi - lo)
    -- the width of each category's band, with the plot starting at this x position
    bandwith left = (plotright - left) / fromIntegral (max 1 n)
    band = bandwith plotleft
    xcenter i = plotleft + band * (fromIntegral i + 0.5)
    barwidth = max 1 $ min 64 (band * 0.6)
    -- names: full account names (since there's no indentation), shortened if very long
    shortName l = let t = lbFull l in if T.length t > 30 then T.take 29 t <> "…" else t
    namewidth = maximumDef 0 [textWidth (shortName l) | (l, _, _) <- rows]
    -- whether names are angled: decided with the widest band (before any room is made for angled names), to avoid a cycle
    angled = namewidth > bandwith ticksright - 4
    labelheight = if angled then namewidth * 0.71 + 16 else 24
    -- room below the plot for the values of negative bars, which may reach its bottom
    negroom = if any (any ((< 0) . snd) . (\(_, _, segs) -> segs)) rows then (if isJust mscale then 22 else 16) else 0

-- | Draw categories as horizontal bars (rows; stacked if several series), with their names on the left (indented as in the report)
-- and an aligned value column on the right.
barsSvg :: Double -> Maybe Quantity -> Amount -> [(ChartLabel, Int, [(DrawSeries, Quantity)])] -> (Int -> Maybe Quantity) -> (Double, Svg)
barsSvg top mscale unit rows rowValue = (fromIntegral (length rows) * rowHeight + 32, do
    -- gridlines and x tick labels (labels thinned to avoid overlap, always including zero)
    forM_ ticks $ \t -> do
      S.line ! A.class_ (if t == 0 then "axis" else "grid")
        ! A.x1 (toValue $ num (xof t)) ! A.x2 (toValue $ num (xof t)) ! A.y1 (toValue $ num plottop) ! A.y2 (toValue $ num plotbottom)
      when (round (t / tickstep) `mod` labelevery == (0 :: Integer)) $
        S.text_ ! A.class_ "tick" ! A.x (toValue $ num (xof t)) ! A.y (toValue $ num (plotbottom + 21)) ! A.textAnchor "middle" $ S.toMarkup (ticklabel t)
    forM_ (zip [0 :: Int ..] rows) $ \(i, (lbl, j, segs)) -> do
      let ymid = plottop + rowHeight * (fromIntegral i + 0.5)
      S.text_ ! A.class_ "label" ! A.x (toValue $ num (pad + 12 * fromIntegral (lbIndent lbl))) ! A.y (toValue $ num (ymid + 5))
        $ S.toMarkup (lbShort lbl)
      forM_ (rowValue j) $ \q ->
        S.text_ ! A.class_ "value" ! A.x (toValue $ num (chartWidth - pad)) ! A.y (toValue $ num (ymid + 5)) ! A.textAnchor "end"
          $ S.toMarkup (showValue unit q)
      stack lbl ymid [(d, q) | (d, q) <- segs, q > 0] 1
      stack lbl ymid [(d, q) | (d, q) <- segs, q < 0] (-1)
  )
  where
    stackedsums sign = [sum [toD q | (_, q) <- segs, signum q == sign] | (_, _, segs) <- rows]
    (lo, hi, ticks, tickdecimals) = panelScale 5 mscale (minimumDef 0 $ stackedsums (-1)) (maximumDef 0 $ stackedsums 1)
    ticklabel = tickLabel unit{acommodity = ""} tickdecimals []
    labelwidth = min (chartWidth * 0.4) $ maximumDef 0 [12 * fromIntegral (lbIndent l) + textWidth (lbShort l) | (l, _, _) <- rows]
    valuewidth = maximumDef 0 [textWidth (showValue unit q) | (_, j, _) <- rows, Just q <- [rowValue j]]
    -- leave room for half of a tick label at each end
    edge = maximumDef 0 (map (textWidth . ticklabel) ticks) / 2
    plotleft = pad + max labelwidth edge + 12
    plotright = chartWidth - pad - max valuewidth edge - 16
    plottop = top + 4
    plotbottom = plottop + rowHeight * fromIntegral (length rows)
    xof v = plotleft + (plotright - plotleft) * (clamp lo hi v - lo) / (hi - lo)
    tickstep = case ticks of t1:t2:_ -> t2 - t1; _ -> 1
    labelevery = max 1 $ ceiling ((maximumDef 0 (map (textWidth . ticklabel) ticks) + 12) / max 1 (xof tickstep - xof 0)) :: Integer
    thickness = 16
    stack :: ChartLabel -> Double -> [(DrawSeries, Quantity)] -> Double -> Svg
    stack lbl ymid segs sign = do
      let cums = scanl1 (+) (map (toD . snd) segs)
          starts = 0 : cums
          clipped = any (\c -> c > hi || c < lo) (lastMay cums)
          outer = maybe (xof 0) xof (lastMay cums)
      roundedEnd (not clipped) False (xof 0) (ymid - thickness / 2) thickness (outer - xof 0) $
        forM_ (zip3 [0 :: Int ..] segs (zip starts cums)) $ \(k, (d, q), (c0, c1)) -> do
          let gap = if k > 0 && abs (xof c1 - xof c0) >= 4 then 2 else 0  -- no gap for tiny segments, which it would hide
              xbase = xof c0 + sign * gap   -- the end nearer the axis, after the gap (the sums are negative for negative segments)
              len = (xof c1 - xbase) * sign
          when (len > 0.5) $ withTooltip (T.intercalate " · " [lbFull lbl, lbFull (seLabel $ dsSeries d), showValue unit q]) $
            S.path ! A.class_ ("fill " <> toValue (dsClass d))
              ! A.d (toValue $ barPath False xbase (ymid - thickness / 2) thickness (sign * len) False)
      -- a bar clipped by a fixed scale has a small triangle beyond its end
      when clipped $ clipMark False (xof $ if sign > 0 then hi else lo) ymid thickness sign

-- | One direction of a vertical stacked bar, with its left edge at x and the given width, on a y scale:
-- segments (each with its series, value and tooltip) from zero outward (up if sign is 1, down if -1),
-- 2px apart (except tiny ones, which a gap would hide), with a rounded end (see 'roundedEnd').
-- If the bar extends beyond the y scale's range (lo, hi), it is clipped, with a small triangle beyond its end instead.
vstack :: (Double, Double) -> (Double -> Double) -> Double -> Double -> [(DrawSeries, Quantity, Text)] -> Double -> Svg
vstack (lo, hi) yof x width segs sign = do
  let cums = scanl1 (+) [toD q | (_, q, _) <- segs]
      starts = 0 : cums
      clipped = any (\c -> c > hi || c < lo) (lastMay cums)
      outer = maybe (yof 0) yof (lastMay cums)
  roundedEnd (not clipped) True x (yof 0) width (outer - yof 0) $
    forM_ (zip3 [0 :: Int ..] segs (zip starts cums)) $ \(k, (d, q, tip), (c0, c1)) -> do
      let gap = if k > 0 && abs (yof c1 - yof c0) >= 4 then 2 else 0
          ybase = yof c0 - sign * gap   -- the end nearer zero, after the gap
          len = (ybase - yof c1) * sign  -- positive when there is something to draw
          path = toValue $ barPath True x ybase width (-sign * len) False
      when (len > 0.5) $ withTooltip tip $ do
        S.path ! A.class_ ("fill " <> toValue (dsClass d)) ! A.d path
        -- a contra amount is hatched
        when (isContra (dsSeries d) q) $ S.path ! A.fill (toValue $ "url(#" <> hatchId <> ")") ! A.d path
  when clipped $ clipMark True (yof $ if sign > 0 then hi else lo) (x + width / 2) width sign

-- | Draw a bar's segments (if rounded) clipped to the bar's outline with a rounded outer end, so that the end
-- is rounded the same however thin its outermost segment. The bar is vertical or horizontal, with its base at a, b
-- (its left or top edge, at the axis), the given size across, and length (negative for up or left).
-- The clipping path's id, unique within the chart, is made from these (with the prefix 'run' makes unique within its pages).
roundedEnd :: Bool -> Bool -> Double -> Double -> Double -> Double -> Svg -> Svg
roundedEnd rounded vertical a b size len segments
  | not rounded || abs len < 0.5 = segments
  | otherwise = do
      S.defs $ S.clippath ! A.id_ (toValue clipid) $ S.path ! A.d (toValue $ barPath vertical a b size len True)
      S.g ! A.clipPath (toValue $ "url(#" <> clipid <> ")") $ segments
  where
    clipid = "hledger-chart-clip-bar" <> T.intercalate "_" [if vertical then "v" else "h", num a, num b, num len]

-- | The id of the hatching pattern for contra amounts.
hatchId :: Text
hatchId = "hledger-chart-hatch"

-- | A small triangle pointing outward from the edge of a plot, marking a bar that was clipped there:
-- for a vertical bar (at y position edge, centred on x position mid) or a horizontal one (at x position edge, centred on y position mid).
-- sign is the direction in which the bar grows (1: up or right, -1: down or left).
clipMark :: Bool -> Double -> Double -> Double -> Double -> Svg
clipMark vertical edge mid barsize sign =
  S.polygon ! A.class_ "clip" ! A.points (toValue $ T.unwords [num a <> "," <> num b | (a, b) <- map orient pts])
  where
    w = max 6 $ min 10 barsize
    -- the points as (position across the bar, distance outward from the edge)
    pts = [(mid - w / 2, 0), (mid + w / 2, 0), (mid, w / 2)]
    orient (across, out)
      | vertical  = (across, edge - sign * out)
      | otherwise = (edge + sign * out, across)

-- | The points of a step line through these points: each value holds until the next point's x position.
steps :: [(Double, Double)] -> [(Double, Double)]
steps ps = take 1 ps ++ concat (zipWith (\(_, py) (x, y) -> [(x, py), (x, y)]) ps (drop 1 ps))

-- | Reduce a line's points to at most four per pixel column (the first, lowest, highest and last, in their original order),
-- which draws the same line, but keeps the SVG small when there are many more points than pixels (eg a long register).
thinPoints :: [(Double, Double)] -> [(Double, Double)]
thinPoints = concatMap thin . groupBy ((==) `on` (column . fst))
  where
    column :: Double -> Integer
    column = round
    thin ps
      | length ps <= 4 = ps
      | otherwise = M.elems $ M.fromList $ catMaybes [headMay indexed, extreme (<), extreme (>), lastMay indexed]
      where
        indexed = zip [0 :: Int ..] ps
        -- the first point whose y is better than all others' by this comparison
        extreme better = foldl' (\acc p -> case acc of
                                   Just (_, (_, y')) | not (snd (snd p) `better` y') -> acc
                                   _ -> Just p) Nothing indexed

-- | Up to 8 ticks for a date axis from the first to the last date, at calendar boundaries
-- (every 1, 2, 7 or 14 days, every 1, 2, 3 or 6 months, or every 1, 2, 5, 10.. years), with labels.
dateTicks :: Day -> Day -> [(Day, Text)]
dateTicks d0 d1 = fromMaybe [] $ find ((<= 8) . length) candidates
  where
    (y0, _, _) = toGregorian d0
    (y1, _, _) = toGregorian d1
    candidates = [dayticks k | k <- [1, 2, 7, 14]] ++ [monthticks k | k <- [1, 2, 3, 6]] ++ [yearticks k | k <- [1, 2, 5, 10, 20, 50, 100, 1000]]
    inrange d = d >= d0 && d <= d1
    dayticks k = [(d, showDate d) | d <- takeWhile (<= d1) $ iterate (addDays k) d0]
    monthticks k = [ (d, T.pack $ printf "%04d-%02d" y m)
                   | y <- [y0 .. y1], m <- [1 .. 12], (m - 1) `mod` k == 0, let d = fromGregorian y m 1, inrange d ]
    yearticks k = [ (d, T.pack $ show y) | y <- [y0 .. y1], y `mod` k == 0, let d = fromGregorian y 1 1, inrange d ]

-- | Give some markup a tooltip, by grouping it with an SVG title.
withTooltip :: Text -> Svg -> Svg
withTooltip t m = S.g $ S.title (S.toMarkup t) >> m

-- | An SVG path for a bar (segment) growing from a base line, with its far end rounded (radius up to 4px) if requested.
-- Vertical: base line at y=b, from x=a with the given width, growing by len pixels (negative is upward, since y grows downward).
-- Horizontal: base line at x=a, from y=b with the given thickness, growing by len pixels (positive is rightward).
barPath :: Bool -> Double -> Double -> Double -> Double -> Bool -> Text
barPath vertical a b size len rounded
  | vertical =
      let e = b + len in T.unwords
      [ "M", num a, num b
      , "V", num (e - sgn * r)
      , "Q", num a, num e, num (a + r), num e
      , "H", num (a + size - r)
      , "Q", num (a + size), num e, num (a + size), num (e - sgn * r)
      , "V", num b, "Z" ]
  | otherwise =
      let e = a + len in T.unwords
      [ "M", num a, num b
      , "H", num (e - sgn * r)
      , "Q", num e, num b, num e, num (b + r)
      , "V", num (b + size - r)
      , "Q", num e, num (b + size), num (e - sgn * r), num (b + size)
      , "H", num a, "Z" ]
  where
    sgn = if len < 0 then -1 else 1
    r = if rounded then minimum [3, size / 2, abs len] else 0

-- | The scale for a panel's values, given their range (which should include 0), divided into about this many intervals:
-- a nice scale covering it ('niceScale'), or with a fixed scale, exactly the range it spans ('scaleSpans'),
-- with the nice scale's ticks that are within it.
panelScale :: Int -> Maybe Quantity -> Double -> Double -> (Double, Double, [Double], Int)
panelScale intervals mscale datamin datamax = case mscale of
  Just s | s > 0 ->
    let (hi, neg) = scaleSpans (toD s) (max 0 datamax) (abs $ min 0 datamin)
        lo = negate neg
        (_, _, ticks, decimals) = niceScale intervals lo hi
        eps = (hi - lo) * 1e-9
    in (lo, hi, filter (\t -> t >= lo - eps && t <= hi + eps) ticks, decimals)
  _ -> niceScale intervals datamin datamax

-- | Limit a value to a range.
clamp :: Double -> Double -> Double -> Double
clamp lo hi = max lo . min hi

-- | A "nice" linear scale covering the given range (which should include 0), in about this many intervals:
-- its low and high ends, its ticks (at multiples of 1, 2 or 5 times a power of ten),
-- and the number of decimal places needed to show the ticks.
niceScale :: Int -> Double -> Double -> (Double, Double, [Double], Int)
niceScale intervals lo0 hi0 = (lo, hi, ticks, decimals)
  where
    (a, b) = if hi0 - lo0 <= 0 then (lo0, lo0 + 1) else (lo0, hi0)
    -- choose the 1, 2 or 5 times a power of ten nearest the step that would give that many intervals (as d3 does)
    rawstep = (b - a) / fromIntegral intervals
    magnitude = 10 ** fromIntegral (floor (logBase 10 rawstep) :: Int)
    err = rawstep / magnitude
    step | err >= sqrt 50 = 10 * magnitude
         | err >= sqrt 10 = 5 * magnitude
         | err >= sqrt 2  = 2 * magnitude
         | otherwise      = magnitude
    kmin = floor (a / step) :: Integer
    kmax = ceiling (b / step) :: Integer
    ticks = [fromIntegral k * step | k <- [kmin .. kmax]]
    lo = fromIntegral kmin * step
    hi = fromIntegral kmax * step
    decimals = max 0 $ negate (floor (logBase 10 step + 1e-9) :: Int)

-- | The value of a tick, as a quantity with the given number of decimal places.
-- | A y axis tick's label: its value in the unit's style, with this many decimal places.
-- To reduce clutter, the unit's commodity is shown only on the outermost of these ticks
-- (the top one, or the bottom one if the top one is zero).
tickLabel :: Amount -> Int -> [Double] -> Double -> Text
tickLabel unit decimals ticks t = showValue (withPrecision decimals u) (tickQuantity decimals t)
  where
    u = if Just t == unittick then unit else unit{acommodity = ""}
    unittick = case lastMay ticks of
      Just top | top /= 0 -> Just top
      _                   -> headMay ticks

tickQuantity :: Int -> Double -> Quantity
tickQuantity decimals = realFracToDecimal (fromIntegral decimals)

-- | Show a quantity as an amount in the given unit (commodity and style).
showValue :: Amount -> Quantity -> Text
showValue unit q = T.pack $ showAmountWith oneLineNoCostFmt unit{aquantity = q}

-- | The unit with its display precision set to this many decimal places.
withPrecision :: Int -> Amount -> Amount
withPrecision d unit = unit{astyle = (astyle unit){asprecision = Precision (fromIntegral d)}}

toD :: Quantity -> Double
toD = realToFrac

-- | A coordinate for SVG, rounded to one decimal place, without a trailing ".0".
num :: Double -> Text
num x
  | r == 0    = "0"
  | otherwise = let t = T.pack (printf "%.1f" r) in fromMaybe t (T.stripSuffix ".0" t)
  where r = fromIntegral (round (x * 10) :: Integer) / 10 :: Double

-- | The runs of consecutive Just values in a list.
runs :: [Maybe a] -> [[a]]
runs ms = case dropWhile isNothing ms of
  [] -> []
  rest -> let (run, more) = span (not . isNothing) rest in catMaybes run : runs more

-- | The CSS class for a colour. (The rules are in 'chartStylesheet'.)
colourClass :: Colour -> Text
colourClass c = case c of
  TypeColour at k -> "t" <> typeCode (mainAccountType at) <> T.pack (show (k `mod` maxTints))
  IndexColour k   -> "s" <> T.pack (show (k `mod` 8))
  LoneColour      -> "lone"
  NeutralColour   -> "neutral"
  OtherColour Nothing   -> "other"
  OtherColour (Just at) -> "o" <> typeCode (mainAccountType at)
  TotalColour     -> "total"

-- | A short code for each main account type, for CSS class names.
typeCode :: AccountType -> Text
typeCode at = case at of
  Asset     -> "a"
  Liability -> "l"
  Equity    -> "e"
  Revenue   -> "r"
  Expense   -> "x"
  _         -> "e"

-- | The chart's styles: a colour-blind-safe categorical palette (8 slots) and chart chrome colours,
-- with a dark variant used when the viewer prefers a dark colour scheme.
-- Colours are written literally in each rule (rather than as CSS custom properties),
-- since some SVG renderers (eg librsvg, used by many image viewers) don't support those.
-- Charts include these styles, unless drawn without them for a page that supplies them (see 'SvgOptions').
chartStylesheet :: Bool -> Text
chartStylesheet dark = T.unlines $
  [ ".hledger-chart{max-width:100%;height:auto;font-family:system-ui,-apple-system,\"Segoe UI\",sans-serif;font-size:14px}"
  , ".hledger-chart .title{font-size:17px;font-weight:600}"
  , ".hledger-chart .heading{font-size:15px;font-weight:600}"
  , ".hledger-chart .value,.hledger-chart .tick{font-variant-numeric:tabular-nums}"
  , ".hledger-chart .grid,.hledger-chart .axis{stroke-width:1}"
  , ".hledger-chart .line{fill:none;stroke-width:3;stroke-linejoin:round;stroke-linecap:round}"
  , ".hledger-chart .mark{stroke-width:2.5}"
  , ".hledger-chart .average{stroke-width:2;stroke-dasharray:5 4}"
  , ".hledger-chart .hit,.hledger-chart .plot{fill:transparent}"
  , ".hledger-chart .hatch{stroke-width:2.5}"
  ]
  ++ colourRules False lightcolours
  ++ (if dark then ["@media (prefers-color-scheme:dark){"] ++ colourRules True darkcolours ++ ["}"] else [])
  where
    -- the chart chrome's colours, and the series' colours (from "Hledger.Write.Chart.Palette")
    colourRules isdark cs =
      [ ".hledger-chart " <> sel <> "{" <> prop <> ":" <> colour <> "}"
      | (sel, prop, role) <- rules, Just colour <- [lookup role cs] ]
      ++ concat [ [ ".hledger-chart .fill." <> cls <> "{fill:" <> hex <> "}", ".hledger-chart .line." <> cls <> "{stroke:" <> hex <> "}" ]
                | c <- seriescolours, let cls = colourClass c, let hex = colourHex isdark c ]
    seriescolours =
      [TypeColour at k | at <- [Asset, Liability, Equity, Revenue, Expense], k <- [0 .. maxTints - 1]]
      ++ map IndexColour [0 .. 7] ++ [OtherColour t | t <- Nothing : map Just [Asset, Liability, Equity, Revenue, Expense]] ++ [LoneColour, NeutralColour]
    rules =
      [ (".bg", "fill", "surface"), (".title", "fill", "ink"), (".heading", "fill", "ink")
      , (".label", "fill", "ink2"), (".value", "fill", "ink2"), (".tick", "fill", "muted")
      , (".grid", "stroke", "grid"), (".axis", "stroke", "axis")
      , (".line.total", "stroke", "ink"), (".mark.total", "stroke", "ink"), (".average", "stroke", "ink"), (".clip", "fill", "ink"), (".hatch", "stroke", "surface")
      -- the shading as of today (see 'SvgOptions'), tinted with the expenses' red where values are below zero
      , (".shade.past-negative", "fill", "pastnegative"), (".shade.future", "fill", "future"), (".shade.future-negative", "fill", "futurenegative")
      , (".axis.zero", "stroke", "zero")
      ]
    lightcolours :: [(Text, Text)]
    lightcolours =
      [ ("surface", "#fcfcfb"), ("ink", "#0b0b0b"), ("ink2", "#52514e"), ("muted", "#898781"), ("grid", "#e1e0d9"), ("axis", "#c3c2b7")
      , ("pastnegative", "#fbe4e4"), ("future", "#f1f0ec"), ("futurenegative", "#f0dcda"), ("zero", "#e34948") ]
    darkcolours =
      [ ("surface", "#1a1a19"), ("ink", "#ffffff"), ("ink2", "#c3c2b7"), ("muted", "#898781"), ("grid", "#2c2c2a"), ("axis", "#383835")
      , ("pastnegative", "#432929"), ("future", "#252524"), ("futurenegative", "#3a2a28"), ("zero", "#e66767") ]

tests_Hledger_Write_Chart_Svg :: TestTree
tests_Hledger_Write_Chart_Svg = testGroup "Write.Chart.Svg" [
   testCase "num" $ do
     map num [0, -0.04, 1, 1.26, -3.14159, 760] @?= ["0", "0", "1", "1.3", "-3.1", "760"]
  ,testCase "niceScale" $ do
     niceScale 5 0 87 @?= (0, 100, [0, 20, 40, 60, 80, 100], 0)
     niceScale 5 (-15) 100 @?= (-20, 100, [-20, 0, 20, 40, 60, 80, 100], 0)
     niceScale 5 0 0 @?= (0, 1, [0, 0.2, 0.4, 0.6000000000000001, 0.8, 1], 1)
     niceScale 5 0 7.3 @?= (0, 8, [0, 2, 4, 6, 8], 0)
     -- fewer intervals, for a shorter plot
     niceScale 5 0 117 @?= (0, 120, [0, 20, 40, 60, 80, 100, 120], 0)
     niceScale 3 0 117 @?= (0, 150, [0, 50, 100, 150], 0)
  ,testCase "plotIntervals" $ do
     map plotIntervals [260, 150, 100, 40] @?= [5, 3, 2, 2]
  ,testCase "barPath" $ do
     barPath True 10 100 20 (-50) True @?= "M 10 100 V 53 Q 10 50 13 50 H 27 Q 30 50 30 53 V 100 Z"
     barPath False 10 5 12 (-30) False @?= "M 10 5 H -20 Q -20 5 -20 5 V 17 Q -20 17 -20 17 H 10 Z"
  ,testCase "a long line has tooltips all along it, at most one per 4px column" $ do
     let hits = T.count "class=\"hit\"" $ svgText defaultSvgOptions $ lineChart 1000
     assertBool ("tooltip targets: " ++ show hits) (hits >= 150 && hits <= 220)
  ,testCase "a line's points can link somewhere, out of the tab order" $ do
     let svg = svgText defaultSvgOptions{svgPointLink = \j -> if even j then Just ("#t" <> T.pack (show j)) else Nothing} $ lineChart 3
     assertBool "the third point links" $ "<a href=\"#t2\" tabindex=\"-1\">" `T.isInfixOf` svg
     assertBool "the second point does not" $ not $ "#t1" `T.isInfixOf` svg
  ,testCase "a plot by date can give its date scale" $ do
     let svg = svgText defaultSvgOptions{svgDateScale = True} $ lineChart 3
     assertBool "its first and last dates" $ "data-d0=\"2024-01-01\" data-d1=\"2024-01-03\"" `T.isInfixOf` svg
  ,testCase "a plot by date can be shaded as of today: the future, and the past below zero" $ do
     -- a line falling below zero, today being its second day
     let shading today = svgText defaultSvgOptions{svgToday = Just today} $ lineChartOf [1, -1, -2]
         svg = shading (fromGregorian 2024 1 2)
     assertBool "the past below zero" $ "class=\"shade past-negative\"" `T.isInfixOf` svg
     assertBool "the future" $ "class=\"shade future\"" `T.isInfixOf` svg
     assertBool "the future below zero" $ "class=\"shade future-negative\"" `T.isInfixOf` svg
     assertBool "the zero line" $ "class=\"axis zero\"" `T.isInfixOf` svg
     -- with today after the last day, it's all past
     assertBool "no future" $ not $ "shade future" `T.isInfixOf` shading (fromGregorian 2024 1 9)
  ]
  where
    svgText opts = TL.toStrict . renderSvg . chartSvgMarkupWith opts
    -- a chart of one line, rising by 1 a day for n days from 2024-01-01
    lineChart n = lineChartOf [fromIntegral k | k <- [1 .. n]]
    -- a chart of one line, with these values on successive days from 2024-01-01
    lineChartOf qs = Chart "" (XDates $ take (length qs) $ iterate (addDays 1) (fromGregorian 2024 1 1))
      [Panel nullamt [Series (ChartLabel "a" "a" 0 Nothing True) SeriesItem MarkLine (map Just qs) 0 0] [] Nothing Diverging False []]
