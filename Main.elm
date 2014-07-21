import Mouse
import Window
import Graphics.Input

side = 11
field = newField side

clearGrey : Color
clearGrey = rgba 111 111 111 0.6

hexagon dimensions = outlined (solid clearGrey) (ngon 6 <| size dimensions)
lineOfHexagons n dimensions = let hexagons = repeat n <| rotate (degrees 30) <| hexagon dimensions
                              in zipWith (\x h -> moveX x h)
                                         (map (\n -> toFloat n * (size dimensions)*(sqrt 3)) [0..n-1])
                                         hexagons

-- if you wonder what these magic numbers in the offsets are
-- get a list of paper and do some geometry
-- (but +s*7.5 is a magic number, I have to admit)
newField n dimensions = let lines = repeat n <| lineOfHexagons n dimensions
                            s = size dimensions
                            offsetX = (map (\n -> toFloat n * -((sqrt 3)/2)*s) [0..n-1])
                            offsetY = (map (\n -> toFloat n * -1.5*s + 7.5*s) [0..n-1])
                        in zipWith (\offset line -> map (\cell -> move offset cell) line)
                                   (zipWith (,) offsetX offsetY)
                                   lines

clicks : [[Graphics.Input.Input Bool]]
clicks = repeat side <| repeat side (Graphics.Input.input False)

makeClickable handle cell = Graphics.Input.clickable handle.handle True cell

clickableField field = zipWith (\handles line -> zipWith makeClickable handles line)
                               clicks
                               field

-- these are some simle formulas for the radius of one hexagon,
-- based on the size of the window
-- it takes the min of them, so that the whole field is inside
-- the window in both width and height
size (w, h) = min (2*h/(3*side)) (w/(2 * side * sqrt 3))

resizeableField (w, h) = collage w h <| map group <| newField side (toFloat w, toFloat h)

main = lift resizeableField Window.dimensions
