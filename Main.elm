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
                                         (map (\i -> inLineOffset i (size dimensions)) [0..n-1])
                                         hexagons

-- if you wonder what these magic numbers in the offsets are
-- get a list of paper and do some geometry
-- (but +s*7.5 and -5*s are magic numbers, I have to admit)
newField n dimensions = let lines = repeat n <| lineOfHexagons n dimensions
                            s = size dimensions
                            offX = map (\row -> offsetX row s) [0..n-1]
                            offY = map (\row -> offsetY row s) [0..n-1]
                        in zipWith (\offset line -> map (\cell -> move offset cell) line)
                                   (zipWith (,) offX offY)
                                   lines

inLineOffset cell size = toFloat cell*size*(sqrt 3)

-- these are offsets for a whole row
-- s = size
offsetX row s = toFloat row * -((sqrt 3)/2)*s - 5*s
offsetY row s = toFloat row * -1.5*s + 7.5*s

cellXRanges s = map (\row -> map (\i -> let offX = offsetX row s
                                        in (offX - s*(sqrt 3)/2, offX + s*(sqrt 3)/2))
                                 [0..side-1])
                    [0..side-1]

cellYRanges s = map (\row -> map (\i -> let offY = offsetY row s
                                        in (offY - s*(sqrt 3)/2, offY + s*(sqrt 3)/2))
                                 [0..side-1])
                    [0..side-1]

-- these are some simle formulas for the radius of one hexagon,
-- based on the size of the window
-- it takes the min of them, so that the whole field is inside
-- the window in both width and height
size (w, h) = min (2*h/(3*side)) (w/(2 * side * sqrt 3))

resizeableField (w, h) = collage w h <| map group <| newField side (toFloat w, toFloat h)
--fieldWidth (w, h) = floor <| 2 * sqrt 3 * size (w, h) * side
--fieldHeight (w, h) = floor <| 3*(size (w, h))*side / 2
--c : (Int, Int) -> Element
--c (w, h) = container (fieldWidth (toFloat w, toFloat h)) (fieldHeight (toFloat w, toFloat h)) middle <| resizeableField (w, h)

-- main = lift (\(w, h) -> asText <| cellXRanges <| size (toFloat w, toFloat h)) Window.dimensions
main = lift resizeableField Window.dimensions
