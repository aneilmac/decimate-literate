# The Carnage Maximus problem

In this literate Haskell file we will visualize the Carnage Maximus problem.

## Problem statement

When dealing to the Roman Army, the term decimate meant that
the entire unit would be broken up into groups of ten soldiers, and
lots would be drawn. The person who was unlucky enough to draw
the short straw would be executed by the other nine members of his
group. The bloodthirsty Roman Centurion Carnage Maximus decided
to apply this to his prisoners, with a few gruesome differences.
Rather than kill every tenth prisoner and allow the rest to live, he
is going to leave only one prisoner alive and kill all of the others.
There are 100 prisoners and they stand in a circle. He goes round
killing every 10th prisoner until one is left. Which one?

## Representing prisoners

We may represent the prisoners standing in a circle using the following data
structure:

```haskell ignore
data CircleList a = CL [a] a [a]
```

where `CL snoc x cons` represents a circle with "focus" `x`, where
`cons` contains elements going counter-clockwise from `x` and `snoc` contains
elements going clockwise from `x`.

For example,  `CL [2,1] 3 [4,5]` is

```text
    3
 /     \
4       2
 \     /
  5 - 1
```

In this example, the next element after `2` is `3` and the element after `5`
is `1`.

> In another sense sense `CircleListC snoc x cons` can be considered an
implementation of a double-ended queue (deque) of minimum size 1 where "focus"
`x` is the head of the collection.

Along with the data structure we will define the necessary actions to manipulate
`CircleList`.

  ```haskell ignore
   -- | Create a CircleList from a focus and list.
  mkCircleList :: (a, [a]) -> CircleList a
  
  -- | Return the element in focus.
  current :: CircleList a -> a 
  
  -- | Returns true if the collection has but 1 element.
  isSingleton :: CircleList a -> Bool 

  -- | Returns a new collection rotated counter-clockwise by 1 element.
  next :: CircleList a -> CircleList a 
  
  -- | Returns a new collection but with the focus element removed.
  remove :: CircleList a -> CircleList a 
  ```

The implementation of which can be found in the appendix of this document.

## WorlTurtle visualization

[WorldTurtle](https://hackage.haskell.org/package/worldturtle) is a package for
drawing Turtle Graphics in Haskell. We will to use this
package to visualize our CircleList data structure. To use WorldTurtle, it must
first be imported as a dependency.

```haskell
import Graphics.WorldTurtle
```

WorldTurtle integrates with [Gloss](https://hackage.haskell.org/package/gloss),
which is a Haskell rendering package that lets us customize our rendering
capabilities beyond what WorldTurtle is normally capable of:

```haskell
import qualified Graphics.Gloss.Data.Picture as G
```

There are some additional modules which will come in handy later.
Of note: WorldTurtle treats turtle instructions as [Monads](https://wiki.haskell.org/All_About_Monads),
and because of this we can import haskell's `Control.Monad` to gain some nice utility
methods like `forM_` to conduct for-loops.

```haskell
import Control.Monad
import Data.List (any, sortOn)
```

## Visualizing CircleList using WorldTurtle

We would like to render `CircleList Int` as a regular polygon with `n` vertices.
I.e. `CL [5,4] 1 [2,3]` should be displayed as:

```text
    1
 /     \
2       5
 \     /
  3 - 4
```

While we want the data within the collection, we also wish to know _how_ in the
collection the element is stored. Is it the focus, a `cons` value or a `snoc`
value?

We can do this by color-coding the elements: green for the focus, red for the
snoc list, and blue for the cons list.

```haskell
asColorValues :: CircleList Int -> [(Int, Color)]
asColorValues (CL snoc x cons) = 
  (x, green) : zip cons (repeat blue) ++ zip snoc (repeat red)
```

We also need to order the output to be consistent. To do this we will order
the data by the elements in the collection.

That is, given: `CL [2,1] 3 [4,5]`, we should display:

```text
         (1, red)
    /                \
   /                  \
(2, red)             (5, blue)
   \                  /
    \                /
 (3, green) -- (4, blue)
```

In haskell we can easily order our intermediate output like so:

```haskell
indexList :: CircleList Int -> [(Int, Color)]
indexList circleList =  sortOn fst $ asColorValues circleList
```

### Vertices

Now that we have our intermediate form, we must render it.

We will focus on how to render the individual vertices of the collection in
WorldTurtle.

Given an element and a color, we will produce a coloured circle containing said
number, e.g. ❶, ❷, ❸, ❹, ❺.

This picture is trivial to produce in code using Gloss. We combine 3 different
elements: a border circle, a fill circle, and the text image.

```haskell
circleNumber :: Color -> Int -> G.Picture
circleNumber c n = G.pictures 
  [ G.color black $ G.circleSolid 110 -- border
  , G.color c $ G.circleSolid 100 -- fill
  , G.translate (-50) (-50) $ G.color white $ G.text $ show n -- number as text
  ]
```

Next comes the task of instructing a turtle to draw this picture. This can be
achieved with the `stamp` command, which copies a turtle's current shape to
the canvas. Here we will make a new `stampPrisoner` command out of the `stamp`
command.

```haskell
stampPrisoner :: Color -> Int -> TurtleCommand ()
stampPrisoner c n = branch $ do
  setHeading 0 -- Ensure text is printed at correct orientation.
  setRepresentation $ circleNumber c n -- Change the turtle shape to the vertex.
  stamp -- Stamp the turtle to the canvas.
```

Finally, we need to instruct our turtle to draw all the prisoners.
We will make a new turtle-command called `drawPrisoners`. Remember that our goal
is to render our collection into a regular polygon with `n` vertices, where `n`
is the number of prisoners.

```haskell
drawPrisoners :: Float -> Float -> [(Int, Color)] -> TurtleCommand ()
drawPrisoners sideLength angle prisoners = do
  setHeading angle -- Adjust initial heading before drawing the vertices.
  -- `forM_` allows us perform the same action for all prisoners in sequence.
  forM_ prisoners $ \(i, c) -> do 
    setVisible True -- Show turtle
    stampPrisoner c i -- Stamp the prisoner picture onto the canvas.
    setVisible False -- Hide turtle again
    forward sideLength -- Turtle goes forward by `sideLength` units.
    left angle -- Rotate turtle left by `angle`.
```

> Note that length of a face, and angle between vertices is constant. We pass
> these values in as parameters which we will calculate later.

### Edges

Drawing the lines between the vertices is broadly similar to drawing the
vertices, but now we only care how many prisoners exist in the collection.

```haskell
drawEdges :: Float -> Float -> Int -> TurtleCommand ()
 -- Do not draw anything if there is 1 vertex!
drawEdges _ _ 1 = return ()
-- Otherwise do the following for n > 1 vertices
drawEdges sideLength angle n = branch $ do
  setHeading angle  -- Adjust initial heading before drawing the vertices.
  setPenDown True -- Allow turtle to draw a line behind it as it moves.
  -- `replicateM_` allows us to repeat the same action `n` times.
  replicateM_ n $ do 
    forward sideLength -- Turtle goes forward by `sideLength` units.
    left angle -- Rotate turtle left by `angle`.
```

### Putting it all together

Now we convert out `CircleList` into an intermediate form, draw the edges, then
draw the vertices on top of these. To do this we will need to calculate the
angle between vertices, and provide a sensible length for the edges.

```haskell
-- Converts the CircleList to a regular polygon with each vertex labelled and 
-- color coded as a prisoner. 
drawCircleList :: CircleList Int -> TurtleCommand ()
drawCircleList cs = do
  let prisoners = indexList cs
  let angle = 360.0 / fromIntegral (length prisoners)
  branch $ drawEdges 1000 angle (length prisoners)
  branch $ drawPrisoners 1000 angle prisoners
```

> Note that the `branch` command performs a command on a turtle, then resets
> the turtle to its previous state before the command was run. This is handy if
> we want to ensure always our turtles starts in same place it ended!

## Animating

To visualize the carnage maximus problem we should animate the various state
changes of the prisoners. We will pretend that we are given a list of CircleLists,
as if we were given frames in a reel of film, and we will animate each one in
sequence.

```haskell
-- Draws each CircleList in sequence, letting each CircleList be rendered for 
-- 1 second before moving onto the subsequent CircleList.
animatePrisoners :: [CircleList Int] -> WorldCommand ()
animatePrisoners cls = do
  t <- makeTurtle -- Create a turtle to run our commands on.
  t >/> do -- Perform the following actions on the turtle:
    setVisible False -- Set turtle to initially invisible
    setPenSize 10 -- Set width of line drawn behind turtle.
    setSpeed 0 -- Turtle has no speed (draws instantly).
    setRotationSpeed 0 -- Turtle has no angular speed (turns instantly).
    setPenDown False -- Turtle does not draw pen behind itself initially.
  forM_ cls $ \ cl -> do -- For each item in the circle list do the following:
    clear -- Clear the canvas of any drawing.
    t >/> drawCircleList cl -- Run our drawCircleListCommand on the turtle.
    sleep 1 -- Wait one second before continuing.
```

## Example solution

To render the using the parameters in the question:

```haskell
main :: IO ()
main = runWorld $ animatePrisoners $ romanHistory 6 3
```

## Appendix

### CircleList implementation

```haskell
data CircleList a = CL [a] a [a]

-- Show instance for a CirclList with the focused element at the front.
instance Show a => Show (CircleList a) where
    show (CL left x right) = show ([x] ++ right ++ (reverse left))

-- Make a circular list with x as the focus and the other elements in order
-- after it.
mkCircleList :: (a, [a]) -> CircleList a
mkCircleList (x, xs) = CL [] x xs

-- Get the current focus of the circular list.
current :: CircleList a -> a
current (CL _ x _) = x

-- Check if the circular list consists of only one element.
isSingleton :: CircleList a -> Bool
isSingleton (CL [] _ []) = True
isSingleton _            = False

-- Change the focus to the next element of the circular list.
--
-- Note that when there are no elemnts to the right we must reverse the elements
-- from the left and take the head of that.
next :: CircleList a -> CircleList a
next (CL [] x []) = CL [] x []
next (CL left x []) = let (y:ys) = reverse left in CL [x] y ys
next (CL left x (y:right)) = CL (x:left) y right

-- Remove the focused element from the list and put the next element in focus.
--
-- Note again we have a special case where there are no elements to the right.
remove :: CircleList a -> CircleList a
remove (CL [] x []) = CL [] x []
remove (CL left x []) = let (y:ys) = reverse left in CL [] y ys
remove (CL left x (y:right)) = CL left y right
```

### Calculating frames

```haskell
-- Remove the nth item. Returns all computations.
removeNthScan :: Int -> CircleList a -> [CircleList a]
removeNthScan 1 cl = [cl, remove cl]
removeNthScan n cl = cl : removeNthScan (n - 1) (next cl)

-- Iterate a function until the predicate is true, returning all computations.
iterateUntilScan :: (CircleList a -> [CircleList a])
                 -> (CircleList a -> Bool)
                 -> CircleList a 
                 -> [CircleList a]
iterateUntilScan f p x = 
    if p x then [x] else let xs = f x 
                          in init xs ++ iterateUntilScan f p (last xs)

-- Let Roman Centurion Carnage Maximus loose. If we have prisoners
-- 1, 2, 3, 4, 5 and nth killed prisoner is 3, then prisoner 3 is killed leaving
-- 1, 2,    4, 5. 
-- This continues until there is only one prisoner remaining.
romanHistory :: Int -- ^  Number of prisoners (must be 1 or greater).
             -> Int -- ^ Nth prisoner to kill.
             -> [CircleList Int] -- ^ CircleList containing the history of kills.
romanHistory numPrisoners nthKilled = 
    let prisoners = mkCircleList (1, [2..numPrisoners])
     in iterateUntilScan (removeNthScan nthKilled) isSingleton prisoners

-- The first argument is the number of prisoners (must be 1 or greater).
-- The second argument which nth prisoner is killed, i.e. if we have prisoners
-- 1, 2, 3, 4, 5 and this argument is 3, then prisoner 3 is killed leaving
-- 1, 2,    4, 5.
-- The result is the single remaining prisoner.
chosenPrisoner :: Int -> Int -> Int
chosenPrisoner numPrisoners = current . last . romanHistory numPrisoners
```
