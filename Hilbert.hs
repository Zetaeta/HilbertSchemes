
import Graphics.X11.Xlib
import Control.Concurrent (threadDelay)

data Mon = Mon Int [Int] deriving Show

mon n l = Mon n $ l++ take (n - length l) (repeat 0)

(?) :: Mon -> Mon -> Bool
Mon n a ? Mon m b = (n==m) && foldl (&&) True (zipWith (<=) a b)

divides :: [Mon] -> Mon -> Bool
l `divides` m = foldl (||) False $ map (? m) l

x :: [Int]
x = [1]
y :: [Int]
y = [0,1]
z :: [Int]
z = [0,0,1]

x2 = mon 2 x
y2 = mon 2 y
x3 = mon 3 x
y3 = mon 3 y
z3 = mon 3 z

(#) :: Mon -> Mon -> Mon
Mon n a # Mon m b = Mon n $ zipWith (+) a b

main :: IO ()
main = do
    dpy <- openDisplay ""
    let dflt = defaultScreen dpy
        border = blackPixel dpy dflt
        bg = whitePixel dpy dflt
    rootw <- rootWindow dpy dflt
    win <- createSimpleWindow dpy rootw 0 0 100 100 1 border bg
    setTextProperty dpy win "Hi" wM_NAME
    mapWindow dpy win
    sync dpy False
    threadDelay (2*1000)
    return ()
