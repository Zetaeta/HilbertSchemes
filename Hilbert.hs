
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU
import Control.Concurrent (threadDelay)

data Mon = Mon Int [Int] deriving Show

mon n l = Mon n $ l++ take (n - length l) (repeat 0)

--(?) :: Mon -> Mon -> Bool
--Mon n a ? Mon m b = (n==m) && foldl (&&) True (zipWith (<=) a b)

divide :: [Mon2] -> Mon2 -> Bool
l `divide` m = foldl (||) False $ map (? m) l

x :: [Int]
x = [1]
y :: [Int]
y = [0,1]
z :: [Int]
z = [0,0,1]

data Mon2 = Mon2 (Int,Int) deriving (Show, Eq)

x2 = Mon2 (1,0)
y2 = Mon2 (0,1)
one = Mon2 (0,0)
x3 = mon 3 x
y3 = mon 3 y
z3 = mon 3 z

Mon2 (x,y) # Mon2 (x',y') = Mon2 (x+x',y+y')
Mon2 (x,y) ? Mon2 (x',y') = x<=x' && y <= y'
m ## n = foldl (#) one $ replicate n m
infixr 8 ##
infixl 7 #

--(#) :: Mon -> Mon -> Mon
--Mon n a # Mon m b = Mon n $ zipWith (+) a b

main :: IO ()
main = do
    (progName, args) <- getArgsAndInitialize
    window <- createWindow "Hi"
    displayCallback $= display2
    clearColor $= Color4 1.0 1.0 1.0 0.0
--    perspective 45.0 1.0 0.1 2.0
    matrixMode $= Projection
    ortho (-10.0) 20.0 (-10.0) 20.0 (-10.0) 20.0
    oblique <- newMatrix ColumnMajor [1.0,0.0,0.0,0.0,
                            0.0,1.0,0.0,0.0,
                            -0.5*sn,-0.5*sn,1.0,0.0,
                            0.0,0.0,0.0,1.0] :: IO (GLmatrix GLdouble)
    multMatrix oblique
    lookAt (Vertex3 1.0 0.0 0.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 0.0 1.0)
    mainLoop
  where sn= sin (pi/3) :: Double

verticate = mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z)

display3 :: DisplayCallback
display3 = do
    clear [ColorBuffer,DepthBuffer]
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Just Less
    lineSmooth $= Enabled
    polygonSmooth $= Enabled
    lineWidth $= 1.8
    currentColor $= Color4 0.0 0.0 0.0 1.0
    renderPrimitive Lines $ verticate (cubes sz cubeList)
    renderPrimitive Quads $ verticate (gens sz genList)
--    renderPrimitive Lines $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) (cubes 2.0 [(0,0,0),(1,0,0)])
--    arrow sz (0,0,3) (2,2,0)
    lineWidth $= 1.4
    singarrows sz
    currentColor $= Color4 1.0 1.0 1.0 0.4
    renderPrimitive Quads $ verticate (cubeFaces sz cubeList)
    flush
  where cubeList = singular
        genList = singgens
        sz = 2.5

example = [(0,0,0),(1,0,0),(2,0,0),(0,1,0),(1,1,0),(2,1,0),(0,0,1),(1,0,1),(0,1,1),(1,1,1),(0,0,2)]

exgens = [(0,2,0),(3,0,0),(2,0,1),(1,0,2),(0,1,2),(0,0,3)]

singular = [(0,0,0),(1,0,0),(0,1,0),(0,0,1)]

small = [(0,0,0),(1,0,0),(0,1,0),(1,1,0),(0,0,1)]

smallgens = [(0,0,2),(0,2,0),(2,0,0),(1,0,1),(0,1,1)]

singgens = [(0,0,2),(0,2,0),(2,0,0),(1,0,1),(0,1,1), (1,1,0)]

big1 = [(0,0,0),(1,0,0),(2,0,0),(0,1,0),(1,1,0),(2,1,0),(0,2,0),(1,2,0),(2,2,0),(0,0,1),(1,0,1),(0,1,1),(1,1,1),(0,0,2)]

(a,b,c) .- (d,e,f) = (a-d,b-e,c-f)
(a,b,c) .+ (d,e,f) = (a+d,b+e,c+f)
(a,b,c) *. (d,e,f) = a*d+b*e+c*f
infixl 6 .-
infixl 6 .+
infixl 7 *.
infixl 7 .*

m .* (a,b,c) = (m*a,m*b,m*c)

nrmlz (a,b,c) = (a/m,b/m,c/m)
    where m= sqrt (a^2+b^2+c^2)

arrows s (a:b:rest) = arrow s a b >> arrows s rest
arrows _ [] = return ()

arrow s (x,y,z) (x',y',z') = do
    renderPrimitive Lines $ verticate [dr, dr']
    renderPrimitive Triangles $ verticate [dr', dr' .- (s' .* v) .- s' .* nrmlz (vz .- (vz*.v).*v), (dx',dy',dz') .- (s' .* v).+ s' .* nrmlz (vz .- (vz*.v).*v)]
    renderPrimitive Triangles $ verticate [dr', dr' .- (s' .* v) .- s' .* nrmlz (vx .- (vx*.v).*v), (dx',dy',dz') .- (s' .* v).+ s' .* nrmlz (vx .- (vx*.v).*v)]
    renderPrimitive Triangles $ verticate [dr', dr' .- (s' .* v) .- s' .* nrmlz (vy .- (vy*.v).*v), (dx',dy',dz') .- (s' .* v).+ s' .* nrmlz (vy .- (vy*.v).*v)]
  where
    (dx,dy,dz)=(fromIntegral x*s+s/2, fromIntegral y*s+s/2, fromIntegral z*s+s/2)
    (dx',dy',dz')=(fromIntegral x' * s+s/2, fromIntegral y'*s+s/2, fromIntegral z'*s+s/2)
    dr=(dx,dy,dz)
    dr'=(dx',dy',dz')
    v = nrmlz (dr' .- dr)
    vx=(1.0,0.0,0.0)
    vy=(0.0,1.0,0.0)
    vz=(0.0,0.0,1.0)
    s' = 0.15*s

points :: [(Double,Double,Double)]
points =[(0.0,0.0,0.0),(0.5,0.5,0.5)]

cubes :: Double -> [(Int,Int,Int)] -> [(Double,Double, Double)]
cubes s = concat . fmap (cube s . itod s)

gens s = concat . map (gener s)

cubeFaces :: Double -> [(Int,Int,Int)] -> [(Double,Double, Double)]
cubeFaces s = concat . fmap (cubeFace s . itod s)

itod s (x,y,z) = (s * fromIntegral x,s * fromIntegral y,s * fromIntegral z)

cube :: Double -> (Double,Double,Double) -> [(Double,Double,Double)]
cube s (x,y,z) = [(x,y,z),(x,y,z+s),
                  (x,y,z),(x,y+s,z),
                  (x,y,z),(x+s,y,z),
                  (x+s,y,z),(x+s,y,z+s),
                  (x+s,y,z),(x+s,y+s,z),
                  (x+s,y+s,z),(x+s,y+s,z+s),
                  (x,y+s,z),(x,y+s,z+s),
                  (x,y+s,z),(x+s,y+s,z),
                  (x,y+s,z+s),(x+s,y+s,z+s),
                  (x,y,z+s),(x+s,y,z+s),
                  (x,y,z+s),(x,y+s,z+s),
                  (x+s,y,z+s),(x+s,y+s,z+s)]

cubeFace :: Double -> (Double,Double,Double) -> [(Double,Double,Double)]
cubeFace s (x,y,z) = [(x,y,z+s),(x+s,y,z+s),(x+s,y+s,z+s),(x,y+s,z+s),
                      (x+s,y,z),(x+s,y+s,z),(x+s,y+s,z+s),(x+s,y,z+s),
                      (x,y+s,z),(x,y+s,z+s),(x+s,y+s,z+s),(x+s,y+s,z)]
--                      (x,y,z),(x,y,z),(x,y,z),(x,y,z),
--                      (x,y,z),(x,y,z),(x,y,z),(x,y,z)]

gener sz (x,y,z) = cubeFace s (x*sz +sz/2 - s/2,y*sz +sz/2  - s/2,z*sz +sz/2  - s/2)
    where s = 0.05*sz

exarrows sz = do
    currentColor $= Color4 0.0 0.808 0.301 0.9
    arrows sz [(3,0,0),(0,0,2), (3,0,0),(1,0,1), (3,0,0),(0,1,1), (3,0,0),(1,1,1), (3,0,0),(2,0,0), (3,0,0),(2,1,0),
               (2,0,1),(0,0,2), (2,0,1),(0,1,1), (2,0,1),(1,0,1), (2,0,1),(1,1,1), (1,0,2),(0,0,2)]
    currentColor $= Color4 0.0 0.0 1.0 0.8
    arrows sz [(0,2,0),(0,0,2), (0,2,0),(0,0,1), (0,2,0),(0,1,1), (0,2,0),(1,0,1), (0,2,0),(1,1,1), (0,2,0),(0,1,0),
               (0,2,0),(1,0,0), (0,2,0),(1,1,0), (0,2,0),(2,0,0), (0,2,0),(2,1,0), (0,1,2),(0,0,2)]
    currentColor $= Color4 1.0 0.0 0.0 0.8
    arrows sz [(0,0,3),(0,0,2), (0,0,3),(1,1,1), (0,0,3),(2,1,0), (0,1,2),(0,1,1), (0,1,2),(1,1,1), (0,1,2),(1,1,0),
               (0,1,2),(2,1,0), (1,0,2),(1,1,1), (1,0,2),(2,1,0), (2,0,1),(2,0,0), (2,0,1),(2,1,0)]

singarrows sz = do
    currentColor $= Color4 0.0 0.8 0.3 0.8
    arrows sz [(0,0,2),(0,0,1), (0,2,0),(0,1,0),(2,0,0),(1,0,0)]
    currentColor $= Color4 0.0 0.0 1.0 0.8
    arrows sz [(0,0,2),(0,1,0), (0,0,2),(1,0,0), (0,2,0),(1,0,0), (0,2,0),(0,0,1), (2,0,0),(0,1,0), (2,0,0),(0,0,1)]
    currentColor $= Color4 1.0 0.0 0.0 0.8
    arrows sz [(0,1,1),(0,1,0), (0,1,1),(0,0,1), (1,0,1),(1,0,0), (1,0,1),(0,0,1), (1,1,0),(0,1,0), (1,1,0),(1,0,0)]
    currentColor $= Color4 1.0 0.65 0.0 0.8
    arrows sz [(0,1,1),(1,0,0), (1,1,0),(0,0,1), (1,0,1),(0,1,0)]

to3 (x,y) = (0,x,y)

square s (x,y) = [(x,y),(x+s,y),(x,y),(x,y+s),(x+s,y),(x+s,y+s),(x,y+s),(x+s,y+s)]

x .** (y,z) = (x*y,x*z)

(x,y) #- (w,z) = (x-w,y-z)

unmon2 (Mon2 (x,y)) = (fromIntegral x, fromIntegral y)

squares sz = concat . map (square sz . (sz .**) )

size = 0.3

right (x,y) = [(sx+size,sy+size),(sx+size,sy)]
    where (sx,sy) = size .** (x,y)

top (x,y) = [(sx,sy+size),(sx+size,sy+size)]
    where (sx,sy) = size .** (x,y)

drawbord (x:[]) = right x
drawbord (x:y:rest) =
    let this = case y #- x of
                 (1,0) -> top y
                 (0,-1) -> right x
                 (1,-1) -> right x ++ top y
                 _ -> []
    in this ++ drawbord (y:rest)

drawborder (x:rest) = top x ++ drawbord (x:rest)

young gens = concat . takeWhile (not . null) $ map reverse [takeWhile (not . (gens `divide`)) [Mon2 (x,y) |  y <- [0..]] | x <- [0..]]

boundary s = filter test s
    where test m = (m#x2 `notElem` s) || (m#y2 `notElem` s)

gen2 (x',y') = [(x+d,y),(x,y+d),(x,y+d),(x-d,y),(x-d,y),(x,y-d),(x,y-d),(x+d,y)]
    where x=(x'+0.5)*size
          y=(y'+0.5)*size
          d=0.1*size


draw :: [Mon2] -> IO ()
draw gens = do
    let s = young gens
        b = boundary s
        axes = concat $ (map right [(-1,x) | x <- [0..100]]) ++ (map top [(x,-1) | x <- [0..100]])
    putStrLn $ show b
    lineWidth $= 1.0
    currentColor $= Color4 0.0 0.0 0.0 1.0
    renderPrimitive Lines $ verticate (to3 <$> squares sz (map unmon2 s))
    lineWidth $= 3.0
--    currentColor $= Color4 1.0 0.0 0.0 1.0
    renderPrimitive Lines $ verticate (to3 <$> drawborder (map unmon2 b))
    renderPrimitive Lines $ verticate (to3 <$> axes)
  where sz = size

drawgens = renderPrimitive Lines . verticate . map to3 . concat . map (gen2 . unmon2)

display2 :: DisplayCallback
display2 = do
    clear [ColorBuffer,DepthBuffer]
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
--    depthFunc $= Just Less
    lineSmooth $= Enabled
    polygonSmooth $= Enabled
    draw [x#x#x,x#x#y,y#y#y#y]
    draw [x#x#x,x#x#y##3,y#y#y#y]
    draw [x##n # y## (50-n) | n <- [0..50]]
    lineWidth $= 1.0
    currentColor $= Color4 1.0 0.0 0.0 1.0
    arrow2 sz (x##3) (x # y)
    arrow2 sz (y##4) (y##3)
--    renderPrimitive Quads $ verticate (gens sz genList)
--    renderPrimitive Lines $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) (cubes 2.0 [(0,0,0),(1,0,0)])
--    arrow sz (0,0,3) (2,2,0)
--    lineWidth $= 1.4
--    singarrows sz
--   currentColor $= Color4 1.0 1.0 1.0 0.4
--    renderPrimitive Quads $ verticate (cubeFaces sz cubeList)
    flush
  where sList = [(0,0),(1,0),(2,0),(0,1)]
        genList = singgens
        sz = size
        x=x2
        y=y2

arrow2 s (Mon2 (x,y)) (Mon2 (x',y')) = do
    renderPrimitive Lines $ verticate [dr, dr']
    renderPrimitive Triangles $ verticate [dr', dr' .- (s' .* v) .- s' .* nrmlz (vz .- (vz*.v).*v), (dx',dy',dz') .- (s' .* v).+ s' .* nrmlz (vz .- (vz*.v).*v)]
  where
    (dx,dy,dz)=(0,fromIntegral x*s+s/2, fromIntegral y*s+s/2)
    (dx',dy',dz')=(0,fromIntegral x' * s+s/2, fromIntegral y'*s+s/2)
    dr=(dx,dy,dz)
    dr'=(dx',dy',dz')
    v = nrmlz (dr' .- dr)
    vz=(0.0,1.0,1.0)
    s' = 0.15*s

tb = [(0,1),(1,0),(2,0)] :: [(Double,Double)]
