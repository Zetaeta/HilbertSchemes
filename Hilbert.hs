
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU
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
    (progName, args) <- getArgsAndInitialize
    window <- createWindow "Hi"
    displayCallback $= display
    clearColor $= Color4 1.0 1.0 1.0 0.0
--    perspective 45.0 1.0 0.1 2.0
    matrixMode $= Projection
    ortho (-10.0) 10.0 (-10.0) 10.0 (-10.0) 10.0
    oblique <- newMatrix ColumnMajor [1.0,0.0,0.0,0.0,
                            0.0,1.0,0.0,0.0,
                            -0.5*sn,-0.5*sn,1.0,0.0,
                            0.0,0.0,0.0,1.0] :: IO (GLmatrix GLdouble)
    multMatrix oblique
    lookAt (Vertex3 1.0 0.0 0.0) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 0.0 1.0)
    mainLoop
  where sn= sin (pi/3) :: Double

verticate = mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z)

display :: DisplayCallback
display = do
    clear [ColorBuffer,DepthBuffer]
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Just Less
    currentColor $= Color4 0.0 0.0 0.0 1.0
    renderPrimitive Lines $ verticate (cubes sz cubeList)
--    renderPrimitive Lines $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z) (cubes 2.0 [(0,0,0),(1,0,0)])
--    arrow sz (0,0,3) (2,2,0)
    exarrows sz
    currentColor $= Color4 1.0 1.0 1.0 0.4
    renderPrimitive Quads $ verticate (cubeFaces sz cubeList)
    flush
  where cubeList = example
        sz = 2.5

example = [(0,0,0),(1,0,0),(2,0,0),(0,1,0),(1,1,0),(2,1,0),(0,0,1),(1,0,1),(0,1,1),(1,1,1),(0,0,2)]

singular = [(0,0,0),(1,0,0),(0,1,0),(0,0,1)]

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
exarrows sz = do
    currentColor $= Color4 0.0 1.0 0.0 0.8
    arrows sz [(3,0,0),(0,0,2), (3,0,0),(1,0,1), (3,0,0),(0,1,1), (3,0,0),(1,1,1), (3,0,0),(2,0,0), (3,0,0),(2,1,0),
               (2,0,1),(0,0,2), (2,0,1),(0,1,1), (2,0,1),(1,0,1), (2,0,1),(1,1,1), (1,0,2),(0,0,2)]
    currentColor $= Color4 0.0 0.0 1.0 0.8
    arrows sz [(0,2,0),(0,0,2), (0,2,0),(0,0,1), (0,2,0),(0,1,1), (0,2,0),(1,0,1), (0,2,0),(1,1,1), (0,2,0),(0,1,0),
               (0,2,0),(1,0,0), (0,2,0),(1,1,0), (0,2,0),(2,0,0), (0,2,0),(2,1,0), (0,1,2),(0,0,2)]
    currentColor $= Color4 1.0 0.0 0.0 0.8
    arrows sz [(0,0,3),(0,0,2), (0,0,3),(1,1,1), (0,0,3),(2,1,0), (0,1,2),(0,1,1), (0,1,2),(1,1,1), (0,1,2),(1,1,0),
               (0,1,2),(2,1,0), (1,0,2),(1,1,1), (1,0,2),(2,1,0), (2,0,1),(2,0,0), (2,0,1),(2,1,0)]

singarrows sz = do
    currentColor $= Color4 0.0 1.0 0.0 0.8
    arrows sz [(0,0,2),(0,0,1), (0,2,0),(0,1,0),(2,0,0),(1,0,0)]
    currentColor $= Color4 0.0 0.0 1.0 0.8
    arrows sz [(0,0,2),(0,1,0), (0,0,2),(1,0,0), (0,2,0),(1,0,0), (0,2,0),(0,0,1), (2,0,0),(0,1,0), (2,0,0),(0,0,1)]
    currentColor $= Color4 1.0 0.0 0.0 0.8
    arrows sz [(0,1,1),(0,1,0), (0,1,1),(0,0,1), (1,0,1),(1,0,0), (1,0,1),(0,0,1), (1,1,0),(0,1,0), (1,1,0),(1,0,0)]
    currentColor $= Color4 1.0 0.0 1.0 0.8
    arrows sz [(0,1,1),(1,0,0), (1,1,0),(0,0,1), (1,0,1),(0,1,0)]
