import Geometry.Cuboid as Cuboid
import Geometry.Sphere as Sphere
import Geometry.Cube as Cube

cuboidVolume = Cuboid.volume 10 20 30
sphere10Volume = Sphere.volume 10
cube10Volume = Cube.volume 10

-- We cannot do this because rectArea is not exported in the Geometry module
-- rectArea = Cuboid.rectArea 10 10