from math import pi,sqrt,sin,cos
import bmesh
import bpy
from mathutils import Vector, Matrix, Euler

def v(x, y): return Vector((x, y, 0))
def t(x, y): return Matrix.Translation((x, y, 0))
def r(a): return Matrix.Rotation(a, 4, 'Z')
def s(k): return Matrix.Scale(k, 4)
def tr(a): return t(cos(a), sin(a))
def c(*ts):
  x = Matrix()
  for t in ts:
    x = t @ x
  return x

KITE = 'Kite'
DART = 'Dart'

kite = [(KITE, Matrix())]
sun = [(KITE, r(n * 0.4 * pi)) for n in range(5)]

phi = (1 + sqrt(5.0))/2

def realise(bm, tile):
  type, xform = tile
  angle = { KITE: pi/5, DART: 3*pi/5 }[type]
  points = [
    v(1,0),
    r(angle) * v(1,0),
    v(0,0),
    r(-angle) * v(1,0),
  ]
  verts = [bm.verts.new((xform * p).to_3d()) for p in points]
  bm.faces.new(verts)

def deflate1(tile):
  type, xform = tile
  tiles = {
      KITE: [
          (DART, c(t(-1, 0), r(-0.8 * pi), s(2 - phi))),
          (KITE, c(r(-0.6 * pi), s(phi - 1), tr(0.2 * pi))),
          (KITE, c(s(phi - 1), r(0.6 * pi), tr(-0.2 * pi))),
      ],
      DART: [
          (DART, c(tr(-0.4 * pi), r(0.8 * pi), s(phi - 1))),
          (KITE, c(r(pi), t(1, 0))),
      ]
  }
  return [(t, xform @ x) for (t, x) in tiles[type]]

def deflate(tiles):
  return sum([deflate1(tile) for tile in tiles],[])

def repeat(f, n, x):
  if n == 0: return x
  else: return f(repeat(f, n-1, x))

bm = bmesh.new()

for tile in repeat(deflate, 6, sun):
  realise(bm, tile)
bm.faces.index_update

mesh = bpy.data.meshes.new('penrose')
bm.to_mesh(mesh)
bpy.context.scene.objects.link(bpy.data.objects.new('penrose', mesh))
