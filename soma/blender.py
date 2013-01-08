import math
import Blender
from Blender import NMesh

def foo():
  meshes = eval(file('/Users/georgevdd/src/soma/shapes.txt').read())

  NumberOfSides = 5
  Radius = 1

  for (name, (verts, faces)) in meshes:

    poly = NMesh.GetRaw()
    poly.name = name

    for v in verts:
      v = NMesh.Vert(*v)
      poly.verts.append(v)

    for face in faces:
      f = NMesh.Face()
      for vert_idx in face:
        f.v.append(poly.verts[vert_idx])
      poly.faces.append(f)

    NMesh.PutRaw(poly)

  Blender.Redraw()

if __name__ == '__main__':
  foo()
