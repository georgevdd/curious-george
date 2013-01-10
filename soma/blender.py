import math
import Blender
from Blender import NMesh

def ReadShapes():
  meshes = eval(file('shapes.txt').read())
  solutions = eval(file('pysolutions.txt').read())

  forms = dict(solutions[0])

  for i, (name, (verts, faces)) in enumerate(meshes):
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

    euler, location = forms[name]

    obj = NMesh.PutRaw(poly)
    obj.setEuler(*euler)
    obj.setLocation(*location)
  Blender.Redraw()

if __name__ == '__main__':
  ReadShapes()
