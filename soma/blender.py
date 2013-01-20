import math
import Blender
from Blender import NMesh, Material

# RGB components are in the range [0,1].
#
# NMesh.materials is a list of Material objects. NMesh.faces[i].mat is the
# index into the mesh's material list of the material to use for the i'th
# face.

def ReadShapes():
  meshes = eval(file('shapes.txt').read())
  solutions = eval(file('pysolutions.txt').read())

  forms = dict(solutions[0])

  materials = {}
  for (_, (_, faces)) in meshes:
    for i, (_, mat_name) in enumerate(faces):
      if mat_name not in materials:
        n = len(materials)
        material = Material.New(mat_name)
        material.setRGBCol(*[(n & (1<<j)) and 1.0 or 0.3 for j in range(3)])
        materials[mat_name] = material

  for i, (name, (verts, faces)) in enumerate(meshes):
    poly = NMesh.GetRaw()

    for v in verts:
      v = NMesh.Vert(*v)
      poly.verts.append(v)

    mesh_materials = []
    for (_, mat_name) in faces:
      if mat_name not in mesh_materials:
        mesh_materials.append(mat_name)
    for j, mat_name in enumerate(mesh_materials):
      poly.addMaterial(Material.Get(mat_name))

    for (verts, mat_name) in faces:
      f = NMesh.Face()
      for vert_idx in verts:
        f.v.append(poly.verts[vert_idx])
      f.mat = mesh_materials.index(mat_name)
      poly.faces.append(f)

    euler, location = forms[name]

    obj = NMesh.PutRaw(poly, name)
    obj.setEuler(*euler)
    obj.setLocation(*location)
    obj.setName(name)
  Blender.Redraw()

if __name__ == '__main__':
  ReadShapes()
