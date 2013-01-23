import math
import bpy
import io

file = io.open

# RGB components are in the range [0,1].
#
# NMesh.materials is a list of Material objects. NMesh.faces[i].mat is the
# index into the mesh's material list of the material to use for the i'th
# face.

# When assigning to a face's vertex index list, a final zero is ignored.
# So, when creating a square face, rotate its vertex list if necessary
# to make sure vertex zero can be specified.
def RawVertices(verts):
  if len(verts) == 4 and verts[-1] == 0:
    return [0] + verts[:-1]
  else:
    return verts

def ReadShapes():
  meshes = eval(file('shapes.txt').read())

  materials = {}
  for (_, (_, faces)) in meshes:
    for i, (_, mat_name) in enumerate(faces):
      if mat_name not in materials:
        n = len(materials)
        material = bpy.data.materials.new(mat_name)
        material.diffuse_color = [(n & (1<<j)) and 1.0 or 0.3 for j in range(3)]
        materials[mat_name] = material

  for i, (name, (verts, faces)) in enumerate(meshes):
    poly = bpy.data.meshes.new(name)

    poly.vertices.add(len(verts))
    for j, v in enumerate(verts):
      poly.vertices[j].co = v

    mesh_materials = []
    for (_, mat_name) in faces:
      if mat_name not in mesh_materials:
        mesh_materials.append(mat_name)
    for mat_name in mesh_materials:
      poly.materials.append(bpy.data.materials[mat_name])

    poly.faces.add(len(faces))
    for j, (verts, mat_name) in enumerate(faces):
      f = poly.faces[j]
      f.vertices_raw = RawVertices(verts)
      f.material_index = mesh_materials.index(mat_name)

    poly.update()
    obj = bpy.data.objects.new(name, poly)
    bpy.context.scene.objects.link(obj)

def ReadSolutions():
  solutions = eval(file('pysolutions.txt').read())
  frames_per_solution = 25
  
  for i, solution in enumerate(solutions + solutions[:1]):
    for name, (euler, location) in solution:
      props = {'rotation_euler': euler, 'location': location }
      
      obj = bpy.data.objects[name]

      anim_data = obj.animation_data or obj.animation_data_create()
      action = anim_data.action
      if action is None:
        action = bpy.data.actions.new('%s_Action' % name)
        for prop in 'location', 'rotation_euler':
          for index in range(3):
            action.fcurves.new(prop, index)
        anim_data.action = action

      for curve in action.fcurves:
        value = props[curve.data_path][curve.array_index]
        curve.keyframe_points.insert(i * frames_per_solution, value)

  bpy.context.scene.frame_end = i * frames_per_solution

if __name__ == '__main__':
  ReadShapes()
  ReadSolutions()
