import math
import mathutils as mu
import bpy
import io

file = io.open

frames_per_solution = 10

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
        col = mu.Color((1,1,1))
        col.s = (n %2 ) and 0.7 or 0.3
        col.h = n / 11.0
        col.v = (n % 2) and 0.3 or 1.0
        material.diffuse_color = col
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

  for i, solution in enumerate(solutions + solutions[:1]):
    for name, (rot, location) in solution:
      props = {'rotation_euler': mu.Matrix(rot).to_euler(),
               'location': location }

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

def ReadCameraKeyframes():
  cam_keys = eval(file('camera_keys.txt').read())
  cam = bpy.data.objects['Camera']
  cam.animation_data_clear()

  anim_data = cam.animation_data_create()
  action = anim_data.action
  if action is None:
    action = (bpy.data.actions.get('Camera_Action') or
              bpy.data.actions.new('Camera_Action'))
    while (action.fcurves):
      action.fcurves.remove(action.fcurves[0])
    for prop in ['location', 'rotation_euler']:
      for index in range(3):
        action.fcurves.new(prop, index)
    anim_data.action = action

  for i, k in enumerate(cam_keys + cam_keys[:1]):
    sign = {'P':-1,'N':1}[k[1]]
    pos = {'X':[sign,0,0],'Y':[0,sign,0],'Z':[0,0,sign]}[k[0]]
    rot = mu.Vector(pos).to_track_quat('Z','Y').to_euler()

    props = {'rotation_euler': rot, 'location': [0,0,0] }

    for curve in action.fcurves:
      value = props[curve.data_path][curve.array_index]
      curve.keyframe_points.insert(i * frames_per_solution, value)

if __name__ == '__main__':
  ReadShapes()
  ReadSolutions()
  bpy.ops.wm.save_as_mainfile(filepath="tenfold.blend", check_existing=False)
  bpy.ops.wm.quit_blender()
