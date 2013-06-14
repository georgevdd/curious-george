# bpy.ops.script.python_file_run(filepath='/Users/georgevdd/georgevdd/src/soma/blender.py')

import io
import math
import os
import sys

import bmesh
import bpy
import mathutils as mu
geom = mu.geometry

if '.' not in sys.path:
  sys.path.append('.')
import gen_materials

file = io.open
pi = math.pi


FRAMES_PER_SOLUTION = 100

SIDE = 0.4
THICKNESS = 0.009
GAP = 0.010


def LinkMaterials():
  for path in [
    os.path.join(os.getcwd(), gen_materials.OUTPUT_FILENAME),
    ]:
    with bpy.data.libraries.load(
        path,
        link=True,
        relative=True) as (data_from, data_to):
      data_to.materials = data_from.materials


def LinkCorner():
  path = os.path.join(os.getcwd(), 'lib/corner_assembly.blend')
  group_names = ['Face Corner', '3-Corner']
  with bpy.data.libraries.load(path, link=True, relative=True) as (data_from, data_to):
    data_to.groups += group_names
  groups = [bpy.data.groups[group_name] for group_name in group_names]
  return groups


def CreateCuboidFaces(verts, dest_bmesh):
  result = []
  for axis in range(3):
    for sign in 0, 1:
      idxs = [(sign << axis) +
              (bool(a & (1<<sign)) << ((axis+1) % 3)) +
              (bool(a & (1<<(1-sign))) << ((axis+2) %3))
              for a in [0,1,3,2]]
      vs = [verts[i] for i in idxs]
      result.append(dest_bmesh.faces.new(vs))
  return result


def EdgeDir(edge):
  return (edge.verts[1].co - edge.verts[0].co) / edge.calc_length()


def LoopDir(loop):
  return (loop.link_loop_next.vert.co - loop.vert.co).normalized()


def GrooveEdges(obj, size):
  bpy.context.scene.objects.active = obj
  bpy.ops.object.mode_set(mode='EDIT')

  obj_mesh = bmesh.from_edit_mesh(obj.data)
  frame_mesh = bmesh.new()
  convex_edges = [edge for edge in obj_mesh.edges if edge.calc_face_angle_signed() > 0.1]
  for edge in convex_edges:
    f1, f2 = edge.link_faces
    edge_dir = EdgeDir(edge)
    seg_verts = [frame_mesh.verts.new(x + (y + z) * size)
                 for x in [edge.verts[0].co - edge_dir * size,
                           edge.verts[1].co + edge_dir * size]
                 for y in [f1.normal * sign for sign in (-1, 1)]
                 for z in [f2.normal * sign for sign in (-1, 1)]]
    CreateCuboidFaces(seg_verts, frame_mesh)

  del obj_mesh

  frame = bpy.data.meshes.new(obj.name + '.frame')
  frame_mesh.to_mesh(frame)
  frame_obj = bpy.data.objects.new(frame.name, frame)
  bpy.context.scene.objects.link(frame_obj)
  ForceRecalculateNormals(frame_obj)

  ModifierSubtract(obj, frame_obj)

  bpy.ops.object.mode_set(mode='OBJECT')
  bpy.context.scene.objects.active = obj
  bpy.ops.object.mode_set(mode='EDIT')
  bpy.ops.mesh.select_all(action='SELECT')
  bpy.ops.mesh.dissolve_limited()

  bpy.ops.object.mode_set(mode='OBJECT')
  bpy.ops.object.select_all(action='DESELECT')
  frame_obj.select = True
  bpy.context.scene.objects.active = frame_obj
  bpy.ops.object.delete()
  del frame_obj
  del frame_mesh
  del frame


def ForceRecalculateNormals(obj):
  if bpy.context.mode != 'OBJECT':
    bpy.ops.object.mode_set(mode='OBJECT')
  bpy.context.scene.objects.active = obj
  bpy.ops.object.mode_set(mode='EDIT')
  bpy.ops.mesh.select_all(action='SELECT')
  bpy.ops.mesh.normals_make_consistent(inside=False)
  bpy.ops.object.mode_set(mode='OBJECT')


def ModifierSubtract(obj_lhs, obj_rhs):
  bpy.context.scene.objects.active = obj_lhs
  bpy.ops.object.modifier_add(type='BOOLEAN')
  mod = obj_lhs.modifiers[-1]
  mod.operation = 'DIFFERENCE'
  mod.object = obj_rhs
  bpy.ops.object.modifier_apply(modifier=mod.name)


def ReadShapes(foot_group, corner_group):
  meshes = eval(file('shapes.txt').read())

  for i, (name, (verts, faces)) in enumerate(meshes):
    bpy.ops.group.create(name=name)
    poly = bpy.data.meshes.new(name)

    mesh = bmesh.new()
    for v in verts:
      mesh.verts.new(mu.Vector(v) * (SIDE + GAP))

    mesh_materials = []
    for (_, (mat_name, uvs)) in faces:
      if mat_name not in mesh_materials:
        mesh_materials.append(mat_name)
    for mat_name in mesh_materials:
      poly.materials.append(bpy.data.materials[mat_name])

    for j, (verts, (mat_name, uvs)) in enumerate(faces):
      f = mesh.faces.new([mesh.verts[k] for k in verts])
      f.material_index = mesh_materials.index(mat_name)

    uv_layer = mesh.loops.layers.uv.new()
    tex_layer = mesh.faces.layers.tex.new()
    for ((_, (_, uvs)), face) in zip(faces, mesh.faces):
      face[tex_layer].image = ([ts for ts in poly.materials[face.material_index].
                                texture_slots if ts][-1].texture.image)
      for loop, uv in zip(face.loops, uvs):
        loop[uv_layer].uv = uv

    mesh.normal_update()
    mesh.to_mesh(poly)

    obj = bpy.data.objects.new(name, poly)
    bpy.context.scene.objects.link(obj)

    ForceRecalculateNormals(obj)

    face_corners = []
    for i, face in enumerate(mesh.faces):
      for j, loop in enumerate(face.loops):
        face_corners.append(((loop.vert.co.copy(), -LoopDir(loop), face.normal), i, j))

    three_corners = []
    for i, vert in enumerate(mesh.verts):
      if len(vert.link_faces) == 3:
        three_corners.append((i, vert.co.copy(), tuple([face.normal.copy()
                                                        for face in vert.link_faces])))

    GrooveEdges(obj, THICKNESS+GAP/2)

    bpy.context.scene.objects.active = obj
    bpy.ops.object.group_link(group=name)

    bpy.ops.object.mode_set(mode='EDIT')
    mesh = bmesh.from_edit_mesh(obj.data)
    for face in mesh.faces:
      if face.calc_area() > 0.1:
        for loop in face.loops:
          loop.vert.co -= face.normal * GAP/2
    bmesh.update_edit_mesh(obj.data)
    bpy.ops.object.mode_set(mode='OBJECT')

    for corner, i, j in face_corners:
      pos, x, z = corner
      y = z.cross(x)
      m = mu.Matrix((x, y, z)).transposed()
      bpy.ops.object.empty_add()
      empty = bpy.context.active_object
      empty.name = '%s.%d.%d' % (name, i, j)
      empty.rotation_euler = m.to_euler()
      empty.location = pos - (x+y+z)*(GAP/2) - (x+y)*THICKNESS
      empty.parent = obj
      empty.empty_draw_size = 0.05
      empty.dupli_type = 'GROUP'
      empty.dupli_group = foot_group
      bpy.ops.object.group_link(group=name)

    for i, pos, axes in three_corners:
      rot = mu.Matrix(axes).transposed()
      if rot.is_negative:
        rot = mu.Matrix((axes[0], axes[2], axes[1])).transposed()
      bpy.ops.object.empty_add()
      empty = bpy.context.active_object
      empty.name = '%s.3-corner.%d' % (name, i)
      empty.rotation_euler = rot.to_euler()
      empty.location = pos - rot*mu.Vector((1,1,1))*GAP/2
      empty.parent = obj
      empty.empty_draw_size = 0.05
      empty.dupli_type = 'GROUP'
      empty.dupli_group = corner_group
      bpy.ops.object.group_link(group=name)


def GetOrCreateAction(obj):
  anim_data = obj.animation_data or obj.animation_data_create()
  action = anim_data.action
  if action is None:
    action = bpy.data.actions.new('%s_Action' % obj.name)
    for prop in 'location', 'rotation_euler':
      for index in range(3):
        action.fcurves.new(prop, index)
    anim_data.action = action
  return action


def SetKeys(action, time, keys):
  for curve in action.fcurves:
    key = keys.get(curve.data_path)
    if key is None:
      continue
    value = key[curve.array_index]
    curve.keyframe_points.insert(time, value)


STAGGER_FRAMES = 5
SLIDE_FRAMES = 7
ORIENT_FRAMES = 7
SOLVED_FRAMES = 20


def ReadSolutions():
  solutions = eval(file('pysolutions.txt').read())
  dismantle_orders = eval(file('dismantle.txt').read())
  num_shapes = len(dismantle_orders[0])
  for solution, order in zip(solutions, dismantle_orders):
    d = dict(solution)
    solution[:] = [(name, d[name]) for name in order]

  exploded_offset_h = mu.Vector((8, 0, 0)) * (SIDE + GAP)
  exploded_offset_v = mu.Vector((0, 0, 4)) * (SIDE + GAP)

  exploded_keys = {}
  all_shapes = [s for (s, _) in solutions[0]]
  for i, name in enumerate(all_shapes):
    rot = mu.Euler((0,
                    (i % 2 and -pi/6 or -pi/12),
                    pi/2+pi*float(i)/len(all_shapes)))
    m = mu.Matrix()
    m.translation = exploded_offset_h
    exploded_pos = rot.to_matrix().to_4x4() * m * exploded_offset_v

    exploded_keys[name] = {'rotation_euler': rot,
                           'location': exploded_pos
                           }

  frames_per_solution = SOLVED_FRAMES + 2 * (ORIENT_FRAMES +
                                             SLIDE_FRAMES +
                                             STAGGER_FRAMES * (num_shapes - 1))

  print("Frames per solution:", frames_per_solution)

  for i, solution in enumerate(solutions):
    for j, (name, (rot, location)) in enumerate(solution):
      base_time = i * frames_per_solution
      solve_time = base_time + frames_per_solution/2.0
      obj = bpy.data.objects[name]
      action = GetOrCreateAction(obj)

      solved_keys = {'rotation_euler': mu.Matrix(rot).to_euler(),
                     'location': mu.Vector(location) * (SIDE + GAP)}
      presolved_keys = {'rotation_euler': solved_keys['rotation_euler'],
                        'location': solved_keys['location'] + exploded_offset_v}

      solve_offset = (j * STAGGER_FRAMES) + (SOLVED_FRAMES / 2.0)

      SetKeys(action, solve_time - (solve_offset + ORIENT_FRAMES + SLIDE_FRAMES), exploded_keys[name])
      SetKeys(action, solve_time - (solve_offset + SLIDE_FRAMES), presolved_keys)
      SetKeys(action, solve_time - solve_offset, solved_keys)
      SetKeys(action, solve_time + solve_offset, solved_keys)
      SetKeys(action, solve_time + (solve_offset + SLIDE_FRAMES), presolved_keys)
      SetKeys(action, solve_time + (solve_offset + ORIENT_FRAMES + SLIDE_FRAMES), exploded_keys[name])

  bpy.context.scene.frame_start = 0
  bpy.context.scene.frame_end = i * frames_per_solution - 1

def main():
  LinkMaterials()
  groups = LinkCorner()
  ReadShapes(*groups)
  ReadSolutions()
  bpy.ops.wm.save_as_mainfile(filepath="tenfold.blend", check_existing=False)
  bpy.ops.wm.quit_blender()

if __name__ == '__main__':
  main()
