import bpy
import itertools
import mathutils as mu
import mathutils.geometry as geom

zero = mu.Vector()

def pairs_from(l):
    return itertools.combinations(l, 2)

def round_vector(v):
    return mu.Vector([round(c, 2) for c in v])

RIDICULOUS_VALUE=1000

TOLERANCE=0.01

def is_ridiculous_coord(c):
    return c > RIDICULOUS_VALUE or c < -RIDICULOUS_VALUE

def is_ridiculous_line(co, no):
    return any([is_ridiculous_coord(c) for c in list(co) + list(no)])


def grow(mesh, dist=0.2):
    vert_to_face = {}
    for f in mesh.faces:
        for f_idx, v_idx in enumerate(f.vertices):
            vert_to_face.setdefault(v_idx,[]).append((f, f_idx))
    selected_faces = [f for f in mesh.faces if f.select]
    if selected_faces:
        for v_idx, fs in vert_to_face.items():
            if not set([f for (f, _) in fs]).intersection(set(selected_faces)):
                fs[:] = []
    vert_to_offset = {}
    for v_idx, fs in vert_to_face.items():
        incompatible_faces=set()
        lines = []
        for (f1, _), (f2, _) in pairs_from(fs):
            dp = f1.normal.dot(f2.normal)
            if dp < -1 + TOLERANCE:
                # Planes are anti-parallel
                incompatible_faces.add((f1, f2))
                print('XX', f1, f2)
                continue
            if dp > 1 - TOLERANCE:
                # Planes are parallel
                print('PAR', f1, f2)
                continue
            line = geom.intersect_plane_plane(
                f1.normal*dist, f1.normal,
                f2.normal*dist, f2.normal)
            a,b = line
            if is_ridiculous_line(*line):
                print('RR', line)
            else:
              lines.append(line)
              
        if incompatible_faces:
            continue
        
        pts = []
        for (co1, no1), (co2, no2) in pairs_from(lines):
            pts.extend(geom.intersect_line_line(co1, co1+no1, co2, co2+no2) or [])
        if pts:
            final_offset = sum(pts, zero) / len(pts)
            vert_to_offset[v_idx] = final_offset
    for v_idx, offset in vert_to_offset.items():
         mesh.vertices[v_idx].co += offset
                

def main(context):
    for ob in context.scene.objects:
        if type(ob.data) is not bpy.types.Mesh:
            continue
        grow(ob.data)


class SimpleOperator(bpy.types.Operator):
    '''Tooltip'''
    bl_idname = "object.simple_operator"
    bl_label = "Simple Object Operator"

    @classmethod
    def poll(cls, context):
        return context.active_object is not None

    def execute(self, context):
        main(context)
        return {'FINISHED'}


def register():
    bpy.utils.register_class(SimpleOperator)


def unregister():
    bpy.utils.unregister_class(SimpleOperator)


if __name__ == "__main__":
    register()

    # test call
    bpy.ops.object.simple_operator()