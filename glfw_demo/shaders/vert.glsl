#version 330 core

vec2 verts[] = {
    vec2(0.0, 0.5),
    vec2(-0.5, -0.5),
    vec2(0.5, -0.5)
}

void main() {
    gl_Position = vec4(verts[gl_VertexID], 0.0, 1.0);
}