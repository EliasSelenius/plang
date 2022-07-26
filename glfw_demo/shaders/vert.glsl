#version 330 core

in vec2 aPos;

uniform float aspect = 9.0 / 16.0;
uniform float zoom = 1.0;
uniform vec2 cam_pos = vec2(0.0);

uniform vec2 entity_pos;
uniform float entity_rot;

void main() {
    vec2 v = aPos;

    v += entity_pos;
    v -= cam_pos;
    v /= zoom;

    v.x *= aspect;
    gl_Position = vec4(v, 0.0, 1.0);
}