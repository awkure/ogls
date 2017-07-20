uniform vec3 LightPosition;

varying float LightIntensity;
varying vec2  MCposition;

void main(void)
{ gl_Position = ftransform(); }
