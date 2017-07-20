uniform vec3 MainColor, MortarColor;
uniform vec2 Size;
uniform vec2 Pct;

varying vec2  MCposition;
varying float LightIntensity;

void main(void)
{ gl_FragColor = vec4 (MainColor, 1.0); }
