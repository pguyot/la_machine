import ddf.minim.*;

PFont font10;
Minim minim;
MyAudioPlayer curPlayer = null;
int fontHeight;

// Servo is 0.3s/60°. 360 = 1800ms
float gSERVO_ANGLE_0 = 125;
float gSERVO_ANGLE_100 = 30;
float gSERVO_FULL_ANGLE_RANGE = abs(gSERVO_ANGLE_0 - gSERVO_ANGLE_100);
float gSERVO_0_100_DUR_MS = 1800*gSERVO_FULL_ANGLE_RANGE/360.0;
float gSERVO_DOOR_PERCENT = 15.0;
float gSERVO_BUTTON_CONTACT_PERCENT = 85.0;
float GAME_SHORT_DUR_S = 2;
float GAME_MEDIUM_DUR_S = 4;
String SOUNDS_FOLDER = "../sounds/";

void setup() {
  fullScreen();
  frameRate(30);
  font10 = loadFont("Geneva-12.vlw");
  textFont(font10, 12);
  fontHeight = 12;
  
  minim = new Minim(this);
  
  float x0 = 0, y0 = height/2, w0 = 300, h0 = height - y0;
  AsLogInit(x0 + w0 + 2, height*0.666, 500, height*0.333 - 1);
  ScenariosInit("choreographies.json");
  ScenariosUIInit(x0, y0, w0, h0);
}

void draw() {
  background(0);
  
  if (gScenarEditor != null) gScenarEditor.loop();
  ScenariosDisplay();

  AsLogDisplay();
}

void keyPressed() {
  if (gScenarEditor != null) gScenarEditor.keyPressed();
}

void mouseMoved() {
  ScenariosMouseMoved();
}

void mouseDragged() {
  ScenariosMouseDragged();
}

void mousePressed() {
  ScenariosMousePressed();
}

void mouseReleased() {
  ScenariosMouseReleased();
}

void mouseWheel(MouseEvent event) {
  ScenariosMouseWheel(event.getCount());
}

// utils
float ms2pixels(float ms) {
  return map(ms, 0, 10000, 0, width);
}

float pixels2ms(float pixx) {
  return map(pixx, 0, width, 0, 10000);
}


// ***** log ******
ArrayList <String> gAsLogs;
int asLogMaxLines = 30;
int asLogLines;
float asLogX0, asLogY0, asLogW0, asLogH0; 
void AsLogInit(float x, float y, float w, float h) {
  gAsLogs = new ArrayList();
  asLogLines = 0;
  asLogX0 = x;
  asLogY0 = y;
  asLogW0 = w;
  asLogH0 = h;
  asLogMaxLines = floor(asLogH0/float(fontHeight))-1;
}
void AsLogDisplay() {
  float x = asLogX0;
  float y = asLogY0 + fontHeight;
  fill(0, 255, 0); noStroke();
  for (int i = 0; i < gAsLogs.size(); i++) {
    String lin = gAsLogs.get(i);
    text(lin, x, y);
    y += fontHeight;
  }
  stroke(0, 255, 0); noFill();
  rect(asLogX0, asLogY0, asLogW0, asLogH0);
}

void AsLog(String txt) {
  if (gAsLogs.size() > asLogMaxLines) gAsLogs.remove(0);
  gAsLogs.add(txt);
}
