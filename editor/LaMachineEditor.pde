import ddf.minim.*;

PFont font10;
Minim minim;
MyAudioPlayer curPlayer = null;
int fontHeight;

float gSERVO_ANGLE_0 = 125;
float gSERVO_ANGLE_100 = 30;
float gSERVO_FULL_ANGLE_RANGE = abs(gSERVO_ANGLE_0 - gSERVO_ANGLE_100);
float gSERVO_FULL_RANGE_TIME_MS = 1350; // for 180°
float gSERVO_0_100_DUR_MS = gSERVO_FULL_RANGE_TIME_MS*gSERVO_FULL_ANGLE_RANGE/180.0;
float gSERVO_DOOR_PERCENT = 15.0;
float gSERVO_BUTTON_CONTACT_PERCENT = 85.0;
float GAME_SHORT_DUR_S = 2;
float GAME_MEDIUM_DUR_S = 4;
String gProjectDir = null;
String gSoundsDir = null;
boolean gProjectLoaded = false;

PShape gLogo;
int gLoadingStep = -1;
volatile boolean gLoadingJsonDone = false;

String soundsPath() {
  return gSoundsDir;
}

String choreoPath(String filename) {
  return gProjectDir + "/" + filename;
}

void settings() {
  // Avoid fullScreen() which calls ThinkDifferent.hideMenuBar() via JNI,
  // crashing on macOS with Java 21+ (NSWindow not on main thread).
  size(displayWidth, displayHeight);
}

void setup() {
  frameRate(30);
  font10 = loadFont("Geneva-12.vlw");
  textFont(font10, 12);
  fontHeight = 12;

  minim = new Minim(this);
  gLogo = loadShape("la_machine.svg");

  // Enter fullscreen: dispose the frame, remove decorations, resize, re-show.
  // This replicates what Processing does internally for fullScreen(), minus
  // the ThinkDifferent.hideMenuBar() call that crashes on macOS with Java 21+.
  processing.awt.PSurfaceAWT.SmoothCanvas canvas =
    (processing.awt.PSurfaceAWT.SmoothCanvas) surface.getNative();
  java.awt.Frame frame = canvas.getFrame();
  frame.dispose();
  frame.setUndecorated(true);
  frame.setBounds(0, 0, displayWidth, displayHeight);
  frame.setBackground(java.awt.Color.BLACK);
  frame.setVisible(true);
  frame.toFront();

  // Draw logo on black background and force-paint to screen,
  // so the logo is visible behind the file dialog.
  background(0);
  drawLogo();
  java.awt.Graphics canvasG = canvas.getGraphics();
  if (canvasG != null) {
    canvasG.drawImage(((processing.awt.PGraphicsJava2D) g).image, 0, 0, null);
    canvasG.dispose();
  }

  // Show file dialog - logo visible behind it
  java.awt.FileDialog dialog = new java.awt.FileDialog(frame,
    "Select choreographies.json of La Machine project", java.awt.FileDialog.LOAD);
  dialog.setFile("choreographies.json");
  dialog.setFilenameFilter(new java.io.FilenameFilter() {
    public boolean accept(File dir, String name) {
      return name.endsWith(".json");
    }
  });
  dialog.setVisible(true);

  if (dialog.getFile() == null) {
    exit();
    return;
  }

  File selection = new File(dialog.getDirectory(), dialog.getFile());
  if (!selection.getName().equals("choreographies.json")) {
    println("ERROR: Please select choreographies.json, not " + selection.getName());
    exit();
    return;
  }

  gProjectDir = selection.getParent();
  resolveSoundsDir();

  gLoadingStep = 0;
}

void resolveSoundsDir() {
  // Check next to choreographies.json
  String candidate = gProjectDir + "/sounds/";
  if (new File(candidate).isDirectory()) {
    gSoundsDir = candidate;
    return;
  }
  // Check in grandparent directory of choreographies.json
  File parent = new File(gProjectDir).getParentFile();
  if (parent != null) {
    candidate = parent.getPath() + "/sounds/";
    if (new File(candidate).isDirectory()) {
      gSoundsDir = candidate;
      return;
    }
  }
  // Fallback to original location
  gSoundsDir = gProjectDir + "/sounds/";
}

void draw() {
  background(0);

  if (gLoadingStep >= 0 && !gProjectLoaded) {
    drawLoadingScreen();
    return;
  }

  if (!gProjectLoaded) return;

  if (gScenarEditor != null) gScenarEditor.loop();
  ScenariosDisplay();

  AsLogDisplay();
}

void drawLogo() {
  float logoW = width * 0.7;
  float logoH = logoW * (126.99 / 764.96);
  float logoX = (width - logoW) / 2;
  float logoY = (height - logoH) / 2 - 30;
  gLogo.disableStyle();
  noStroke();
  fill(128);
  shape(gLogo, logoX, logoY, logoW, logoH);
}

void drawLoadingScreen() {
  drawLogo();

  // Progress bar below logo
  float logoW = width * 0.7;
  float logoH = logoW * (126.99 / 764.96);
  float barW = logoW;
  float barH = 6;
  float barX = (width - logoW) / 2;
  float barY = (height - logoH) / 2 - 30 + logoH + 40;
  float progress;
  String status;

  if (gLoadingStep == 0) {
    // First frame: show logo, start background loading
    progress = 0;
    status = "Loading choreographies...";
    thread("loadProjectAsync");
    gLoadingStep = 1;
  } else if (gLoadingStep == 1) {
    // Wait for background thread to finish
    if (gLoadingJsonDone) {
      progress = 0.7;
      status = "Initializing editor...";
      gLoadingStep = 2;
    } else {
      progress = 0.3;
      status = "Loading choreographies...";
    }
  } else if (gLoadingStep == 2) {
    // Init UI (fast)
    float x0 = 0, y0 = height/2, w0 = 300, h0 = height - y0;
    AsLogInit(x0 + w0 + 2, height*0.666, 500, height*0.333 - 1);
    ScenariosUIInit(x0, y0, w0, h0);
    progress = 1.0;
    status = "Ready";
    gLoadingStep = 3;
  } else {
    gProjectLoaded = true;
    return;
  }

  // Draw progress bar
  stroke(80); noFill();
  rect(barX, barY, barW, barH);
  noStroke(); fill(128);
  rect(barX, barY, barW * progress, barH);

  // Status text
  fill(100);
  textAlign(CENTER);
  text(status, width/2, barY + barH + 20);
  textAlign(LEFT);
}

void loadProjectAsync() {
  ScenariosInit("choreographies.json");
  gLoadingJsonDone = true;
}

void keyPressed() {
  if (!gProjectLoaded) return;
  if (gScenarEditor != null) gScenarEditor.keyPressed();
}

void mouseMoved() {
  if (!gProjectLoaded) return;
  ScenariosMouseMoved();
}

void mouseDragged() {
  if (!gProjectLoaded) return;
  ScenariosMouseDragged();
}

void mousePressed() {
  if (!gProjectLoaded) return;
  ScenariosMousePressed();
}

void mouseReleased() {
  if (!gProjectLoaded) return;
  ScenariosMouseReleased();
}

void mouseWheel(MouseEvent event) {
  if (!gProjectLoaded) return;
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
