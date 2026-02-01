
/// ************ LaMachine
LaMachine lamachine;

class LaMachine {
  float x, y, w, h;
  float posRat;
  
  LaMachine(float ax, float ay, float aw, float ah) {
    x = ax; y = ay; w = aw; h = ah;
    posRat = 0;
  }
  
  void Display() {
    float h100 = w/4;
    float y0 = y + h100*gSERVO_DOOR_PERCENT/100.0;
    float buttony = y0 - h100*gSERVO_BUTTON_CONTACT_PERCENT/100.0;
    float buttonw = 2*h100*(100.0 - gSERVO_BUTTON_CONTACT_PERCENT)/100.0;
    float bitoW = w/20;
    float bitoH = w*0.666;

    // box
    fill(221, 155, 0); noStroke();
    rect(x, y, w, w);

    // button
    fill(255, 0, 0);
    ellipse(x + w/2, buttony - buttonw/2, buttonw, buttonw);

    // bras
    fill(100, 100, 100); noStroke();
    rect(x + w/2 - bitoW/2, y0 - h100*posRat, bitoW, bitoH);
    
    // text
    fill(255);
    text(""+floor(posRat*100), x + 10, y + h - 10);
  }
  
  void set(float position) {
    posRat = position/100.0;
  }
  
}


///// ************ ScenarEditors

class ScenarElemEditor {
  ScenarEditor editor;
  int anchort;
  String type;
  float x, y, w, h;
  
  ScenarElemEditor(ScenarEditor aeditor, int aanchort, float ax, float ay, float aw, float ah) {
    editor = aeditor;
    type = "super";
    anchort = aanchort;
    x = ax; y = ay; w = aw; h = ah;
    //println("created editor for:"+elem.command+","+elem.arg+" at:"+x+","+y+","+w+","+","+h);
  }
  void Display() {}
  void Play() {}
  void Stop() {}
  float dur_ms() { return 0.0; }
  boolean IsPlaying() {return false;}
  boolean IsMD(float mx, float my) { return false; }
}

// *********** AudioEditor

class AudioEditor extends ScenarElemEditor {
  MyAudioPlayer audioPlayer;
  String audioPath;
  int dur_ms;
  
  AudioEditor(ScenarEditor aeditor, int aanchort, String aaudioPath, int adur_ms, float ax, float ay, float aw, float ah) {
    super(aeditor, aanchort, ax, ay, aw, ah);
    type = "audio";
    audioPath = aaudioPath;
    dur_ms = adur_ms;
    audioPlayer = new MyAudioPlayer(MP3_FOLDER+mp3FileNameForAac(audioPath));
    println("AudioEditor audioPath="+audioPath+" dur_ms="+dur_ms);
  }
  
  void Display() {
    if (audioPlayer != null) {
      float posX = x + ms2pixels(anchort);
      float ww = ms2pixels(dur_ms);
      audioPlayer.AudioDraw(posX, y, ww, h);
      fill(255, 255, 0); noStroke();
      text(audioPath, posX, y+12);
    }
  }
  
  void Play() {
    audioPlayer.Play(0);
  }
  
  float dur_ms() {
    return dur_ms;
  }
    
  void Stop() {
    audioPlayer.Stop();
  }
  
  boolean IsPlaying() {
    return audioPlayer.IsPlaying();
  }
  
  boolean IsMD(float mx, float my) {
    float posX = x + ms2pixels(anchort);
    float ww = ms2pixels(dur_ms);
    return (mx > posX && mx < posX + ww && my > y && my < y + h);
  }
}

// *********** ServoEditor

class ServoEditor extends ScenarElemEditor {
  float percent;
  float dur_ms;
  boolean edited;
  boolean end_edited;
  float start_play = -1;
  
  ServoEditor(ScenarEditor aeditor, int aanchort, float apercent, float adur_ms, float ax, float ay, float aw, float ah) {
    super(aeditor, aanchort, ax, ay, aw, ah);
    percent = apercent;
    dur_ms = adur_ms;
    start_play = -1;
    edited = false;
    end_edited = false;
    type = "servo";
  }
  
  float min_dur_ms_from(float apercent) {
    return gSERVO_0_100_DUR_MS*abs(percent - apercent)/100.0;
  }
  
  float min_dur_ms() {
    float prevpercent = editor.getServoPercentBefore(anchort);
    return min_dur_ms_from(prevpercent);
  }

  float dur_ms_from(float apercent) {
    float min_dur_ms = gSERVO_0_100_DUR_MS*abs(percent - apercent)/100.0;
    if (dur_ms < min_dur_ms) return min_dur_ms;
    return dur_ms;
  }
    
  float dur_ms() {
    float prevpercent = editor.getServoPercentBefore(anchort);
    return dur_ms_from(prevpercent);
  }

  float percentAtFrom(float tt, float aPercent) {
    float real_dur_ms = dur_ms_from(aPercent);
    if (tt > anchort + real_dur_ms) {
      return percent;
    }
    return map(tt - anchort, 0, real_dur_ms, aPercent, percent);
  }

  boolean IsMD(float mx, float my) {
    float posX = x + ms2pixels(anchort);
    float posXEnd = x + ms2pixels(anchort+dur_ms());
    float posY = y + h - h*percent/100;
    float clickDist2 = 6;
    if (pow(mx - posX, 2) + pow(my - posY, 2) < pow(clickDist2, 2)) {
      edited = true;
      return true;
    }
    if (pow(mx - posXEnd, 2) + pow(my - posY, 2) < pow(clickDist2, 2)) {
      end_edited = true;
      return true;
    }
    return false;
  }
  
  void followMouse(float mx, float my) {
    if (!edited && !end_edited) return;
    
    // y
    percent = map(my - y, 0.0, h, 100.0, 0.0);
    if (percent < 0) percent = 0;
    if (percent > 100) percent = 100;
    percent = floor(percent);
    
    // x
    if (edited) {
      anchort = max(0, floor(pixels2ms(mx - x)));
    }
    if (end_edited) {
      float post = max(0, floor(pixels2ms(mx - x)));
      float min_dur = max(0, min_dur_ms());
      dur_ms = max(min_dur, post - anchort);
    }
  }
  
  void edited(boolean isEdited) {
    edited = isEdited;
    end_edited = isEdited;
  }
  
  void Display() {
    
    float posX = x + ms2pixels(anchort);
    float posXEnd = x + ms2pixels(anchort+dur_ms());
    float posY = y + h - h*percent/100;
    float selectSiz = 10;
    float normalSiz = 6;
    float min_dur = min_dur_ms();
    float real_dur = dur_ms();
    boolean isAtMin = (min_dur == real_dur);
    
    stroke(255, 0, 0); noFill();
    line(posX, posY, posXEnd, posY);
    
    if (edited) {
      noStroke(); fill(255, 0, 0);
      ellipse(posX, posY, selectSiz, selectSiz);
    } else {
      noFill(); stroke(255, 0, 0);
      ellipse(posX, posY, normalSiz, normalSiz);    
    }
    
    if (end_edited) {
      noStroke();
      if (isAtMin) fill(255, 0, 0); else fill(0, 255, 0);
      ellipse(posXEnd, posY, selectSiz, selectSiz);
      text(""+real_dur, posXEnd+5, posY - 1);
    } else {
      noFill(); if (isAtMin) stroke(255, 0, 0); else stroke(0, 255, 0);
      ellipse(posXEnd, posY, normalSiz, normalSiz);
    }
    
    fill(255); noStroke();
    text(""+percent, posX+5, (percent < 95) ? posY - 1 : posY + fontHeight);
  }
  
  void Play() {
    start_play = millis();
  }
  
  void Stop() {
    start_play = -1;
  }
  
  boolean IsPlaying() {
    if (start_play < 0) return false;
    if (millis() - start_play > dur_ms) return false;
    return true;
  }
}

// *********** ScenarEditor

class ScenarEditor {
  Scenario scen;
  String name;
  ArrayList <ScenarElemEditor> editors;
  boolean playing = false;
  float playPreviousPercent, playCurrentPercent;
  float playt0, playlastt;
  int playNextIndex;
  AudioEditor runningAudioEditor = null;
  ServoEditor runningServoEditor = null;
  AudioEditor editedAudioEditor = null;
  ServoEditor editedServoEditor = null;
  float x0 = 0, y0 = 0, w0 = width, h0 = height/2;
  float audioEditorAnchor0;
  AsTextField nameField;

  ScenarEditor(Scenario ascen) {
    scen = ascen;
    name = scen.name;
    loadScenario(scen);
    float hbito = 400;
    lamachine = new LaMachine(width/2 + 200 - hbito/2, height - hbito, hbito, hbito);
    nameField = new AsTextField(name, x0 + w0/2, y0 + h0 + 2, 0, fontHeight+6);
  }
  
  void loadScenario(Scenario scen) {
    // create editors elements
    editors = new ArrayList();
    ArrayList <ScenarElem> elements = scen.elements();
    
    int cursor_ms = 0;
    int lastAudioDuration = -1;
    int lastAudioStart = -1;
    for (int i = 0; i < elements.size(); i++) {
      ScenarElem elem = elements.get(i);
      
      if (elem.command.equals("servo")) {
        float dur_ms = elem.intArg2 < 0 ? 0 : elem.intArg2;
        ServoEditor editor = new ServoEditor(this, cursor_ms, elem.intArg, dur_ms, x0, y0 + h0/2, w0, h0/2);
        editors.add(editor);
        
      } else if (elem.command.equals("wait")) {
        if (elem.arg.equals("sound")) {
          // add the file end
          if (lastAudioStart >= 0 && lastAudioDuration >= 0) {
            // test finished 
            if (cursor_ms < lastAudioStart + lastAudioDuration) {
              int durLeft = lastAudioStart + lastAudioDuration - cursor_ms;
              cursor_ms += durLeft;
            } else { // else no wait
              lastAudioStart = 0;
              lastAudioDuration = 0;
            }
          } // else no wait
        } else {
          // add the wait arg
          cursor_ms += elem.intArg;
        }
        
      } else if (elem.command.equals("aac")) {
        lastAudioStart = cursor_ms;
        lastAudioDuration = elem.intArg;
        AudioEditor editor = new AudioEditor(this, cursor_ms, elem.arg, elem.intArg, x0, y0, w0, h0/2);
        editors.add(editor);
        
      } else {
        // unknown command
        println("ERROR unknown command:"+elem.command);
      }
    }
  }
  
  String computeScenarioDef() {
    // "{wait, 800}, {aac, <<\"gears/simple2.aac\">>}, {servo, 35}, ... ,{servo,0}",

    String res = "";
    int cursor_ms = 0;
    
    for (int i = 0; i < editors.size(); i++) {
      ScenarElemEditor editor = editors.get(i);
      
      if (editor.anchort > cursor_ms) {
        res = res + "{wait, "+(editor.anchort - cursor_ms)+"}, ";
        cursor_ms = editor.anchort;
      }
      
      if (editor.type.equals("audio")) {
        AudioEditor aEditor = (AudioEditor)editor;
        res = res + "{aac, <<\\\""+aEditor.audioPath+"\\\">>}";
        
      } else if (editor.type.equals("servo")) {
        ServoEditor sEditor = (ServoEditor)editor;
        float dur_ms = sEditor.dur_ms;
        float min_dur = sEditor.min_dur_ms();
        res = res + "{servo, "+floor(sEditor.percent)+(dur_ms <= min_dur ? "" : ", "+floor(dur_ms))+"}";
      }
      
      if (i < editors.size()-1) {
        res = res + ", ";
      }
    }
    
    return res;
  }
  
  void ResortEditors() {
    if (editors == null) return;
    editors.sort( (a, b) -> { return a.anchort - b.anchort; } );
  }
  
  float duration() {
    if (editors == null) return 0.0;
    if (editors.size() == 0) return 0.0;
    ResortEditors();
    
    float duration = 0.0;
    for (int i = 0; i < editors.size(); i++) {
      ScenarElemEditor editor = editors.get(i);
      float end_ms = editor.anchort + editor.dur_ms();
      duration = max(duration, end_ms);
    }
    return duration;
  }
  
  float getServoPercentBefore(float anchort) {
    ResortEditors();
    
    float servoPos = 0;
    if (editors == null) return servoPos;
    
    int editorIndex = 0;
    ServoEditor sEditor1 = null;
    ServoEditor sEditor2 = null;
    do {
      sEditor1 = sEditor2;
            
      // find next seditor starting at editorIndex
      sEditor2 = null;
      if (editorIndex < editors.size()) {
        do {
          ScenarElemEditor editor = editors.get(editorIndex);
          if ("servo".equals(editor.type)) {
            sEditor2 = (ServoEditor)editor;
            break;
          }
          editorIndex++;          
        } while(editorIndex < editors.size());
      }
      
      if (sEditor2 == null) {
        // finish
        if (sEditor1 == null) {
          // no servo
          return servoPos;
        } else {
          return sEditor1.percentAtFrom(anchort, servoPos);
        }
      }
      
      if (sEditor1 != null) {
        servoPos = sEditor1.percentAtFrom(sEditor2.anchort, servoPos);
      }
      
      if (sEditor2.anchort >= anchort) return servoPos;
      editorIndex++;  
    } while(true);
  }
  
  float servoPercentToY(float percent) {
    return y0+h0-percent*0.01*h0/2;
  }
  
  void Display() {
    // box
    noFill(); stroke(255);
    rect(x0, y0, w0, h0);
    // séparation audio/servo
    line(x0, y0 + h0/2, x0 + w0, y0 + h0/2);
    
    // time scale
    noFill(); stroke(255, 255, 255, 100);
    for (int ms = 0; ms < pixels2ms(width); ms+= 100) {
      float i = ms2pixels(ms);
      if (ms % 1000 == 0) {
        stroke(255, 255, 255, 150);
      } else {
        stroke(255, 255, 255, 75);
      }
      line(x0 + i, y0+h0, x0 + i, y0+h0/2);
    }
    // horizontal lines
    stroke(255, 255, 255, 75);
    for (float yy = 0.25; yy < 1.0; yy+= 0.25) {
      float yyy = servoPercentToY(yy*100);
      line(x0, yyy, x0 + w0, yyy);
    }
    
    // door
    float yy = gSERVO_DOOR_PERCENT/100.0;
    float yyy = y0+h0-yy*h0/2;
    stroke(255, 0, 0, 127);
    line(x0, yyy, x0 + w0, yyy);
    // contact
    yy = gSERVO_BUTTON_CONTACT_PERCENT/100.0; //<>// //<>// //<>// //<>//
    yyy = y0+h0-yy*h0/2;
    stroke(255, 0, 0, 127);
    line(x0, yyy, x0 + w0, yyy);

    // display editors
    if (editors != null) {
      float servoPercent1 = 0.0;
      ServoEditor sEditor1 = null;
      for (int i = 0; i < editors.size(); i++) {
        ScenarElemEditor editor = editors.get(i);
        
        if ("servo".equals(editor.type)) {
          ServoEditor sEditor2 = (ServoEditor)editor;
          float x1, y2;
          float y1 = servoPercentToY(servoPercent1);
          float x2 = ms2pixels(sEditor2.anchort);
          noFill(); stroke(0, 255, 0);
          if (sEditor1 == null) {
            x1 = 0; //<>// //<>// //<>// //<>//
            y2 = y1;
            line(x1, y1, x2, y2); //<>// //<>// //<>// //<>//
          } else {
            // we have sEditor1
            x1 = ms2pixels(sEditor1.anchort);
            float percent2 = sEditor1.percentAtFrom(sEditor2.anchort, servoPercent1);
            if (percent2 == sEditor1.percent) {
              // OK, reached
              
              // first line
              float durEnd = sEditor1.dur_ms_from(servoPercent1);
              x2 = ms2pixels(sEditor1.anchort + durEnd);
              y2 = servoPercentToY(sEditor1.percent);
              line(x1, y1, x2, y2);
              
              // second line
              x1 = x2; y1 = y2;
              x2 = ms2pixels(sEditor2.anchort);
              servoPercent1 = sEditor1.percent;
              y2 = servoPercentToY(servoPercent1);
              line(x1, y1, x2, y2);
            } else {
              servoPercent1 = percent2;
              y2 = servoPercentToY(servoPercent1);
              line(x1, y1, x2, y2);
            }
          }
          
          sEditor1 = sEditor2;
        }
        editor.Display();
      }
      if (sEditor1 != null) {
        float x1 = ms2pixels(sEditor1.anchort);
        float y1 = servoPercentToY(servoPercent1);
        float durEnd = sEditor1.dur_ms_from(servoPercent1);
        float x2 = ms2pixels(sEditor1.anchort + durEnd);
        float y2 = servoPercentToY(sEditor1.percent);
        noFill(); stroke(0, 255, 0);
        line(x1, y1, x2, y2);
      }
    }
    
    // lamachine
    lamachine.Display();
    
    // name
    nameField.Display();
    
    if (playing) {
      // cursor
      stroke(255, 0, 0, 200); noFill();
      float elapsed = millis() - playt0;
      float cursorX = x0 + ms2pixels(elapsed);
      line(cursorX, y0, cursorX, y0 + h0);
    }
  }
  
  void loop() {
    if (!playing) return;
    float elapsed = millis() - playt0;
    
    // playing, start events which should be triggered, starting at playNextIndex
    if (playNextIndex < editors.size()) {
      do {
        ScenarElemEditor editor = editors.get(playNextIndex);
        if (editor.anchort > elapsed) break;
        
        editor.Play();
        if (editor.type.equals("audio")) runningAudioEditor = (AudioEditor)editor;
        if (editor.type.equals("servo")) {
          if (runningServoEditor == null) {
            playPreviousPercent = 0;
          } else {
            playPreviousPercent = runningServoEditor.percentAtFrom(elapsed, playPreviousPercent);
          }
          
          runningServoEditor = (ServoEditor)editor;
        }
        
        playNextIndex++;
        if (playNextIndex >= editors.size()) {
          break;
        }
      } while (true);
    }
    
    if (runningServoEditor == null) {
      playCurrentPercent = 0;
    } else {
      playCurrentPercent = runningServoEditor.percentAtFrom(elapsed, playPreviousPercent);
    }
    lamachine.set(playCurrentPercent);
    
    // at the end wait all finished
    if (playNextIndex >= editors.size()) {
      boolean finished = true;
      if (runningAudioEditor != null && runningAudioEditor.IsPlaying()) finished = false;
      if (runningServoEditor != null && runningServoEditor.IsPlaying()) finished = false;
      if (finished) {
        Stop();
      }
    }
  }
  
  void Stop() {
      // stop
      playing = false;
      
      // stop audio
      if (runningAudioEditor != null) {
        runningAudioEditor.Stop();
        runningAudioEditor = null;
      }
      
      runningServoEditor = null;
      playPreviousPercent = 0;
      playCurrentPercent = 0;
      
      lamachine.set(0);
      
      playNextIndex = 0;
  }
  
  void TogglePlay() {
    if (playing) {
      Stop();
    } else {
      // Play
      playing = true;
      playt0 = playlastt = millis();
      playNextIndex = 0;
      playPreviousPercent = playCurrentPercent = 0;
      runningServoEditor = null;    
      runningAudioEditor = null;
      editedServoEditor = null;    
      editedAudioEditor = null;
    }
  }
  
  boolean checkMouseInside() {
    if (mouseX < x0) return false;
    if (mouseX > x0 + w0) return false;
    if (mouseY < y0) return false;
    if (mouseY > y0 + h0) return false;
    return true;
  }
  
  void mousePressed() {
    if (nameField.IsInside(mouseX, mouseY)) {
        nameField.mousePressed();
        return;
    }
    
    if (checkMouseInside()) {
      
      Stop();
      
      if (mouseY < y0 + h0/2) {
        // audio line
        
        // find audio clicked
        editedAudioEditor = null;
        for (int i = 0; i < editors.size(); i++) {
          ScenarElemEditor editor = editors.get(i);
          if (!editor.type.equals("audio")) continue;
          AudioEditor sEditor = (AudioEditor)editor;
          if (!sEditor.IsMD(mouseX, mouseY)) continue;
          // ok
          editedAudioEditor = sEditor;
          break;
        }
        if (editedAudioEditor != null) {
          audioEditorAnchor0 = pixels2ms(mouseX) - editedAudioEditor.anchort;
          editedAudioEditor.anchort = floor(pixels2ms(mouseX) - audioEditorAnchor0);
        }
      } else {
        // servo line
        
        // find servo clicked
        editedServoEditor = null;
        for (int i = 0; i < editors.size(); i++) {
          ScenarElemEditor editor = editors.get(i);
          if (!editor.type.equals("servo")) continue;
          ServoEditor sEditor = (ServoEditor)editor;
          if (!sEditor.IsMD(mouseX, mouseY)) continue;
          // ok
          editedServoEditor = sEditor;
          editedServoEditor.followMouse(mouseX, mouseY);
          break;
        }
        
        if (editedServoEditor == null) {
          // click in background => create new
          float initPercent = map(mouseY - (y0 + h0/2), 0, h0/2, 100, 0);
          if (initPercent < 0) initPercent = 0;
          if (initPercent > 100) initPercent = 100;
          editedServoEditor = new ServoEditor(this, floor(pixels2ms(mouseX - x0)), initPercent, 0, x0, y0 + h0/2, w0, h0/2);
          editors.add(editedServoEditor);
          editedServoEditor.edited = true;
          editedServoEditor.followMouse(mouseX, mouseY);
        }
      }
    }
  }
  
  void mouseDragged() {
    if (editedServoEditor != null) {
      editedServoEditor.followMouse(mouseX, mouseY);
      ResortEditors();
    }
    if (editedAudioEditor != null) {
      editedAudioEditor.anchort = max(0, floor(pixels2ms(mouseX) - audioEditorAnchor0));
    }
  }
  
  void mouseReleased() {
    if (editedServoEditor != null) {
      editedServoEditor.edited(false);
      editedServoEditor = null;
      // modified => resort
      ResortEditors();
    }
    if (editedAudioEditor != null) {
      editedAudioEditor = null;
    }
  }
  
  void keyPressed() {
    if (nameField.editing) {
      // capture all key events
      nameField.keyPressed();
      if (!nameField.editing) {
        // finished editing
        name = nameField.label;
        if (!scen.name.equals(name)) {
          scen.name = name;
          ScenariosResort();
          gScen_curScenarIndex = gScenarios.indexOf(scen);
          AsLog("new name='"+name+"'");
        }
      }
      return;
    }
    
    if (keyCode == BACKSPACE) {
      Backspace();
      return;
    }
    
    if (key == ' ') {
      TogglePlay();
      return;
    }
    
    println("keyCode="+keyCode);
    if (keyCode == UP) {
      ScenariosSelectNext(-1);
      return;
    }
    if (keyCode == DOWN) {
      ScenariosSelectNext(1);
      return;
    }
  }
  
  void Backspace() {
    if (editedServoEditor != null) {
      // delete it
      editedServoEditor.edited(false);
      editors.remove(editedServoEditor);
      editedServoEditor = null;
      // modified => resort
      ResortEditors();
    }
  }
}

// ******** ScenariosDisplay
float gScen_x0, gScen_y0, gScen_w0, gScen_h0;
AsButton gScen_saveBut;
AsButton gScen_newBut;
AsButton gScen_reloadBut;
AsButton gScen_parsePriv;

int gScen_curScenarIndex = -1;
int gScen_scollIndex = 0;

void ScenariosUIInit(float x, float y, float w, float h) {
  gScen_x0 = x;
  gScen_y0 = y;
  gScen_w0 = w;
  gScen_h0 = h;
  ScenariosChangeAllGameNames();
}

void ScenariosChangeAllGameNames() {
  // change all games names : parse all scenarios and change games names if needed
  if (gScenarios.size() > 0) {
    // list
    for (int i = 0; i < gScenarios.size(); i++) {
      Scenario scenar = gScenarios.get(i);
      if (scenar.name.startsWith("game")) {
        gScen_curScenarIndex = i;
        gScenarEditor = new ScenarEditor(scenar);
        ScenariosSaveCurrent();
      }
    }
    gScen_curScenarIndex = -1;
    gScenarEditor = null;
  }
}

void ScenariosDisplay() {
  float dy0 = fontHeight + 2;
  float y0 = gScen_y0;
  if (gScenarios.size() == 0) return;
  
  fill(255, 50); stroke(255, 100);
  rect(gScen_x0, gScen_y0, gScen_w0, gScen_h0);

  ScenariosResort();

  y0 += dy0;
  String prevPrefix = "";
  int index = 0;
  if (gScenarios.size() > 0) {
    // list
    
    // good index if not first line
    for (int i = 0; i < gScen_scollIndex; i++) {
      Scenario scenar = gScenarios.get(i);
      String nam = scenar.name;
      
      // compute index
      String prefix = nam.substring(0, nam.lastIndexOf('_'));
      if (!prefix.equals(prevPrefix)) {
        index = 1;
        prevPrefix = prefix;
      } else {
        index++;
      }
    }
    
    for (int i = gScen_scollIndex; i < gScenarios.size() && y0 < gScen_y0 + gScen_h0; i++) {
      Scenario scenar = gScenarios.get(i);
      String nam = scenar.name;
      
      // compute index
      String prefix = nam.substring(0, nam.lastIndexOf('_'));
      if (!prefix.equals(prevPrefix)) {
        index = 1;
        prevPrefix = prefix;
      } else {
        index++;
      }
      
      // hilight
      if (gScen_curScenarIndex >= 0 && i == gScen_curScenarIndex) {
        fill(255, 255, 0, 50); noStroke();
        rect(gScen_x0+1, y0-dy0+2, gScen_w0-2, dy0);
      }
      
      // color according to number of elements
      ArrayList <ScenarElem> elements = scenar.elements();
      int elemNumber = elements.size();

      // label
      if (elemNumber <= 1) fill(255); else fill(0, 255, 0); noStroke();
      text(""+index+":"+nam, gScen_x0, y0); y0 += dy0;
    }
    // scrollbar
    int maxLines = floor(gScen_h0/dy0);
    int displayedLines = gScenarios.size() - gScen_scollIndex;
    int visibleLines = displayedLines >= maxLines ? maxLines : displayedLines;
    float scrollBarH = gScen_h0*float(visibleLines)/float(gScenarios.size());
    float scrollBarY = gScen_y0 + gScen_h0*float(gScen_scollIndex)/float(gScenarios.size());
    fill(255); noStroke();
    rect(gScen_x0+gScen_w0-5, scrollBarY, 5, scrollBarH);
  }
  if (gScenarEditor != null) {
    gScenarEditor.Display();
  }
  
  // buttons
  float yyy = gScen_y0 + 20;
  float dyyy = fontHeight + 6 + 2;
  if (gScen_newBut == null) gScen_newBut  = new AsButton("New Scenario", gScen_x0 + gScen_w0 + 2, yyy, 0, fontHeight + 6);
  gScen_newBut.Display();
  yyy += dyyy;

  if (gScen_saveBut == null) gScen_saveBut  = new AsButton("SAVE (json+erl)", gScen_x0 + gScen_w0 + 2, yyy, 0, fontHeight + 6);
  gScen_saveBut.Display();
  yyy += dyyy;
  yyy += dyyy;
  yyy += dyyy;

  if (gScen_reloadBut == null) gScen_reloadBut  = new AsButton("RELOAD json", gScen_x0 + gScen_w0 + 2, yyy, 0, fontHeight + 6);
  gScen_reloadBut.Display();
  yyy += dyyy;

  if (gScen_parsePriv == null) gScen_parsePriv  = new AsButton("(RE)PARSE priv/", gScen_x0 + gScen_w0 + 2, yyy, 0, fontHeight + 6);
  gScen_parsePriv.Display();
  yyy += dyyy;
  
}

int ScenariosParseSoundFolder() {
  int total = 0;
  File privFolder = new File(sketchPath(PRIV_FOLDER));
  File [] privFiles = privFolder.listFiles();
  if (privFiles != null) {
    for (int i = 0; i < privFiles.length; i++) {
      File f = privFiles[i];
      if (f.getName().equals(".DS_Store")) continue;
      if (f.isDirectory()) {
        String folderName = f.getName();
        File [] folderFiles = f.listFiles();
        for (int j = 0; j < folderFiles.length; j++) {
          File ff = folderFiles[j];
          String ffName = ff.getName();
          if (ffName.equals(".DS_Store")) continue;
          String ext = fileNameGetExtension(ffName);
          if (ext.equals("aac")) {
            if (ScenariosCreateScenarioForAudioIfNeeded(folderName, ffName)) {
              total++;
            }
          } else {
            AsLog("E : File not aac:"+folderName+"/"+ffName);
          }
        }
        
        if (total > 0) {
          ScenariosResort();
        }

        //println("Directory " + f.getName());
      }
    }
  }
  
  ScenariosChangeAllGameNames();

  return total;
}

void ScenariosSaveCurrent() {
  if (gScenarEditor == null || gScen_curScenarIndex < 0) return;
      
  // create new scenario from editor
  String newDef = gScenarEditor.computeScenarioDef();
  
  String name = gScenarEditor.name;
  // if game, change name to reflect duration
  if (name.startsWith("game")) {
    // end number
    String numberstr = name.substring(name.lastIndexOf('_')+1);
    float dur_ms = gScenarEditor.duration();
    String prefix = "";
    if (dur_ms == 0) {
      // duration unknown
    } else if (dur_ms < GAME_SHORT_DUR_S*1000) {
      prefix = "_short";
    } else if (dur_ms < GAME_MEDIUM_DUR_S*1000) {
      prefix = "_medium";
    } else {
      prefix = "_long";
    }
    name = "game"+prefix+"_"+numberstr;
  }
  
  Scenario scenar = new Scenario(name, newDef);
  
  // save in data
  gScenarios.set(gScen_curScenarIndex, scenar);
  AsLog("SAVED "+name);
}

void ScenariosSaveAll() {
  // save current edited scenario in json + save json
  ScenariosSaveCurrent();
  
  FileCopy("choreographies.json", "choreographiesSAVE.json");
  ScenariosSaveToFile("choreographies.json");
}

boolean ScenariosMouseIsInside() {
  if (mouseX < gScen_x0) return false;
  if (mouseX > gScen_x0 + gScen_w0) return false;
  if (mouseY < gScen_y0) return false;
  if (mouseY > gScen_y0 + gScen_h0) return false;
  return true;
}

boolean ScenariosListMousePressed() {
  if (!ScenariosMouseIsInside()) return false;

  // save and close previous
  if (gScenarEditor != null) {
    ScenariosSaveCurrent();
    gScenarEditor.Stop();
    gScenarEditor = null;
  }

  int lin = floor((mouseY - gScen_y0 - 4)/float(fontHeight + 2)) + gScen_scollIndex;
  println("clicked lin="+lin);
  if (lin < 0) return false;
  if (lin >= gScenarios.size()) return false;
  Scenario scenar = gScenarios.get(lin);
  
  gScen_curScenarIndex = lin;
  gScenarEditor = new ScenarEditor(scenar);
  return true;
}

void ScenariosSelectNext(int direction) {
  // save and close previous
  if (gScenarEditor != null) {
    ScenariosSaveCurrent();
    gScenarEditor.Stop();
    gScenarEditor = null;
  }

  if (gScen_curScenarIndex < 0) {
    gScen_curScenarIndex = 0;
  } else {
    gScen_curScenarIndex += direction;
    if (gScen_curScenarIndex < 0) gScen_curScenarIndex = 0;
    if (gScen_curScenarIndex >= gScenarios.size()) gScen_curScenarIndex = gScenarios.size()-1;
  }
  Scenario scenar = gScenarios.get(gScen_curScenarIndex);
  gScenarEditor = new ScenarEditor(scenar);
}

void ScenariosMouseDragged() {
  if (gScenarEditor != null) gScenarEditor.mouseDragged();
}

void ScenariosMouseMoved() { 
  if (gScen_saveBut != null && gScen_parsePriv != null && gScen_reloadBut != null && gScen_newBut != null) {
    if (gScen_saveBut.IsInside(mouseX, mouseY)
      || gScen_newBut.IsInside(mouseX, mouseY)
      || gScen_parsePriv.IsInside(mouseX, mouseY)
      || gScen_reloadBut.IsInside(mouseX, mouseY)) {
      cursor(HAND);
      return;
    }
  }
  cursor(ARROW);
}

void ScenariosMouseWheel(float amount) {
  if (amount < 0) {
      if (gScen_scollIndex > 0) {
        gScen_scollIndex -= 1;
        return;
      }
  }
  if (amount > 0) {
      if (gScen_scollIndex < gScenarios.size() - 5) {
        gScen_scollIndex += 1;
        return;
      }
  }
}

void ScenariosMousePressed() {
  if (ScenariosListMousePressed()) return;
  
  if (gScen_saveBut != null && gScen_parsePriv != null && gScen_reloadBut != null && gScen_newBut != null) {
    if (gScen_saveBut.IsInside(mouseX, mouseY)) {
      ScenariosSaveAll();
      AsLog("SAVED choreographies.json");
      return;
    }
    if (gScen_reloadBut.IsInside(mouseX, mouseY)) {
      ScenariosInit("choreographies.json");
      AsLog("RELOADED");
      return;
    }
    if (gScen_parsePriv.IsInside(mouseX, mouseY)) {
      int loaded = ScenariosParseSoundFolder();
      AsLog("PARSED, and loaded:"+loaded);
      return;
    }
    if (gScen_newBut.IsInside(mouseX, mouseY)) {
      // save current
      ScenariosSaveCurrent();
      // create
      Scenario scenar = ScenariosCreateEmptyScenario();
      // edit
      gScen_curScenarIndex = gScenarios.indexOf(scenar);
      gScenarEditor = new ScenarEditor(scenar);
      
      AsLog("Created scenario:"+scenar.name);
      return;
    }
  }
  
  if (gScenarEditor != null) gScenarEditor.mousePressed();
}

void ScenariosMouseReleased() {
  if (gScenarEditor != null) gScenarEditor.mouseReleased();
}
