

class AsButton {
  float x0, y0, w0, h0;
  String label;
  
  AsButton(String alabel, float x, float y, float w, float h) {
    x0 = x;
    y0 = y;
    w0 = w;
    h0 = h;
    label = alabel;
    w0 = max(w0, textWidth(alabel) + 4*2);
  }
  
  void Display() {
    fill(255, 255, 255, 100); stroke(255);
    rect(x0, y0, w0, h0);
    float ww = textWidth(label);
    fill(255, 255, 255);
    text(label, x0 + w0/2 - ww/2, y0 + h0 - 3);
  }
  
  boolean IsInside(float mx, float my) {
    if (mx < x0) return false;
    if (mx > x0 + w0) return false;
    if (my < y0) return false;
    if (my > y0 + h0) return false;
    return true;
  }
}

class AsTextField {
  float x0, y0, w0, h0;
  String label;
  String editedLabel;
  boolean editing = false;
  int cursorPos;
  float lastBlink;
  boolean blinkOn;
  
  AsTextField(String alabel, float x, float y, float w, float h) {
    x0 = x;
    y0 = y;
    w0 = w;
    h0 = h;
    label = alabel;
    w0 = max(w0, textWidth(alabel) + 4*2);
  }
  
  void Display() {
    if (editing) {
      fill(255, 0, 0, 100); stroke(255, 0, 0);
      w0 = max(w0, textWidth(editedLabel) + 4*2);
    } else {
      fill(255, 255, 255, 100); stroke(255);
      w0 = max(w0, textWidth(label) + 4*2);
    }
    rect(x0, y0, w0, h0);
    fill(255, 255, 255);
    text(editing ? editedLabel : label, x0 + 2, y0 + h0 - 3);
    
    if (editing) {
      
      // cursor
      if (millis() > lastBlink + 500) {
        blinkOn = !blinkOn;
        lastBlink = millis();
      }
      if (blinkOn) {
        String sub = editedLabel.substring(0, cursorPos);
        float xCursor = textWidth(sub);
        noFill(); stroke(255);
        line(x0 + xCursor + 1 + 2, y0+2, x0 + xCursor + 1 + 2, y0 + h0 - 2);
      }
    }
  }
  
  boolean IsInside(float mx, float my) {
    if (mx < x0) return false;
    if (mx > x0 + w0) return false;
    if (my < y0) return false;
    if (my > y0 + h0) return false;
    return true;
  }
  
  void keyPressed() {
    if (keyCode == RETURN || keyCode == ENTER) {
      // OK
      editing = false;
      label = editedLabel;
      return;
    }
    if (keyCode == LEFT) {
      if (cursorPos > 0) cursorPos--;
      return;
    }
    if (keyCode == RIGHT) {
      if (cursorPos < editedLabel.length()) cursorPos++;
      return;
    }
    if (keyCode == BACKSPACE) {
      if (cursorPos > 0) {
        editedLabel = editedLabel.substring(0, cursorPos-1) + editedLabel.substring(cursorPos);
        cursorPos--;
      }
      return;
    }
    if (keyCode == ESC) {
      // abort
      editing = false;
      editedLabel = "";
      return;
    }
    String allchars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_";
    if (allchars.indexOf(""+key) >= 0) {
      String sub1 = editedLabel.substring(0, cursorPos);
      String sub2 = editedLabel.substring(cursorPos);
      editedLabel = sub1+key+sub2;
      cursorPos++;
      return;
    }
  }
  
  void mousePressed() {
    if (!IsInside(mouseX, mouseY)) return;
    if (!editing) editedLabel = label;
    cursorPos = floor(map(mouseX - x0 + 2, 0, textWidth(editedLabel), 0, editedLabel.length()));
    if (cursorPos < 0) cursorPos = 0; 
    cursorPos = min(cursorPos, editedLabel.length());
    lastBlink = millis();
    blinkOn = false;
    editing = true;
  }
    
}
