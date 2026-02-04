import ddf.minim.*;
import ddf.minim.analysis.*;
import ddf.minim.spi.*;


class MyAudioPlayer {
  AudioPlayer pPlayStream = null;
  float[] gAmplitudes;

  MyAudioPlayer(String audioPath) {
    _analyzeAmplitudes(audioPath);
    pPlayStream = minim.loadFile(audioPath, 512);
  }
  
  void Close() {
    if (pPlayStream == null) return;
    pPlayStream.pause();
    pPlayStream.close();
    pPlayStream = null;
  }

  boolean _analyzeAmplitudes(String audioPath) {
    AudioRecordingStream stream = minim.loadFileStream(audioPath, 512, false); // audio/01salusalucebarbara.mp3
    if (stream == null) {
      println("bad stream");
      return false;
    }
    
    stream.play();
    int dur_ms = stream.getMillisecondLength();
    int ww = round(ms2pixels(dur_ms))+1;
    float sampleRate = stream.getFormat().getSampleRate();
    int nbChannels = stream.getFormat().getChannels();
    int bufferSize = round(pixels2ms(1)*sampleRate/1000.0);
    MultiChannelBuffer buffer = new MultiChannelBuffer(bufferSize, nbChannels);
    gAmplitudes = new float[ww];
    for (int ci = 0; ci < ww; ci++) {
      stream.read( buffer );
      float mean = 0.0;
      float[] left = buffer.getChannel(0);
      for (int i = 0; i < bufferSize; i++) {
        mean += abs(left[i]);
      }
      mean /= float(bufferSize);
      gAmplitudes[ci] = mean;
    }
        
    stream.close();
    stream = null;
    return true;
  }
 
   // from top left
  void AudioDraw(float x0, float y0, float w, float h) {
    int dur_ms = floor(pixels2ms(w));

    if (pPlayStream == null) return;
    
    // scale secondes
    fill(255, 255, 0, 50); noStroke();
    rect(x0, y0, w, h);
    stroke(255, 255, 0); noFill();
    rect(x0, y0, w, h);
    
    stroke(255, 255, 0, 75); noFill();
    for (int ms = 0; ms < dur_ms; ms += 100) {
      int i = round(ms2pixels(ms));
      line(x0 + i, y0+h, x0 + i, y0);
    }
    stroke(255, 255, 0, 120); noFill();
    for (int ms = 0; ms < dur_ms; ms += 1000) {
      int i = round(ms2pixels(ms));
      line(x0 + i, y0+h, x0 + i, y0);
    }
    
    // draw waveform
    if (gAmplitudes != null) {
      stroke(255, 255, 0, 200); noFill();
      line(x0, y0 + h/2, x0+w, y0 + h/2);
      for (int i = 0; i < gAmplitudes.length; i++) {
        float ampl = gAmplitudes[i]*h;
        line(x0 + i, y0+h/2-ampl/2, x0 + i, y0+h/2+ampl/2);
      }
    }
  
    // cursor
    if (pPlayStream.isPlaying()) {
      int cursorPosMs = pPlayStream.position();
      float cursorPos = ms2pixels(cursorPosMs);
      noFill(); stroke(255, 0, 0);
      line(x0  + cursorPos, y0+h, x0 + cursorPos, y0);
    }
  }
  
  void Stop() {
    if (pPlayStream == null) return;
    pPlayStream.pause();
    pPlayStream.cue(0);
  }
    
  void Play() {
    if (pPlayStream == null) return;
    pPlayStream.cue(0);
    pPlayStream.play();
  }
  
  boolean IsPlaying() {
    if (pPlayStream == null) return false;
    return pPlayStream.isPlaying();
  }

}



// audio utils

int getAudioFileDur_ms(String audioPath) {
  AudioRecordingStream stream = minim.loadFileStream(audioPath, 512, false);
  if (stream == null) {
    println("bad stream");
    return 0;
  }
  
  stream.play();
    
  int dur_ms = stream.getMillisecondLength();  
  stream.close();
  stream = null;
  
  return dur_ms;
}
