import ddf.minim.*;
import ddf.minim.analysis.*;
import ddf.minim.spi.*;


class MyAudioPlayer {
  AudioPlayer gSoundFileStream = null;
  float[] gAmplitudes;

  MyAudioPlayer(String audioPath) {
    _analyzeAmplitudes(audioPath);
    gSoundFileStream = minim.loadFile(audioPath, 512);
  }
  
  void Close() {
    if (gSoundFileStream == null) return;
    gSoundFileStream.pause();
    gSoundFileStream.close();
    gSoundFileStream = null;
  }

  boolean _analyzeAmplitudes(String audioPath) {
    AudioRecordingStream stream = minim.loadFileStream(audioPath, 512, false); // audio/01salusalucebarbara.aac
    if (stream == null) {
      println("bad stream");
      return false;
    }
    
    stream.play();
      
    int totalSamples = int( (stream.getMillisecondLength() / 1000.0) * stream.getFormat().getSampleRate() );
    
    // width = 10s
    int bufsize = floor(stream.getFormat().getSampleRate()*10/width);
    MultiChannelBuffer buffer = new MultiChannelBuffer(bufsize, stream.getFormat().getChannels());
    int totalChunks = (totalSamples / bufsize) + 1;
    gAmplitudes = new float[totalChunks];
    
    for(int chunkIdx = 0; chunkIdx < totalChunks; ++chunkIdx) {
      stream.read( buffer );
      
      float mean = 0.0;
      float[] left = buffer.getChannel(0);
      for (int i = 0; i < bufsize; i++) {
        mean += abs(left[i]);
      }
      mean /= float(bufsize);
      gAmplitudes[chunkIdx] = mean;
    }
    
    stream.close();
    stream = null;
    return true;
  }
 
   // from top left
  void AudioDraw(float x0, float y0, float w, float h) {
    if (gSoundFileStream == null) return;
    
    // scale secondes
    fill(255, 255, 0, 50); noStroke();
    rect(x0, y0, w, h);
    stroke(255, 255, 0); noFill();
    rect(x0, y0, w, h);
    stroke(255, 255, 0, 75); noFill();
    for (int i = 0; i < w; i += width/10) {
      line(x0 + i, y0+h, x0 + i, y0);
    }
    
    // draw waveform
    if (gAmplitudes != null) {
      for (int i = 0; i < gAmplitudes.length; i++) {
        line(x0 + i, y0+h, x0 + i, y0+h-gAmplitudes[i]*h);
      }
    }
  
    // cursor
    if (gSoundFileStream.isPlaying()) {
      int cursorPosMs = gSoundFileStream.position();
      float cursorPos = ms2pixels(cursorPosMs);
      noFill(); stroke(255, 0, 0);
      line(x0  + cursorPos, y0+h, x0 + cursorPos, y0);
    }
  }
  
  void Stop() {
    if (gSoundFileStream == null) return;
    gSoundFileStream.pause();
    gSoundFileStream.cue(0);
  }
  
  void Cue(float ms) {
    if (gSoundFileStream == null) return;
    gSoundFileStream.cue(0);
  }
  
  void Play(int ms) {
    if (gSoundFileStream == null) return;
    Cue(ms);
    gSoundFileStream.play();
  }
  
  boolean IsPlaying() {
    if (gSoundFileStream == null) return false;
    return gSoundFileStream.isPlaying();
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
