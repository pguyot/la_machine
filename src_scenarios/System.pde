import java.io.BufferedReader;
import java.io.InputStreamReader;

void execCommand(String[] args) {
  try {
    ProcessBuilder pb = new ProcessBuilder(args);
    pb.directory(new File(sketchPath("")));
    pb.redirectErrorStream(true);
    Process p = pb.start();
    BufferedReader reader = new BufferedReader(new InputStreamReader(p.getInputStream()));
    String line;
    while ((line = reader.readLine()) != null) {
      println(line);
    }
    int exitCode = p.waitFor();
    if (exitCode != 0) {
      println("Command exited with code: " + exitCode);
    }
  }
  catch (Exception e) {
    println("Error running command!");
    println(e);
  }
}

void FileCopy(String in, String out) {
  execCommand(new String[]{"cp", in, out});
}

void ConvertAAC2MP3(String aacfile, String mp3file) {
  execCommand(new String[]{"/opt/homebrew/bin/ffmpeg", "-y", "-hide_banner", "-loglevel", "error", "-i", aacfile, "-c:a", "libmp3lame", "-qscale:a", "2", mp3file});
}

void MkDir(String dir) {
  execCommand(new String[]{"mkdir", "-p", dir});
}

// file utils


// File String utils

String fileNameGetExtension(String str) {
  if(str != null && str.contains(".")) return str.substring(str.lastIndexOf('.')+1);
  return "";
}

String fileNameRemoveExtension(String str) {
  if(str != null && str.contains(".")) return str.substring(0, str.lastIndexOf('.'));
  return str;
}

String folderNameOfFileName(String str) {
  if(str != null && str.contains("/")) return str.substring(0, str.lastIndexOf('/'));
  return "";
}

String filePathGetFileName(String str) {
  if(str != null && str.contains("/")) return str.substring(str.lastIndexOf('/')+1);
  return "";
}


// strings
// folder/file.aac -> folder/file.mp3
String mp3FileNameForAac(String aacFile) {
  return aacFile+".mp3";
  //int index = aacFile.lastIndexOf(".");
  //if (index < 0) return aacFile+".mp3";
  //return aacFile.substring(0, index)+".mp3";
}

boolean fileExists(String fullPath) {
  File f = new File(fullPath);
  return f.exists();
}
