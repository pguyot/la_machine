

void execShellCommand(String commandToRun) {
  try {
    Process p = Runtime.getRuntime().exec(commandToRun, null, new File(sketchPath("")));
    p.waitFor();
  }
  catch (Exception e) {
    println("Error running command!"); 
    println(e);
  }
}

void FileCopy(String in, String out) {
  String cmd = "cp "+in+" "+out;
  execShellCommand(cmd);
}

void ConvertToErl(String injson, String outErl) {
  String cmd = "python3 json_to_erlang_multi.py "+injson+" "+outErl;
  execShellCommand(cmd);
}

void ConvertAAC2MP3(String aacfile, String mp3file) {
  String cmd = "/opt/homebrew/bin/ffmpeg -y -hide_banner -loglevel error -i "+aacfile+" -c:a libmp3lame -qscale:a 2 "+mp3file;
  execShellCommand(cmd);
}

void MkDir(String dir) {
  String cmd = "mkdir -p "+dir;
  execShellCommand(cmd);
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
