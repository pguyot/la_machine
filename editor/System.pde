import java.nio.file.Files;
import java.nio.file.StandardCopyOption;

void FileCopy(String in, String out) {
  try {
    Files.copy(new File(in).toPath(), new File(out).toPath(), StandardCopyOption.REPLACE_EXISTING);
  } catch (Exception e) {
    println("Error copying " + in + " to " + out);
    println(e);
  }
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

boolean fileExists(String fullPath) {
  File f = new File(fullPath);
  return f.exists();
}
