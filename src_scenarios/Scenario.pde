
import java.util.*;

// *************** ScenarElem

class ScenarElem {
  String def;
  String command;
  String arg;
  int intArg;
  String arg2;
  int intArg2;
  String stringArg;
  
  ScenarElem(String adef) { // "{cmd, arg}"
    def = adef;
    parseDef();
  }
  
  // {wait, 800}
  // {wait,sound}
  // {servo, 35}
  // {aac, <<\"joy/0426.aac\">>}
  void parseDef() {
    // remove {}
    String inDef = def.substring(1, def.length()-1);
    inDef = inDef.trim();
    String[] elemsDefs = split(inDef, ",");
    
    if (elemsDefs.length > 1) {
      command = elemsDefs[0].trim();
      arg = elemsDefs[1].trim();
      
      //
      if ((command.equals("wait") && !arg.equals("sound")) || command.equals("servo")) {
        intArg = int(arg);
        
        if (command.equals("servo") && elemsDefs.length > 2) {
          arg2 = elemsDefs[2].trim();
          intArg2 = int(arg2);
        } else {
          arg2 = "";
          intArg2 = -1;
        }
        
      } else if (command.equals("aac")) {
        // arg = '<<\"joy/0426.aac\">>'
        //println("aac arg ='"+arg+"'");
        // remove quotes
        int index = arg.indexOf('\"');
        int index2 = arg.lastIndexOf('\"');
        arg = arg.substring(index+1, index2-1);
        
        // intArg contains the duration
        // if mp3 file does not exist, create it
        String mp3Path = sketchPath(MP3_FOLDER)+mp3FileNameForAac(arg);
        if (!fileExists(mp3Path)) {
          String aacPath = sketchPath(PRIV_FOLDER)+arg;
          println("Converting "+aacPath+" to "+mp3Path);
          // first create necessary directories
          MkDir(MP3_FOLDER+folderNameOfFileName(arg));
          ConvertAAC2MP3(aacPath, mp3Path);
        }
        if (!fileExists(mp3Path)) {
          // fatal error
          intArg = 0;
        } else {
          intArg = getAudioFileDur_ms(mp3Path);
        }
        
        // now:
        // arg = "joy/0426.aac"
        // intArg = dur_ms
      }
    }
    
    //println("def='"+def+"' command='"+command+"' arg='"+arg+"' intArg="+intArg);
  }
  
  void recomputeDef() {
  }
  
  String getDef() {
    if (def != null && !def.equals("")) return def;
    recomputeDef();
    return def;
  }
}

// *************** Scenario

class Scenario {
  String name;
  String def;
  ArrayList <ScenarElem> elements;
 
  // "{wait, 800}, {aac, <<\"gears/simple2.aac\">>}, {servo, 35}, ... ,{servo,0}",
  Scenario(String aname, String adef) {
    name = aname;
    def = adef;
    elements = new ArrayList();
  }
  
  ArrayList <ScenarElem> parseElements() {
    if (elements.size() == 0) {
      parseDef();
    }
    return elements;
  }
  
  String getDef() {
    return def;
  }
   
  void parseDef() {
     elements = new ArrayList();
     //println("... Parsing :'"+def+"'");
     String[] elemsDefs = split(def, "{");
     if (elemsDefs.length == 0) {
       println("No elements !");
     } else {
       for (int i = 0; i < elemsDefs.length; i++) {
         String elemDef = elemsDefs[i]; // "wait, 800}, "
         if (elemDef.length() > 0) {
           int endBracket = elemDef.indexOf('}');
           if (endBracket >= 0) {
             elemDef = elemDef.substring(0, endBracket); // wait, 800
             ScenarElem elem = new ScenarElem("{"+elemDef+"}");
             elements.add(elem);
           } else {
             println("no end bracket: '"+elemDef+"'");
           }
         }
       }
    }
  }
}

/// ************ scenarios List

String gJsonFile;
ArrayList <Scenario> gScenarios;
ScenarEditor gScenarEditor = null;

void ScenariosInit(String jsonfile) {
  ScenariosLoadFile(jsonfile);
}

void ScenariosLoadFile(String jsonfile) {
  String lines[] = loadStrings(jsonfile);
  gJsonFile = jsonfile;
  
  gScenarios = new ArrayList();
  for (int i = 0; i < lines.length; i++) {
    String lin = lines[i];
    String [] parts = split(lin, ":");
    if (parts.length > 1) {
      String name = parts[0];
      String scenarioDef = parts[1];
      // cleanup
      // name '  "clockwork"'
      int index = name.indexOf('"');
      int index2 = name.lastIndexOf('"');
      name = name.substring(index+1, index2);
      // def : ' "{wait, 500}, {servo, 80}, {wait, 200}, {servo, 30},{wait,200},{servo,100},{wait,10},{servo,0}",'
      index = scenarioDef.indexOf('"');
      index2 = scenarioDef.lastIndexOf('"');
      scenarioDef = scenarioDef.substring(index+1, index2);
      Scenario scenar = new Scenario(name, scenarioDef);
      gScenarios.add(scenar);
    }
  }
  println("Loaded "+gScenarios.size()+" scenarios");}

void ScenariosSaveToFile(String file) {
  String[] strinList = new String[gScenarios.size() + 2];
  strinList[0] = "{";
  for (int i = 0; i < gScenarios.size(); i++) {
    Scenario scenar = gScenarios.get(i);
    String name = scenar.name;
    String def = scenar.getDef();
    String lin = "  "+"\""+name+"\""+": "+"\""+def+"\"" + ((i == gScenarios.size() - 1) ? "" : ",");
    strinList[i+1] = lin;
  }
  strinList[gScenarios.size()+1] = "}";
  saveStrings(file, strinList);
}

// audioFileName = "joy/aaa.aac" "game/aaa.aac"
// return = joy_aaa
// game_short_aaa
String ScenarioNameForAudio(String audioFilePath) {
  String folder = folderNameOfFileName(audioFilePath);
  return folder+"_"+fileNameRemoveExtension(filePathGetFileName(audioFilePath));
}

boolean ScenariosCreateScenarioForAudioIfNeeded(String folderName, String audioFileName) {
  String scenarName = ScenarioNameForAudio(folderName+"/"+audioFileName); // joy_aaa game_short_aaa
  
  boolean found = false;
  for (int i = 0; i < gScenarios.size(); i++) {
    Scenario scenar = gScenarios.get(i);
    if (scenar.name.equals(scenarName)) {
      found = true;
      break;
    }
  }
  if (!found) {
    // create it raw
    String audioFilePath = folderName+"/"+audioFileName;
    String audioDef = "{aac, <<\\\""+audioFilePath+"\\\">>}";
    Scenario scenar = new Scenario(scenarName, audioDef);
    gScenarios.add(scenar);
    AsLog("Created :"+audioFilePath);
    return true;
  }
   return false;
}

void ScenariosResort() {
  if (gScenarios == null) return;
  gScenarios.sort( (a, b) -> { return a.name.compareTo(b.name); } );

}
