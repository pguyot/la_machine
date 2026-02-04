
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
  // {mp3, <<\"joy/0426.mp3\">>}
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
        
      } else if (command.equals("mp3")) {
        // arg = '<<\"joy/0426.mp3\">>'
        //println("mp3 arg ='"+arg+"'");
        // remove quotes
        int index = arg.indexOf('\"');
        int index2 = arg.lastIndexOf('\"');
        arg = arg.substring(index+1, index2-1);
        
        // intArg contains the duration
        String mp3Path = soundsPath()+arg;
        if (!fileExists(mp3Path)) {
          // fatal error
          intArg = 0;
        } else {
          intArg = getAudioFileDur_ms(mp3Path);
        }
        
        // now:
        // arg = "joy/0426.mp3"
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
 
  // "{wait, 800}, {mp3, <<\"gears/simple2.mp3\">>}, {servo, 35}, ... ,{servo,0}",
  Scenario(String aname, String adef) {
    name = aname;
    def = adef;
    elements = new ArrayList();
  }
  
  ArrayList <ScenarElem> elements() {
    if (elements.size() == 0) {
      parseDef();
    }
    return elements;
  }
  int nbAudioElements() {
    ArrayList <ScenarElem> elems = elements();
    int nb = 0;
    for (ScenarElem elem : elems) {
      if (elem.command.equals("mp3")) nb++;
    }
    return nb;
  }
  int nbServoElements() {
    ArrayList <ScenarElem> elems = elements();
    int nb = 0;
    for (ScenarElem elem : elems) {
      if (elem.command.equals("servo")) nb++;
    }
    return nb;
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
  String lines[] = loadStrings(choreoPath(jsonfile));
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
  ScenariosResort();
  println("Loaded "+gScenarios.size()+" scenarios");
}

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
  saveStrings(choreoPath(file), strinList);
}


boolean ScenariosCreateScenarioForAudioIfNeeded(String folderName, String audioFileName) {
  String audioFileRadix = fileNameRemoveExtension(audioFileName);
  String scenarName = folderName+"_"+audioFileRadix; // joy_aaa game_aaa
  
  boolean found = false;
  for (int i = 0; i < gScenarios.size(); i++) {
    Scenario scenar = gScenarios.get(i);
    String loadedScenarName = scenar.name; // joy_aaa game_short_aaa ...
    
    if (loadedScenarName.equals(scenarName)) {
      found = true;
      break;
    }
    
    if (loadedScenarName.startsWith("game")) {
      // remove _short, _medium, _long
      String numberstr = loadedScenarName.substring(loadedScenarName.lastIndexOf('_')+1);
      String miniName = "game_"+numberstr;
      if (miniName.equals(scenarName)) {
        found = true;
        break;
      }
    }
  }
  if (!found) {
    // create it raw
    String audioFilePath = folderName+"/"+audioFileName;
    String audioDef = "{mp3, <<\\\""+audioFilePath+"\\\">>}";
    Scenario scenar = new Scenario(scenarName, audioDef);
    gScenarios.add(scenar);
    AsLog("Created :"+audioFilePath);
    return true;
  }
  //AsLog("Already present:"+scenarName);
  return false;
}

Scenario ScenariosCreateEmptyScenario() {
  String name = "empty_"+gScenarios.size();
  Scenario scenar = new Scenario(name, "");
  gScenarios.add(scenar);
  ScenariosResort();
  return scenar;
}

void ScenariosResort() {
  if (gScenarios == null) return;
  gScenarios.sort( (a, b) -> { return a.name.compareTo(b.name); } );

}
