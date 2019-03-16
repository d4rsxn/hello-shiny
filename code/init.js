var initVoice = function() {
if (annyang) {
	
//Define the voice commands to be used and their corresponding actions

  var commands = {
    'type *type': function(type) {
      Shiny.onInputChange('type', type);
    },
	
    'origin *origin': function(origin) {
      Shiny.onInputChange('origin', origin);
    },
                    'go to *gt': function(gt) {
      Shiny.onInputChange('gt', gt);
   },
   'show top :top': function(top) {
        
        if(top=='five')
        {
          top=5;
        }
        else if (top=='ten')
        {
          top=10;
        }
        else if (top=='fifteen')
        {
          top=15;
        }
                                else
                                {
                                }
                                Shiny.onInputChange('top',parseInt(top));
   },
   'select :chord_org (and) :chord_dest': function(chord_org,chord_dest){
        Shiny.onInputChange('plotx',chord_org);
        Shiny.onInputChange('ploty',chord_dest);
        },
                                'country *map_country': function(map_country) {
      Shiny.onInputChange('country', map_country);
    },
                                
       'hello shiny': function() { alert('Hello world!'); }         
  };
  annyang.addCommands(commands);
  
  // Tell KITT to use annyang
  SpeechKITT.annyang();

  // Define a stylesheet for KITT to use
  //SpeechKITT.setStylesheet('//cdnjs.cloudflare.com/ajax/libs/SpeechKITT/1.0.0/themes/flat.css');
  SpeechKITT.setStylesheet('//dl.dropboxusercontent.com/s/5gf8ojvw0drqi08/flat.css');
  
  //Show the recognized sentence
  
  SpeechKITT.displayRecognizedSentence(true);
  
  SpeechKITT.setInstructionsText('Speech Recognized: '); 
   
  // Render KITT's interface
  SpeechKITT.vroom();
  annyang.debug();
  annyang.start();

  }
};
$(function() {
  setTimeout(initVoice, 1);
});
