// get the midi URL from the url parameters
const params   = new URLSearchParams(window.location.search);
const midiURL  = params.get("src");
const fileName = params.get("name"); 

console.log("Got the parameter: ", midiURL, " & name: ", fileName);

// set the player source
document.getElementById("player").src = midiURL;
document.getElementById("title").innerText = "File: " + fileName; 

