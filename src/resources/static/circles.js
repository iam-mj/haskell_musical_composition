let groups  = [];
let circles = [];
let minPitch = 127;
let maxPitch = 0;
let minColor;
let maxColor;

const bpm       = 120;
let currentBeat = 0;
let start;
let screenH;
let screenW;
const noteRadius = 10;

// TODO: start visualizer when starting the player

function preload() {
  loadJSON('notes.json', handleGroups);
  const parent = document.getElementById('canvas');
  screenH = 0.95 * parent.offsetHeight;
  screenW = 0.95 * parent.offsetWidth;
}

function absPitch(pitch, octCh) {
  return max(0, min(127, pitch + octCh * 12));
}

// pitch -> between -1 & 12 (result of pitchToInt)
function handleGroups(data) {
  // will have to compute the time to trigger each circle
  let time = 0;
  const events = [];
  
  for(let group of data.groups)
  {
    let event = {};
    event.start  = time;
    event.end    = time + group.root.duration;
    event.others = group.semitones;
    // TODO: absolute pitch - maybe send this directly
    if(group.type != "rest")
    {
      event.pitch = absPitch(group.root.pitch, group.root.octaveChange);
      if(event.pitch < minPitch) minPitch = event.pitch;
      if(event.pitch > maxPitch) {
        maxPitch = event.pitch;
        if(event.others) maxPitch = event.pitch + event.others[0];
      }
    }
    else {
      event.pitch = null;
    }
    events.push(event);
    time = event.end;
  }
  
  groups = groups.concat(events);
}

function computeHeight(pitch) {
  return (maxPitch - pitch) / (maxPitch - minPitch) * (screenH - 100) + 50;
}

function computeColor(height) {
  let relativeHeight = height / screenH;
  return lerpColor(minColor, maxColor, relativeHeight);
}

function setup() {
  let canvas = createCanvas(screenW, screenH);
  canvas.parent('canvas');

  noStroke();
  start = millis();
  minColor = color(173, 216, 230);
  maxColor = color(50, 55, 100);
  
  // frameCount + frameRate for controlling duration + fading out
  
  // TODO: make sure they don't go off the screen
  for(let event of groups)
  {
    // won't draw a rest
    if(event.pitch > 0)
    {
      let eX = random(50, screenW - 50);
      let eY = computeHeight(event.pitch);
      let eColor = computeColor(eY);

      if(event.others) {
        for (let stone of event.others) {
          let newY      = computeHeight(event.pitch + stone);
          let newColor  = computeColor(newY);
          let newRadius = newY - eY - noteRadius;
          
          circles.push(new Circle(eX, eY, newRadius, newColor, event.start, event.end));
        }
      }
      
      circles.push(new Circle(eX, eY, noteRadius, eColor, event.start, event.end));
    }
    
  }
  
}

class Circle {
  constructor(x, y, rad, colour, start, end)
  {
    this.x      = x;
    this.y      = y;
    this.rad    = rad;
    this.colour = colour;
    this.start  = start;
    this.end    = end;
    this.alph   = 255;
  }
  
  update(beat) {
    let visible = beat >= this.start && beat <= this.end;
    
    if(visible) {
      let progress = (beat - this.start) / (this.end - this.start);
      this.alph = 255 * (1 - progress);
    }
    else {
      this.alph = 0;
    }
  }
  
  show() {
    if(this.alph > 0) {
      fill(red(this.colour), green(this.colour), blue(this.colour), this.alph);
      circle(this.x, this.y, this.rad * 2);
    }
  }
}

function draw() {
  background(255);
  
  // currentBeat knowing how long it passed since the beginning
  currentBeat = ((millis() - start) / 1000) * (bpm / 60);
  
  for(let c of circles)
  {
    c.update(currentBeat);
    c.show();
  }
}