import { computeColor, computeHeight, randomInt } from "./utils.js";

const border     = 100;
const noteRadius = 10;

export function makeCirclesFromEvents(p, events, screenW, screenH, minPitch, maxPitch, minColor, maxColor) 
{
    let circles = [];

    for(let event of events) {
        // won't draw a rest
        if(event.pitch != null) {
            let eX = randomInt(border, screenW - border);
            let eY = computeHeight(event.pitch, minPitch, maxPitch, screenH, border);
            let eColor = computeColor(p, eY, screenH, minColor, maxColor);
            
            if(event.others)
                for (let stone of event.others) {
                    let newY      = computeHeight(event.pitch + stone, minPitch, maxPitch, screenH, border);
                    let newColor  = computeColor(p, newY, screenH, minColor, maxColor);
                    let newRadius = newY - eY - noteRadius;
                    
                    circles.push(new Circle(p, eX, eY, newRadius, newColor, event.start, event.end));
                }
            circles.push(new Circle(p, eX, eY, noteRadius, eColor, event.start, event.end));
        }
    }

    return circles;
}

export class Circle {
  constructor(p, x, y, rad, colour, start, end) {
    this.p      = p;
    this.x      = x;
    this.y      = y;
    this.rad    = rad;
    this.colour = colour;
    this.start  = start;
    this.end    = end;
    this.alph   = 255;
  }
  
  // at a certain beat, lower the circle's alpha if it's still visible
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
  
  // draw the circle if visible
  show() {
    if(this.alph > 0) {
      (this.p).fill((this.p).red(this.colour), (this.p).green(this.colour), (this.p).blue(this.colour), this.alph);
      (this.p).circle(this.x, this.y, this.rad * 2);
    }
  }
}