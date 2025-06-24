
import { makeEventsFromGroups } from "./vis/groups.js";
import { makeCirclesFromEvents } from "./vis/circles.js";

let events;
let circles;
let minPitch = 127;
let maxPitch = -127;

// my song's bpm = 120 where 1 beat is 1 qn, but in my representation 1 beat is 1 wn
const bpm       = 120 / 4;
let currentBeat = 0;
let start;

let screenH;
let screenW;
let drawing = false;

function handleGroups(data) {
    const result = makeEventsFromGroups(data.groups, minPitch, maxPitch);
    events   = result.events;
    maxPitch = result.max;
    minPitch = result.min;
}

const circleVis = (p) => {
    p.preload = () => {
        // get the groups from the file
        p.loadJSON("notes.json", handleGroups);
    
        // get parent height and width
        const parent = document.getElementById("canvas");
        screenH = 0.95 * parent.offsetHeight;
        screenW = 0.95 * parent.offsetWidth;

        // set up event listeners on the player
        const player = document.getElementById("player");
        player.addEventListener("start", () => {
            drawing = true;
            start   = p.millis();
        });
        player.addEventListener("stop", () => {
            drawing = false;
        });
    };

    p.setup = () => {
        let canvas = p.createCanvas(screenW, screenH);
        canvas.parent("canvas");

        const minColor = p.color(173, 216, 230);
        const maxColor = p.color(50, 55, 100);

        p.noStroke();
        circles = makeCirclesFromEvents(p, events, screenW, screenH, minPitch, maxPitch, minColor, maxColor);
    };

    p.draw = () => {
        if(drawing) {
            p.background(255);
    
            // currentBeat knowing how long it passed since the beginning
            currentBeat = ((p.millis() - start) / 1000) * (bpm / 60);
            
            // draw the circles
            for(let c of circles) {
                c.update(currentBeat);
                c.show();
            }
        }
    };
};

new p5(circleVis);