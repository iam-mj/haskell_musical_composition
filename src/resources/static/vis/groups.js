import { absPitch } from "./utils.js";

export function makeEventsFromGroups(groups, minPitch, maxPitch) {

    // will have to compute the time to trigger each circle
    let time     = 0;
    const events = [];

    for(let group of groups)
    {
        let event = {};
        event.start  = time;
        event.end    = time + group.root.duration;
        event.others = group.semitones;

        if(group.type != "rest")
        {
            event.pitch = absPitch(group.root.pitch, group.root.octaveChange);
            if(event.pitch < minPitch) minPitch = event.pitch;
            if(event.pitch > maxPitch) {
                maxPitch = event.pitch;
                if(event.others) maxPitch = event.pitch + event.others[0];
            }
        }
        else event.pitch = null;
        
        events.push(event);
        time = event.end;
    }

    return { events: events, max: maxPitch, min: minPitch };
}