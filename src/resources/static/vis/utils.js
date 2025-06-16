
export function randomInt(a, b) {
    return Math.floor(Math.random() * (b - a + 1)) + a;
}

// get absolute pitch from pitch + octave change
export function absPitch(pitch, octCh) {
  return Math.max(0, Math.min(127, pitch + octCh * 12));
}

export function computeColor(p, height, screenH, minColor, maxColor) {
  let relativeHeight = height / screenH;
  return p.lerpColor(minColor, maxColor, relativeHeight);
}

export function computeHeight(pitch, minPitch, maxPitch, screenH, border) {
    return (maxPitch - pitch) / (maxPitch - minPitch) * (screenH - 2 * border) + border;
}