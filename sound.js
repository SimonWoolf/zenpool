var epsilon = 0.000001;
var context = new AudioContext();

function ding(startFreq) {
  var now = context.currentTime;
  var oscillator = context.createOscillator();
  oscillator.type = "sine";
  oscillator.frequency.setValueAtTime(startFreq, now);

  var gainFilter = context.createGain();
  oscillator.connect(gainFilter);
  gainFilter.connect(context.destination);
  gainFilter.gain.setValueAtTime(epsilon, now);
  gainFilter.gain.exponentialRampToValueAtTime(0.2, now + 0.1);
  gainFilter.gain.exponentialRampToValueAtTime(epsilon, now + 5);

  oscillator.start(now);
  oscillator.stop(now + 5);
}

