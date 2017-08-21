if(window["AudioContext"]) {
    var audioCtx = new AudioContext();
    var gainNode = audioCtx.createGain();
    var oscillator = audioCtx.createOscillator();
    gainNode.connect(audioCtx.destination);
    gainNode.gain.value = 0;
    oscillator.connect(gainNode);
    oscillator.type = "sine";
    oscillator.detune.value = 0;
    oscillator.frequency.value = 500;
    oscillator.start(0);
}
function beep() {
    if(!document.hasFocus() && gainNode && state.notificationsOn){
        gainNode.gain.value = 0.5;
        setTimeout(()=>{gainNode.gain.value = 0}, 150);
    }
}
addEventListener("post", beep);
