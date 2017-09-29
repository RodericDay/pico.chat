if(!window["AudioContext"]) { AudioContext = window["webkitAudioContext"]; }
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
function beep() {
    if(gainNode){
        gainNode.gain.value = 0.5;
        setTimeout(()=>{gainNode.gain.value = 0}, 150);
    }
}
function detectAudio(stream) {
    let audioContext = new AudioContext();
    let analyser = audioContext.createAnalyser();
    let microphone = audioContext.createMediaStreamSource(stream);
    let javascriptNode = audioContext.createScriptProcessor(256, 1, 1);

    analyser.smoothingTimeConstant = 0;

    microphone.connect(analyser);
    analyser.connect(javascriptNode);
    javascriptNode.connect(audioContext.destination);

    let timeout = null;
    let isSpeaking = false;

    javascriptNode.onaudioprocess = function() {
        if(timeout===null) {
            let array =  new Uint8Array(analyser.frequencyBinCount);
            analyser.getByteFrequencyData(array);
            let volume = array.reduce((a,b)=>(a+b));
            let isSpeakingCheck = volume > 1000;
            if(isSpeaking !== isSpeakingCheck) {
                isSpeaking = isSpeakingCheck;
                wire("peerVolume", volume);
            }
            timeout = setTimeout(()=>{timeout=null}, 500);
        }
    }
}
function humanize(sizeInBytes) {
    let chunk = (u,i) => [+(sizeInBytes/Math.pow(10,3*i)).toFixed(1),u];
    return ['B','KB','MB','GB'].map(chunk).filter(([n,u])=>n>1).pop().join(' ')
}
function isEmpty(object) {
    return Object.keys(object).length > 0
}
function sorted(set) {
    /* Combination of TypeScript, Sets, and ES5 needs a workaround */
    var sortable = [];
    set.forEach(item=>sortable.push(item));
    return sortable.sort()
}
