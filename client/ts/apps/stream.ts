let config = {
    iceServers: [
        {urls: ['stun:stun.l.google.com:19302']},
        {urls: ['turn:159.203.33.68:3478'], username: 'bionic', credential: 'hunter2'},
    ],
    media: {audio: true, video: {width: 320, height: 240, facingMode: "user"}},
}

function getPeer(username) {
    if(!state.peers[username] && state.streams[state.username]) {
        var rpc = new RTCPeerConnection({iceServers: config.iceServers});
        rpc.addStream(state.streams[state.username]);
        rpc.oniceconnectionstatechange = (e) => {
            if(rpc.iceConnectionState === "failed") {
                closePeer(username);
            }
            m.redraw();
        }
        rpc.onicecandidate = (e) => {
            if(rpc.iceGatheringState === "complete") {
                sendMessage("peerInfo", {sdp: rpc.localDescription}, username);
            }
        }
        (rpc as any).ontrack = (e) => {
            state.streams[username] = e.streams[0];
            m.redraw();
        }
        state.peers[username] = rpc;
    }
    return state.peers[username]
}
function onPeerInfo(event) {
    console.log(`(${event.detail.sender}) ${event.detail.value.sdp.type}`);
    if(event.detail.sender !== state.username) {
        var rpc = getPeer(event.detail.sender);
    }
    if(rpc && event.detail.value.sdp.type === "start") {
        rpc.createOffer().then(offer=>rpc.setLocalDescription(offer));
    }
    else if(rpc && event.detail.value.sdp.type === "offer") {
        rpc.setRemoteDescription(new RTCSessionDescription(event.detail.value.sdp));
        rpc.createAnswer().then(answer=>rpc.setLocalDescription(answer));
    }
    else if(rpc && event.detail.value.sdp.type === "answer") {
        rpc.setRemoteDescription(new RTCSessionDescription(event.detail.value.sdp));
    }
    else if(rpc && event.detail.value.sdp.type === "stop") {
        closePeer(event.detail.sender);
    }
    m.redraw();
}
function closePeer(username) {
    if(state.streams[username]) {
        for(var track of state.streams[username].getTracks()) {
            track.stop();
        }
        delete state.streams[username];
    }
    if(state.peers[username]) {
        state.peers[username].close();
        delete state.peers[username];
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
                sendMessage("peerVolume", isSpeaking);
            }
            timeout = setTimeout(()=>{timeout=null}, 500);
        }
    }
}
async function streamingStart() {
    if(!navigator.mediaDevices) {
        alert("Your browser does not support WebRTC!");
        return
    }
    if(!state.streams[state.username]) {
        var stream = await navigator.mediaDevices.getUserMedia(config.media);
        detectAudio(stream);
        state.streams[state.username] = stream;
    }
    if(state.streams[state.username]) {
        sendMessage("peerInfo", {sdp: {type: "start"}});
    }
    m.redraw();
}
async function streamingStop() {
    state.users.forEach(closePeer);
    closePeer(state.username);
    sendMessage("peerInfo", {sdp: {type: "stop"}});
    m.redraw();
}
function isEmpty(object) {
    return Object.keys(state.streams).length > 0
}
function onPeerVolume(e:CustomEvent) {
    let div = document.querySelector(`.streamContainer .info.${e.detail.sender}`);
    let isSpeaking = e.detail.value;
    if(div) {
        isSpeaking ? div.classList.add("loud") : div.classList.remove("loud");
    }
}
var viewStream = (username) => {
    var localConfig = {
        srcObject: state.streams[username],
        autoplay: true,
        muted: username === state.username,
    }
    return m("div.streamContainer",
        m("video", localConfig),
        m(`div.info.${username}`, username),
    )
}
let Streams = {
    view: ()=>
        m("div#streamGrid", Object.keys(state.streams).sort().map(viewStream))
}
addEventListener("peerVolume", onPeerVolume);
addEventListener("peerInfo", onPeerInfo);
addEventListener("disconnect", (e:CustomEvent)=>{closePeer(e.detail.value)});
addEventListener("logout", streamingStop);
