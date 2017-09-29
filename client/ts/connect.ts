let signal = (type, value?) => dispatchEvent(new CustomEvent(type, {detail: value}));
let listen = (type, func) => addEventListener(type, (e)=>func(e.detail));
/*
*
* WebSockets
*
*/
let ws = null;
let wsUser = null;
function wire(kind, value, target=undefined, sender=wsUser) {
    // backend will complain if sender doesn't match sign-on name
    if(value.constructor.name !== "String") {
        value = JSON.stringify(value);
    }
    ws.send(JSON.stringify({kind: kind, value: value, sender: sender, target: target}));
}
function openConnection(username, channel) {
    let isLocal = location.href.match("http://localhost:8000/");
    let wsUrl = isLocal ? "ws://localhost:9160/" : "wss://permanentsignal.com/ws/";
    ws = new WebSocket(wsUrl);
    wsUser = username;
    ws.onopen = (e) => {
        wire("login", channel);
    }
    ws.onclose = (e) => {
        signal("logout", wsUser);
        wsUser = null;
    }
    ws.onmessage = (e) => {
        try {
            var message = JSON.parse(e.data);
            try {
                message.value = JSON.parse(message.value);
            }
            catch(SyntaxError) {
                // pass
            }
        }
        catch(SyntaxError) {
            var message:any = {kind: "socketError", value: e.data}
        }
        signal(message.kind, message);
    }
    ws.onerror = (error) => {
        signal("socketError", error);
    }
}
/*
*
* WebRTC
*
*/
const iceServers = [
    {urls: ['stun:stun.l.google.com:19302']},
    {urls: ['turn:159.203.33.68:3478'], username: 'bionic', credential: 'hunter3'},
];
const streamingConfigs = {
    dataOnly: {audio: false, video: false},
    audioOnly: {audio: true, video: false},
    screen: {audio: true, video: {mediaSource: 'screen'}},
    window: {audio: true, video: {mediaSource: 'window'}},
    default: {audio: true, video: {width: {ideal: 320}, facingMode: "user"}},
}
let currentConstraints = null;
let peerConnections:{[user: string]: RTCPeerConnection} = {};
let peerStreams:{[user: string]: MediaStream} = {};
let peerDataChannels:{[user: string]: any} = {};
function getOrCreatePeerConnection(otherUser) {
    if(!peerConnections[otherUser]) {
        let rpc = new RTCPeerConnection({iceServers: iceServers});
        rpc.oniceconnectionstatechange = (e) => {
            if(rpc.iceConnectionState === "failed") {
                closePeer(otherUser);
            }
            signal("peerUpdate");
        }
        rpc.onicecandidate = (e) => {
            if(e.candidate) {
                let message = {sdp: {type: "candidate"}, candidate: e.candidate};
                wire("peerInfo", message, otherUser)
            }
        }
        (rpc as any).ondatachannel = (e) => {
            peerDataChannels[otherUser] = e.channel;
            peerDataChannels[otherUser].onmessage = (e) => console.log(e.data);
            signal("peerUpdate");
        }
        (rpc as any).ontrack = (e) => {
            peerStreams[otherUser] = e.streams[0];
            signal("onStream", otherUser);
        }
        peerConnections[otherUser] = rpc;
    }
    return peerConnections[otherUser]
}
async function onPeerInfo(info) {
    console.log(`(${info.sender}) ${info.value.sdp.type}`);
    if(info.sender === wsUser) {
        signal("peerUpdate");
        return
    }

    let rpc = getOrCreatePeerConnection(info.sender);
    let msgType = info.value.sdp.type; // sometimes synthetic
    if(msgType === "request") {
        let x = JSON.stringify(currentConstraints);
        let y = JSON.stringify(info.value.constraints);
        if(x!==y) return;
        peerDataChannels[info.sender] = (rpc as any).createDataChannel("data");
        peerDataChannels[info.sender].onmessage = (e) => console.log(e.data);
        if(peerStreams[wsUser]) rpc.addStream(peerStreams[wsUser]);
        let offer = await rpc.createOffer();
        await rpc.setLocalDescription(offer);
        wire("peerInfo", {sdp: rpc.localDescription}, info.sender);
    }
    else if(msgType === "offer") {
        if(peerStreams[wsUser]) rpc.addStream(peerStreams[wsUser]);
        let sdp = new RTCSessionDescription(info.value.sdp);
        await rpc.setRemoteDescription(sdp);
        let answer = await rpc.createAnswer();
        await rpc.setLocalDescription(answer);
        wire("peerInfo", {sdp: rpc.localDescription}, info.sender);
    }
    else if(msgType === "answer") {
        let sdp = new RTCSessionDescription(info.value.sdp);
        rpc.setRemoteDescription(sdp);
    }
    else if(msgType === "candidate") {
        let candidate = new RTCIceCandidate(info.value.candidate);
        rpc.addIceCandidate(candidate);
    }
    else if(msgType === "stop") {
        closePeer(info.sender);
    }
    signal("peerUpdate");
}
function closePeer(user) {
    if(peerStreams[user]) {
        peerStreams[user].getTracks().map(track=>track.stop());
        delete peerStreams[user];
        signal("onStream", user);
    }
    if(peerConnections[user]) {
        peerConnections[user].close();
        delete peerConnections[user];
    }
    if(peerDataChannels[user]) {
        delete peerDataChannels[user];
    }
}
async function streamingStart(config="default") {
    await streamingStop();
    currentConstraints = streamingConfigs[config];
    if(!currentConstraints) {
        alert("You selected an invalid constraint.");
        return streamingStop();
    }
    else if(config!=="dataOnly") {
        try {
            let stream;
            try {
                stream = await navigator.mediaDevices.getUserMedia(currentConstraints);
            }
            catch(error) {
                console.info("temporarily changing resolutions for iPhone");
                currentConstraints.video.width.ideal = 352; // x 288
                stream = await navigator.mediaDevices.getUserMedia(currentConstraints);
                currentConstraints.video.width.ideal = 320;
                console.info("reset temporary change");
            }
            peerStreams[wsUser] = stream;
            signal("onStream", wsUser);
        }
        catch(error) {
            alert(`Cannot start stream because ${error.message}`);
            return streamingStop();
        }
    }
    wire("peerInfo", {sdp: {type: "request"}, constraints: currentConstraints});
}
async function streamingStop() {
    Object.keys(peerConnections).forEach(closePeer);
    closePeer(wsUser);
    currentConstraints = null;
    wire("peerInfo", {sdp: {type: "stop"}});
}
/*
*
* Initialize
*
*/
listen("peerInfo", onPeerInfo);
listen("disconnect", (message)=>{closePeer(message.sender)});
listen("logout", streamingStop);
