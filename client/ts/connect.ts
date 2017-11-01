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
    ws.send(JSON.stringify({kind, value, sender, target}));
}
function openConnection(username, channel) {
    ws = new WebSocket(settings.wsUrl);
    wsUser = username;
    ws.onopen = (e) => {
        wire("login", channel);
    }
    ws.onclose = (e) => {
        signal("logout", wsUser);
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
    ws.onerror = (e) => {
        signal("socketError", e);
    }
}
/*
*
* WebRTC
*
*/
let peerConnections:{[user: string]: RTCPeerConnection} = {};
let peerStreams:{[user: string]: MediaStream} = {};
let peerDataChannels:{[user: string]: any} = {};
function getOrCreatePeerConnection(otherUser) {
    if(!peerConnections[otherUser]) {
        let rpc = new RTCPeerConnection({iceServers: settings.iceServers});
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
            signal("peerDataChannel", peerDataChannels[otherUser]);
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
        let x = JSON.stringify(settings.constraints);
        let y = JSON.stringify(info.value.constraints);
        if(x!==y) return;
        peerDataChannels[info.sender] = (rpc as any).createDataChannel("data");
        signal("peerDataChannel", peerDataChannels[info.sender]);
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
async function streamingStart() {
    await streamingStop();
    let constraints = {audio: settings.audio, video: settings.video};
    if(!constraints) {
        alert("You selected an invalid constraint.");
        return streamingStop();
    }
    else if(constraints.audio || constraints.video) {
        try {
            let stream;
            try {
                stream = await navigator.mediaDevices.getUserMedia(constraints);
            }
            catch(error) {
                console.log("Adjusting settings for iPhone");
                constraints.video.width.ideal = 352; // x 288
                stream = await navigator.mediaDevices.getUserMedia(constraints);
            }
            peerStreams[wsUser] = stream;
            signal("onStream", wsUser);
        }
        catch(error) {
            wire("post", `Failed to set ${JSON.stringify(constraints)} with error ${JSON.stringify(error)}`);
            alert(`Cannot start stream because ${error.message}`);
            return streamingStop();
        }
    }
    wire("peerInfo", {sdp: {type: "request"}, constraints: settings.constraints});
}
async function streamingStop() {
    Object.keys(peerConnections).forEach(closePeer);
    closePeer(wsUser);
    wire("peerInfo", {sdp: {type: "stop"}});
}
/*
*
* Settings / Config
*
*/
let defaults = {
    username: "",
    channel: location.hash,
    wsUrl: `wss://${location.host}/ws/`,
    iceServers: [],
    controls: false,
    audio: true,
    video: {width: {ideal: 320}, facingMode: "user"},
}
const opts = {
    audio: [false],
    controls: [true],
    video: [true, false, {mediaSource: 'screen'}, {mediaSource: 'window'}],
}
const stevedore = {
    set: (obj, prop, value) => {
        obj[prop] = JSON.stringify(value);
        return true
    },
    get: (obj, prop) => {
        try {
            return JSON.parse(obj[prop]);
        }
        catch(error) {
            return defaults[prop];
        }
    },
}
const settings = new Proxy(localStorage, stevedore);
/*
*
* Initialize
*
*/
listen("peerInfo", onPeerInfo);
listen("disconnect", (message)=>{closePeer(message.sender)});
listen("logout", streamingStop);
