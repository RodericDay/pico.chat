/*
*
* WebSockets
*
*/
let ws = null;
let wsUser = null;
function sendMessage(kind, value, target=undefined, sender=wsUser) {
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
        sendMessage("login", channel);
    }
    ws.onclose = (e) => {
        dispatchEvent(new CustomEvent("logout", {detail: wsUser}));
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
        dispatchEvent(new CustomEvent(message.kind, {detail: message}));
    }
    ws.onerror = (error) => {
        dispatchEvent(new CustomEvent("socketError", {detail: error}));
    }
}
function sync(eventName, objectName) {
    // helper for emission and modification of serialized objects
    addEventListener("connect", (e:CustomEvent)=>{
        if(e.detail.value!==wsUser){
            sendMessage(eventName, window[objectName], e.detail.value);
        }
    });
    addEventListener(eventName, (e:CustomEvent)=>{
        window[objectName] = e.detail.value;
        dispatchEvent(new CustomEvent("socketEvent"));
    });
}
/*
*
* WebRTC
*
*/
const streamingConfigs = {
    dataOnly: null,
    audioOnly: {audio: true, video: false},
    screen: {audio: true, video: {mediaSource: 'screen' || 'window'}}, // mozilla only?
    default: {audio: true, video: {width: {ideal: 352}, facingMode: "user"}}, // safari 352x288
}
let peerConnections:{[user: string]: RTCPeerConnection} = {};
let peerStreams:{[user: string]: MediaStream} = {};
let peerDataChannels:{[user: string]: any} = {};
function getOrCreatePeerConnection(otherUser) {
    if(!peerConnections[otherUser]) {
        let iceServers = [
            {urls: ['stun:stun.l.google.com:19302']},
            {urls: ['turn:159.203.33.68:3478'], username: 'bionic', credential: 'hunter3'},
        ];
        let rpc = new RTCPeerConnection({iceServers: iceServers});
        rpc.oniceconnectionstatechange = (e) => {
            if(rpc.iceConnectionState === "failed") {
                closePeer(otherUser);
            }
            dispatchEvent(new CustomEvent("peerUpdate"));
        }
        rpc.onicecandidate = (e) => {
            if(e.candidate===null) { // rpc.iceGatheringState === "complete"
                console.log(`(${wsUser}) ${rpc.localDescription.type}`);
                sendMessage("peerInfo", {sdp: rpc.localDescription}, otherUser);
            }
        }
        (rpc as any).ondatachannel = (e) => {
            peerDataChannels[otherUser] = e.channel;
            dispatchEvent(new CustomEvent("peerUpdate"));
        }
        (rpc as any).ontrack = (e) => {
            peerStreams[otherUser] = e.streams[0];
            dispatchEvent(new CustomEvent("peerUpdate"));
        }
        peerConnections[otherUser] = rpc;
    }
    return peerConnections[otherUser]
}
async function onPeerInfo(event) {
    console.log(`(${event.detail.sender}) ${event.detail.value.sdp.type}`);
    if(peerStreams[wsUser] && event.detail.sender !== wsUser) {
        // if this user is not streaming, ignore requests
        var rpc = getOrCreatePeerConnection(event.detail.sender);
    }
    if(rpc && event.detail.value.sdp.type === "request") { // synthetic sdp
        rpc.addStream(peerStreams[wsUser]);
        let offer = await rpc.createOffer();
        rpc.setLocalDescription(offer);
    }
    else if(rpc && event.detail.value.sdp.type === "offer") {
        rpc.addStream(peerStreams[wsUser]);
        let sdp = new RTCSessionDescription(event.detail.value.sdp);
        rpc.setRemoteDescription(sdp);
        let answer = await rpc.createAnswer();
        rpc.setLocalDescription(answer);
    }
    else if(rpc && event.detail.value.sdp.type === "answer") {
        let sdp = new RTCSessionDescription(event.detail.value.sdp);
        rpc.setRemoteDescription(sdp);
    }
    else if(rpc && event.detail.value.sdp.type === "stop") { // synthetic sdp
        closePeer(event.detail.sender);
    }
    dispatchEvent(new CustomEvent("peerUpdate"));
}
function closePeer(user) {
    if(peerStreams[user]) {
        peerStreams[user].getTracks().map(track=>track.stop());
        delete peerStreams[user];
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
    let constraints = streamingConfigs[config];
    if(constraints===undefined) {
        alert("You selected an invalid constraint.");
    }
    if(config!=="dataOnly") {
        try {
            let stream = await navigator.mediaDevices.getUserMedia(constraints);
            peerStreams[wsUser] = stream;
            dispatchEvent(new CustomEvent("newStream"));
        }
        catch(error) {
            alert(`Cannot start stream because ${error.message}`);
        }
    }
    sendMessage("peerInfo", {sdp: {type: "request"}});
}
async function streamingStop() {
    Object.keys(peerConnections).forEach(closePeer);
    closePeer(wsUser);
    sendMessage("peerInfo", {sdp: {type: "stop"}});
}
addEventListener("peerInfo", onPeerInfo);
