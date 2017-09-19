let peerConfig = {
    iceServers: [
        {urls: ['stun:stun.l.google.com:19302']},
        {urls: ['turn:159.203.33.68:3478'], username: 'bionic', credential: 'hunter2'},
    ],
    media: {audio: true, video: {width: 320, height: 240, facingMode: "user"}},
}
let peerConnections:{[username: string]: RTCPeerConnection} = {};
let peerStreams:{[username: string]: MediaStream} = {};
let peerDataChannels:{[username: string]: any} = {};
function getPeer(username) {
    if(!peerConnections[username] && peerStreams[state.username]) {
        let rpc = new RTCPeerConnection({iceServers: peerConfig.iceServers});

        let myStream = peerStreams[wsUser];
        if(myStream) { rpc.addStream(myStream) }

        let chan = (rpc as any).createDataChannel("data");
        chan.onmessage = (e) => console.log(e.data);
        peerDataChannels[username] = chan;

        rpc.oniceconnectionstatechange = (e) => {
            if(rpc.iceConnectionState === "failed") {
                closePeer(username);
            }
            dispatchEvent(new CustomEvent("peerUpdate"));
        }
        rpc.onicecandidate = (e) => {
            if(rpc.iceGatheringState === "complete") {
                sendMessage("peerInfo", {sdp: rpc.localDescription}, username);
            }
        }
        (rpc as any).ondatachannel = (e) => {
            let chan = e.channel;
            chan.onmessage = (e) => console.log(e.data);
            peerDataChannels[username] = e.channel;
        }
        (rpc as any).ontrack = (e) => {
            peerStreams[username] = e.streams[0];
            dispatchEvent(new CustomEvent("peerUpdate"));
        }
        peerConnections[username] = rpc;
    }
    return peerConnections[username]
}
function onPeerInfo(event) {
    console.log(`(${event.detail.sender}) ${event.detail.value.sdp.type}`);
    if(event.detail.sender !== state.username) {
        var rpc = getPeer(event.detail.sender);
    }
    if(rpc && event.detail.value.sdp.type === "start") { // synthetic sdp
        rpc.createOffer().then(offer=>rpc.setLocalDescription(offer));
    }
    else if(rpc && event.detail.value.sdp.type === "offer") {
        rpc.setRemoteDescription(new RTCSessionDescription(event.detail.value.sdp));
        rpc.createAnswer().then(answer=>rpc.setLocalDescription(answer));
    }
    else if(rpc && event.detail.value.sdp.type === "answer") {
        rpc.setRemoteDescription(new RTCSessionDescription(event.detail.value.sdp));
    }
    else if(rpc && event.detail.value.sdp.type === "stop") { // synthetic sdp
        closePeer(event.detail.sender);
    }
    dispatchEvent(new CustomEvent("peerUpdate"));
}
function closePeer(username) {
    if(peerStreams[username]) {
        peerStreams[username].getTracks().map(track=>track.stop());
        delete peerStreams[username];
    }
    if(peerConnections[username]) {
        peerConnections[username].close();
        delete peerConnections[username];
    }
    if(peerDataChannels[username]) {
        delete peerDataChannels[username];
    }
}
async function streamingStart() {
    if(!navigator.mediaDevices) {
        alert("Your browser does not support WebRTC!");
        return
    }
    if(!peerStreams[state.username]) {
        let stream = await navigator.mediaDevices.getUserMedia(peerConfig.media);
        detectAudio(stream);
        peerStreams[state.username] = stream;
    }
    if(peerStreams[state.username]) {
        sendMessage("peerInfo", {sdp: {type: "start"}});
    }
}
async function streamingStop() {
    state.users.forEach(closePeer);
    closePeer(state.username);
    sendMessage("peerInfo", {sdp: {type: "stop"}});
}
addEventListener("peerInfo", onPeerInfo);
