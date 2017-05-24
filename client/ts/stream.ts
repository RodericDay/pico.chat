var peers: {[username: string]: RTCPeerConnection} = {};
var streams: {[username: string]: MediaStream} = {};
var streamConfig = {
    servers: {iceServers: [{urls: ['stun:stun.l.google.com:19302']}]},
    // gum: {audio: false, video: true},
    gum: {audio: true, video: {width: 320, height: 240, facingMode: {exact: "user"}}},
}
function getPeer(username) {
    if(!peers[username] && streams[state.username]) {
        var rpc = new RTCPeerConnection(streamConfig.servers);
        rpc.addStream(streams[state.username]);
        rpc.oniceconnectionstatechange = (e) => {
            if(rpc.iceConnectionState === "failed") {
                closePeer(username);
            }
            renderStreams();
        }
        rpc.onicecandidate = (e) => {
            if(rpc.iceGatheringState === "complete") {
                var data = {
                    type: "peerInfo",
                    target: username,
                    sdp: rpc.localDescription,
                }
                state.ws.send(JSON.stringify(data));
            }
        }
        (rpc as any).ontrack = (e) => {
            streams[username] = e.streams[0];
            renderStreams();
        }
        peers[username] = rpc;
    }
    return peers[username]
}
function onPeer(event) {
    console.log(`(${event.detail.sender}) ${event.detail.sdp.type}`);
    if(event.detail.sender !== state.username) {
        var rpc = getPeer(event.detail.sender);
    }
    if(rpc && event.detail.sdp.type === "start") {
        rpc.createOffer().then(offer=>rpc.setLocalDescription(offer));
    }
    else if(rpc && event.detail.sdp.type === "offer") {
        rpc.setRemoteDescription(new RTCSessionDescription(event.detail.sdp));
        rpc.createAnswer().then(answer=>rpc.setLocalDescription(answer));
    }
    else if(rpc && event.detail.sdp.type === "answer") {
        rpc.setRemoteDescription(new RTCSessionDescription(event.detail.sdp));
    }
    else if(rpc && event.detail.sdp.type === "stop") {
        closePeer(event.detail.sender);
    }
    renderStreams();
}
function closePeer(username) {
    if(streams[username]) {
        for(var track of streams[username].getTracks()) {
            track.stop();
        }
        delete streams[username];
    }
    if(peers[username]) {
        peers[username].close();
        delete peers[username];
    }
}
async function startStreaming() {
    if(!streams[state.username]) {
        var stream = await navigator.mediaDevices.getUserMedia(streamConfig.gum);
        streams[state.username] = stream;
    }
    if(streams[state.username]) {
        state.ws.send(JSON.stringify({type: "peerInfo", sdp: {type: "start"}}));
    }
}
async function stopStreaming() {
    state.users.forEach(closePeer);
    state.ws.send(JSON.stringify({type: "peerInfo", sdp: {type: "stop"}}))
}
var viewStream = (username) => {
    var config = {
        srcObject: streams[username],
        autoplay: true,
        muted: username===state.username,
    }
    return m("div.streamContainer",
        m("video", config),
        m("div.info", username),
    )
}
var renderStreams = function() {
    var root = document.getElementById("streams");
    m.render(root, !state.loggedIn?[]:[
        m("div.streamOptions",
            streams[state.username]
            ? m("img[src=svg/camera-x.svg]", {onclick: stopStreaming})
            : m("img[src=svg/camera.svg].shadow", {onclick: startStreaming}),
        ),
        Object.keys(streams).sort().map(viewStream),
    ])
}
window.addEventListener("peerInfo", onPeer);
window.addEventListener("login", renderStreams);
window.addEventListener("logout", renderStreams);
