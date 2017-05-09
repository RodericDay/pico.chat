var peers: {[username: string]: RTCPeerConnection} = {};
var streams: {[username: string]: MediaStream} = {};

function getPeer(username) {
    if(!peers[username]) {
        var servers = {iceServers: [{urls:['stun:stun.l.google.com:19302']}]};
        var rpc = new RTCPeerConnection(servers);
        var icedOnce = false;
        var killTimeout = null;
        rpc.addStream(streams[state.username]);
        rpc.oniceconnectionstatechange = (e) => {
            console.log(e, rpc, rpc.iceConnectionState);
            if(["closed","disconnected"].includes(rpc.iceConnectionState)) {
                killTimeout = setTimeout(()=>cleanUp(username), 2500);
            }
            else {
                clearInterval(killTimeout);
            }
        }
        rpc.onicecandidate = (e) => {
            if(rpc.iceGatheringState === "complete" && !icedOnce) {
                icedOnce = true;
                var data = {
                    type: "peer",
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
    if(!streams[state.username]) {
        console.log("Setting up A/V");
        var settings = {audio:true, video:{width:320,height:240}};
        // var settings = {audio:false, video:{width:320,height:240}};
        // var settings = {audio:true, video:false};
        var assign = (stream) => {
            streams[state.username] = stream;
            renderStreams();
            onPeer(event);
        };
        navigator.mediaDevices.getUserMedia(settings).then(assign);
        return
    }
    var rpc = getPeer(event.detail.sender);
    var forMe = event.detail.target === state.username;
    if(event.detail.sdp === undefined) {
        console.log(`(${event.detail.sender}) 0 -> 1`);
        rpc.createOffer().then(offer=>rpc.setLocalDescription(offer));
    }
    else if(forMe && event.detail.sdp.type === "offer") {
        console.log(`(${event.detail.sender}) 1 -> 2`);
        rpc.setRemoteDescription(new RTCSessionDescription(event.detail.sdp));
        rpc.createAnswer().then(answer=>rpc.setLocalDescription(answer));
    }
    else if(forMe && event.detail.sdp.type === "answer") {
        console.log(`(${event.detail.sender}) 2 -> 3`);
        rpc.setRemoteDescription(new RTCSessionDescription(event.detail.sdp));
    }
}
function call(username) {
    if(username !== state.username) {
        onPeer({detail: {sender: username}})
    }
}
function hangUp(username) {
    if(username !== state.username) {
        if(peers[username]) { peers[username].close() }
    }
}
function cleanUp(username) {
    if(streams[username]) { streams[username].getTracks().forEach(track=>track.stop()); }
    delete streams[username];
    delete peers[username];
    /* if only self video left, shut it down */
    var leftover = Object.keys(streams);
    if(leftover.length === 1 && leftover[0] === state.username) {
        cleanUp(state.username);
    }
    renderStreams();
}
var viewStream = (s) =>
    m("div.streamContainer.magnet", [
        streams[s]
        ? [
            m("video", {srcObject: streams[s], autoplay: true, muted: s===state.username}),
            m("button.streamEnd", {onclick: ()=>hangUp(s)}),
            ]
        : [
            m("button.streamStart", {onclick: ()=>call(s)}),
            ],
        m("span", s),
    ]);
var renderStreams = () =>
    m.render(r,
        (RTCPeerConnection && state.loggedIn)
        ? [...state.users].sort().map(viewStream)
        : []
    );
window.addEventListener("peer", onPeer);
window.addEventListener("user", renderStreams);
var r = document.getElementById("streams");
renderStreams();
