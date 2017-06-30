function getPeer(username) {
    if(!state.peers[username] && state.streams[state.username]) {
        var rpc = new RTCPeerConnection({iceServers: config.iceServers});
        rpc.addStream(state.streams[state.username]);
        rpc.oniceconnectionstatechange = (e) => {
            if(rpc.iceConnectionState === "failed") {
                closePeer(username);
            }
            renderStreams();
        }
        rpc.onicecandidate = (e) => {
            if(rpc.iceGatheringState === "complete") {
                sendMessage("peerInfo", {sdp: rpc.localDescription}, username);
            }
        }
        (rpc as any).ontrack = (e) => {
            state.streams[username] = e.streams[0];
            renderStreams();
        }
        state.peers[username] = rpc;
    }
    return state.peers[username]
}
function onPeer(event) {
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
    renderStreams();
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
async function startStreaming() {
    if(!state.streams[state.username]) {
        var stream = await navigator.mediaDevices.getUserMedia(config.media);
        state.streams[state.username] = stream;
    }
    if(state.streams[state.username]) {
        sendMessage("peerInfo", {sdp: {type: "start"}});
    }
}
async function stopStreaming() {
    state.users.forEach(closePeer);
    sendMessage("peerInfo", {sdp: {type: "stop"}});
}
function uploadFile(event) {
    for(let file of event.target.files) {
        let reader = new FileReader();
        // humanize filesize
        // breaks w/ 0
        let size = ['B','KB','MB','GB'].map((u,i)=>[+(file.size/Math.pow(10,3*i)).toFixed(1),u]).filter(([n,u])=>n>1).pop().join('');
        reader.onload = (e) => {
            sendMessage("post", `<a download="${file.name}" href="${reader.result}">${file.name} (${size})</a>`)
        }
        reader.readAsDataURL(file);
    }
}
var viewStream = (username) => {
    var config = {
        srcObject: state.streams[username],
        autoplay: true,
        muted: username === state.username,
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
            m("input#fileInput[type=file][multiple=multiple]", {onchange: uploadFile}),
            m("img[src=svg/upload.svg].shadow", {onclick: ()=>{
                (document.getElementById("fileInput") as HTMLInputElement).click()
            }}),
            state.streams[state.username]
            ? m("img[src=svg/camera-x.svg]", {onclick: stopStreaming})
            : m("img[src=svg/camera.svg].shadow", {onclick: startStreaming}),
        ),
        Object.keys(state.streams).sort().map(viewStream),
    ])
}
window.addEventListener("peerInfo", onPeer);
window.addEventListener("connect", renderStreams);
window.addEventListener("disconnect", renderStreams);
window.addEventListener("logout", renderStreams);
