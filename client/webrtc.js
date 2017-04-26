var myRPCs = {};
var myStreams = {};
function getRPC(emitter, remoteAgentName) {
    var rpc = myRPCs[remoteAgentName];
    if (rpc === undefined) {
        var servers = { iceServers: [{ urls: ['stun:stun.l.google.com:19302'] }] };
        var rpc = new RTCPeerConnection(servers);
        rpc.oniceconnectionstatechange = function (e) {
            console.log(`(${remoteAgentName}) ${rpc.iceConnectionState}`);
            if (['closed', 'disconnected'].includes(rpc.iceConnectionState)) {
                delete myRPCs[remoteAgentName];
                delete myStreams[remoteAgentName];
                var left = Object.keys(myStreams);
                if (left.length === 1 && left[0] === uid) {
                    myStreams[uid].getTracks().forEach(track => track.stop());
                    delete myRPCs[uid];
                    delete myStreams[uid];
                }
                updateUi({ type: "stream" });
            }
        };
        rpc.onicecandidate = e => onIceCandidate(rpc, emitter, remoteAgentName);
        rpc.ontrack = e => setupStream(e.streams[0], remoteAgentName);
        rpc.addStream(myStreams[uid]);
        myRPCs[remoteAgentName] = rpc;
    }
    return rpc;
}
function connect(emitter, remoteAgentName, message) {
    /* emitter should be a function places message somewhere for collection */
    if (!myStreams[uid]) {
        createMyStream().then(_ => connect(emitter, remoteAgentName, message));
        return;
    }
    var rpc = getRPC(emitter, remoteAgentName);
    if (message.type === 'request') {
        console.log(`(${remoteAgentName}) requesting`);
        rpc.createOffer()
            .then(offer => rpc.setLocalDescription(offer))
            .catch(error => console.log(error));
    }
    else if (message.type === 'offer') {
        rpc.setRemoteDescription(new RTCSessionDescription(message));
        console.log(`(${remoteAgentName}) connecting`);
        rpc.createAnswer()
            .then(answer => rpc.setLocalDescription(answer))
            .catch(error => console.log(error));
    }
    else if (message.type === 'answer') {
        console.log(`(${remoteAgentName}) connecting`);
        rpc.setRemoteDescription(new RTCSessionDescription(message))
            .catch(error => console.log(error));
    }
}
function createMyStream() {
    var settings = { audio: true, video: { width: 320, height: 240 } };
    // var settings = {audio:true,video:false};
    return navigator.mediaDevices
        .getUserMedia(settings)
        .then(stream => setupStream(stream, uid))
        .catch(error => console.log(error));
}
function setupStream(stream, agentName) {
    myStreams[agentName] = stream;
    updateUi({ type: "stream" });
}
function onIceCandidate(rpc, emitter, remoteAgentName) {
    if (rpc.iceGatheringState === 'complete') {
        var message = {
            target: remoteAgentName,
            type: rpc.localDescription.type,
            sdp: rpc.localDescription.sdp,
        };
        emitter.send(JSON.stringify(message));
    }
}
