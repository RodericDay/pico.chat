var myRPCs = {};
function getRPC(emitter, remoteAgentName) {
    var rpc = myRPCs[remoteAgentName];
    if(rpc===undefined) {
        var servers = {iceServers: [{urls:['stun:stun.l.google.com:19302']}]};
        var rpc = new RTCPeerConnection(servers);
        rpc.pending = true;
        rpc.onicecandidate = e=>onIceCandidate(rpc, emitter, remoteAgentName);
        rpc.ontrack = e=>setupStream(e.streams[0]);
        if (myStream) { rpc.addStream(myStream); }
        myRPCs[remoteAgentName] = rpc;
    }
    return rpc
}

function connect(emitter, remoteAgentName, message) {
    /* emitter should be a function places message somewhere for collection */

    if(!myStream) {
        createMyStream().then(_=>connect(emitter, remoteAgentName, message));
        return
    }

    var rpc = getRPC(emitter, remoteAgentName);

    if(message.type==='new') {
        console.log("connecting to ", remoteAgentName);
        rpc.createOffer()
            .then(offer=>rpc.setLocalDescription(offer))
            .catch(error=>console.log(error));
    }

    else if(message.type==='offer') {
        rpc.setRemoteDescription(new RTCSessionDescription(message));
        console.log("connecting to ", remoteAgentName);
        rpc.createAnswer()
            .then(answer=>rpc.setLocalDescription(answer))
            .catch(error=>console.log(error));
    }

    else if(message.type==='answer') {
        rpc.setRemoteDescription(new RTCSessionDescription(message))
            .catch(error=>console.log(error));
    }

}

var myStream = null;
function createMyStream() {
    var settings = {audio:true,video:{width:320,height:240}};
    // var settings = {audio:true,video:false};
    return navigator.mediaDevices
        .getUserMedia(settings)
        .then(setupStream)
        .catch(error=>console.log(error));
}

function setupStream(stream) {
    var container = document.getElementById(stream.id)
                || document.createElement("video");
    container.id = stream.id;
    container.autoplay = "autoplay";
    container.srcObject = stream;
    if(!myStream) {
        myStream = stream;
        container.muted = true;
    }
    videos.appendChild(container);
    return stream
}

function onIceCandidate(rpc, emitter, remoteAgentName) {
    if(rpc.iceGatheringState==='complete' && rpc.pending) {
        rpc.pending = false;
        var message = {
            target: remoteAgentName,
            type: rpc.localDescription.type,
            sdp: rpc.localDescription.sdp,
        };
        emitter.send(JSON.stringify(message));
    }
}
