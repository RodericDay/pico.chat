function updateUi(lastMessage) {

    if(lastMessage.target && lastMessage.target == uid) {
        connect(ws, lastMessage.sender, lastMessage);
    }

    var chatLog = m("div#chat-log", messages
            .filter(e=>e.type==='message')
            .map(e=>m.trust(marked(e.sender + ': ' + e.text)))
        );

    var inputField = m("input#input-field", {onkeyup: sendMessage});

    var userPanel = m("div#user-panel", [...users]
            .sort()
            .map(makeButton)
        );

    var videoStreams = m("div.videoContainerContainer", Object.keys(myStreams)
            .sort()
            .map((u,i)=>m("div.videoContainer", [
                    m("video", {srcObject: myStreams[u], autoplay: true, muted: u==uid}),
                    m("button", {disabled: u==uid, onclick: ()=>hang(u)}, u),
                ])
            )
        );

    if(lastMessage.type === "stream") {
        m.render(document.getElementById("streams"), videoStreams);
    }

    m.render(document.getElementById("main"), [chatLog, userPanel, inputField]);
    var scrollable = document.querySelector('#chat-log');
    scrollable.scrollTop = scrollable.scrollHeight;

}

function makeButton(username) {
    var active = Object.keys(myStreams).includes(username);
    var attrs = {
        disabled: username===uid,
        onclick: ()=>active?hang(username):call(username),
    }
    return m("button", attrs, username + (username===uid?' (you)':''))
}

function call(remoteUid) {
    connect(ws, remoteUid, {type: 'request'});
    updateUi({});
}

function hang(remoteUid) {
    myRPCs[remoteUid].close();
    updateUi({});
}

function sendMessage(event) {
    if(event.keyCode === 13 && this.value) {
        ws.send(JSON.stringify({text: this.value, type: 'message'}));
        this.value = '';
    }
}

window.onload = function() {
    if(window.location.hash !== "#debug") {
        uid = "" + window.prompt("Provide an alias, or use random default.", uid);
    }
    updateUi({});
}
