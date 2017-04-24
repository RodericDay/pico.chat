function updateUi(lastMessage) {

    if(lastMessage && lastMessage.target && lastMessage.target == uid) {
        connect(ws, lastMessage.sender, lastMessage);
    }

    var chatLog = m("div#chat-log", messages.map(e=>m("div", e.sender + ': ' + e.text)));

    var inputField = m("input#input-field", {onkeyup: sendMessage});

    var userPanel = m("div#user-panel", [...users]
            .filter(u=>u!=uid)
            .sort()
            .map(makeButton)
        );

    var videoStreams = Object.keys(myStreams)
        .sort()
        .map(u=>m("div", [
                m("button", {disabled: u==uid, onclick: ()=>hang(u)}, u),
                m("video", {srcObject: myStreams[u], autoplay: true, muted: u==uid}),
            ])
        );

    m.render(main, [chatLog, userPanel, inputField, videoStreams]);

    var scrollable = document.querySelector('#chat-log');
    scrollable.scrollTop = scrollable.scrollHeight;
}

function makeButton(username) {
    var attrs = {
        disabled: Object.keys(myStreams).includes(username),
        onclick: ()=>call(username)
    }
    return m("button", attrs, username)
}

function call(remoteUid) {
    connect(ws, remoteUid, {type: 'request'});
    updateUi();
}

function hang(remoteUid) {
    myRPCs[remoteUid].close();
    updateUi();
}

function sendMessage(event) {
    if(event.keyCode === 13 && this.value) {
        ws.send(JSON.stringify({text: this.value, type: 'message'}));
        this.value = '';
    }
}

window.onload = updateUi;
