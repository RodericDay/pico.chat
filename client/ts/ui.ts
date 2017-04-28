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

    if(lastMessage.type === "cards") {
        cardsReceive(lastMessage);
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


function cardsBroadcast() {
    var moves = getCards("selected").map(card=>card.json);
    ws.send(JSON.stringify({type:"cards", moves: moves}));
}

function cardsReceive(message) {
    for(var info of message.moves) {
        var card = document.getElementById(info.id)["card"];
        card.x = info.x;
        card.y = info.y;
        card.z = info.z;
    }
}

function newUid(uid, message="Choose an alias:") {
    var random = Math.random().toFixed(16).slice(2, 8);
    localStorage.uid = window.prompt(message) || random;
    location.replace(location.origin); // refresh page
}

var uid = localStorage.uid;
var users = new Set();
var messages = [];
var ws = null;
window.onload = function() {
    if(location.href.includes('?')) {
        newUid(uid);
    }
    else {
        ws = createWebSocket();
        updateUi({});
    }
}
