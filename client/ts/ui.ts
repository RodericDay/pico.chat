function updateUi(lastMessage) {

    if(lastMessage.target && lastMessage.target == uid) {
        connect(ws, lastMessage.sender, lastMessage);
    }

    if(lastMessage.type === "magnet") {
        onMagnet({detail: lastMessage.text});
    }

    var chatMessages = messages.filter(e=>e.type==='message');
    var chatLog = m("div#chat-log", chatMessages
            .map(e=>e.sender==='server'?`${e.text}`:`${e.sender}: ${e.text}`)
            .map(s=>m.trust(marked(s)))
        );

    var inputField = m("input#input-field", {onkeyup: sendMessage});

    var userPanel = m("div#user-panel", [...users]
            .sort()
            .map(makeButton)
        );

    var renderMagnets = m("div#magnetic-surface", magnets
            .map(([[[cs,s]],x,y,z],i)=>m(`div.magnet${cs}#${i}`, {
                style:`left: ${x}px; top: ${y}px; z-index: ${z};`,
            }, s))
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
    m.render(document.getElementById("magnets"), [renderMagnets]);
    var scrollable = document.querySelector('#chat-log');
    scrollable.scrollTop = scrollable.scrollHeight;
    if(lastMessage.type === "message" && lastMessage.sender === uid) {
        numSeen = chatMessages.length;
    }
    document.title = `${title} (${chatMessages.length - numSeen})`;

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

function newUid(uid, message="Choose an alias:") {
    var random = Math.random().toFixed(16).slice(2, 8);
    localStorage.uid = window.prompt(message) || random;
    location.replace(location.origin); // refresh page without hash
}

var title = document.title;
var uid = localStorage.uid;
var users = new Set();
var messages = [
    {type: "message", sender: "server", text: `Welcome ${uid}!`},
    {type: "message", sender: "server", text: "Click [here](https://chat.roderic.ca/?) to change your alias."},
    {type: "message", sender: "server", text: "Click a user to start a call."},
    {type: "message", sender: "server", text: "Click again to hang up."},
];


type magnet = [[string,string][], number, number, number];
var magnets:magnet[] = [];

/* chess */
[...`
♜♞♝♛♚♝♞♜
♟♟♟♟♟♟♟♟
・・・・・・・・
・・・・・・・・
・・・・・・・・
・・・・・・・・
♙♙♙♙♙♙♙♙
♖♘♗♕♔♗♘♖
`].filter(c=>c!=='\n').forEach(function(s, i){
    if(s !== "・") {
        magnets.push([[[".piece", s]], 40*(i%8), 40*(i/8|0), i]);
    }
});

/* codenames */
function setupCodenames(text) {
    text.split('\n').forEach(function(t, i){
        if(t) {
            var states = t.split(',').map((s:string):[string, string]=>[".card", s]);
            magnets.push([states, 90, 90, i]);
        }
    });
}
fetch('codenames.txt').then(r=>r.text()).then(setupCodenames);

var numSeen = messages.length;
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
