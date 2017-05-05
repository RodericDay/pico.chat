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

    var renderMagnets = [
        m("div#magnetic-surface", magnets
            .map(([[s],x,y,z],i)=>m(`${s}#${i}`, {
                style:`left: ${x}px; top: ${y}px; z-index: ${z};`,
            }))
        ),
        m("div#magnetic-actions",
          m("button", `(${picked.length})`),
          Object.keys(actions)
            .filter(s=>picked.length>0 && s[0]!=='_')
            .map(s=>m("button", {onclick: actions[s]}, s)))
    ];

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


type magnet = [string[], number, number, number];
var magnets:magnet[] = [];

/* chess */
// [...`
// ♜♞♝♛♚♝♞♜
// ♟♟♟♟♟♟♟♟
// ・・・・・・・・
// ・・・・・・・・
// ・・・・・・・・
// ・・・・・・・・
// ♙♙♙♙♙♙♙♙
// ♖♘♗♕♔♗♘♖
// `].filter(c=>c!=='\n').forEach(function(s, i){
//     if(s !== "・") {
//         magnets.push([[[".piece", s]], 40*(i%8), 40*(i/8|0), i]);
//     }
// });

/* codenames */
function setupCodenames(text) {
    text.split('\n').reverse().forEach(function(t, i){
        if(t) {
            var states = t.split(',').map(s=>`div.magnet.card[text=${s}]`);
            magnets.push([states, -5, 5, i]);
        }
    });
    var [p1, p2] = Math.random() > 0.5 ? ["blue", "red"] : ["red", "blue"];
    var squares = ("black,"+"blue,".repeat(8)+"red,".repeat(8)+"gray,".repeat(7)+p1).split(",");
    for(var color of squares) {
        magnets.push([[`div.magnet.card.${color}`], -5, 5, 2000])
    }
    /* little trick to generate a dynamic image */
    var canvas = document.createElement("canvas");
    canvas.width = 60;
    canvas.height = 60;
    var ctx = canvas.getContext("2d");
    ctx.fillStyle = p1;
    ctx.fillRect(0, 0, 60, 60);
    var src1 = canvas.toDataURL();
    ctx.fillStyle = "white";
    ctx.fillRect(2, 2, 56, 56);
    for(var i=0;i<squares.length;i++) {
        var r = Math.floor(Math.random()*(squares.length-1));
        squares.push(squares.splice(r, 1)[0]);
    }
    squares.forEach((color, i)=>{
            ctx.fillStyle = color;
            ctx.fillRect(3+11*(i%5), 3+11*(i/5|0), 10, 10);
        });
    var src2 = canvas.toDataURL();
    magnets.push([[`img.magnet[src=${src1}]`,`img.magnet[src=${src2}]`], 50, 50, 1000]);
}
fetch2('codenames.txt', setupCodenames);

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
