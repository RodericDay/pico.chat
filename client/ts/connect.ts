function openConnection() {
    state.ws = new WebSocket(config.wsUrl);
    state.ws.onopen = (e) => {
        location.hash = state.channel;
        sendMessage("login", state.channel);
    }
    state.ws.onclose = (e) => {
        dispatchEvent(new CustomEvent("logout", {detail: state.username}));
    }
    state.ws.onmessage = (e) => {
        try {
            var message = JSON.parse(e.data);
            try {
                message.value = JSON.parse(message.value);
            }
            catch(SyntaxError) {
                // pass
            }
        }
        catch(SyntaxError) {
            var message:any = {kind: "socketError", value: e.data}
        }
        dispatchEvent(new CustomEvent(message.kind, {detail: message}));
    }
    state.ws.onerror = (e) => {
        state.status = "WebSocket error";
        m.redraw();
    }
}
function sendMessage(kind, value, target=undefined) {
    if(value.constructor.name !== "String") {
        value = JSON.stringify(value);
    }
    state.ws.send(JSON.stringify({kind: kind, value: value, sender: state.username, target: target}));
}
function sync(eventName, objectName) {
    // helper for emission and modification of serialized objects
    addEventListener("connect", (e:CustomEvent)=>{
        if(e.detail.value!==state.username){
            sendMessage(eventName, window[objectName], e.detail.value);
        }
    });
    addEventListener(eventName, (e:CustomEvent)=>{
        window[objectName] = e.detail.value;
        m.redraw()
    });
}
function login(event) {
    event.preventDefault();
    openConnection();
}
function logout() {
    state.status = "";
    state.ws.close();
}
let LoginForm = {
    view: function() {
        return [
            m("form[name=login]", {onsubmit: login}, [
                m("input[name=username]", {
                    oninput: (e)=>{state.username=e.target.value},
                    value: state.username,
                    autocomplete: "off",
                    placeholder: "pick any username!",
                }),
                m("button", {style: "display:none;"}),
            ]),
            m("img", {src: "svg/login.svg", onclick: login, title: "log in"}),
        ]
    }
}
let StatusBar = {
    view: function() {
        return [
            ! state.loggedIn
            ? LoginForm.view()
            : [
                m("img", {style: {opacity: state.chessOn?1:0.5}, src: "svg/chess.svg", onclick: ()=>state.chessOn=!state.chessOn, title: "chess"}),
                m("img", {style: {opacity: cards.length?1:0.5}, src: "svg/deal.svg", onclick: deal, title: "codenames"}),
                m("img", {style: {opacity: isEmpty(state.streams)?1:0.5}, src: "svg/stream.svg", onclick: ()=>isEmpty(state.streams)?streamingStop():streamingStart(), title: "stream"}),
                m("img", {style: {opacity: state.chatOn?1:0.5}, src: "svg/chat.svg", onclick: ()=>state.chatOn=!state.chatOn, title: "chat"}),
                m("img", {src: "svg/logout.svg", onclick: logout, title: "log out"}),
            ],
        ]
    }
}
addEventListener("socketError", (e:CustomEvent) => {
    alert(e.detail.value);
    m.redraw();
});
addEventListener("login", (e:CustomEvent) => {
    try {
        state.status = `logged in as <b>${state.username}</b>`;
        localStorage.username = JSON.stringify(state.username);
    }
    catch(error) {
        // quota exceeded on Safari?
    }
    state.loggedIn = true;
    m.redraw();
});
addEventListener("logout", (e:CustomEvent) => {
    state.loggedIn = false;
    m.redraw();
});
var statusBar = document.createElement("footer");
document.body.appendChild(statusBar);
m.mount(statusBar, StatusBar);
if(state.username){openConnection()};
