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
function login(event) {
    event.preventDefault();
    openConnection();
}
function logout() {
    state.status = "";
    state.ws.close();
}
function makeButton(f) {
    if(!f.name){f=f()} /* allow toggle-able functions according to conditional */
    return m("img", {src: `svg/${f.name}.svg`, onclick: f, title: f.name})
}
let LoginForm = {
    view: function() {
        return [
        m("form[name=login]", {onsubmit: login}, [
            m("input[name=username]", {
                onkeyup: (e)=>{state.username=e.target.value},
                value: state.username,
                autocomplete: "off",
                placeholder: "username",
            }),
            m("button", {style: "display:none;"}),
        ]),
        makeButton(login),
        ]
    }
}
let StatusBar = {
    view: function() {
            return [
                state.loggedIn
                ? state.actions.slice().reverse().map(makeButton)
                : LoginForm.view(),
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
state.actions.push(logout);
var statusBar = document.createElement("footer");
document.body.appendChild(statusBar);
m.mount(statusBar, StatusBar);
if(state.username){openConnection()};
