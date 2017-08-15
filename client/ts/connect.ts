function openConnection() {
    state.ws = new WebSocket(config.wsUrl);
    state.ws.onopen = (e) => {
        state.loginError = null,
        state.status = `Logged in as <b>${state.username}</b>`;
        sendMessage("login", state.username);
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
        state.loginError = "WebSocket error";
        m.redraw();
    }
}
function sendMessage(kind, value, target=undefined) {
    if(value.constructor.name !== "String") {
        value = JSON.stringify(value);
    }
    state.ws.send(JSON.stringify({kind: kind, value: value, target: target}));
}
function tryLogin(event) {
    event.preventDefault();
    openConnection();
}
function logout() {
    state.ws.close();
}
let Login = {
    view: function() {
        if(state.loggedIn) {
            return m("footer", [
                m("span", m.trust(state.status)),
                ...state.actions.map((f)=>m("img", {src: `svg/${f.name}.svg`, onclick: f, alt: f.name})),
            ])
        }
        else {
            return m("form.centered[name=login]", {onsubmit: tryLogin}, [
                m("a", {href: "https://roderic.ca"}, "created by roderic"),
                m("input[name=channel]", {
                    onkeyup: (e)=>{state.channel=e.target.value},
                    value: state.channel,
                    autocomplete: "off",
                    placeholder: "channel",
                }),
                m("input[name=username]", {
                    onkeyup: (e)=>{state.username=e.target.value},
                    value: state.username,
                    autocomplete: "off",
                    placeholder: "username",
                }),
                m("button", "login"),
                m("div.error", state.loginError),
            ])
        }
    },
}
addEventListener("socketError", (e:CustomEvent) => {
    state.loginError = e.detail.value;
    m.redraw();
});
addEventListener("login", (e:CustomEvent) => {
    try {
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
var loginRoot = document.createElement("div");
document.body.appendChild(loginRoot);
m.mount(loginRoot, Login);
openConnection();
