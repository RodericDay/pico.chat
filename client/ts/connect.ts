function openConnection() {
    state.ws = new WebSocket(config.wsUrl);
    state.ws.onopen = (e) => {
        state.loginError = null,
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
let Login = {
    view: function() {
        if(state.loggedIn) {
            return m("footer", [
                m("button", {onclick: ()=>{state.ws.close()}}, "logout"),
            ])
        }
        else {
            return m("form.centered[name=login]", {onsubmit: tryLogin}, [
                state.loginError ? m("div.error", state.loginError) : [],
                m("input[name=username]", {
                    onkeyup: (e)=>{state.username=e.target.value},
                    value: state.username,
                    autocomplete: "off",
                    placeholder: "username",
                }),
                m("button", "login"),
            ])
        }
    },
}
addEventListener("socketError", (e:CustomEvent) => {
    state.loginError = e.detail.value;
    m.redraw();
});
addEventListener("login", (e:CustomEvent) => {
    localStorage.username = JSON.stringify(state.username);
    state.loggedIn = true;
    m.redraw();
});
addEventListener("logout", (e:CustomEvent) => {
    state.loggedIn = false;
    m.redraw();
});
var loginRoot = document.getElementById("login");
m.mount(loginRoot, Login);
openConnection();
