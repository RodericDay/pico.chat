function openConnection() {
    let ws = new WebSocket(config.wsUrl);
    ws.onopen = (e) => {
        sendMessage("login", state.username);
    }
    ws.onclose = (e) => {
        dispatchEvent(new CustomEvent("disconnect", {detail: state.username}));
    }
    ws.onmessage = (e) => {
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
    return ws
}
function sendMessage(kind, value, target=undefined) {
    if(value.constructor.name !== "String") {
        value = JSON.stringify(value);
    }
    ws.send(JSON.stringify({kind: kind, value: value, target: target}));
}
addEventListener("socketError", (e:any) => {console.error(e.detail.value)});
let ws = openConnection();
