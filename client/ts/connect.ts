let ws = null;
let wsUser = null;
function sendMessage(kind, value, target=undefined, sender=wsUser) {
    // backend will complain if sender doesn't match sign-on name
    if(value.constructor.name !== "String") {
        value = JSON.stringify(value);
    }
    ws.send(JSON.stringify({kind: kind, value: value, sender: sender, target: target}));
}
function openConnection(username, channel) {
    let isLocal = location.href.match("http://localhost:8000/");
    let wsUrl = isLocal ? "ws://localhost:9160/" : "wss://permanentsignal.com/ws/";
    ws = new WebSocket(wsUrl);
    wsUser = username;
    ws.onopen = (e) => {
        sendMessage("login", channel);
    }
    ws.onclose = (e) => {
        dispatchEvent(new CustomEvent("logout", {detail: wsUser}));
        wsUser = null;
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
    ws.onerror = (error) => {
        dispatchEvent(new CustomEvent("socketError", {detail: error}));
    }
    return ws
}
function sync(eventName, objectName) {
    // helper for emission and modification of serialized objects
    addEventListener("connect", (e:CustomEvent)=>{
        if(e.detail.value!==wsUser){
            sendMessage(eventName, window[objectName], e.detail.value);
        }
    });
    addEventListener(eventName, (e:CustomEvent)=>{
        window[objectName] = e.detail.value;
        dispatchEvent(new CustomEvent("socketEvent"));
    });
}
