function createWebSocket() {
    var protocol = window.location.protocol == "https:" ? "wss://" : "ws://";
    var host = window.location.hostname || 'localhost';
    var uri = protocol + host + '/ws/';
    var ws = new WebSocket(uri);
    ws.onopen = onOpen;
    ws.onclose = onClose;
    ws.onmessage = onMessage;
    return ws
}

function onOpen(event) {
    ws.send('Hi! I am ' + uid);
    updateUi({});
}

function onClose(event) {
    messages.push({type: "info", sender: "server", text: "logout"});
    updateUi({});
}

function onMessage(event) {
    var components = event.data.split(': ');
    var sender = components.shift();
    var data = components.join(': ');
    try {
        var message = JSON.parse(data);
        message.sender = sender;
    }
    catch(error) {
    }

    if(event.data.match(/^Welcome!.*/)) {
        users = new Set([...users, ...data.split(',')]);
        users.delete("");
        messages.push({type: "info", sender: "server", text: "login"});
        messages.push({type: "message", sender: "server", text: `Welcome ${uid}!`});
        messages.push({type: "message", sender: "server", text: "Click [here](https://chat.roderic.ca/?) to change your username."});
        messages.push({type: "message", sender: "server", text: "Click on a username to start a video call."});
        messages.push({type: "message", sender: "server", text: "Click again to hang up."});
    }

    else if(event.data.match(/^[^:]* joined/)) {
        var name = event.data.replace(/ .*/, '');
        users.add(name);
        messages.push({type: "info", sender: name, text: "connect"});

    }

    else if(event.data.match(/^[^:]* disconnected/)) {
        var name = event.data.replace(/ .*/, '');
        users.delete(name);
        messages.push({type: "info", sender: name, text: "disconnect"});
    }

    else if(event.data.match(/^User already exists/)) {
        newUid(uid, `<${uid}> taken! Try a different alias:`);
        return
    }

    else {
        messages.push(message);
    }

    updateUi(messages[messages.length-1]);
}
